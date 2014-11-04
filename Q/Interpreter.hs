{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Q.Interpreter where

import Control.Applicative
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar, MVar())
import Control.Concurrent.STM (atomically, modifyTVar, newTVar, readTVar, retry, TVar() )
import Control.Monad
import Control.Monad.IO.Class
import Control.MonadPlus.Operational
import Control.Monad.Reader

import Data.Dynamic
import Data.Maybe
import Data.Typeable

import Lib
import Q
import Q.DB

-- | The monad stack which the interpreter is written in.
-- 
--   There is a runtime context that consists of TVars
--   that contain global variables shared by every execution.
--
--   Each interpreter action should also take place in a
--   local context that needs to be formed differently for
--   each step in the program.


type InterpreterAction final = ReaderT LocalContext (ReaderT (GlobalContext final) IO)

data GlobalContext final = GlobalContext {
    _dbRef :: TVar (DB final),
    _threadRef :: TVar Integer,
    _resultIdRef :: TVar Integer
  }

-- | Local context for an instruction, forming
--   something like a stack trace that will then
--   be attached to each result.
--   Eventually might want to record multiple
--   contexts for a result, but for now, only one
--   (probably the first encountered in realtime)
--   will be recorded.
type LocalContext = ResultContext

-- * The interpreter

evalQ :: Q final final -> IO [final]
evalQ m = do
  (results, db) <- runQ m
  return results

runQ  :: Q final final -> IO ([final], DB final)
runQ m = do

  dbRef <- atomically (newTVar emptyDB)
  threadRef <- atomically $ newTVar 0
  ridRef <- atomically $ newTVar 10000
  let globalContext = GlobalContext
        { _dbRef = dbRef
        , _threadRef = threadRef
        , _resultIdRef = ridRef
        }

  let (Q p) = do
             r <- m
             pushFinalResult r

  runReaderT (runReaderT (runQProgram p) (ResultContext ["root run"])) globalContext

  -- block until the thread count is 0, so no more
  -- activity can happen.
  liftIO $ atomically $ do
    threadCount <- readTVar (_threadRef globalContext)
    when (threadCount > 0) retry

  db <- atomically $ readTVar $ _dbRef globalContext
  return (finalResults db, db)

runQProgram :: QProgram final x -> InterpreterAction final [x]
runQProgram m = iRunViewedQ (view m)

-- | runs a Q in a new thread, rewrapping it in
-- a ReaderT (so sort of performing a commute of
-- reader and IO-fork...)
forkQProgram :: QProgram final () -> InterpreterAction final ()
forkQProgram m = do
  globalContext <- askGlobalContext
  localContext <- askLocalContext
  liftIO $ atomically $ modifyTVar (_threadRef globalContext) (+1)
  -- TODO better to use forkFinally finally rather than forkIO
  -- so as to catch any exceptions. but it appears my dev ghc base
  -- is too old
  liftIO $ forkIO $ do
    void $ runReaderT (runReaderT (runQProgram m) localContext) globalContext
    liftIO $ atomically $ modifyTVar (_threadRef globalContext) (+(-1))

  return ()

iRunViewedQ :: ProgramViewP (QInstruction final) x -> InterpreterAction final [x]
iRunViewedQ i = case i of

  Return v -> {-# SCC case_return #-} return [v]

  (QPushFinalResult v) :>>= k ->  {-# SCC case_final #-} do
    liftIO $ putStrLn $ "FINAL"
    ref <- askDB
    liftIO $ atomically $ modifyTVar ref $ \olddb -> olddb { finalResults = finalResults olddb ++ [v] }
    runQProgram (k ())

  (QT a) :>>= k ->  {-# SCC case_T #-} do
    v <- liftIO a
    runQProgram (k v)

  (QLaunch q) :>>= k ->  {-# SCC case_launch #-} do

    -- TODO: maybe want to store the current context associated with
    -- the launch. We don't need to change the local context for k,
    -- though, because the launch does not return any information
    -- for k to use.

    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "LAUNCH: " ++ (show q)
    let newLaunchPL = PreviousLaunch q

    ref <- askDB

    -- Atomically add to the launched list, and if it wasn't already there,
    -- run it
    toRun <- liftIO $ atomically $ do
      prevs <- previousLaunches <$> readTVar ref
      if not (newLaunchPL `elem` prevs) then do
        modifyTVar ref 
                 $ \olddb -> olddb { previousLaunches = (previousLaunches olddb) ++ [newLaunchPL] }
        return True
       else do
        return False

    when toRun $ do
      liftIO $ putStrLn $ "Launching query " ++ (show q)
      let p' = unQ $ runQable q
      forkQProgram p'
      return ()

    runQProgram (k ())

  (QRecord q a) :>>= k ->  {-# SCC case_record #-} do

    -- TODO: maybe want to record the current context against this
    -- recorded information for use later. The record does not
    -- return any information so we do not need to change the
    -- context for k.

    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)

    -- For putting in an atomic block this is a little bit sensitive...
    -- processNewResult modifies the database to add the new result in
    -- but that needs to happen atomically with the previousResultsForQuery

    -- is this new?
    ref <- askDB
    rid <- nextResultId
    localContext <- askLocalContext
    doNewResult <- liftIO $ atomically $ do
      db <- readTVar ref
      let rs = previousResultsForQuery db q
      if not (a `elem` rs) then do
         let newResult = PreviousResult q a rid
         modifyTVar ref $ \olddb -> olddb
           { previousResults = (previousResults olddb) ++ [newResult]
           , resultContexts = (resultContexts olddb) ++ [(rid, localContext)]
           }
         return (Just db)

       else return Nothing

    case doNewResult of
     Just db -> do
      liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)
        -- TODO: find any existing Pulls that have requested results
        -- from this query.
      
        -- There is probably ordering subtlety here about when the
        -- callback list is acquired, vs when the callbacks are made,
        -- vs when the result is added as a previous result.
        -- TODO: ^ tests to check that subtlety
      
        -- TODO: THREADING BUG? I am unsure, because I haven't really done
        -- any reasoning about this, if this needs to happen within the
        -- atomic block that calls processNewResult? I think it might need
        -- to... does the "previous pulls" list need to be generated at the
        -- same instant in db-time as the new result is added to the database?
        -- I think yes.
        -- In which case its probably better to get this function and
        -- processNewResult all expanded out in the calling function, and
        -- then potentially refactor afterwards.
      
      cbs <- do
        
        let fm = mapfor (previousPulls db) $ \r@(PreviousPull q' a' c') ->
             case (cast q') of
               Just q'' | q'' == q -> (c',) <$> cast a'
               _ -> Nothing
        return $ catMaybes fm
      
      liftIO $ putStrLn $ "For query " ++ (show q) ++ " there are " ++ (show $ length cbs) ++ " callbacks"
      prefixLocalContext
        ("For query " ++ (show q) ++ " pulling answer " ++ (show a) ++ " (path A)")
       $ mapM_ (\(ctx, PreviousPullContinuation f) -> concatLocalContext ctx $ runQProgram (unQ $ f a)) cbs 
-- TODO: XXX - use ctx context to augment current context somehow (we want access to both contexts - do I just append them or can there be more interesting tree-like description?
     Nothing -> return ()

    runQProgram (k ())

  (QPull q) :>>= k ->  {-# SCC case_pull #-} do

    liftIO $ putStrLn $ "PULL: " ++ (show q)

    -- TODO: there will be some context associated with
    -- the return value of this pull which we should
    -- make available for k.
    -- For now, record the fact that a pull with
    -- a specific query was invoked.   
    let additionalContext = "Pulled a result from query: " ++ (show q)
    prefixLocalContext additionalContext $ do
    -- TODO: register some kind of continuation of k to be run when
    -- new results are encountered
    -- BUG? The rest of the program may come up with other results
    -- so I need to register the new callback before running the
    -- rest of the program for existing results
    -- TODO: make a test case to exercise this subtlety

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull
      localContext <- askLocalContext
      let callback = PreviousPull q (PreviousPullContinuation (\a -> Q $ k a >> return ())) localContext

      ref <- askDB

      rs <- liftIO $ atomically $ do
        modifyTVar ref $ \olddb -> olddb { previousPulls = (previousPulls olddb) ++ [callback] }

        -- run the rest of the program for every result that is
        -- already known. This may results in the above callback
        -- being invoked, if relevant pushes happen.
        db <- readTVar ref
        return $ previousResultsForQuery db q

      liftIO $ putStrLn $ "Processing previous results for query " ++ (show q)

      rrs <- mapM
        (\v -> prefixLocalContext ("For query " ++ (show q) ++ " pulling answer " ++ (show v) ++ " (path B)") $ runQProgram (k v))
        rs

      return $ concat rrs

  -- | mplus should follow the left distribution law
  MPlus l ->  {-# SCC case_mplus #-} do
    rs <- mapM iRunViewedQ l

    return $ concat rs

  (QContext ctx) :>>= k -> {-# SCC case_context #-} do
    liftIO $ putStrLn $ "Adding context " ++ ctx
    prefixLocalContext ctx $ runQProgram (k ())

concatLocalContext (ResultContext ctxs) a = replaceLocalContext
  (\(ResultContext ctxs') -> ResultContext (ctxs ++ ctxs'))
  a

prefixLocalContext ctx a = replaceLocalContext
  (\(ResultContext ctxs) -> ResultContext (ctx:ctxs))
  a

replaceLocalContext :: (LocalContext -> LocalContext) -> InterpreterAction final a -> InterpreterAction final a
replaceLocalContext f a = do
  local f a

unQ (Q p) = p

-- TODO: can the PreviousResult and PreviousPull types be
-- turned into some query-referenced shared type with a
-- parameter for the RHS? Then the above two functions
-- could share most of the implementation.

askDB :: InterpreterAction final (TVar (DB final))
askDB = _dbRef <$> askGlobalContext

askLocalContext :: InterpreterAction x LocalContext
askLocalContext = ask

askGlobalContext :: InterpreterAction final (GlobalContext final)
askGlobalContext = lift $ ask

nextResultId :: InterpreterAction x Integer
nextResultId = do
  tvar <- _resultIdRef <$> askGlobalContext
  liftIO $ atomically $ do
    n <- readTVar tvar
    modifyTVar tvar (+1)
    return n

-- TODO: single threaded provenance logging output so that multi-line output does not interleave
