{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | The monad stack which the interpreter is written in.
type InterpreterAction final = ReaderT (RuntimeContext final) IO

data RuntimeContext final = RuntimeContext {
    _dbRef :: TVar (DB final),
    _threadRef :: TVar Integer,
    _resultIdRef :: TVar Integer
  }

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
  let context = RuntimeContext dbRef threadRef ridRef

  let (Q p) = do
             r <- m
             pushFinalResult r

  runReaderT (iRunQ p) context

  -- block until the thread count is 0, so no more
  -- activity can happen.
  liftIO $ atomically $ do
    threadCount <- readTVar (_threadRef context)
    when (threadCount > 0) retry

  db <- atomically $ readTVar $ _dbRef context
  return (finalResults db, db)

iRunQ :: QProgram final x -> InterpreterAction final [x]
iRunQ m = iRunViewedQ (view m)

-- | runs a Q in a new thread, rewrapping it in
-- a ReaderT (so sort of performing a commute of
-- reader and IO-fork...)
forkIRunQ :: QProgram final () -> InterpreterAction final ()
forkIRunQ m = do
  context <- ask
  liftIO $ atomically $ modifyTVar (_threadRef context) (+1)
  -- TODO better to use forkFinally finally rather than forkIO
  -- so as to catch any exceptions. but it appears my dev ghc base
  -- is too old
  liftIO $ forkIO $ do
    void $ runReaderT (iRunViewedQ (view m)) context
    liftIO $ atomically $ modifyTVar (_threadRef context) (+(-1))

  return ()

iRunViewedQ :: ProgramViewP (QInstruction final) x -> InterpreterAction final [x]
iRunViewedQ i = case i of

  Return v -> {-# SCC case_return #-} return [v]

  (QPushFinalResult v) :>>= k ->  {-# SCC case_final #-} do
    liftIO $ putStrLn $ "FINAL"
    ref <- askDB
    liftIO $ atomically $ modifyTVar ref $ \olddb -> olddb { finalResults = finalResults olddb ++ [v] }
    iRunQ (k ())

  (QT a) :>>= k ->  {-# SCC case_T #-} do
    v <- liftIO a
    iRunQ (k v)

  (QLaunch q) :>>= k ->  {-# SCC case_launch #-} do
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
      forkIRunQ p'
      return ()

    iRunQ (k ())

  (QRecord q a) :>>= k ->  {-# SCC case_record #-} do

    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)

    -- For putting in an atomic block this is a little bit sensitive...
    -- processNewResult modifies the database to add the new result in
    -- but that needs to happen atomically with the previousResultsForQuery

    -- is this new?
    ref <- askDB
    rid <- nextResultId
    doNewResult <- liftIO $ atomically $ do
      db <- readTVar ref
      let rs = previousResultsForQuery db q
      if not (a `elem` rs) then do
         let newResult = PreviousResult q a rid
         modifyTVar ref $ \olddb -> olddb { previousResults = (previousResults olddb) ++ [newResult] }
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
        
        let fm = mapfor (previousPulls db) $ \r@(PreviousPull q' a') ->
             case (cast q') of
               Just q'' | q'' == q -> cast a'
               _ -> Nothing
        return $ catMaybes fm
      
      liftIO $ putStrLn $ "For query " ++ (show q) ++ " there are " ++ (show $ length cbs) ++ " callbacks"
      mapM_ (\(PreviousPullContinuation f) -> iRunQ (unQ $ f a)) cbs 
     Nothing -> return ()

    iRunQ (k ())

  (QPull q) :>>= k ->  {-# SCC case_pull #-} do
    liftIO $ putStrLn $ "PULL: " ++ (show q)

    -- TODO: register some kind of continuation of k to be run when
    -- new results are encountered
    -- BUG? The rest of the program may come up with other results
    -- so I need to register the new callback before running the
    -- rest of the program for existing results
    -- TODO: make a test case to exercise this subtlety

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull
    let callback = PreviousPull q (PreviousPullContinuation (\a -> Q $ k a >> return ()))

    ref <- askDB

    rs <- liftIO $ atomically $ do
      modifyTVar ref $ \olddb -> olddb { previousPulls = (previousPulls olddb) ++ [callback] }

      -- run the rest of the program for every result that is
      -- already known. This may results in the above callback
      -- being invoked, if relevant pushes happen.
      db <- readTVar ref
      return $ previousResultsForQuery db q

    liftIO $ putStrLn $ "Processing previous results for query " ++ (show q)

    rrs <- mapM (\v -> iRunQ (k v)) rs

    return $ concat rrs

  -- | mplus should follow the left distribution law
  MPlus l ->  {-# SCC case_mplus #-} do
    rs <- mapM iRunViewedQ l

    return $ concat rs

unQ (Q p) = p

previousResultsForQuery :: (Qable q) => DB x -> q -> [Answer q]
previousResultsForQuery db q  = do
  let fm = mapfor (previousResults db) $ \(PreviousResult q' a' _) -> 
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
   in catMaybes fm

-- TODO: can the PreviousResult and PreviousPull types be
-- turned into some query-referenced shared type with a
-- parameter for the RHS? Then the above two functions
-- could share most of the implementation.

askDB :: InterpreterAction x (TVar (DB x))
askDB = _dbRef <$> ask

nextResultId :: InterpreterAction x Integer
nextResultId = do
  tvar <- _resultIdRef <$> ask
  liftIO $ atomically $ do
    n <- readTVar tvar
    modifyTVar tvar (+1)
    return n

-- TODO: single threaded provenance logging output so that multi-line output does not interleave
