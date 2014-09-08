{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q.Interpreter where

import Control.Applicative
import Control.Concurrent.STM (atomically, modifyTVar, newTVar, readTVar, TVar() )
import Control.Monad
import Control.Monad.IO.Class
import Control.MonadPlus.Operational
import Control.Monad.Reader

import Data.Dynamic
import Data.Maybe
import Data.Typeable

import Lib
import Q

-- * The interpreter

evalQ :: Q final final -> IO [final]
evalQ m = do
  (results, db) <- runQ m
  return results

runQ  :: Q final final -> IO ([final], DB final)
runQ m = do
  let (Q p) = do
             r <- m
             pushFinalResult r
  dbRef <- atomically (newTVar emptyDB)
  runReaderT (iRunQ p) dbRef
  db <- atomically $ readTVar dbRef
  return (finalResults db, db)

iRunQ :: QProgram final x -> ReaderT (TVar (DB final)) IO [x]
iRunQ m = iRunViewedQ (view m)

iRunViewedQ :: ProgramViewP (QInstruction final) x -> ReaderT (TVar (DB final)) IO [x]
iRunViewedQ i = case i of

  Return v -> return [v]

  (QPushFinalResult v) :>>= k -> do
    liftIO $ putStrLn $ "FINAL"
    ref <- ask
    liftIO $ atomically $ modifyTVar ref $ \olddb -> olddb { finalResults = finalResults olddb ++ [v] }
    iRunQ (k ())

  (QT a) :>>= k -> do
    v <- lift $ a
    iRunQ (k v)

  (QLaunch q) :>>= k -> do
    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "LAUNCH: " ++ (show q)
    let newLaunchPL = PreviousLaunch q

    ref <- ask

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
      iRunQ p'
      return ()

    iRunQ (k ())

  (QRecord q a) :>>= k -> do

    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)

    -- For putting in an atomic block this is a little bit sensitive...
    -- processNewResult modifies the database to add the new result in
    -- but that needs to happen atomically with the previousResultsForQuery

    -- is this new?
    ref <- ask
    doNewResult <- liftIO $ atomically $ do
      db <- readTVar ref
      let rs = previousResultsForQuery' db q
      if not (a `elem` rs) then do
       
         modifyTVar ref $ \olddb -> olddb { previousResults = (previousResults olddb) ++ [PreviousResult q a] }
         return (Just db)

       else return Nothing

    case doNewResult of
     Just db -> do
      liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)
        -- dumpPreviousResults
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
      mapM_ (\(PPQ f) -> iRunQ (unQ $ f a)) cbs 
     Nothing -> return ()

    iRunQ (k ())

  (QPull q) :>>= k -> do
    liftIO $ putStrLn $ "PULL: " ++ (show q)
    -- dumpPreviousResults


    -- TODO: register some kind of continuation of k to be run when
    -- new results are encountered
    -- BUG? The rest of the program may come up with other results
    -- so I need to register the new callback before running the
    -- rest of the program for existing results
    -- TODO: make a test case to exercise this subtlety

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull
    let callback = PreviousPull q (PPQ (\a -> Q $ k a >> return ()))

    ref <- ask

    rs <- liftIO $ atomically $ do
      modifyTVar ref $ \olddb -> olddb { previousPulls = (previousPulls olddb) ++ [callback] }

      -- run the rest of the program for every result that is
      -- already known. This may results in the above callback
      -- being invoked, if relevant pushes happen.
      db <- readTVar ref
      return $ previousResultsForQuery' db q

    liftIO $ putStrLn $ "Processing previous results for query " ++ (show q)

    rrs <- mapM (\v -> iRunQ (k v)) rs

    return $ concat rrs

  -- | mplus should follow the left distribution law
  MPlus l -> do
    rs <- mapM iRunViewedQ l

    return $ concat rs

unQ (Q p) = p

previousResultsForQuery' :: (Qable q) => DB x -> q -> [Answer q]
previousResultsForQuery' db q  = do
  let fm = mapfor (previousResults db) $ \(PreviousResult q' a') -> 
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
   in catMaybes fm

-- TODO: can the PreviousResult and PreviousPull types be
-- turned into some query-referenced shared type with a
-- parameter for the RHS? Then the above two functions
-- could share most of the implementation.

dumpPreviousResults :: ReaderT (TVar (DB x)) IO ()
dumpPreviousResults = do
      liftIO $ putStrLn "Previous results: "
      ref <- ask
      db <- liftIO $ atomically $ readTVar ref
      liftIO $ print $ previousResults db

