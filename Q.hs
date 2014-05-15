{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Q where

import Control.Applicative
import Control.Monad
import Control.MonadPlus.Operational
import Control.Monad.State.Strict

import Data.Dynamic
import Data.Maybe
import Data.Typeable

-- * The monad on which everything runs

class (Show q, Show a, Eq q, Eq a, Typeable q, Typeable a)
  => Qable q a | q -> a where

  -- | Run the query. The answer type does not appear in
  -- the type signature because there is nothing magic
  -- about the answer type: running the query might push
  -- many different answers to many different queries in
  -- addition to this query.
  runQable :: q -> Q final ()

-- | QInstruction are the simple instructions to be used
-- inside the operational monad.

type Q final x = ProgramP (QInstruction final) x

data QInstruction final x where
  -- runs an action in the underlying IO monad
  QT :: IO r -> QInstruction final r
  -- | Asks for a query to be made without waiting for any answers
  QLaunch :: Qable q a => q -> QInstruction final ()
  -- | Records an answer to a query (which might not have been launched...)
  QRecord :: Qable q a => q -> a -> QInstruction final ()
  -- | QPull asks for the responses to a query without launching that
  --   query. Often it should follow a push, I think, but sometimes not -
  --   for example when looking for cached values.
  -- but what should the type of this look like?
  QPull :: (Typeable final, Qable q a) => q -> QInstruction final a
  QPushFinalResult :: final -> QInstruction final ()

-- * DB bits

data PreviousResult where
  PreviousResult :: forall q . forall a . (Qable q a) => q -> a -> PreviousResult

instance Show PreviousResult where 
  show (PreviousResult q a) = "Query " ++ (show q) ++ " => " ++ (show a)

-- | a previous pull request. We can restart but only a computation that
-- eventually ends with no useful value returned. Potentially we could be
-- returning a value and discarding it, but using () makes it clearer
-- in the type system that there cannot be a value.
data PreviousPull final where
  PreviousPull :: forall final . forall q . forall a . (Typeable final, Qable q a) => q -> PPQ final a -> PreviousPull final

-- | this is a wrapper that gives a Typeable instance for a -> Q ()
-- because using that type on its own wasn't Typeable
data PPQ final a = PPQ (a -> Q final ()) deriving Typeable


data DB final = DB {
    previousResults :: [PreviousResult],
    previousPulls :: [PreviousPull final],
    finalResults :: [final]
  }

emptyDB = DB {
    previousResults = [],
    previousPulls = [],
    finalResults = []
  }



-- * The interpreter

runQ :: Q final final -> IO [final]
runQ m = do
  let p = do
             r <- m
             pushFinalResult r
  db <- execStateT (iRunQ p) emptyDB
  return $ finalResults db

iRunQ :: Q final x -> StateT (DB final) IO [x]
iRunQ m = iRunViewedQ (view m)

iRunViewedQ :: ProgramViewP (QInstruction final) x -> StateT (DB final) IO [x]
iRunViewedQ i = case i of

  Return v -> return [v]

  (QPushFinalResult v) :>>= k -> do
    liftIO $ putStrLn "Pushing a final result"
    modify $ \olddb -> olddb { finalResults = finalResults olddb ++ [v] }
    iRunQ (k ())

  (QT a) :>>= k -> do
    v <- lift $ a
    iRunQ (k v)

  (QLaunch q) :>>= k -> do
    liftIO $ putStrLn "LAUNCH"
    liftIO $ putStrLn $ "Launching query " ++ (show q)
    iRunQ (runQable q)
    iRunQ (k ())

  (QRecord q a) :>>= k -> do
    liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)

    -- is this new?
    rs <- previousResultsForQuery q
    if not (a `elem` rs) then processNewResult q a
     else liftIO $ putStrLn "Duplicate result. Not processing as new result"
    iRunQ (k ())

  (QPull q) :>>= k -> do
    liftIO $ putStrLn "PULL"
    dumpPreviousResults


    -- TODO: register some kind of continuation of k to be run when
    -- new results are encountered
    -- BUG? The rest of the program may come up with other results
    -- so I need to register the new callback before running the
    -- rest of the program for existing results
    -- TODO: make a test case to exercise this subtlety

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull
    let callback = PreviousPull q (PPQ (\a -> k a >> return ()))

    modify $ \olddb -> olddb { previousPulls = (previousPulls olddb) ++ [callback] }

    -- run the rest of the program for every result that is
    -- already known. This may results in the above callback
    -- being invoked, if relevant pushes happen.
    liftIO $ putStrLn $ "Processing previous results for query " ++ (show q)
    rs <- previousResultsForQuery q
    rrs <- mapM (\v -> iRunQ (k v)) rs

    return $ concat rrs

  -- | mplus should follow the left distribution law
  MPlus l -> do
    rs <- mapM iRunViewedQ l

    return $ concat rs

processNewResult :: (Qable q a) => q -> a -> StateT (DB x) IO ()
processNewResult q v = do
  dumpPreviousResults
  -- TODO: find any existing Pulls that have requested results
  -- from this query.

  -- There is probably ordering subtlety here about when the
  -- callback list is acquired, vs when the callbacks are made,
  -- vs when the result is added as a previous result.
  -- TODO: ^ tests to check that subtlety

  modify $ \olddb -> olddb { previousResults = (previousResults olddb) ++ [PreviousResult q v] }

  cbs <- previousPullsForQuery q

  liftIO $ putStrLn $ "For query " ++ (show q) ++ " there are " ++ (show $ length cbs) ++ " callbacks"
  mapM_ (\(PPQ f) -> iRunQ (f v)) cbs 

  dumpPreviousResults


previousResultsForQuery :: (Qable q a) => q -> StateT (DB x) IO [a]
previousResultsForQuery q  = do
  db <- get
  let fm = for (previousResults db) $ \(PreviousResult q' a') -> 
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
  return $ catMaybes fm

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull

previousPullsForQuery :: (Qable q a) => q -> StateT (DB final) IO [PPQ final a]
previousPullsForQuery q = do
  db <- get
  let fm = for (previousPulls db) $ \r@(PreviousPull q' a') ->
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
  return $ catMaybes fm

-- TODO: can the PreviousResult and PreviousPull types be
-- turned into some query-referenced shared type with a
-- parameter for the RHS? Then the above two functions
-- could share most of the implementation.

for = flip map

dumpPreviousResults :: StateT (DB x) IO ()
dumpPreviousResults = do
      liftIO $ putStrLn "Previous results: "
      db <- get
      liftIO $ print $ previousResults db

unsafeQT :: IO x -> Q final x
unsafeQT a = singleton $ QT a

qlaunch q = singleton $ QLaunch q

qrecord q a = singleton $ QRecord q a

qpull q = singleton $ QPull q

query q = qlaunch q *> qpull q

pushFinalResult v = singleton $ QPushFinalResult v
