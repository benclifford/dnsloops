{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.MonadPlus.Operational
import Control.Monad.State.Strict

import Data.Dynamic
import Data.Maybe
import Data.Typeable

import Lib

-- * The monad on which everything runs

class (Show q, Show (Answer q), Eq q, Eq (Answer q), Typeable q, Typeable (Answer q))
  => Qable q where

  type Answer q
  -- | Run the query. The answer type does not appear in
  -- the type signature because there is nothing magic
  -- about the answer type: running the query might push
  -- many different answers to many different queries in
  -- addition to this query.
  runQable :: Typeable final => q -> Q final ()

-- | QInstruction are the simple instructions to be used
-- inside the operational monad.

type QProgram final x = ProgramP (QInstruction final) x
newtype Q final x = Q (QProgram final x)
  deriving (Applicative, Alternative, Functor, Monad,
            MonadPlus)

instance MonadIO (Q final) where
  liftIO a = Q $ singleton $ QT a

data QInstruction final x where
  -- runs an action in the underlying IO monad
  QT :: IO r -> QInstruction final r
  -- | Asks for a query to be made without waiting for any answers
  QLaunch :: (Typeable final, Qable q) => q -> QInstruction final ()
  -- | Records an answer to a query (which might not have been launched...)
  QRecord :: (Typeable final, Qable q) => q -> (Answer q) -> QInstruction final ()
  -- | QPull asks for the responses to a query without launching that
  --   query. Often it should follow a push, I think, but sometimes not -
  --   for example when looking for cached values.
  -- but what should the type of this look like?
  -- TODO: I do not entirely remember why I need to 
  -- force typeable to be final here. It is used in PreviousPull
  -- somehow. Investigate if it is necessary (because it pushes
  -- that typeable constraint further out into client programs
  -- sometimes in a way that seems superficially unnecessary)

  QPull :: (Typeable final, Qable q) => q -> QInstruction final (Answer q)
  QPushFinalResult :: final -> QInstruction final ()

-- * DB bits

data PreviousLaunch where
  PreviousLaunch :: forall q . (Qable q) => q -> PreviousLaunch

deriving instance Show PreviousLaunch

instance Eq PreviousLaunch where
  (PreviousLaunch l) == (PreviousLaunch r) =
    Just l == cast r

data PreviousResult where
  PreviousResult :: forall q . (Qable q) => q -> (Answer q) -> PreviousResult

instance Show PreviousResult where 
  show (PreviousResult q a) = "Query " ++ (show q) ++ " => " ++ (show a)

-- | a previous pull request. We can restart but only a computation that
-- eventually ends with no useful value returned. Potentially we could be
-- returning a value and discarding it, but using () makes it clearer
-- in the type system that there cannot be a value.
data PreviousPull final where
  PreviousPull :: forall final . forall q . (Typeable final, Qable q) => q -> PPQ final (Answer q) -> PreviousPull final

-- | this is a wrapper that gives a Typeable instance for a -> Q ()
-- because using that type on its own wasn't Typeable
data PPQ final a = PPQ (a -> Q final ()) deriving Typeable

data DB final = DB {
    previousLaunches :: [PreviousLaunch],
    previousResults :: [PreviousResult],
    previousPulls :: [PreviousPull final],
    finalResults :: [final]
  }

emptyDB = DB {
    previousLaunches = [],
    previousResults = [],
    previousPulls = [],
    finalResults = []
  }



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
  db <- execStateT (iRunQ p) emptyDB
  return (finalResults db, db)

iRunQ :: QProgram final x -> StateT (DB final) IO [x]
iRunQ m = iRunViewedQ (view m)

iRunViewedQ :: ProgramViewP (QInstruction final) x -> StateT (DB final) IO [x]
iRunViewedQ i = case i of

  Return v -> return [v]

  (QPushFinalResult v) :>>= k -> do
    liftIO $ putStrLn $ "FINAL"
    modify $ \olddb -> olddb { finalResults = finalResults olddb ++ [v] }
    iRunQ (k ())

  (QT a) :>>= k -> do
    v <- lift $ a
    iRunQ (k v)

  (QLaunch q) :>>= k -> do
    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "LAUNCH: " ++ (show q)
    prevs <- previousLaunches <$> get
    -- liftIO $ putStr "Previously launched queries: "
    -- liftIO $ print prevs
    let newLaunchPL = PreviousLaunch q
    if not (newLaunchPL `elem` prevs) then do
      liftIO $ putStrLn $ "Launching query " ++ (show q)
      modify $ \olddb -> olddb { previousLaunches = (previousLaunches olddb) ++ [newLaunchPL] }
      let (Q p) = runQable q
      iRunQ p
      return ()
     else do
      -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "Duplicate query submission. Not launching again."
      return ()
    iRunQ (k ())

  (QRecord q a) :>>= k -> do

    -- TODO: maybe still want to log this in debug mode? liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)

    -- is this new?
    rs <- previousResultsForQuery q
    if not (a `elem` rs) then do
       
       liftIO $ putStrLn $ "Recording result: query " ++ (show q) ++ " => " ++ (show a)
       processNewResult q a
     else return () -- TODO: maybe want to log this in debug mode? liftIO $ putStrLn "Duplicate result. Not processing as new result"
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

processNewResult :: (Qable q) => q -> (Answer q) -> StateT (DB x) IO ()
processNewResult q v = do
  -- dumpPreviousResults
  -- TODO: find any existing Pulls that have requested results
  -- from this query.

  -- There is probably ordering subtlety here about when the
  -- callback list is acquired, vs when the callbacks are made,
  -- vs when the result is added as a previous result.
  -- TODO: ^ tests to check that subtlety

  modify $ \olddb -> olddb { previousResults = (previousResults olddb) ++ [PreviousResult q v] }

  cbs <- previousPullsForQuery q

  liftIO $ putStrLn $ "For query " ++ (show q) ++ " there are " ++ (show $ length cbs) ++ " callbacks"
  mapM_ (\(PPQ f) -> iRunQ (unQ $ f v)) cbs 

  -- dumpPreviousResults

unQ (Q p) = p

previousResultsForQuery :: (Qable q) => q -> StateT (DB x) IO [Answer q]
previousResultsForQuery q  = do
  db <- get
  let fm = mapfor (previousResults db) $ \(PreviousResult q' a') -> 
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
  return $ catMaybes fm

--  PreviousPull :: forall q . forall a . (Qable q a) => q -> (a -> Q ()) -> PreviousPull

previousPullsForQuery :: (Qable q) => q -> StateT (DB final) IO [PPQ final (Answer q)]
previousPullsForQuery q = do
  db <- get
  let fm = mapfor (previousPulls db) $ \r@(PreviousPull q' a') ->
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
  return $ catMaybes fm

-- TODO: can the PreviousResult and PreviousPull types be
-- turned into some query-referenced shared type with a
-- parameter for the RHS? Then the above two functions
-- could share most of the implementation.

dumpPreviousResults :: StateT (DB x) IO ()
dumpPreviousResults = do
      liftIO $ putStrLn "Previous results: "
      db <- get
      liftIO $ print $ previousResults db


qlaunch :: (Typeable final, Qable q) => q -> Q final (Answer q)
qlaunch q = Q $ (singleton $ QLaunch q) *> empty

qrecord q a = Q $ singleton $ QRecord q a

qpull q = Q $ singleton $ QPull q

query q = qlaunch q <|> qpull q

pushFinalResult v = Q $ singleton $ QPushFinalResult v
