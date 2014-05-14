{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

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

  -- | Run the query in the underlying monad,
  -- giving an answer
  runQable :: q -> IO a

-- | QInstruction are the simple instructions to be used
-- inside the operational monad.

type Q x = ProgramP QInstruction x

data QInstruction x where
  -- runs an action in the underlying IO monad
  QT :: IO x -> QInstruction x
  -- | QPush asks for a query to be made without waiting for any answers
  QPush :: Qable q a => q -> QInstruction ()
  -- | QPull asks for the responses to a query without launching that
  --   query. Often it should follow a push, I think, but sometimes not -
  --   for example when looking for cached values.
  -- but what should the type of this look like?
  QPull :: Qable q a => q -> QInstruction a

-- * DB bits

data PreviousResult where
  PreviousResult :: forall q . forall a . (Qable q a) => q -> a -> PreviousResult

instance Show PreviousResult where 
  show (PreviousResult q a) = "Query " ++ (show q) ++ " => " ++ (show a)

data DB = DB { previousResults :: [PreviousResult] }

emptyDB = DB {
    previousResults = []
  }



-- * The interpreter

runQ :: Q x -> IO [x]
runQ m = evalStateT (iRunQ m) emptyDB

iRunQ :: Q x -> StateT DB IO [x]
iRunQ m = iRunViewedQ (view m)

iRunViewedQ :: ProgramViewP QInstruction x -> StateT DB IO [x]
iRunViewedQ i = case i of

  Return v -> return [v]

  (QT a) :>>= k -> do
    v <- lift $ a
    iRunQ (k v)

  (QPush q) :>>= k -> do
    liftIO $ putStrLn "PUSH"
    liftIO $ putStrLn $ "Log query " ++ (show q) ++ " for future use"
    -- TODO: for now, perform the query immediately in IO
    -- In future, I'll want to be able to run it inside Q,
    -- sharing the DB, so I can't run it as a direct
    -- invocation.
    liftIO $ putStrLn $ "Running uncached query " ++ (show q)
    v <- liftIO $ runQable q
    liftIO $ putStrLn $ "Value returned from query: " ++ (show v)

    -- is this new?
    rs <- previousResultsForQuery q
    if not (v `elem` rs) then do

    -- TODO: find any existing Pulls that have requested results
    -- from this query

      liftIO $ putStrLn "Previous db: "
      db <- get
      liftIO $ print $ previousResults db
      modify (\olddb -> olddb { previousResults = (previousResults db) ++ [PreviousResult q v] })
      liftIO $ putStrLn "New db: "
      newdb <- get
      liftIO $ print $ previousResults newdb
     else do
      liftIO $ putStrLn "Duplicate result. Not processing as new result"
    iRunQ (k ())

  (QPull q) :>>= k -> do
    liftIO $ putStrLn "PULL"
    liftIO $ putStrLn "Previous results are: "
    db <- get
    liftIO $ print $ previousResults db

    -- TODO: look for previously cached results of the correct query
    rs <- previousResultsForQuery q

    -- TODO: register some kind of continuation of k to be run when
    -- new results are encountered

    rrs <- mapM (\v -> iRunQ (k v)) rs
    return $ concat rrs

  MPlus l -> do
    rs <- mapM iRunViewedQ l

    return $ concat rs

previousResultsForQuery :: (Qable q a) => q -> StateT DB IO [a]
previousResultsForQuery q  = do
  db <- get
  let
   for = flip map
   fm = for (previousResults db) $ \(PreviousResult q' a') -> 
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
  return $ catMaybes fm

unsafeQT :: IO x -> Q x
unsafeQT a = singleton $ QT a

qpush q = singleton $ QPush q

qpull q = singleton $ QPull q

query q = qpush q *> qpull q

