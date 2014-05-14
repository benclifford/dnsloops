{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-X LANGUAGE ImpredicativeTypes #-}

module Q where

import Control.Applicative
import Control.Monad
import Control.Monad.Operational

import Data.Dynamic

-- * The monad on which everything runs

class (Show q, Show a) => Qable q a | q -> a where

  -- | Run the query in the underlying monad,
  -- giving an answer
  runQable :: q -> IO a

-- | QInstruction are the simple instructions to be used
-- inside the operational monad.

type Q x = Program QInstruction x

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


data PreviousResult where
  PreviousResult :: forall q . forall a . (Qable q a) => q -> a -> PreviousResult

instance Show PreviousResult where 
  show (PreviousResult q a) = "Query " ++ (show q) ++ " => " ++ (show a)

data DB = DB { previousResults :: [PreviousResult] }

emptyDB = DB {
    previousResults = []
  }

runQ :: Q x -> IO x
runQ m = iRunQ emptyDB m

iRunQ :: DB -> Q x -> IO x
iRunQ db m = case view m of 

  Return v -> return v

  (QT a) :>>= k -> do
    v <- a
    iRunQ db (k v)
  (QPush q) :>>= k -> do
    putStrLn "PUSH"
    putStrLn $ "Log query " ++ (show q) ++ " for future use"
    -- TODO: for now, perform the query immediately in IO
    -- In future, I'll want to be able to run it inside Q,
    -- sharing the DB, so I can't run it as a direct
    -- invocation.
    putStrLn $ "Running uncached query " ++ (show q)
    v <- runQable q
    putStrLn $ "Value returned from query: " ++ (show v)
    putStrLn "Previous db: "
    print $ previousResults db
    let newdb = db { previousResults = (previousResults db) ++ [PreviousResult q v] }
    putStrLn "New db: "
    print $ previousResults newdb
    iRunQ newdb (k ())

  (QPull q) :>>= k -> do
    putStrLn "PULL"
    putStrLn "Previous results are: "
    print $ previousResults db
    putStrLn $ "Running uncached query " ++ (show q)
    v <- runQable q
    putStrLn $ "Value returned from query: " ++ (show v)
    iRunQ db (k v)

unsafeQT :: IO x -> Q x
unsafeQT a = singleton $ QT a

qpush q = singleton $ QPush q

qpull q = singleton $ QPull q

query q = qpush q *> qpull q

