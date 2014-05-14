{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Q where

import Control.Applicative
import Control.Monad
import Control.Monad.Operational

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

runQ :: Q x -> IO x
runQ m = case view m of 
  Return v -> return v
  (QT a) :>>= k -> do
    v <- a
    runQ (k v)
  (QPush q) :>>= k -> do
    putStrLn $ "TODO: log query " ++ (show q) ++ " for future use"
    runQ (k ())
  (QPull q) :>>= k -> do
    putStrLn $ "Running uncached query " ++ (show q)
    v <- runQable q
    putStrLn $ "Value returned from query: " ++ (show v)
    runQ (k v)

unsafeQT :: IO x -> Q x
unsafeQT a = singleton $ QT a

qpush q = singleton $ QPush q

qpull q = singleton $ QPull q

query q = qpush q *> qpull q

