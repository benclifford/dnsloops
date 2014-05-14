{-# LANGUAGE MultiParamTypeClasses #-}

module Test where

import Control.Monad

import Q

main = do
  putStrLn "dnsloops test"

  putStrLn "test return ()"
  runQ $ return ()

  putStrLn "test push a query"
  runQ $ qpush $ StrLenQuery "Hello"


  putStrLn "test push and pull a query"
  r <- runQ $ do
    let query = StrLenQuery "Hello"
    qpush query
    qpull query
  putStr "test result: "
  print r
  when (r /= 5) (error "test failed")

instance Qable StrLenQuery Int where
  runQable (StrLenQuery s) = return (length s)

data StrLenQuery = StrLenQuery String deriving Show

test :: Q ()
test = return ()

