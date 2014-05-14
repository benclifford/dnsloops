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
  r <- runQ $ query $ StrLenQuery "Hello"
  putStr "test result: "
  print r
  when (r /= 5) (error "test failed")

  putStrLn "test push and pull another query"
  r <- runQ $ query $ StrLenQuery "Hello World"
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Inc 10"
  r <- runQ $ query $ Inc 10
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Dec 33"
  r <- runQ $ query $ Dec 33
  putStr "test result: "
  print r
  when (r /= 32) (error "test failed")

  putStrLn "Test a sequence of queries"
  runQ $ do
    query $ Dec 33
    query $ Inc 15
    query $ StrLenQuery "Hi"


instance Qable StrLenQuery Int where
  runQable (StrLenQuery s) = return (length s)

data StrLenQuery = StrLenQuery String deriving Show


instance Qable IntegerQuery Int where
  runQable (Inc n) = return (n + 1)
  runQable (Dec n) = return (n - 1)

data IntegerQuery = Inc Int | Dec Int deriving Show


test :: Q ()
test = return ()

