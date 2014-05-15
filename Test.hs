{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test where

import Data.List
import Data.Typeable
import Control.Monad

import Q

main = do
  putStrLn "dnsloops test"

  putStrLn "test return ()"
  runQ $ return ()

  putStrLn "test launch a query"
  runQ $ qlaunch $ StrLenQuery "Hello"


  putStrLn "test launch and pull a query"
  [r] <- runQ $ query $ StrLenQuery "Hello"
  putStr "test result: "
  print r
  when (r /= 5) (error "test failed")

  putStrLn "test launch and pull another query"
  [r] <- runQ $ query $ StrLenQuery "Hello World"
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Inc 10"
  [r] <- runQ $ query $ Inc 10
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Dec 33"
  [r] <- runQ $ query $ Dec 33
  putStr "test result: "
  print r
  when (r /= 32) (error "test failed")

  putStrLn "Test a sequence of queries"
  runQ $ do
    query $ Dec 33
    query $ Inc 15
    query $ StrLenQuery "Hi"

  putStrLn "Test same query twice"
  rv <- runQ $ do
    r1 <- query $ Inc 15
    r2 <- query $ Inc 15
    return (r1, r2)
  putStrLn "rv = "
  print rv
  when ( rv /= [(16,16)]) $ error "test failed"  

  putStrLn "A launch and a pull in mplus-style parallel"
  r <- runQ $ (qlaunch (Inc 15) >> mzero) `mplus` (qpull (Inc 15))
  putStrLn "Test result: "
  print r
  when ( r /= [16] ) $ error "test failed"

  putStrLn "A pull and a launch in mplus-style parallel"
  r <- runQ $ (qpull (Inc 15)) `mplus` (qlaunch (Inc 15) >> mzero)
  putStrLn "Test result: "
  print r
  when ( r /= [16] ) $ error "test failed"

  putStrLn "Query ArbA"
  r <- runQ $ (query ArbA)
  when (r /= [ArbRes "Hi"]) (error "test failed")

  putStrLn "Query ArbB mplus query ArbA"
  r <- runQ $ ((query ArbB >> mzero) `mplus` query ArbA)
  when (sort r /= sort [ArbRes "Hello", ArbRes "Hi"]) (error $ "test failed. got " ++ (show r))


instance Qable StrLenQuery Int where
  runQable q@(StrLenQuery s) = qrecord q (length s)

data StrLenQuery = StrLenQuery String deriving (Show, Eq, Typeable)


instance Qable IntegerQuery Int where
  runQable q@(Inc n) = qrecord q (n + 1)
  runQable q@(Dec n) = qrecord q (n - 1)

data IntegerQuery = Inc Int | Dec Int deriving (Show, Eq, Typeable)


data ArbQuery = ArbA | ArbB deriving (Show, Eq, Typeable)
data ArbRes = ArbRes String deriving (Show, Eq, Typeable, Ord)

instance Qable ArbQuery ArbRes where
  runQable ArbA = qrecord ArbA (ArbRes "Hi")
  runQable ArbB = qrecord ArbB (ArbRes "Bye") >> qrecord ArbA (ArbRes "Hello")

