{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test where

import Data.IORef
import Data.List
import Data.Typeable
import Control.Monad

import System.IO.Unsafe
  -- this is used to check repeated query
  -- execution. If Q became a transfomer
  -- then it could be over a state monad
  -- and unsafe IO would not be needed to
  -- give this external counter.

import Q

main = do
  putStrLn "dnsloops test"

  putStrLn "test return ()"
  evalQ $ return ()

  putStrLn "test launch a query"
  evalQ $ qlaunch $ StrLenQuery "Hello"


  putStrLn "test launch and pull a query"
  [r] <- evalQ $ query $ StrLenQuery "Hello"
  putStr "test result: "
  print r
  when (r /= 5) (error "test failed")

  putStrLn "test launch and pull another query"
  [r] <- evalQ $ query $ StrLenQuery "Hello World"
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Inc 10"
  [r] <- evalQ $ query $ Inc 10
  putStr "test result: "
  print r
  when (r /= 11) (error "test failed")

  putStrLn "test Dec 33"
  [r] <- evalQ $ query $ Dec 33
  putStr "test result: "
  print r
  when (r /= 32) (error "test failed")

  putStrLn "Test a sequence of queries"
  evalQ $ do
    query $ Dec 33
    query $ Inc 15
    query $ StrLenQuery "Hi"

  putStrLn "Test same query twice"
  rv <- evalQ $ do
    r1 <- query $ Inc 15
    r2 <- query $ Inc 15
    return (r1, r2)
  putStrLn "rv = "
  print rv
  when ( rv /= [(16,16)]) $ error "test failed"  

  putStrLn "A launch and a pull in mplus-style parallel"
  r <- evalQ $ (qlaunch (Inc 15) >> mzero) `mplus` (qpull (Inc 15))
  putStrLn "Test result: "
  print r
  when ( r /= [16] ) $ error "test failed"

  putStrLn "A pull and a launch in mplus-style parallel"
  r <- evalQ $ (qpull (Inc 15)) `mplus` (qlaunch (Inc 15) >> mzero)
  putStrLn "Test result: "
  print r
  when ( r /= [16] ) $ error "test failed"

  putStrLn "Query ArbA"
  r <- evalQ $ (query ArbA)
  when (r /= [ArbRes "Hi"]) (error "test failed")

  putStrLn "Query ArbB mplus query ArbA"
  r <- evalQ $ ((query ArbB >> mzero) `mplus` query ArbA)
  when (sort r /= sort [ArbRes "Hello", ArbRes "Hi"]) (error $ "test failed. got " ++ (show r))

  putStrLn "Query ArbB mplus query ArbA mplus ArbC"
  r <- evalQ $ ((query ArbB >> mzero) `mplus` query ArbA `mplus` (query ArbC >> mzero))
  when (sort r /= sort [ArbRes "Hello", ArbRes "Hi"]) (error $ "test failed. got " ++ (show r))

  putStrLn "Same query launched twice should not launch runQable twice"
  r <- evalQ $ qlaunch ArbCounterUnsafeIO `mplus` qlaunch ArbCounterUnsafeIO `mplus` qpull ArbCounterUnsafeIO
  putStrLn $ "r = " ++ (show r) ++ " -- probably expecting a single result"
  when (length r /= 1) (error "Test failed: r should have exactly one element")

  putStrLn "Tests finished without failure"

instance Qable StrLenQuery Int where
  runQable q@(StrLenQuery s) = qrecord q (length s)

data StrLenQuery = StrLenQuery String deriving (Show, Eq, Typeable)


instance Qable IntegerQuery Int where
  runQable q@(Inc n) = qrecord q (n + 1)
  runQable q@(Dec n) = qrecord q (n - 1)

data IntegerQuery = Inc Int | Dec Int deriving (Show, Eq, Typeable)


data ArbQuery = ArbA | ArbB | ArbC | ArbCounterUnsafeIO deriving (Show, Eq, Typeable)
data ArbRes = ArbRes String deriving (Show, Eq, Typeable, Ord)

instance Qable ArbQuery ArbRes where
  runQable ArbA = qrecord ArbA (ArbRes "Hi")
  runQable ArbB = qrecord ArbB (ArbRes "Bye") >> qrecord ArbA (ArbRes "Hello") >> qrecord ArbC (ArbRes "test")
  runQable ArbC = return ()
  runQable q@ArbCounterUnsafeIO = do
    unsafeQT $ modifyIORef arbCounterRef (+1)
    v <- unsafeQT $ readIORef arbCounterRef
    qrecord q (ArbRes $ show v)

arbCounterRef = unsafePerformIO $ newIORef (0 :: Integer)

