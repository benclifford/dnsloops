{-# LANGUAGE DeriveDataTypeable #-}

module Query.GetRRSet where

import Data.Typeable(Typeable)
import Network.DNS (Domain(), TYPE())

import Domain

data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable, Ord)
data GetRRSetAnswer = GetRRSetAnswer (Either String CRRSet) deriving (Show, Eq, Typeable)

