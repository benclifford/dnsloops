{-# LANGUAGE DeriveDataTypeable #-}

module Query.Resolver where

import Data.Typeable (Typeable)
import Network.DNS

import Instances()

data ResolverQuery = ResolverQuery ResolvConf Domain TYPE deriving (Show, Eq, Typeable)
data ResolverAnswer =
    ResolverAnswer (Either ResolverError DNSFormat)
  deriving (Show, Eq, Typeable)

data ResolverError =
    ResolverDNSError DNSError
  | ResolverIOError IOError
  deriving (Show, Eq, Typeable)

