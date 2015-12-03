{-# LANGUAGE StandaloneDeriving #-}

-- | Standalone deriving instances for third-party
-- DNS library
module Instances where

import Network.DNS

deriving instance Show ResolvConf
instance Eq ResolvConf where
  (ResolvConf a b c d) == (ResolvConf a' b' c' d') = {-# SCC resolvConfEq #-}
       a == a'
    && b == b'
    && c == c'
    && d == d'

deriving instance Show FileOrNumericHost

-- manually defined to put in profiling information
instance Eq FileOrNumericHost where
  (RCFilePath p1) == (RCFilePath p2) = {-# SCC fileOrNumericHostEqFile #-} p1 == p2
  (RCHostName p1) == (RCHostName p2) = {-# SCC fileOrNumericHostEqHost #-} p1 == p2
  _ == _ = {-# SCC fileOrNumericHostEqDefault #-} False

deriving instance (Ord o) => Ord (RD o)
-- deriving instance Ord ResourceRecord
deriving instance Ord TYPE


