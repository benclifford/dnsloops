{-# LANGUAGE StandaloneDeriving #-}

-- | Standalone deriving instances for third-party
-- DNS library
module Instances where

import Network.DNS

deriving instance Show ResolvConf
deriving instance Eq ResolvConf
deriving instance Show FileOrNumericHost
deriving instance Eq FileOrNumericHost
deriving instance (Ord o) => Ord (RD o)
-- deriving instance Ord ResourceRecord
deriving instance Ord TYPE


