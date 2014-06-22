{-# LANGUAGE StandaloneDeriving #-}

-- | Standalone deriving instances for third-party
-- DNS library
module Instances where

import Network.DNS

deriving instance Show ResolvConf
deriving instance Eq ResolvConf
deriving instance Show FileOrNumericHost
deriving instance Eq FileOrNumericHost
deriving instance Ord TYPE


