{-# LANGUAGE PatternGuards #-}

-- | types and functions for domains
module Domain where

import Data.ByteString.Char8 (unpack, pack)
import Network.DNS (Domain, RDATA(RD_NS))

{-
-- | terminology from RFC1035
type LDomain = [LLabel]

type LLabel = String

-}

-- | compares domains, ignoring the
-- final dot or not.
-- TODO: this shouldn't exist, and
-- instead a proper normalised form
-- for domains should be used right
-- from the start.
(=.=) :: Domain -> Domain -> Bool
l =.= r = (dropDot l) == (dropDot r)
-- TODO:   (==) `on` dropDot ?

dropDot :: Domain -> Domain
dropDot s | s' <- unpack s
          , s' /= []
          , last s' == '.' = pack $ init s'
dropDot s = s

-- | This might fail. Ideally I'd return a Maybe or handle
-- the failure some other way... TODO
rdataNSToDomain :: RDATA -> Domain
rdataNSToDomain (RD_NS domain) = domain


