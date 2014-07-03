{-# LANGUAGE PatternGuards #-}

-- | types and functions for domains
module Domain where

import Data.ByteString.Char8 (unpack)
import Network.DNS (Domain)

{-
-- | terminology from RFC1035
type LDomain = [LLabel]

type LLabel = String

-}

dropdot :: String -> String
dropdot s | last s == '.' = init s
dropdot s = error $  "Cannot drop the dot off a string that does not end with a dot: " ++ s

-- | compares domains, ignoring the
-- final dot or not.
-- TODO: this shouldn't exist, and
-- instead a proper normalised form
-- for domains should be used right
-- from the start.
(=.=) :: Domain -> Domain -> Bool
l =.= r = (dropDot $ unpack l) == (dropDot $ unpack r)

dropDot s | s /= [], last s == '.' = init s
dropDot s = s



