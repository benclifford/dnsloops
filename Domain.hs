{-# LANGUAGE PatternGuards #-}

-- | types and functions for domains
module Domain where

import Instances

import Control.Applicative ( (<$>) )
import Data.ByteString.Char8 (unpack, pack, intercalate, split)
import Data.List (tails, groupBy, sortBy)
import Data.Ord (comparing)
import Network.DNS

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

ancestorDomains :: Domain -> [Domain]
ancestorDomains qname = let
  shreddedDomain = split '.' qname
  -- TODO BUG: handling of . inside domain labels (rather than as a separator)
  domainSuffixes = tails shreddedDomain
  domainParents = (intercalate (pack ".")) <$> domainSuffixes
  in domainParents


-- | groups resource records into RRsets
-- where the records in each RRset have
-- the same (qname,qtype)
rrlistToRRsets :: [ResourceRecord] -> [[ResourceRecord]]
rrlistToRRsets rrs = let
  l `eqRRset` r = (rrname l == rrname r)
               && (rrtype l == rrtype r)
  key rr = (rrname rr, rrtype rr)
  compareRRset = comparing key
  in groupBy eqRRset $ sortBy compareRRset rrs



