{-# LANGUAGE PatternGuards #-}

module Rules.RefusedQueries where

import Control.Applicative ( (<$>) )
import Control.Monad.Reader (ask)

-- TODO: these slightly unsafe functions
-- could be replaced by a Q-level function
-- to perform all the desired casting?
import Data.Maybe (catMaybes, fromJust)
import Data.Typeable (cast)

import Data.Traversable (for)

import Network.DNS

import Lib
import Q
import Query.Resolver
import Stages

-- TODO: we could be hooking the refused response in the dynamic bit, where
-- the main code currently (august 2014) emits an "unexpected response"
-- message.

-- TODO: should I also be detecting other similar errors such as the
-- server being down/connection refused/etc in this same summary?
-- i.e. a summary of "servers that I thought would answer but did not?"

-- TODO: should I order by server or by domain name? Both could be useful...
-- TODO: could report when all servers for a domain, or all domains sent
--       to a server are refused
displayRefusedQueries :: StaticStage
displayRefusedQueries = do
  putIO "Refused queries:"

  db <- ask
  let rqs = catMaybes $ maybeResolverQuery <$> (previousResults db)
  
  putIO $ "There are " ++ (show $ length rqs) ++ " resolver queries"

  let refuseds = filter isRefused rqs
  putIO $ "There are " ++ (show $ length refuseds) ++ " refused resolver queries"

  dumpRefuseds refuseds

  return ()


{-
Right (DNSFormat {header = DNSHeader {identifier = 4387,
                                      flags = DNSFlags {qOrR = QR_Response, opcode = OP_STD, authAnswer = False, trunCation = False, recDesired = True, recAvailable = False, rcode = Refused},
                                      qdCount = 1,
                                      anCount = 0,
                                      nsCount = 0,
                                      arCount = 0
                                     },
                  question = [Question {qname = "88-95.94.155.90.in-addr.arpa.", qtype = NS}],
                  answer = [],
                  authority = [],
                  additional = []
                 }
      )
-}



isRefused (_, ResolverAnswer (Right df)) | (rcode . flags . header) df == Refused = True
isRefused _ = False

dumpRefuseds l = for l $ \(q,a) -> do
  putIO $ "  For query: " ++ (show q)
  putIO $ "  Got refusal: " ++ (show a)
  putIO $ "  "

{-
{-# LANGUAGE PatternGuards #-}
module Rules.DuplicateRRs where

import Control.Applicative ( (<$>) )
import Control.Monad
import Control.Monad.Reader (ask)
import Data.ByteString.Char8 (unpack)
import Data.Function (on)
import Data.List
import Data.Maybe (fromJust, catMaybes)
import Data.Typeable (cast)

import Domain
import Lib
import Q
import Query.GetRRSet
import Stages

displayDuplicateRRSets :: StaticStage
displayDuplicateRRSets = do
  putIO $ "Queries for which more than one RRSet was found:"
  db <- ask
  let pRes = previousResults db
  let filtered = catMaybes (maybeGetRRSetQuery <$> pRes)
  putIO $ "There are " ++ (show $ length filtered) ++ " GetRRSetQueries"

  let sortedByQ = sortBy (compare `on` fst) filtered
  let groupedByQ = groupBy ( (==) `on` fst) sortedByQ

  forM_ groupedByQ $ \g -> do
    let (GetRRSetQuery name typ,_) = head g -- There must be at least one group because this somes from groupBy, and the fst element should be the same for all elements in the group
    putIO $ (unpack name) ++ "/" ++ (show typ) ++ ":"
    forM_ g $ \(_,GetRRSetAnswer a) -> case a of
      Left err -> putIO $ "  Error: " ++ err
      Right (CRRSet rrset) -> do
        putIO "  ["
        forM_ rrset $ \rr -> putIO $ "    " ++ (show rr)
        putIO "  ]"

-}

maybeResolverQuery :: PreviousResult -> Maybe (ResolverQuery, ResolverAnswer)
maybeResolverQuery (PreviousResult q a) | Just q' <- (cast q) = Just (q', fromJust $ cast a)
maybeResolverQuery _ = Nothing

