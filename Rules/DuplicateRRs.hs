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

import Network.DNS.Types 

import Domain
import Lib
import Main
import Q

displayDuplicateRRSets :: StaticStage
displayDuplicateRRSets = do
  putIO $ "; Queries for which more than one RRSet was found:"
  db <- ask
  let pRes = previousResults db
  let filtered = catMaybes (maybeGetRRSetQuery <$> pRes)
  putIO $ "; There are " ++ (show $ length filtered) ++ " GetRRSetQueries with more than one RRSet"

  let sortedByQ = sortBy (compare `on` fst3) filtered
  let groupedByQ = groupBy ( (==) `on` fst3) sortedByQ
  let duplicatesByQ = filter (\l -> length l > 1) groupedByQ

  forM_ duplicatesByQ $ \g -> do
    let (GetRRSetQuery name typ,_,_) = head g -- There must be at least one group because this comes from groupBy, and the fst element should be the same for all elements in the group
    let rrNameString = (unpack name) ++ " " ++ (show typ)
    putIO $ ""
    putIO $ "; " ++ rrNameString
    forM_ g $ \(_,GetRRSetAnswer a, rid) -> case a of
      Left err -> do
        putIO $ "; [RID " ++ (show rid) ++ "] (" ++ rrNameString ++ ") non-RRSet response: " ++ err
      Right (CRRSet rrset) -> do
        putIO $ "; [RID " ++ (show rid) ++ "]  " ++ (show $ length rrset) ++ " resource record(s)"
        forM_ rrset $ \rr -> putIO $ renderResourceRecordAsZoneFile rr

-- root-servers.net. NS  ResourceRecord {rrname = "root-servers.net.", rrtype = NS, rrttl = 172800, rdlen = 4, rdata = f.root-servers.net.}
renderResourceRecordAsZoneFile rr = (unpack $ rrname rr) ++ " " ++ (show $ rrtype rr) ++ " " ++ (show $ rrttl rr) ++ " " ++ (show $ rdata rr) ++ " ; rdlen = " ++ (show $ rdlen rr)

fst3 (v,_,_) = v

maybeGetRRSetQuery :: PreviousResult -> Maybe (GetRRSetQuery, GetRRSetAnswer, ResultId)
maybeGetRRSetQuery (PreviousResult q a rid) | Just q' <- (cast q) = Just (q', fromJust $ cast a, rid)
maybeGetRRSetQuery _ = Nothing

