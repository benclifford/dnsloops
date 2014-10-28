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

  let sortedByQ = sortBy (compare `on` fst3) filtered
  let groupedByQ = groupBy ( (==) `on` fst3) sortedByQ
  let duplicatesByQ = filter (\l -> length l > 1) groupedByQ

  forM_ duplicatesByQ $ \g -> do
    let (GetRRSetQuery name typ,_,_) = head g -- There must be at least one group because this comes from groupBy, and the fst element should be the same for all elements in the group
    putIO $ (unpack name) ++ "/" ++ (show typ) ++ ":"
    forM_ g $ \(_,GetRRSetAnswer a, rid) -> case a of
      Left err -> putIO $ "  Non-RRSet response: " ++ err ++ " [RID " ++ (show rid) ++ "]"
      Right (CRRSet rrset) -> do
        putIO "  ["
        forM_ rrset $ \rr -> putIO $ "    " ++ (show rr)
        putIO $ "  ] [RID " ++ (show rid) ++ "]"

fst3 (v,_,_) = v

maybeGetRRSetQuery :: PreviousResult -> Maybe (GetRRSetQuery, GetRRSetAnswer, ResultId)
maybeGetRRSetQuery (PreviousResult q a rid) | Just q' <- (cast q) = Just (q', fromJust $ cast a, rid)
maybeGetRRSetQuery _ = Nothing

