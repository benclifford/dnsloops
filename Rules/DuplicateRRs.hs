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


maybeGetRRSetQuery :: PreviousResult -> Maybe (GetRRSetQuery, GetRRSetAnswer)
maybeGetRRSetQuery (PreviousResult q a) | Just q' <- (cast q) = Just (q', fromJust $ cast a)
maybeGetRRSetQuery _ = Nothing

