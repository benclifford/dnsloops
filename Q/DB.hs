
module Q.DB where

import Data.Typeable (cast)
import Data.Maybe (catMaybes, fromMaybe)

import Lib (mapfor)
import Q

previousResultsForQuery :: (Qable q) => DB x -> q -> [Answer q]
previousResultsForQuery db q  = do
  let fm = mapfor (previousResults db) $ \(PreviousResult q' a' _) ->
       case (cast q') of
         Just q'' | q'' == q -> cast a'
         _ -> Nothing
   in catMaybes fm

previousResultsForQueryWithIds :: (Qable q) => DB x -> q -> [(Answer q, ResultId)]
previousResultsForQueryWithIds db q  = do
  let fm = mapfor (previousResults db) $ \(PreviousResult q' a' rid) ->
       case (cast q') of
         Just q'' | q'' == q -> Just (fromMaybe (error $ "Internal error: result did not match expected type, rid "++(show rid)) 
                                                (cast a')
                                     ,rid)
         _ -> Nothing
   in catMaybes fm

previousResultForId :: DB x -> ResultId -> PreviousResult
previousResultForId db rid = head $ filter p $ previousResults db
  where p (PreviousResult _ _ rid') = rid == rid'

