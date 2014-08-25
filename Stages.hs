module Stages where

import Control.Monad.Reader(ReaderT())

import Q
import Query.GetRRSet

-- There are two stages to processing:
-- * the dynamic stage happens in the Q monad, and so code
--   (eg checking rules) can launch new queries and cause
--   new answers to appear. Because of this, some checks
--   (such as processing over the entire list of results
--   of a query) cannot be performed.
-- * the static stage happens in the IO monad, or functionally,
--   with read-only access to the query database. Processing
--   performed at this stage cannot launch new queries and
--   so no new answers will appear. This means we can run
--   queries such as examining the entire list of results
--   of a query.
type DynamicStage = Q GetRRSetAnswer GetRRSetAnswer
type StaticStage = ReaderT (DB GetRRSetAnswer) IO ()

  
