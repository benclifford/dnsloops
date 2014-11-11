module Rules.Stats where

import Control.Applicative ( (<$>) )
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.List (nub)
import Data.Typeable (typeOf)

import Lib
import Q
import Stages

displayStats :: StaticStage
displayStats = do
  db <- ask
  putIO   "Database statistics:"
  putIO $ "  Number of previous launches: " ++ (show $ length $ previousLaunches db)
  putIO $ "  Number of previous results: " ++ (show $ length $ previousResults db)
  putIO $ "  Number of previous pulls: " ++ (show $ length $ previousPulls db)
  putIO $ "  Number of final results: " ++ (show $ length $ finalResults db)

displayStatsByType :: StaticStage
displayStatsByType = do
  db <- ask
  let launchTypes = typeOfPreviousLaunch <$> ((previousLaunches db))
        where typeOfPreviousLaunch (PreviousLaunch q) = typeOf q

  putIO $ "Launch types: " ++ (show $ nub launchTypes)
  forM_ (nub launchTypes) $ \lt -> putIO $
       "  "
    ++ (show lt)
    ++ ": "
    ++ (show $ length $ filter (== lt) $ launchTypes)

  -- we could get the type of a here, but there is slightly
  -- more information contained in q because multiple
  -- q types can share the same a type.
  let resultTypes = typeOfPreviousResult <$> ((previousResults db))
        where typeOfPreviousResult (PreviousResult q _ _) = typeOf q

  putIO $ "Result types: " ++ (show $ nub resultTypes)
  forM_ (nub resultTypes) $ \lt -> putIO $
       "  "
    ++ (show lt)
    ++ ": "
    ++ (show $ length $ filter (== lt) $ resultTypes)

