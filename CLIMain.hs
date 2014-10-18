{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module CLIMain where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT ())
import Data.ByteString.Char8 (pack, unpack)
import Data.Function (on)
import Data.IP
import Data.List (tails, nub, groupBy, sortBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Typeable

import Network.DNS

import System.Environment (getArgs)
import System.IO.Error

import CLIArgs
import Domain
import Instances
import Lib
import Main
import Q
import Q.Interpreter
import Query.GetRRSet
import Query.Resolver
import qualified Rules.DuplicateRRs
import qualified Rules.RefusedQueries
import qualified Rules.Stats
import Stages
import Util
import Control.Monad

main = do
  putStrLn "DNSLoops main"

  (h, ty) <- getOptions

  let hostname = ensureDot $ pack h

  putStrLn "Dynamic stage:"

  (res, db) <- {-# SCC dynamicStage #-} runQ $
        populateRootHints
    <|> (query $ GetRRSetQuery hostname ty)

  putStrLn "Result of main query: "
  print `mapM` res

  putStrLn "Static stage:"

  {-# SCC staticStage #-} (flip runReaderT) db $ do
      Rules.Stats.displayStats
      Rules.Stats.displayStatsByType
      Rules.DuplicateRRs.displayDuplicateRRSets
      Rules.RefusedQueries.displayRefusedQueries


