{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module CLI.Main where

import Control.Applicative
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack)

import CLI.Args
import Domain
import Instances()
import Main
import Q
import Q.Interpreter
import Query.GetRRSet
-- import Query.Resolver
import qualified Rules.DuplicateRRs
import qualified Rules.RefusedQueries
import qualified Rules.Stats
-- import Stages
-- import Util
import Control.Monad (void)

main :: IO ()
main = do
  putStrLn "DNSLoops main"

  (h, ty) <- getOptions

  let hostname = ensureDot $ pack h

  putStrLn "Dynamic stage:"

  (res, db) <- {-# SCC dynamicStage #-} runQ $
        populateRootHints
    <|> (query $ GetRRSetQuery hostname ty)

  putStrLn "Result of main query: "
  print `mapM_` res

  putStrLn "Static stage:"

  {-# SCC staticStage #-} void $ (flip runReaderT) db $ do
      Rules.Stats.displayStats
      Rules.Stats.displayStatsByType
      Rules.DuplicateRRs.displayDuplicateRRSets
      Rules.RefusedQueries.displayRefusedQueries

