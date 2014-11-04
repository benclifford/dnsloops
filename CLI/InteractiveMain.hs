{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module CLI.InteractiveMain where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT ())
import Data.ByteString.Char8 (pack, unpack)
import Data.Function (on)
import Data.IP
import Data.List (tails, nub, groupBy, isPrefixOf, sortBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Typeable

import Network.DNS

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.IO.Error

import CLI.Args
import Domain
import Instances
import Lib
import Main
import Q
import Q.DB
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

  let q = GetRRSetQuery hostname ty

  (res', db) <- {-# SCC dynamicStage #-} runQ $
        populateRootHints
    <|> query q

  -- can I get the results a different way which
  -- returns the result ID info too?
  -- previousResultsForQuery :: (Qable q) => DB x -> q -> [Answer q]
  let res = previousResultsForQueryWithIds db q

  putStrLn "Result of main query: "
  print `mapM` res

{-
  putStrLn "Static stage:"

  {-# SCC staticStage #-} (flip runReaderT) db $ do
      Rules.Stats.displayStats
      Rules.Stats.displayStatsByType
      Rules.DuplicateRRs.displayDuplicateRRSets
      Rules.RefusedQueries.displayRefusedQueries
-}

  interactiveStage db

interactiveStage db = do
  putStr "dnsloops> "
  hFlush stdout
  cmd <- getLine
  -- What commands?
  -- only "explain" for now
  -- and what is the parameter?
  case cmd of
    [] -> return ()
    _ | "explain " `isPrefixOf` cmd -> explain db $ drop (length "explain ") cmd
    _ -> putStrLn "Error: unrecognised command"
  interactiveStage db

explain db resultId = do
  putStrLn $ "Explaining result " ++ resultId

  putStrLn $ "Result is: " ++ (show $ previousResultForId db (read resultId))

  -- This result came from a query. Q. which is encoded in the
  -- query/result pair.

  -- * Why did we ask that query? (i.e. what query stack were we
  --   inside when we made this query)
  -- * Inside the query, what other queries were made on the way
  --   to getting this result? Maybe I need an explanation
  --   stack attached to each result id?

  -- TODO: find the context for result identified by param
  let resultContext = Prelude.lookup (read resultId) (resultContexts db)
  -- TODO: display the context

  case resultContext of
    Nothing -> putStrLn "No result context"
    Just (ResultContext ctxs) -> do
      putStrLn "Result context: "
      mapM_ putStrLn ctxs

  putStrLn $ "End explain of result " ++ resultId

