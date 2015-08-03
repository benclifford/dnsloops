{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module CLI.InteractiveMain where

import Control.Applicative
import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)

import CLI.Args
import Domain
import Instances()
import Main
import Q
import Q.DB
import Q.Interpreter
{-
import qualified Rules.DuplicateRRs
import qualified Rules.RefusedQueries
import qualified Rules.Stats
-}

main :: IO ()
main = do
  putStrLn "DNSLoops main"

  (h, ty) <- getOptions

  let hostname = ensureDot $ pack h

  putStrLn "Dynamic stage:"

  let q = GetRRSetQuery hostname ty

  (_, db) <- {-# SCC dynamicStage #-} runQ $
        populateRootHints
    <|> query q

  -- can I get the results a different way which
  -- returns the result ID info too?
  -- previousResultsForQuery :: (Qable q) => DB x -> q -> [Answer q]
  let res = previousResultsForQueryWithIds db q

  putStrLn "Result of main query: "
  void $ print `mapM` res

  interactiveStage db

interactiveStage :: DB final -> IO ()
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

explain :: DB final -> String -> IO ()
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
      putStrLn $ "Result context: (" ++ (show $ length ctxs) ++ " entries)"
      mapM_ (\ctx -> putStrLn ctx >> putStrLn ";" ) ctxs

  putStrLn $ "End explain of result " ++ resultId

