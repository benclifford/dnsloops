{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Args where

import Control.Monad.State.Lazy
import System.Console.GetOpt
import System.Environment
import System.Exit
import Network.DNS (TYPE)

type OptionState = (String, TYPE)
 
type OptionConfigurer = StateT OptionState IO ()

data Opt    = Name String
            | Type TYPE
            | Help
            | NonOption String
  deriving Show

optionDescrs :: [OptDescr OptionConfigurer]
optionDescrs =
  [
    Option "n" ["name"] (ReqArg updateName "NAME") "Domain name to look up"
  , Option "t" ["type"] (ReqArg updateType "TYPE") "Type to look up"
  , Option ""  ["help"] (NoArg doHelp) "Display help"
  ]


getOptions :: IO OptionState
getOptions = do
  rawArgs <- getArgs
  let (options :: [OptionConfigurer], nonOptions, errors) = getOpt (ReturnInOrder wrapNonOption) optionDescrs rawArgs
  result <- execStateT (sequence options) ("example.com", read "A")
  return result

updateName :: String -> OptionConfigurer
updateName newName = modify $ \(_,t) -> (newName,t)

updateType :: String -> OptionConfigurer
updateType newTyp = modify $ \(n,_) -> (n, read newTyp)

wrapNonOption :: String -> OptionConfigurer
wrapNonOption nonopt = error $ "Cannot deal with non-option " ++ nonopt

doHelp :: OptionConfigurer
doHelp = do
  liftIO $ putStrLn "HELP HERE"
  liftIO $ exitWith ExitSuccess

