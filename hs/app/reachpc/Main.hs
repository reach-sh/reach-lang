{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

-- import ReachPC.DirScan
-- import ReachPC.Config (readOrCreateConfigToml, Config)

-- used for stdio green threading / streaming stuff
-- import Control.Concurrent (ThreadId, forkIO)
-- import Control.Monad (forever)
-- runStdinThread :: (String -> IO ()) -> IO ThreadId
-- runStdinThread callback = forkIO $ forever $ getLine >>= callback . (<> "\n") 

import System.Environment (getEnvironment, getArgs)
import Data.List (isPrefixOf, intercalate)
import Data.List.Extra (splitOn)
import NeatInterpolation (text)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import System.Exit (die)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- Parse cli args
  CliOptions {..} <- parseCliOptions
  forM_ co_error die

  -- Grab forwarded env vars
  let forwardedEnvVars = fromMaybe [] co_envVars
  let shouldForwardVar var = "REACH" `isPrefixOf` var || var `elem` forwardedEnvVars
  environment <- filter (shouldForwardVar . fst) <$> getEnvironment
  
  -- Exec command
  case fst co_command of
    "internal help" -> TIO.putStrLn helpMessage
    "version" -> version
    "config" -> config
    "auth" -> auth
    "local-down" -> localDown
    "local-install" -> localInstall
    "local-up" -> localUp
    _ -> execRemote environment co_command

  return ()

execRemote :: [(String, String)] -> (String, [String]) -> IO ()
execRemote env (cmd, args) = do
  putStrLn "Executed on remote:"
  putStrLn $ cmd <> " " <> show args
  print env

helpMessage :: Text
helpMessage = [text|
reach - Reach command-line tool

Usage: reach [OPTIONS] COMMAND [ARGS...]

Provide a COMMAND and ARGS to be forwarded to Reach Cloud or Reach Local,
depending on your configuration. If COMMAND is one of the following special
commands, its behavior will be executed by this program instead.

Run `reach help` to see the remote's help message.

Available options:
  -h,--help         Show this help message
  -e,--env LIST     Provide a comma separated list of environment variables
                    that should be forwarded to the remote. E.g. VAR1,VAR2,VAR3
                    Vars prefixed with REACH are forwarded automatically

Special commands:
  auth              Authenticate with Reach cloud
  config            Configure default Reach settings
  local-install     Install Reach Local
  local-up          Bring up Reach Local
  local-down        Halt Reach Local
  version           Display version
|]

data CliOptions = CliOptions
  { co_command :: (String, [String])
  , co_error :: Maybe String
  , co_envVars :: Maybe [String]
  }

parseCliOptions :: IO CliOptions
parseCliOptions = flip go emptyCliOptions <$> getArgs
 where
  emptyCliOptions = CliOptions ("", []) Nothing Nothing
  go args opts = case args of
    ("-h":args')         -> go ("--help" : args') opts
    ("-e":args')         -> go ("--env"  : args') opts
    ("--help":_)         -> go ["internal help"] opts
    ("--env":list:args') -> go args' opts{ co_envVars = Just $ splitOn "," list }
    (arg@('-':_):_)      -> opts{ co_error = Just $ "Unknown or bad use of argument " <> arg }
    (cmd:args')          -> opts{ co_command = (cmd, args') }
    []                   -> opts{ co_error = Just "No command was specified.\nRun with -h to see local commands, or run with the `help` command to see remote commands." }

version :: IO ()
version = putStrLn "reachpc version todo"

config :: IO ()
config = do
  localOrCloud <- askUser "Will you use Reach Cloud or Reach Local?" ["cloud", "local"]
  connector <- askUser "What connector will you use?" ["algo", "cfx", "eth"]
  return ()
 where
  askUser prompt answers = do
    putStrLn $ prompt <> " (" <> intercalate "/" answers <> ") "
    line <- getLine
    if line `elem` answers then return line else askUser prompt answers

auth :: IO ()
auth = return ()

localUp :: IO ()
localUp = return ()

localDown :: IO ()
localDown = return ()

localInstall :: IO ()
localInstall = return ()
