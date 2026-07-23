{- HLINT ignore "Use newtype instead of data" -}

module Main (main) where

import Control.Monad (forM_)
import Data.List (isPrefixOf)
import qualified Data.Text.IO as TIO
import qualified Reach.Version
import ReachPC.CommandLine (CliOptions (..), helpMessage, parseCliOptions)
import ReachPC.Config (Config (..), getProjectConfig, interactiveCreateGlobalsToml, interactiveCreateReachToml)
import System.Environment (getEnvironment)
import System.Exit (die)

main :: IO ()
main = do
  -- Parse cli flags and config files
  cliOptions@CliOptions {..} <- parseCliOptions
  forM_ cli_error die

  -- Exec command
  case fst cli_command of
    "local-help" -> TIO.putStrLn helpMessage
    "version" -> version
    "init" -> interactiveCreateReachToml False
    "config" -> interactiveCreateReachToml True
    "config-global" -> interactiveCreateGlobalsToml
    "auth" -> auth
    "local-down" -> localDown
    "local-install" -> localInstall
    "local-up" -> localUp
    _ -> do
      projectConfig <- getProjectConfig cliOptions
      execRemoteCommand cli_command projectConfig

execRemoteCommand :: (String, [String]) -> Config -> IO ()
execRemoteCommand (cmd, args) Config {..} = do
  env <- getForwardedEnvVars
  putStrLn "Executed on remote:"
  putStrLn $ cmd <> " " <> show args
  print env
  where
    shouldForwardEnvVar var = "REACH" `isPrefixOf` var || var `elem` cfg_forwardEnvVars
    getForwardedEnvVars = filter (shouldForwardEnvVar . fst) <$> getEnvironment

version :: IO ()
version = putStrLn $ "reachpc " <> Reach.Version.versionStr

auth :: IO ()
auth = return ()

localUp :: IO ()
localUp = return ()

localDown :: IO ()
localDown = return ()

localInstall :: IO ()
localInstall = return ()
