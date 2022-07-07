{- HLINT ignore "Use newtype instead of data" -}

module Main (main) where

import ReachPC.CommandLine (CliOptions(..), parseCliOptions, helpMessage)
import ReachPC.Config (Config(..), getProjectConfig)
import System.Environment (getEnvironment)
import System.Exit (die)
import Data.List (isPrefixOf)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Control.Exception (catch, IOException)
import qualified Reach.Version

main :: IO ()
main = do
  -- Parse cli flags and config files
  cliOptions@CliOptions{..} <- parseCliOptions
  forM_ cli_error die

  -- Exec command
  case fst cli_command of
    "local-help" -> TIO.putStrLn helpMessage
    "version" -> version
    "config" -> config
    "auth" -> auth
    "local-down" -> localDown
    "local-install" -> localInstall
    "local-up" -> localUp
    _ -> do
      projectConfig <- getProjectConfig cliOptions
      execRemoteCommand cli_command projectConfig

execRemoteCommand :: (String, [String]) -> Config -> IO ()
execRemoteCommand (cmd, args) Config{..} = do
  env <- getForwardedEnvVars
  putStrLn "Executed on remote:"
  putStrLn $ cmd <> " " <> show args
  print env
 where
  shouldForwardEnvVar var = "REACH" `isPrefixOf` var || var `elem` cfg_forwardEnvVars
  getForwardedEnvVars = filter (shouldForwardEnvVar . fst) <$> getEnvironment

version :: IO ()
version = putStrLn $ "reachpc " <> Reach.Version.versionStr

config :: IO ()
config = do
  _localOrCloud <- askUser "Will you use Reach Cloud or Reach Local?" ["CLOUD", "LOCAL"]
  _connector <- askUser "What connector will you use?" ["ALGO", "CFX", "ETH"]
  return ()
 where
  askUser prompt answers = do
    let (enumAnswers :: [(Int, String)]) = zip [1..] answers
    putStrLn prompt
    forM_ enumAnswers $ \(n, ans) -> putStrLn $ show n <> ") " <> ans
    (n :: Int) <- catch readLn (\(_ :: IOException) -> return (-1))
    case lookup n enumAnswers of
      Just ans -> return ans
      Nothing -> askUser prompt answers

auth :: IO ()
auth = return ()

localUp :: IO ()
localUp = return ()

localDown :: IO ()
localDown = return ()

localInstall :: IO ()
localInstall = return ()
