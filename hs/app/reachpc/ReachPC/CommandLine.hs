{-# LANGUAGE QuasiQuotes #-}

module ReachPC.CommandLine (CliOptions(..), parseCliOptions, helpMessage) where

import Data.Text (Text)
import NeatInterpolation (text)
import Data.List.Extra (splitOn)
import System.Environment (getArgs)

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
  config            Interactively configure the current project's `reach.toml`
  config-global     Interactively configure Reach global settings
  init              Initialize a project in the current directory
  local-install     Install Reach Local
  local-up          Bring up Reach Local
  local-down        Halt Reach Local
  version           Display version
|]

data CliOptions = CliOptions
  { cli_command :: (String, [String])
  , cli_error :: Maybe String
  , cli_forwardEnvVars :: Maybe [String]
  , cli_cloudOrLocal :: Maybe Bool -- True = cloud, False = local
  , cli_connector :: Maybe String
  , cli_project :: Maybe String
  , cli_organization :: Maybe String
  }

emptyCliOptions :: CliOptions
emptyCliOptions = CliOptions ("", []) Nothing Nothing Nothing Nothing Nothing Nothing

parseCliOptions :: IO CliOptions
parseCliOptions = flip go emptyCliOptions <$> getArgs
 where
  go args opts = case args of
    ("-h":args')            -> go ("--help" : args') opts
    ("-e":args')            -> go ("--env"  : args') opts
    ("--help":_)            -> go ["local-help"] opts
    ("--env":list:args')    -> go args' opts{ cli_forwardEnvVars = Just $ maybe [] (splitOn "," list <>) $ cli_forwardEnvVars opts }
    ("--cloud":args')       -> go args' opts{ cli_cloudOrLocal = Just True }
    ("--local":args')       -> go args' opts{ cli_cloudOrLocal = Just False }
    ("--connector":c:args') -> go args' opts{ cli_connector = Just c }
    ("--project":p:args')   -> go args' opts{ cli_project = Just p }
    (arg@('-':_):_)         -> opts{ cli_error = Just $ "Unknown or bad use of argument " <> arg }
    (cmd:args')             -> opts{ cli_command = (cmd, args') }
    []                      -> opts{ cli_error = Just "No command was specified.\nRun with -h to see local commands, or run with the `help` command to see remote commands." }
