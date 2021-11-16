module Reach.CommandLine
  ( CompilerToolArgs (..)
  , CompilerOpts (..)
  , CompilerToolEnv (..)
  , compiler
  , getCompilerArgs
  , getCompilerEnv
  ) where

import Options.Applicative
import System.Environment
import Data.Maybe (isJust)

data CompilerToolArgs = CompilerToolArgs
  { cta_disableReporting :: Bool
  , cta_errorFormatJson :: Bool
  , cta_co :: CompilerOpts
  }

data CompilerOpts = CompilerOpts
  { co_moutputDir :: Maybe FilePath
  , co_source :: FilePath
  , co_topl :: [String]
  , co_intermediateFiles :: Bool
  , co_mdirDotReach :: Maybe FilePath
  , co_installPkgs :: Bool
  , co_stopAfterEval :: Bool
  , co_verifyTimeout :: Integer
  , co_sim :: Bool
  }

compiler :: Parser CompilerToolArgs
compiler =
  CompilerToolArgs
    <$> (switch (long "disable-reporting" <> internal))
    <*> (switch (long "error-format-json" <> internal))
    <*> (CompilerOpts
    <$> (optional
          (strOption
            (long "output"
              <> short 'o'
              <> metavar "DIR"
              <> help "Directory for output files"
              <> showDefault)))
    <*> (strArgument ((metavar "SOURCE") <> value ("index.rsh")))
    <*> (many (strArgument (metavar "EXPORTS...")))
    <*> (switch (long "intermediate-files"
          <> help "Store intermediate files in output DIR"))
    <*> (optional
          (strOption
            (long "dir-dot-reach"
              <> metavar "DIR-DOT-REACH"
              <> help "Package imports cache+lock directory"
              <> internal
              <> showDefault)))
    <*> (switch (long "install-pkgs"
          <> help "Allow Reach to fetch remote package imports"))
    <*> (switch (long "stop-after-eval"
          <> help "Stop compilation process after evaluation"))
    <*> (option
          auto
          ((metavar "TIMEOUT-MS")
            <> long "verify-timeout"
            <> value (1000 * 60 * 2)
            <> help "Timeout per verification theorem in milliseconds"
            <> showDefault))
    <*> (switch (long "sim"
          <> help "Run Simulator"))
    )

getCompilerArgs :: String -> IO CompilerToolArgs
getCompilerArgs versionCliDisp = do
  let opts =
        info
          (compiler <**> helper)
          (fullDesc
             <> progDesc "verify and compile an Reach program"
             <> header versionCliDisp)
  execParser opts


data CompilerToolEnv = CompilerToolEnv
  { cte_REACHC_ID :: Maybe String
  , cte_REACHC_HASH :: Maybe String
  , cte_CI :: Maybe String -- most CI services
  , cte_GITHUB_ACTIONS :: Maybe String -- Github Actions
  , cte_TF_BUILD :: Maybe String -- Azure Pipelines
  , cte_REACH_DEBUG :: Bool
  }

getCompilerEnv :: IO CompilerToolEnv
getCompilerEnv = do
  cte_REACHC_ID <- lookupEnv "REACHC_ID"
  cte_REACHC_HASH <- lookupEnv "REACHC_HASH"
  cte_CI <- lookupEnv "CI"
  cte_GITHUB_ACTIONS <- lookupEnv "GITHUB_ACTIONS"
  cte_TF_BUILD <- lookupEnv "TF_BUILD"
  cte_REACH_DEBUG <- fmap isJust $ lookupEnv "REACH_DEBUG"
  return CompilerToolEnv {..}
