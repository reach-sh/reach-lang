module Reach.CommandLine
  ( CompilerToolArgs (..)
  , CompilerOpts (..)
  , CompilerToolEnv (..)
  , compiler
  , getCompilerArgs
  , getCompilerEnv
  , truthyEnv
  , SupportToolArgs(..)
  , SupportOpts(..)
  , supportFromCommandLineHs
  )
where

import Data.Char
import Options.Applicative
import System.Environment

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
  , co_printKeywordInfo :: Bool
  , co_verifyTimeout :: Integer
  , co_sim :: Bool
  , co_verifyFirstFailQuit :: Bool
  }

newtype SupportToolArgs = SupportToolArgs {sta_so :: SupportOpts}

data SupportOpts = SupportOpts
  { so_filesToUpload :: [FilePath]
  }

supportFromCommandLineHs :: Parser SupportToolArgs
supportFromCommandLineHs =
  SupportToolArgs
    <$> (SupportOpts
           <$> (many (strArgument (metavar "path/to/files..."))))

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
           <*> (switch
                  (long "intermediate-files"
                     <> help "Store intermediate files in output DIR"))
           <*> (optional
                  (strOption
                     (long "dir-dot-reach"
                        <> metavar "DIR-DOT-REACH"
                        <> help "Package imports cache+lock directory"
                        <> internal
                        <> showDefault)))
           <*> (switch
                  (long "install-pkgs"
                     <> help "Allow Reach to fetch remote package imports"))
           <*> (switch
                  (long "stop-after-eval"
                     <> help "Stop compilation process after evaluation"))
           <*> (switch
                  (long "print-keyword-info"
                     <> help "Print keyword completion data for editor plugin"))
           <*> (option
                  auto
                  ((metavar "TIMEOUT-MS")
                     <> long "verify-timeout"
                     <> value (1000 * 60 * 2)
                     <> help "Timeout per verification theorem in milliseconds"
                     <> showDefault))
           <*> (switch
                  (long "sim"
                     <> help "Run Simulator"))
           <*> (switch
                  (long "verify-fail-once"
                     <> help "Quit after a single verification failure")))

getCompilerArgs :: String -> IO CompilerToolArgs
getCompilerArgs versionCliDisp = do
  let opts =
        info
          (compiler <**> helper)
          (fullDesc
             <> progDesc "verify and compile a Reach program"
             <> header versionCliDisp)
  execParser opts

data CompilerToolEnv = CompilerToolEnv
  { cte_REACHC_ID :: Maybe String
  , cte_REACHC_HASH :: Maybe String
  , cte_CI :: Maybe String -- most CI services
  , cte_GITHUB_ACTIONS :: Maybe String -- Github Actions
  , cte_TF_BUILD :: Maybe String -- Azure Pipelines
  , cte_REACH_DEBUG :: Bool
  , cte_REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY :: Bool
  }

getCompilerEnv :: IO CompilerToolEnv
getCompilerEnv = do
  cte_REACHC_ID <- lookupEnv "REACHC_ID"
  cte_REACHC_HASH <- lookupEnv "REACHC_HASH"
  cte_CI <- lookupEnv "CI"
  cte_GITHUB_ACTIONS <- lookupEnv "GITHUB_ACTIONS"
  cte_TF_BUILD <- lookupEnv "TF_BUILD"
  cte_REACH_DEBUG <- truthyEnv <$> lookupEnv "REACH_DEBUG"
  cte_REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY <- truthyEnv <$> lookupEnv "REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY"
  return CompilerToolEnv {..}

truthyEnv :: Maybe String -> Bool
truthyEnv = \case
  Nothing -> False
  Just s -> not $ elem (map toLower s) ["", "0", "false", "f", "#f", "no", "off", "n"]
