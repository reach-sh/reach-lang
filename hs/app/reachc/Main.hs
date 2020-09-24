module Main (main) where

import Control.Exception
import Options.Applicative
import Reach.CompilerTool
import Reach.Report
import Reach.Version
import System.Environment
import System.FilePath

data CompilerToolArgs = CompilerToolArgs
  { cta_intermediateFiles :: Bool
  , cta_disableReporting :: Bool
  , cta_outputDir :: Maybe FilePath
  , cta_source :: FilePath
  , cta_tops :: [String]
  }

data CompilerToolEnv = CompilerToolEnv
  { cte_REACHC_ID :: Maybe String
  , cte_CI :: Maybe String -- most CI services
  , cte_GITHUB_ACTIONS :: Maybe String -- Github Actions
  , cte_TF_BUILD :: Maybe String -- Azure Pipelines
  }

makeCompilerToolOpts :: CompilerToolArgs -> CompilerToolEnv -> CompilerToolOpts
makeCompilerToolOpts CompilerToolArgs {..} CompilerToolEnv {} =
  CompilerToolOpts
    { cto_outputDir = maybe defaultOutputDir id cta_outputDir
    , cto_source = cta_source
    , cto_tops = if null cta_tops then ["main"] else cta_tops
    , cto_intermediateFiles = cta_intermediateFiles
    }
  where
    defaultOutputDir = takeDirectory cta_source </> "build"

compiler :: Parser CompilerToolArgs
compiler =
  CompilerToolArgs
    <$> switch (long "intermediate-files")
    <*> switch (long "disable-reporting")
    <*> optional
      (strOption
         (long "output"
            <> short 'o'
            <> metavar "DIR"
            <> help "Directory for output files"
            <> showDefault))
    <*> strArgument ((metavar "SOURCE") <> value ("index.rsh"))
    <*> many (strArgument (metavar "EXPORTS..."))

getCompilerArgs :: IO CompilerToolArgs
getCompilerArgs = do
  let opts =
        info
          (compiler <**> helper)
          (fullDesc
             <> progDesc "verify and compile an Reach program"
             <> header ("reachc " <> versionStr <> " - Reach compiler"))
  execParser opts

getCompilerEnv :: IO CompilerToolEnv
getCompilerEnv = do
  cte_REACHC_ID <- lookupEnv "REACHC_ID"
  cte_CI <- lookupEnv "CI"
  cte_GITHUB_ACTIONS <- lookupEnv "GITHUB_ACTIONS"
  cte_TF_BUILD <- lookupEnv "TF_BUILD"
  return CompilerToolEnv {..}

shouldReport :: CompilerToolArgs -> CompilerToolEnv -> Bool
shouldReport CompilerToolArgs {..} CompilerToolEnv {..} =
  not cta_disableReporting
    && all emptyish [cte_CI, cte_GITHUB_ACTIONS, cte_TF_BUILD]
  where
    emptyish Nothing = True
    emptyish (Just x) = x == ""

main :: IO ()
main = do
  args <- getCompilerArgs
  env <- getCompilerEnv
  report <-
    case shouldReport args env of
      False -> return $ const $ return ()
      True -> startReport (cte_REACHC_ID env)
  let ctool_opts = makeCompilerToolOpts args env
  (e :: Either SomeException ()) <-
    try $ compilerToolMain ctool_opts
  report e
  case e of
    Left exn -> throwIO exn
    Right () -> return ()
