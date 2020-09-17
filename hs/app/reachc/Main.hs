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
  reachcId <- lookupEnv "REACHC_ID"
  return
    CompilerToolEnv
      { cte_REACHC_ID = reachcId
      }

main :: IO ()
main = do
  args <- getCompilerArgs
  env <- getCompilerEnv
  report <-
    case cta_disableReporting args of
      True -> return $ const $ return ()
      False -> startReport (cte_REACHC_ID env)
  let ctool_opts = makeCompilerToolOpts args env
  (e :: Either SomeException ()) <-
    try $ compilerToolMain ctool_opts
  report e
  case e of
    Left exn -> throwIO exn
    Right () -> return ()
