module Main (main) where

import Control.Exception
import Options.Applicative
import Reach.CompilerTool
import Reach.Report
import System.Directory
import System.Environment

data CompilerToolArgs = CompilerToolArgs
  { cta_outputDir :: FilePath
  , cta_source :: FilePath
  , cta_tops :: [String]
  , cta_intermediateFiles :: Bool
  , cta_disableReporting :: Bool
  }

data CompilerToolEnv = CompilerToolEnv
  { cte_REACHC_ID :: Maybe String
  }

makeCompilerToolOpts :: CompilerToolArgs -> CompilerToolEnv -> CompilerToolOpts
makeCompilerToolOpts CompilerToolArgs {..} CompilerToolEnv {} =
  CompilerToolOpts
    { cto_outputDir = cta_outputDir
    , cto_source = cta_source
    , cto_tops = if null cta_tops then ["main"] else cta_tops
    , cto_intermediateFiles = cta_intermediateFiles
    }

compiler :: FilePath -> Parser CompilerToolArgs
compiler cwd =
  CompilerToolArgs
    <$> strOption
      (long "output"
         <> short 'o'
         <> metavar "DIR"
         <> help "Directory for output files"
         <> showDefault
         <> value cwd)
    <*> strArgument (metavar "SOURCE")
    <*> many (strArgument (metavar "EXPORTS..."))
    <*> switch (long "intermediate-files")
    <*> switch (long "disable-reporting")

getCompilerArgs :: IO CompilerToolArgs
getCompilerArgs = do
  cwd <- getCurrentDirectory
  let opts =
        info
          (compiler cwd <**> helper)
          (fullDesc
             <> progDesc "verify and compile an Reach program"
             <> header "reachc - Reach compiler")
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
  --- TODO: collect interesting stats to report at the end
  (e :: Either SomeException ()) <- try $ compilerToolMain ctool_opts
  report e
  case e of
    Left exn -> throwIO exn
    Right () -> return ()
