module Main (main) where

import Options.Applicative
import Reach.CompilerTool
import System.Directory

data CompilerToolArgs = CompilerToolArgs
  { cta_outputDir :: FilePath
  , cta_source :: FilePath
  , cta_tops :: [String]
  , cta_intermediateFiles :: Bool
  }

data CompilerToolEnv = CompilerToolEnv
  {}

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
  return
    CompilerToolEnv
      {
      }

main :: IO ()
main = do
  args <- getCompilerArgs
  env <- getCompilerEnv
  let ctool_opts = makeCompilerToolOpts args env
  compilerToolMain ctool_opts
