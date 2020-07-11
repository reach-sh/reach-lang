module Main where

import Control.Monad.Except
import Data.Char
import System.Directory
import System.Environment
import Options.Applicative
import qualified Filesystem.Path.CurrentOS as FP

import Reach.Compiler(CompilerOpts(CompilerOpts), compile)
import Reach.CompilerNL(compileNL)

data CompilerToolOpts = CompilerToolOpts
  { output_dir :: FilePath
  , source :: FilePath
  , enableExperimentalConnectors :: Bool
  }

compiler :: FilePath -> Bool -> Parser CompilerToolOpts
compiler cwd expCon = CompilerToolOpts
  <$> strOption
  ( long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Directory for output files"
    <> showDefault
    <> value cwd )
  <*> strArgument (metavar "SOURCE")
  <*> pure expCon

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  expCon <- checkTruthyEnv "REACHC_ENABLE_EXPERIMENTAL_CONNECTORS"
  expComp <- checkTruthyEnv "REACHC_ENABLE_EXPERIMENTAL_COMPILER"
  let opts = info ( compiler cwd expCon <**> helper )
               ( fullDesc
               <> progDesc "verify and compile an Reach program"
               <> header "reachc - Reach compiler" )
  ctool_opts <- execParser opts
  copts <- makeCompilerOpts ctool_opts
  when expComp (compileNL copts)
  compile copts

makeCompilerOpts :: CompilerToolOpts -> IO CompilerOpts
makeCompilerOpts ctool_opts = do
  let srcp = source ctool_opts
  let srcbp = FP.basename $ FP.decodeString srcp
  let outd = output_dir ctool_opts
  let outdp = FP.decodeString outd
  let outn ext = FP.encodeString $ FP.append outdp $ srcbp `FP.addExtension` ext
  let out ext con = writeFile (outn ext) con
  createDirectoryIfMissing True outd
  return $ CompilerOpts out outn srcp (enableExperimentalConnectors ctool_opts)

checkTruthyEnv :: String -> IO Bool
checkTruthyEnv varName = do
  varValMay <- lookupEnv varName
  case varValMay of
    Just varVal -> return $ not isFalsy where
      isFalsy = isNo || isFalse || isEmpty || isZero
      varValLower = map toLower varVal
      isNo = varValLower == "no"
      isFalse = varValLower == "false"
      isEmpty = varValLower == ""
      isZero = varValLower == "0"
    Nothing -> return False
