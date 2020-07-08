module Main where

import Data.Char
import System.Directory
import System.Environment
import Options.Applicative

import Reach.Compiler

compiler :: FilePath -> Bool -> Parser CompilerOpts
compiler cwd skipGoalEnvOpt = CompilerOpts
  <$> strOption
  ( long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Directory for output files"
    <> showDefault
    <> value cwd )
  <*> strArgument (metavar "SOURCE")
  <*> pure skipGoalEnvOpt

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  skipGoalEnvOpt <- checkTruthyEnv "REACHC_SKIP_GOAL"

  let opts = info ( compiler cwd skipGoalEnvOpt <**> helper )
               ( fullDesc
               <> progDesc "verify and compile an Reach program"
               <> header "reachc - Reach compiler" )
  copts <- execParser opts
  compile copts


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
