module Main where

import System.Directory
import Options.Applicative

import Reach.Compiler

compiler :: FilePath -> Parser CompilerOpts
compiler cwd = CompilerOpts
  <$> strOption
  ( long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Directory for output files"
    <> showDefault
    <> value cwd )
  <*> strArgument (metavar "SOURCE")

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let opts = info ( compiler cwd <**> helper )
               ( fullDesc
               <> progDesc "verify and compile an Reach program"
               <> header "reachc - Reach compiler" )
  copts <- execParser opts
  compile copts
