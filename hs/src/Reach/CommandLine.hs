module Reach.CommandLine (CompilerToolArgs (..), compiler, getCompilerArgs) where

import Options.Applicative

data CompilerToolArgs = CompilerToolArgs
  { cta_intermediateFiles :: Bool
  , cta_disableReporting :: Bool
  , cta_errorFormatJson :: Bool
  , cta_installPkgs :: Bool
  , cta_dirDotReach :: Maybe FilePath
  , cta_outputDir :: Maybe FilePath
  , cta_source :: FilePath
  , cta_tops :: [String]
  }

compiler :: Parser CompilerToolArgs
compiler = CompilerToolArgs
  <$> switch (long "intermediate-files")
  <*> switch (long "disable-reporting")
  <*> switch (long "error-format-json")
  <*> switch (long "install-pkgs")
  <*> optional (strOption (long "dir-dot-reach"
      <> metavar "DIR-DOT-REACH"
      <> help "Package imports cache+lock directory"
      <> showDefault))
  <*> optional (strOption (long "output"
      <> short 'o'
      <> metavar "DIR"
      <> help "Directory for output files"
      <> showDefault))
  <*> strArgument ((metavar "SOURCE") <> value ("index.rsh"))
  <*> many (strArgument (metavar "EXPORTS..."))

getCompilerArgs :: String -> IO CompilerToolArgs
getCompilerArgs versionCliDisp = do
  let opts =
        info
          (compiler <**> helper)
          (fullDesc
             <> progDesc "verify and compile an Reach program"
             <> header versionCliDisp)
  execParser opts
