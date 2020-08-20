module Main (main) where

import Control.Exception
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.TH
import Data.Fixed (Pico)
import Data.Time
import Options.Applicative
import Reach.CompilerTool
import Reach.Report
import Reach.Report.TH
import System.Directory

data CompilerToolArgs = CompilerToolArgs
  { cta_outputDir :: FilePath
  , cta_source :: FilePath
  , cta_tops :: [String]
  , cta_intermediateFiles :: Bool
  , cta_enableReporting :: Bool
  }

$(deriveJSON reachJSONOptions 'CompilerToolArgs)

data CompilerToolEnv = CompilerToolEnv
  {}

$(deriveJSON reachJSONOptions 'CompilerToolEnv)

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
    <*> switch (long "enable-reporting")

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

-- Note: This impl is a little more manual than other invocations because
-- we want to make sure startCompileLogId and CompileLogId match.
sendStartReport :: CompilerToolArgs -> CompilerToolEnv -> IO ()
sendStartReport args env = do
  let val = object ["args" .= args, "env" .= env]
  r <- makeReport ReportArgs {ra_tag = "start", ra_val = val}
  let compileLogId = report_CompileLogId r
  setStartCompileLogId compileLogId
  let r' = r {report_startCompileLogId = Just compileLogId}
  dest <- getReportDestination
  sendReport dest r'

errorGracePeriodMicroseconds :: Int
errorGracePeriodMicroseconds = 10_000_000

successGracePeriodMicroseconds :: NominalDiffTime -> Int
successGracePeriodMicroseconds elapsed =
  picoToMicroInt $ nominalDiffTimeToSeconds elapsed * graceFactor
  where
    graceFactor = 0.1
    -- Note: fromEnum on Pico overflows the Int at 100s of days,
    -- but our use case is seconds/mins which should be fine.
    picoToMicroInt (p :: Pico) = fromEnum $ p / picosInMicro
    picosInMicro = 1_000_000

main :: IO ()
main = do
  startTime <- getCurrentTime
  args <- getCompilerArgs
  env <- getCompilerEnv
  let ctool_opts = makeCompilerToolOpts args env
  when (cta_enableReporting args) $ enableReporting
  sendStartReport args env
  -- TODO: collect interesting stats to report at the end
  (e :: Either SomeException ()) <- try $ compilerToolMain ctool_opts
  either reportError return e
  endTime <- getCurrentTime
  let result :: String = either (const "failure") (const "success") e
  let elapsed = diffUTCTime endTime startTime
  let stats =
        object
          [ "elapsed" .= elapsed
          , "result" .= result
          ]
  report "end" stats
  waitReports 3_000_000 -- microseconds
  case e of
    Left exn -> do
      waitReports errorGracePeriodMicroseconds
      throwIO exn
    Right () -> do
      waitReports $ successGracePeriodMicroseconds elapsed
      return ()
