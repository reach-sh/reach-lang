module Reach.Report
  ( Report (..)
  , ReportDestination (..)
  , ReportUrl (..)
  , ReportRef (..)
  , ReportArgs (..)
  , CompileLogId
  , makeReport
  , newCompileLogId
  , reportToDest
  , makeReportUrl
  , makeReportRef
  , makeDynamoReportUrl
  , -- more
    waitReports
  , getStartCompileLogId
  , setReportDestination
  , getReportDestination
  , enableReporting
  , disableReporting
  , setStartCompileLogId
  , reportText
  , reportError
  , reportInfo
  , report
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client.Conduit (httpNoBody)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import Reach.Report.Types
import Reach.Report.UnsafeGlobals
import Reach.Version

data ReportArgs = ReportArgs
  { ra_tag :: Text
  , ra_val :: Value
  }
  deriving (Eq, Show)

data ReportUrl = ReportUrl Request Manager (IORef [Async ()])

makeReportUrl :: String -> IO ReportUrl
makeReportUrl url = do
  req <- parseRequest url
  manager <- newManager tlsManagerSettings
  let req' = setRequestMethod "POST" req
  asyncsRef <- newIORef []
  return $ ReportUrl req' manager asyncsRef

makeDynamoReportUrl :: IO ReportUrl
makeDynamoReportUrl = makeReportUrl "https://log.reach.sh/submit"

instance ReportDestination ReportUrl where
  sendReport (ReportUrl req manager asyncsRef) r = do
    m <- async . void $
      flip runReaderT manager $ do
        lift $ putStrLn "sending..."
        let req' = setRequestBodyJSON r req
        void $ httpNoBody req'
        lift $ putStrLn "...sent"
    modifyIORef asyncsRef (m :)

  finalizeReports (ReportUrl _ _ asyncsRef) microseconds = do
    asyncs <- readIORef asyncsRef
    waitAll <- async $ mapM_ waitCatch asyncs
    timeUp <- async $ threadDelay microseconds
    waitEither_ waitAll timeUp

data ReportRef = ReportRef (IORef [Report])

makeReportRef :: IO ReportRef
makeReportRef = ReportRef <$> newIORef []

instance ReportDestination ReportRef where
  sendReport (ReportRef ref) r = do
    modifyIORef' ref (r :)

newCompileLogId :: IO CompileLogId
newCompileLogId = CompileLogId <$> getCurrentTime

getReportUser :: IO ReportUser
getReportUser = return $ ReportUser "XXX:ReportUser"

makeReport :: ReportArgs -> IO Report
makeReport args = do
  compileLogId <- newCompileLogId
  user <- getReportUser
  -- Instead of doing this, let's just have them match
  -- even though cli is supposed to be opaque.
  -- time <- getCurrentTime
  let CompileLogId time = compileLogId
  return $
    Report
      { report_CompileLogId = compileLogId
      , report_startCompileLogId = Nothing
      , report_version = version
      , report_user = user
      , report_time = time
      , report_tag = ra_tag args
      , report_val = ra_val args
      }

-- | Note: A fresh CompileLogId will be generated
-- for this report, but the startCompileLogId is to link it back
-- to other reports in the same compiler invocation.
reportToDest :: ReportDestination dest => dest -> CompileLogId -> ReportArgs -> IO ()
reportToDest dest startCompileLogId args = do
  r <- makeReport args
  sendReport dest $ r {report_startCompileLogId = Just startCompileLogId}

enableReporting :: IO ()
enableReporting = do
  dest <- SomeReportDestination <$> makeDynamoReportUrl
  setReportDestination dest

-- | Just a type-constrained report
reportText :: Text -> Text -> IO ()
reportText = report

reportError :: SomeException -> IO ()
reportError = report "error" . show

reportInfo :: Text -> IO ()
reportInfo = reportText "info"

report :: ToJSON v => Text -> v -> IO ()
report tag val = do
  compileLogId <- getStartCompileLogId
  dest <- readIORef globalReportDestination
  reportToDest dest compileLogId $
    ReportArgs
      { ra_tag = tag
      , ra_val = toJSON val
      }

waitReports :: Int -> IO ()
waitReports microseconds = do
  dest <- getReportDestination
  finalizeReports dest microseconds

getStartCompileLogId :: IO CompileLogId
getStartCompileLogId =
  readIORef globalStartCompileLogId >>= \case
    Just compileLogId -> return compileLogId
    -- XXX probably don't want this
    Nothing -> do
      compileLogId <- newCompileLogId
      setStartCompileLogId compileLogId
      return compileLogId

setReportDestination :: SomeReportDestination -> IO ()
setReportDestination dest = do
  writeIORef globalReportDestination dest

getReportDestination :: IO SomeReportDestination
getReportDestination = readIORef globalReportDestination

disableReporting :: IO ()
disableReporting = setReportDestination noReportDestination

setStartCompileLogId :: CompileLogId -> IO ()
setStartCompileLogId compileLogId = do
  writeIORef globalStartCompileLogId $ Just compileLogId
