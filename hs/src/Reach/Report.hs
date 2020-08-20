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
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.IORef
import Data.Text (Text)
import Data.Time
import Data.Version (Version)
import Network.HTTP.Client.Conduit (httpNoBody)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import Paths_reach (version)
import Reach.Report.TH

data ReportArgs = ReportArgs
  { ra_tag :: Text
  , ra_val :: Value
  }
  deriving (Eq, Show)

-- | A unique identifier for each report
newtype CompileLogId = CompileLogId UTCTime
  deriving newtype (Eq, FromJSON, Show, ToJSON)

-- | An attempt to uniquely identify users
newtype ReportUser = ReportUser Text
  deriving newtype (Eq, FromJSON, Show, ToJSON)

data Report = Report
  { report_CompileLogId :: CompileLogId
  , report_startCompileLogId :: Maybe CompileLogId
  , report_version :: Version
  , report_user :: ReportUser
  , report_time :: UTCTime
  , report_tag :: Text
  , report_val :: Value
  }
  deriving (Eq, Show)

$(deriveJSON reachJSONOptions 'Report)

class ReportDestination dest where
  sendReport :: dest -> Report -> IO ()

  -- | Int is number of microseconds to wait
  finalizeReports :: dest -> Int -> IO ()
  finalizeReports _ _ = return ()

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

-- How to turn off reports
instance ReportDestination () where
  sendReport () _r = return ()

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
