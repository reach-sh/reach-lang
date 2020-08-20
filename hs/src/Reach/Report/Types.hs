{-# LANGUAGE GADTs #-}

module Reach.Report.Types
  ( CompileLogId (..)
  , ReportUser (..)
  , Report (..)
  , ReportDestination (..)
  , SomeReportDestination (..)
  , noReportDestination
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time
import Data.Version
import Reach.Report.TH

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

data SomeReportDestination where
  SomeReportDestination :: ReportDestination a => a -> SomeReportDestination

instance ReportDestination SomeReportDestination where
  sendReport (SomeReportDestination dest) t = sendReport dest t
  finalizeReports (SomeReportDestination dest) = finalizeReports dest

-- How to turn off reports
instance ReportDestination () where
  sendReport () _r = return ()

noReportDestination :: SomeReportDestination
noReportDestination = SomeReportDestination ()
