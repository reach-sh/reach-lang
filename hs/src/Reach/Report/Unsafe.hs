{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For advice on writing unsafe functions, see:
-- http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html

module Reach.Report.Unsafe
  ( -- For regular usage
    unsafeReportError
  , report
  , reportError
  , reportInfo
  , ReportArgs (..)
  , setStartCompileLogId
  , -- For regular usage (opt out) & testing
    disableReporting
  , enableReporting
  , waitReports
  , getReportDestination
  , -- For testing
    setReportDestination
  , SomeReportDestination
  )
where

import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Reach.Report
import System.IO.Unsafe

data SomeReportDestination where
  SomeReportDestination :: ReportDestination a => a -> SomeReportDestination

instance ReportDestination SomeReportDestination where
  sendReport (SomeReportDestination dest) t = sendReport dest t
  finalizeReports (SomeReportDestination dest) = finalizeReports dest

-- | Global var for report destination.
-- Reporting is disabled by default.
-- Use enableReporting, disableReporting, or setReportDestination.
globalReportDestination :: IORef SomeReportDestination
globalReportDestination = unsafePerformIO $ do
  newIORef noReportDestination
{-# NOINLINE globalReportDestination #-}

-- | Global var for start CompileLogId
globalStartCompileLogId :: IORef (Maybe CompileLogId)
globalStartCompileLogId = unsafePerformIO $ do
  newIORef Nothing
{-# NOINLINE globalStartCompileLogId #-}

setReportDestination :: SomeReportDestination -> IO ()
setReportDestination dest = do
  writeIORef globalReportDestination dest

getReportDestination :: IO SomeReportDestination
getReportDestination = readIORef globalReportDestination

dynamoReportDestination :: SomeReportDestination
dynamoReportDestination = unsafePerformIO $ do
  SomeReportDestination <$> makeDynamoReportUrl
{-# NOINLINE dynamoReportDestination #-}

noReportDestination :: SomeReportDestination
noReportDestination = SomeReportDestination ()

unsafeReportError :: HasCallStack => Text -> a
unsafeReportError t = error $! s
  where
    s = unsafePerformIO $ do
      reportError t
      return (T.unpack t)
{-# NOINLINE unsafeReportError #-}

disableReporting :: IO ()
disableReporting = setReportDestination noReportDestination

setStartCompileLogId :: CompileLogId -> IO ()
setStartCompileLogId compileLogId = do
  writeIORef globalStartCompileLogId $ Just compileLogId

getStartCompileLogId :: IO CompileLogId
getStartCompileLogId =
  readIORef globalStartCompileLogId >>= \case
    Just compileLogId -> return compileLogId
    -- XXX probably don't want this
    Nothing -> do
      compileLogId <- newCompileLogId
      setStartCompileLogId compileLogId
      return compileLogId

enableReporting :: IO ()
enableReporting = do
  setReportDestination dynamoReportDestination

-- | Just a type-constrained report
reportText :: Text -> Text -> IO ()
reportText = report

reportError :: Text -> IO ()
reportError = reportText "error"

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
