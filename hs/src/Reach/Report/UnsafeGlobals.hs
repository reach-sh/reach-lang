{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For advice on writing unsafe functions, see:
-- http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html

module Reach.Report.UnsafeGlobals
  ( globalReportDestination
  , globalStartCompileLogId
  )
where

import Data.IORef
import Reach.Report.Types
import System.IO.Unsafe

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
