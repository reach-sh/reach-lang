module Reach.Verify.Shared
  ( VerifySt (..)
  , VerifyOpts (..)
  )
where

import Data.IORef
import Reach.Connector
import Reach.Counter
import Reach.OutputUtil
import Reach.VerifyReport

data VerifyOpts = VerifyOpts
  { vo_out :: Outputer
  , vo_mvcs :: Maybe Connectors
  , vo_timeout :: Integer
  , vo_dir :: FilePath
  , vo_first_fail_quit :: Bool
  , vo_report :: Maybe (IORef VerifyReportAccum)
  }

data VerifySt = VerifySt
  { vst_vo :: VerifyOpts
  , vst_res_succ :: Counter
  , vst_res_fail :: Counter
  , vst_res_time :: Counter
  , vst_res_reps :: Counter
  }
