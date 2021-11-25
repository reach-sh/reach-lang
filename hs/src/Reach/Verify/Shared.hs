module Reach.Verify.Shared
  ( VerifySt (..)
  , VerifyOpts (..)
  ) where

import qualified Data.Text as T
import Reach.Connector
import Reach.Counter

data VerifyOpts = VerifyOpts
  { vo_out :: Maybe (T.Text -> String)
  , vo_mvcs :: Maybe [Connector]
  , vo_timeout :: Integer
  , vo_dir :: FilePath
  }

data VerifySt = VerifySt
  { vst_vo :: VerifyOpts
  , vst_res_succ :: Counter
  , vst_res_fail :: Counter
  , vst_res_time :: Counter
  , vst_res_reps :: Counter
  }
