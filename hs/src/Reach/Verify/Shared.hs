module Reach.Verify.Shared (VerifySt (..)) where

import Reach.Counter

data VerifySt = VerifySt
  { vst_res_succ :: Counter
  , vst_res_fail :: Counter
  }
