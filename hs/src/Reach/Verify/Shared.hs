module Reach.Verify.Shared (VerifySt(..)) where

import Data.IORef

data VerifySt = VerifySt
  { vst_res_succ :: IORef Int
  , vst_res_fail :: IORef Int }

