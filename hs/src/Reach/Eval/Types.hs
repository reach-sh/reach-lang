{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Eval.Types where

import qualified Data.Map.Strict as M
import Generics.Deriving
import Reach.AST.Base
import Reach.AST.SL
import Reach.AST.DLBase

recursionDepthLimit :: Int
recursionDepthLimit = 2 ^ (16 :: Int)

type SLValTy = (SLVal, Maybe DLType)

data SLMode
  = --- The top-level of a module, before the App starts
    SLM_Module
  | --- The app starts in a "step"
    SLM_Step
  | --- An "only" moves from "step" to "local step" and then to "step" again, where x = live
    SLM_LocalStep
  | SLM_LocalPure
  | --- A "toconsensus" moves from "step" to "consensus step" then to "step" again
    SLM_ConsensusStep
  | SLM_ConsensusPure
  deriving (Bounded, Enum, Eq, Generic)

instance Show SLMode where
  show = \case
    SLM_Module -> "module"
    SLM_Step -> "step"
    SLM_LocalStep -> "local step"
    SLM_LocalPure -> "local pure"
    SLM_ConsensusStep -> "consensus step"
    SLM_ConsensusPure -> "consensus pure"

--- A state represents the state of the protocol, so it is returned
--- out of a function call.
data SLState = SLState
  { --- A function call may modify the mode
    st_mode :: SLMode
  , st_live :: Bool
  , st_after_first :: Bool
  , --- A function call may cause a participant to join
    st_pdvs :: SLPartDVars
  }
  deriving (Eq, Show)

all_slm_modes :: [SLMode]
all_slm_modes = enumFrom minBound

type SLPartDVars = M.Map SLPart DLVar

