{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.LL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase

data LLCommon a
  = LL_Return SrcLoc
  | LL_Let SrcLoc (Maybe DLVar) DLExpr a
  | LL_ArrayMap SrcLoc DLVar DLArg DLVar LLBlock a
  | LL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar LLBlock a
  | LL_Var SrcLoc DLVar a
  | LL_Set SrcLoc DLVar DLArg a
  | LL_LocalIf SrcLoc DLArg LLLocal LLLocal a
  | LL_LocalSwitch SrcLoc DLVar (SwitchCases LLLocal) a
  deriving (Eq, Show)

data LLLocal
  = LLL_Com (LLCommon LLLocal)
  deriving (Eq, Show)

data LLBlock
  = LLBlock SrcLoc [SLCtxtFrame] LLLocal DLArg
  deriving (Eq, Show)

data LLConsensus
  = LLC_Com (LLCommon LLConsensus)
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Switch SrcLoc DLVar (SwitchCases LLConsensus)
  | LLC_FromConsensus SrcLoc SrcLoc LLStep
  | --- inv then cond then body then kont
    LLC_While
      { llc_w_at :: SrcLoc
      , llc_w_asn :: DLAssignment
      , llc_w_inv :: LLBlock
      , llc_w_cond :: LLBlock
      , llc_w_body :: LLConsensus
      , llc_w_k :: LLConsensus
      }
  | LLC_Continue SrcLoc DLAssignment
  | LLC_Only SrcLoc SLPart LLLocal LLConsensus
  deriving (Eq, Show)

data LLStep
  = LLS_Com (LLCommon LLStep)
  | LLS_Stop SrcLoc
  | LLS_Only SrcLoc SLPart LLLocal LLStep
  | LLS_ToConsensus
      { lls_tc_at :: SrcLoc
      , lls_tc_send :: M.Map SLPart (Bool, [DLArg], DLArg, DLArg)
      , lls_tc_recv :: (DLVar, [DLVar], DLVar, LLConsensus)
      , lls_tc_mtime :: Maybe (DLArg, LLStep)
      }
  deriving (Eq, Show)

data LLOpts = LLOpts
  { llo_deployMode :: DeployMode
  , llo_verifyOverflow :: Bool
  }
  deriving (Generic, Eq, Show)

data LLProg
  = LLProg SrcLoc LLOpts SLParts LLStep
  deriving (Eq, Show)
