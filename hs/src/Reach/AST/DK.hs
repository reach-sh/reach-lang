{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DK where

import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL

data DKCommon
  = DKC_ LLCommon
  | DKC_FluidSet SrcLoc FluidVar DLArg
  | DKC_FluidRef SrcLoc DLVar FluidVar
  deriving (Eq, Show)

data DKTail
  = DK_Com DKCommon DKTail
  | DK_Stop SrcLoc
  | DK_Only SrcLoc SLPart LLTail DKTail
  | DK_ToConsensus
      { dk_tc_at :: SrcLoc
      , dk_tc_send :: M.Map SLPart (Bool, [DLArg], DLArg, DLArg)
      , dk_tc_recv :: (Maybe DLVar, DLVar, [DLVar], DLVar, DLVar, DKTail)
      , dk_tc_mtime :: Maybe (DLArg, DKTail)
      }
  | DK_If SrcLoc DLArg DKTail DKTail
  | DK_Switch SrcLoc DLVar (SwitchCases DKTail)
  | DK_FromConsensus SrcLoc SrcLoc DKTail
  | DK_While
      { dk_w_at :: SrcLoc
      , dk_w_asn :: DLAssignment
      , dk_w_inv :: DKBlock
      , dk_w_cond :: DKBlock
      , dk_w_body :: DKTail
      , dk_w_k :: DKTail
      }
  | DK_Continue SrcLoc DLAssignment
  deriving (Eq, Show)

data DKBlock = DKBlock SrcLoc [SLCtxtFrame] DKTail DLArg
  deriving (Eq, Show)

data DKProg
  = DKProg SrcLoc DLOpts SLParts DLInit DKTail

