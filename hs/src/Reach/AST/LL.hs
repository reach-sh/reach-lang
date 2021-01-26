{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.LL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase

type LLVar = Maybe DLVar

type LLCommon = DLinStmt LLVar

type LLTail = DLinTail LLVar

type LLBlock = DLinBlock LLVar

data LLConsensus
  = LLC_Com LLCommon LLConsensus
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Switch SrcLoc DLVar (SwitchCases LLConsensus)
  | LLC_FromConsensus SrcLoc SrcLoc LLStep
  | LLC_While
      { llc_w_at :: SrcLoc
      , llc_w_asn :: DLAssignment
      , llc_w_inv :: LLBlock
      , llc_w_cond :: LLBlock
      , llc_w_body :: LLConsensus
      , llc_w_k :: LLConsensus
      }
  | LLC_Continue SrcLoc DLAssignment
  | LLC_Only SrcLoc SLPart LLTail LLConsensus
  deriving (Eq, Show)

data LLStep
  = LLS_Com LLCommon LLStep
  | LLS_Stop SrcLoc
  | LLS_Only SrcLoc SLPart LLTail LLStep
  | LLS_ToConsensus
      { lls_tc_at :: SrcLoc
      , lls_tc_send :: M.Map SLPart (Bool, [DLArg], DLArg, DLArg)
      , lls_tc_recv :: (Maybe DLVar, DLVar, [DLVar], DLVar, DLVar, LLConsensus)
      , lls_tc_mtime :: Maybe (DLArg, LLStep)
      }
  deriving (Eq, Show)

data LLOpts = LLOpts
  { llo_deployMode :: DeployMode
  , llo_verifyOverflow :: Bool
  , llo_counter :: Int
  }
  deriving (Generic, Eq, Show)

data LLProg
  = LLProg SrcLoc LLOpts SLParts DLInit LLStep
  deriving (Eq, Show)
