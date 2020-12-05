{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DL where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase

data DLStmt
  = DLS_Let SrcLoc (Maybe DLVar) DLExpr
  | DLS_ArrayMap SrcLoc DLVar DLArg DLVar DLBlock
  | DLS_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar DLBlock
  | DLS_If SrcLoc DLArg StmtAnnot DLStmts DLStmts
  | DLS_Switch SrcLoc DLVar StmtAnnot (SwitchCases DLStmts)
  | DLS_Return SrcLoc Int (Either Int DLArg)
  | DLS_Prompt SrcLoc (Either Int (DLVar, M.Map Int (DLStmts, DLArg))) DLStmts
  | DLS_Stop SrcLoc
  | DLS_Only SrcLoc SLPart DLStmts
  | DLS_ToConsensus
      { dls_tc_at :: SrcLoc
      , dls_tc_send :: M.Map SLPart ([DLArg], DLArg)
      , dls_tc_recv :: (DLVar, [DLVar], DLVar, DLStmts)
      , dls_tc_mtime :: Maybe (DLArg, DLStmts)
      }
  | DLS_FromConsensus SrcLoc DLStmts
  | DLS_While
      { dls_w_at :: SrcLoc
      , dls_w_asn :: DLAssignment
      , dls_w_inv :: DLBlock
      , dls_w_cond :: DLBlock
      , dls_w_body :: DLStmts
      }
  | DLS_Continue SrcLoc DLAssignment
  | DLS_FluidSet SrcLoc FluidVar DLArg
  | DLS_FluidRef SrcLoc DLVar FluidVar
  deriving (Eq, Generic, NFData, Show)

instance SrcLocOf DLStmt where
  srclocOf = \case
    DLS_Let a _ _ -> a
    DLS_ArrayMap a _ _ _ _ -> a
    DLS_ArrayReduce a _ _ _ _ _ _ -> a
    DLS_If a _ _ _ _ -> a
    DLS_Switch a _ _ _ -> a
    DLS_Return a _ _ -> a
    DLS_Prompt a _ _ -> a
    DLS_Stop a -> a
    DLS_Only a _ _ -> a
    DLS_ToConsensus {..} -> dls_tc_at
    DLS_FromConsensus a _ -> a
    DLS_While {..} -> dls_w_at
    DLS_Continue a _ -> a
    DLS_FluidSet a _ _ -> a
    DLS_FluidRef a _ _ -> a

instance IsPure DLStmt where
  isPure = \case
    DLS_Let _ _ e -> isPure e
    DLS_ArrayMap {} -> True
    DLS_ArrayReduce {} -> True
    DLS_If _ _ a _ _ -> isPure a
    DLS_Switch _ _ a _ -> isPure a
    DLS_Return {} -> False
    DLS_Prompt _ _ ss -> isPure ss
    DLS_Stop {} -> False
    DLS_Only _ _ ss -> isPure ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> isPure ss
    DLS_While {} -> False
    DLS_Continue {} -> False
    DLS_FluidSet {} -> False
    DLS_FluidRef {} -> True

instance IsLocal DLStmt where
  isLocal = \case
    DLS_Let _ _ e -> isLocal e
    DLS_ArrayMap {} -> True
    DLS_ArrayReduce {} -> True
    DLS_If _ _ a _ _ -> isLocal a
    DLS_Switch _ _ a _ -> isLocal a
    DLS_Return {} -> True
    DLS_Prompt _ _ ss -> isLocal ss
    DLS_Stop {} -> False
    DLS_Only _ _ ss -> isLocal ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> isLocal ss
    DLS_While {} -> False
    DLS_Continue {} -> False
    DLS_FluidSet {} -> True
    DLS_FluidRef {} -> True

type DLStmts = Seq.Seq DLStmt

data DLBlock
  = DLBlock SrcLoc [SLCtxtFrame] DLStmts DLArg
  deriving (Eq, Generic, NFData, Show)

data DLOpts = DLOpts
  { dlo_deployMode :: DeployMode
  , dlo_verifyOverflow :: Bool
  , dlo_verifyPerConnector :: Bool
  , dlo_connectors :: [T.Text]
  }
  deriving (Eq, Generic, NFData, Show)

data DLProg
  = DLProg SrcLoc DLOpts SLParts DLStmts
  deriving (Generic, NFData)

