{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.LL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

data LLConsensus
  = LLC_Com DLStmt LLConsensus
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Switch SrcLoc DLVar (SwitchCases LLConsensus)
  | LLC_FromConsensus SrcLoc SrcLoc LLStep
  | LLC_While
      { llc_w_at :: SrcLoc
      , llc_w_asn :: DLAssignment
      , llc_w_inv :: DLBlock
      , llc_w_cond :: DLBlock
      , llc_w_body :: LLConsensus
      , llc_w_k :: LLConsensus
      }
  | LLC_Continue SrcLoc DLAssignment
  | LLC_ViewIs SrcLoc SLPart SLVar (Maybe DLExportBlock) LLConsensus
  deriving (Eq)

instance Pretty LLConsensus where
  pretty = \case
    LLC_Com x k -> prettyCom x k
    LLC_If _at ca t f -> prettyIfp ca t f
    LLC_Switch _at ov csm -> prettySwitch ov csm
    LLC_FromConsensus _at _ret_at k ->
      prettyCommit <> hardline <> pretty k
    LLC_While _at asn inv cond body k ->
      prettyWhile asn inv cond (pretty body) <> hardline <> pretty k
    LLC_Continue _at asn -> prettyContinue asn
    LLC_ViewIs _ vn vk a k ->
      prettyViewIs vn vk a <> hardline <> pretty k

data LLStep
  = LLS_Com DLStmt LLStep
  | LLS_Stop SrcLoc
  | LLS_ToConsensus
      { lls_tc_at :: SrcLoc
      , lls_tc_send :: M.Map SLPart DLSend
      , lls_tc_recv :: DLRecv LLConsensus
      , lls_tc_mtime :: Maybe (DLTimeArg, LLStep)
      }
  deriving (Eq)

instance Pretty LLStep where
  pretty = \case
    LLS_Com x k -> prettyCom x k
    LLS_Stop _at -> prettyStop
    LLS_ToConsensus {..} ->
      prettyToConsensus__ lls_tc_send lls_tc_recv lls_tc_mtime

data LLOpts = LLOpts
  { llo_deployMode :: DeployMode
  , llo_verifyArithmetic :: Bool
  , llo_counter :: Counter
  , llo_droppedAsserts :: Int
  }
  deriving (Generic, Eq)

instance HasCounter LLOpts where
  getCounter (LLOpts {..}) = llo_counter

data LLProg
  = LLProg SrcLoc LLOpts SLParts DLInit DLExports DLViews LLStep
  deriving (Eq)

instance HasCounter LLProg where
  getCounter (LLProg _ llo _ _ _ _ _) = getCounter llo

instance Pretty LLProg where
  pretty (LLProg _at _ sps dli dex dvs db) =
    "#lang ll" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> hardline
      <> pretty dex
      <> hardline
      <> pretty dvs
      <> hardline
      <> pretty db
