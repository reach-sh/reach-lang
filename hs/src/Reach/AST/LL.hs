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
  | LLC_FromConsensus SrcLoc SrcLoc [SLCtxtFrame] LLStep
  | LLC_While
      { llc_w_at :: SrcLoc
      , llc_w_asn :: DLAssignment
      , llc_w_invs :: [DLInvariant DLBlock]
      , llc_w_cond :: DLBlock
      , llc_w_body :: LLConsensus
      , llc_w_k :: LLConsensus
      }
  | LLC_Continue SrcLoc DLAssignment
  | LLC_ViewIs SrcLoc (Maybe SLPart) SLVar (Maybe DLExportBlock) LLConsensus
  deriving (Eq)

instance SrcLocOf LLConsensus where
  srclocOf = \case
    LLC_Com s _ -> srclocOf s
    LLC_If a _ _ _ -> a
    LLC_Switch a _ _ -> a
    LLC_FromConsensus a _ _ _ -> a
    LLC_While {..} -> llc_w_at
    LLC_Continue a _ -> a
    LLC_ViewIs a _ _ _ _ -> a

instance Pretty LLConsensus where
  pretty = \case
    LLC_Com x k -> prettyCom x k
    LLC_If _at ca t f -> prettyIfp ca t f
    LLC_Switch _at ov csm -> pretty $ SwitchCasesUse ov csm
    LLC_FromConsensus _at _ret_at _fs k ->
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
      , lls_tc_lct :: DLArg
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
      prettyToConsensus__ lls_tc_lct lls_tc_send lls_tc_recv lls_tc_mtime

data LLOpts = LLOpts
  { llo_counter :: Counter
  , llo_droppedAsserts :: Counter
  , llo_aem :: ALGOExitMode
  }
  deriving (Generic, Eq)

instance HasCounter LLOpts where
  getCounter = llo_counter

instance HasALGOExitMode LLOpts where
  getALGOExitMode = llo_aem

data LLProg = LLProg
  { llp_at :: SrcLoc
  , llp_opts :: LLOpts
  , llp_parts :: SLParts
  , llp_init :: DLInit
  , llp_exports :: DLExports
  , llp_views :: DLViews
  , llp_apis :: DLAPIs
  , llp_aliases :: Aliases
  , llp_events :: DLEvents
  , llp_step :: LLStep
  }
  deriving (Eq)

instance HasCounter LLProg where
  getCounter = getCounter . llp_opts

instance Pretty LLProg where
  pretty (LLProg {..}) =
    "#lang ll" <> hardline
      <> pretty llp_parts
      <> hardline
      <> hardline
      <> pretty llp_init
      <> hardline
      <> pretty llp_exports
      <> hardline
      <> pretty llp_views
      <> hardline
      <> pretty llp_apis
      <> hardline
      <> pretty llp_aliases
      <> hardline
      <> pretty llp_events
      <> hardline
      <> pretty llp_step
