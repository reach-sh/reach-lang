{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DK where

import qualified Data.Map.Strict as M
import Generics.Deriving
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.Pretty
import Reach.Texty

data DKCommon
  = DKC_Let SrcLoc (Maybe DLVar) DLExpr
  | DKC_ArrayMap SrcLoc DLVar DLArg DLVar DKBlock
  | DKC_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar DKBlock
  | DKC_Var SrcLoc DLVar
  | DKC_Set SrcLoc DLVar DLArg
  | DKC_LocalIf SrcLoc DLArg DKTail DKTail
  | DKC_LocalSwitch SrcLoc DLVar (SwitchCases DKTail)
  | DKC_MapReduce SrcLoc Int DLVar DLMVar DLArg DLVar DLVar DKBlock
  | DKC_FluidSet SrcLoc FluidVar DLArg
  | DKC_FluidRef SrcLoc DLVar FluidVar
  deriving (Eq, Generic)

instance Pretty DKCommon where
  pretty = \case
    DKC_Let _at x de -> "const" <+> pretty x <+> "=" <+> pretty de <> semi
    DKC_ArrayMap _ ans x a f -> prettyMap ans x a f
    DKC_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
    DKC_Var _at dv -> "let" <+> pretty dv <> semi
    DKC_Set _at dv da -> pretty dv <+> "=" <+> pretty da <> semi
    DKC_LocalIf _at ca t f -> prettyIfp ca t f
    DKC_LocalSwitch _at ov csm -> prettySwitch ov csm
    DKC_MapReduce _ _mri ans x z b a f -> prettyReduce ans x z b a f
    DKC_FluidSet at fv a ->
      pretty (DLS_FluidSet at fv a)
    DKC_FluidRef at dv fv ->
      pretty (DLS_FluidRef at dv fv)

data DKTail
  = DK_Com DKCommon DKTail
  | DK_Stop SrcLoc
  | DK_Only SrcLoc SLPart DKTail DKTail
  | DK_ToConsensus
      { dk_tc_at :: SrcLoc
      , dk_tc_send :: M.Map SLPart (Bool, [DLArg], DLArg, DLArg)
      , dk_tc_recv :: (DLVar, [DLVar], DLVar, DLVar, DKTail)
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
  deriving (Eq, Generic)

instance Pretty DKTail where
  pretty = \case
    DK_Com m k -> prettyCom m k
    DK_Stop _ -> prettyStop
    DK_Only _ who body k -> prettyOnlyK who body k
    DK_ToConsensus {..} ->
      prettyToConsensus_ pretty pretty dk_tc_send dk_tc_recv dk_tc_mtime
    DK_If _at ca t f -> prettyIfp ca t f
    DK_Switch _at ov csm -> prettySwitch ov csm
    DK_FromConsensus _at _ret_at k ->
      prettyCommit <> hardline <> pretty k
    DK_While _at asn inv cond body k ->
      prettyWhile asn inv cond (pretty body) <> hardline <> pretty k
    DK_Continue _at asn -> prettyContinue asn

data DKBlock = DKBlock SrcLoc [SLCtxtFrame] DKTail DLArg
  deriving (Eq)

instance Pretty DKBlock where
  pretty (DKBlock _ _ k a) = prettyBlockP k a

data DKProg
  = DKProg SrcLoc DLOpts SLParts DLInit DKTail

instance Pretty DKProg where
  pretty (DKProg _at _ sps dli t) =
    "#lang dk" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> pretty t
