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
  = DKC_Let SrcLoc DLLetVar DLExpr
  | DKC_ArrayMap SrcLoc DLVar [DLArg] [DLVar] DLVar DKBlock
  | DKC_ArrayReduce SrcLoc DLVar [DLArg] DLArg DLVar [DLVar] DLVar DKBlock
  | DKC_Var SrcLoc DLVar
  | DKC_Set SrcLoc DLVar DLArg
  | DKC_LocalDo SrcLoc DKTail
  | DKC_LocalIf SrcLoc DLArg DKTail DKTail
  | DKC_LocalSwitch SrcLoc DLVar (SwitchCases DKTail)
  | DKC_Only SrcLoc SLPart DKTail
  | DKC_MapReduce SrcLoc Int DLVar DLMVar DLArg DLVar DLVar DKBlock
  | DKC_FluidSet SrcLoc FluidVar DLArg
  | DKC_FluidRef SrcLoc DLVar FluidVar
  | DKC_setApiDetails SrcLoc SLPart [DLType] (Maybe String)
  | DKC_TokenMetaGet TokenMeta SrcLoc DLVar DLArg (Maybe Int)
  | DKC_TokenMetaSet TokenMeta SrcLoc DLArg DLArg (Maybe Int) Bool
  deriving (Eq, Generic)

instance Pretty DKCommon where
  pretty = \case
    DKC_Let _at x de -> "const" <+> pretty x <+> "=" <+> pretty de <> semi
    DKC_ArrayMap _ ans x a i f -> prettyMap ans x a i f
    DKC_ArrayReduce _ ans x z b a i f -> prettyReduce ans x z b a i f
    DKC_Var _at dv -> "let" <+> pretty dv <> semi
    DKC_Set _at dv da -> pretty dv <+> "=" <+> pretty da <> semi
    DKC_LocalDo _at k -> "do" <+> render_nest (pretty k) <> semi
    DKC_LocalIf _at ca t f -> "local" <+> prettyIfp ca t f
    DKC_LocalSwitch _at ov csm -> prettySwitch (pretty ov <+> "{ local}") csm
    DKC_MapReduce _ _mri ans x z b a f -> prettyReduce ans x z b a () f
    DKC_FluidSet at fv a -> pretty (DLS_FluidSet at fv a)
    DKC_FluidRef at dv fv -> pretty (DLS_FluidRef at dv fv)
    DKC_Only _at who t -> prettyOnly who t
    DKC_setApiDetails _ p tys mc -> "setApiDetails" <> parens (render_das [pretty p, pretty tys, pretty mc])
    DKC_TokenMetaGet ty _ dv tok mp -> "tokenMetaGet" <> parens (comma_sep [viaShow ty, pretty dv, pretty tok, pretty mp])
    DKC_TokenMetaSet ty _ tok val mp i -> "tokenMetaSet" <> parens (comma_sep [viaShow ty, pretty tok, pretty val, pretty mp, pretty i])

data DKTail
  = DK_Com DKCommon DKTail
  | DK_Stop SrcLoc
  | DK_ToConsensus
      { dk_tc_at :: SrcLoc
      , dk_tc_send :: M.Map SLPart DLSend
      , dk_tc_recv :: DLRecv DKTail
      , dk_tc_mtime :: Maybe (DLTimeArg, DKTail)
      }
  | DK_If SrcLoc DLArg DKTail DKTail
  | DK_Switch SrcLoc DLVar (SwitchCases DKTail)
  | DK_FromConsensus SrcLoc SrcLoc [SLCtxtFrame] DKTail
  | DK_While
      { dk_w_at :: SrcLoc
      , dk_w_asn :: DLAssignment
      , dk_w_inv :: DKBlock
      , dk_w_cond :: DKBlock
      , dk_w_body :: DKTail
      , dk_w_k :: DKTail
      }
  | DK_Continue SrcLoc DLAssignment
  | DK_ViewIs SrcLoc (Maybe SLPart) SLVar (Maybe DKExportBlock) DKTail
  | DK_Unreachable SrcLoc [SLCtxtFrame] String
  | DK_LiftBoundary SrcLoc DKTail
  deriving (Eq, Generic)

instance Pretty DKTail where
  pretty = \case
    DK_Com m k -> prettyCom m k
    DK_Stop _ -> prettyStop
    DK_ToConsensus {..} ->
      prettyToConsensus__ ("?" :: String) dk_tc_send dk_tc_recv dk_tc_mtime
    DK_If _at ca t f -> prettyIfp ca t f
    DK_Switch _at ov csm -> prettySwitch ov csm
    DK_FromConsensus _at _ret_at _fs k ->
      prettyCommit <> hardline <> pretty k
    DK_While _at asn inv cond body k ->
      prettyWhile asn inv cond (pretty body) <> hardline <> pretty k
    DK_Continue _at asn -> prettyContinue asn
    DK_ViewIs _ vn vk a k ->
      prettyViewIs vn vk a <> hardline <> pretty k
    DK_Unreachable {} -> "unreachable;"
    DK_LiftBoundary _ k ->
      "liftBoundary();" <> hardline <> pretty k

data DKBlock = DKBlock SrcLoc [SLCtxtFrame] DKTail DLArg
  deriving (Eq)

instance Pretty DKBlock where
  pretty (DKBlock _ _ k a) = prettyBlockP k a

type DKExportBlock = DLinExportBlock DKBlock

type DKExports = M.Map SLVar DKExportBlock

type DKViews = DLViews

data DKProg
  = DKProg SrcLoc DLOpts SLParts DLInit DKExports DKViews DLAPIs DLEvents DKTail

instance Pretty DKProg where
  pretty (DKProg _at _ sps dli dex dvs das devt t) =
    "#lang dk" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> hardline
      <> pretty dex
      <> hardline
      <> pretty dvs
      <> hardline
      <> pretty das
      <> hardline
      <> pretty devt
      <> hardline
      <> pretty t
