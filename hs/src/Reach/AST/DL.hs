{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DL where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

data StmtAnnot = StmtAnnot
  { sa_pure :: Bool
  , sa_local :: Bool
  }
  deriving (Eq, Generic, Show)

instance Semigroup StmtAnnot where
  (StmtAnnot xp xl) <> (StmtAnnot yp yl) = (StmtAnnot (xp && yp) (xl && yl))

instance Monoid StmtAnnot where
  mempty = StmtAnnot True True

instance IsPure StmtAnnot where
  isPure = sa_pure

instance IsLocal StmtAnnot where
  isLocal = sa_local

mkAnnot :: IsPure a => IsLocal a => a -> StmtAnnot
mkAnnot a = StmtAnnot {..}
  where
    sa_pure = isPure a
    sa_local = isLocal a

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
      , dls_tc_send :: M.Map SLPart (Bool, [DLArg], DLArg, DLArg)
      , dls_tc_recv :: (DLVar, [DLVar], DLVar, DLVar, DLStmts)
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
  deriving (Eq, Generic)

instance Pretty DLStmt where
  pretty d =
    case d of
      DLS_Let _ mv e ->
        case mv of
          Just v ->
            "const" <+> pretty v <+> "=" <+> pretty e <> semi
          Nothing ->
            pretty e <> semi
      DLS_ArrayMap _ ans x a f -> prettyMap ans x a f
      DLS_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
      DLS_If _ ca _ ts fs ->
        prettyIf ca (render_dls ts) (render_dls fs)
      DLS_Switch _ ov _ csm ->
        prettySwitch ov csm
      DLS_Return _ ret sv ->
        "throw" <> parens (pretty sv) <> ".to" <> parens (viaShow ret) <> semi
      DLS_Prompt _ ret bodys ->
        "prompt" <> parens pret <+> ns bodys <> semi
        where
          pret =
            case ret of
              Left dv -> "const" <+> pretty dv
              Right (dv, retm) ->
                "let" <+> pretty dv <+> "with" <+> render_obj retm
      DLS_Stop _ ->
        prettyStop
      DLS_Only _ who onlys ->
        prettyOnly who (ns onlys)
      DLS_ToConsensus {..} ->
        prettyToConsensus_ render_dls render_dls dls_tc_send dls_tc_recv dls_tc_mtime
      DLS_FromConsensus _ more ->
        prettyCommit <> hardline <> render_dls more
      DLS_While _ asn inv cond body ->
        prettyWhile asn inv cond $ render_dls body
      DLS_Continue _ cont_da ->
        prettyContinue cont_da
      DLS_FluidSet _ fv da ->
        "fluid" <+> pretty fv <+> ":=" <+> pretty da
      DLS_FluidRef _ dv fv ->
        pretty dv <+> "<-" <+> "fluid" <+> pretty fv
    where
      ns x = render_nest $ render_dls x

render_dls :: DLStmts -> Doc
render_dls ss = concatWith (surround hardline) $ fmap pretty ss

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
    DLS_Only {} -> False
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> isLocal ss
    DLS_While {} -> False
    DLS_Continue {} -> False
    DLS_FluidSet {} -> True
    DLS_FluidRef {} -> True

type DLStmts = Seq.Seq DLStmt

instance Pretty (Seq.Seq DLStmt) where
  pretty = render_dls

data DLBlock
  = DLBlock SrcLoc [SLCtxtFrame] DLStmts DLArg
  deriving (Eq, Generic)

instance Pretty DLBlock where
  pretty (DLBlock _at _ ss da) = prettyBlock (render_dls ss) da

data DLOpts = DLOpts
  { dlo_deployMode :: DeployMode
  , dlo_verifyOverflow :: Bool
  , dlo_verifyPerConnector :: Bool
  , dlo_connectors :: [T.Text]
  , dlo_counter :: Counter
  }
  deriving (Eq, Generic)

data DLProg
  = DLProg SrcLoc DLOpts SLParts DLInit DLStmts
  deriving (Generic)

instance Pretty DLProg where
  pretty (DLProg _at _ sps dli ds) =
    "#lang dl" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> render_dls ds
