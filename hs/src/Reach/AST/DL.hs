{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DL where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

data Purity
  = ImpureUnless (S.Set Int)
  | Impure
  | Pure
  deriving (Eq, Generic, Show)

impureUnless :: S.Set Int -> Purity
impureUnless x =
  case S.null x of
    True -> Pure
    False -> ImpureUnless x

instance Semigroup Purity where
  ImpureUnless x <> ImpureUnless y = ImpureUnless (x <> y)
  Impure <> _ = Impure
  _ <> Impure = Impure
  Pure <> y = y
  x <> Pure = x

instance Monoid Purity where
  mempty = Pure

instance Pretty Purity where
  pretty = \case
    Impure -> "impure"
    ImpureUnless rets -> "impure(" <> pretty rets <> ")"
    Pure -> "pure"

class HasPurity a where
  hasPurity :: a -> Purity

hasIsPure :: HasPurity a => a -> Bool
hasIsPure x =
  case hasPurity x of
    Pure -> True
    Impure -> False
    ImpureUnless r -> S.null r

instance HasPurity a => HasPurity (Seq.Seq a) where
  hasPurity = foldMap hasPurity

data StmtAnnot = StmtAnnot
  { sa_purity :: Purity
  , sa_local :: Bool
  }
  deriving (Eq, Generic, Show)

instance Pretty StmtAnnot where
  pretty (StmtAnnot {..}) = pretty sa_purity <> h sa_local "local"
    where
      h x s = if x then " " <> s else ""

instance Semigroup StmtAnnot where
  (StmtAnnot xp xl) <> (StmtAnnot yp yl) = (StmtAnnot (xp <> yp) (xl && yl))

instance Monoid StmtAnnot where
  mempty = StmtAnnot mempty True

instance HasPurity StmtAnnot where
  hasPurity = sa_purity

instance IsLocal StmtAnnot where
  isLocal = sa_local

mkAnnot :: HasPurity a => IsLocal a => a -> StmtAnnot
mkAnnot a = StmtAnnot {..}
  where
    sa_purity = hasPurity a
    sa_local = isLocal a

data DLSStmt
  = DLS_Let SrcLoc DLLetVar DLExpr
  | DLS_ArrayMap SrcLoc DLVar DLArg DLVar DLSBlock
  | DLS_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar DLSBlock
  | DLS_If SrcLoc DLArg StmtAnnot DLStmts DLStmts
  | DLS_Switch SrcLoc DLVar StmtAnnot (SwitchCases DLStmts)
  | DLS_Return SrcLoc Int (Either Int DLArg)
  | DLS_Prompt SrcLoc (Either Int (DLVar, M.Map Int (DLStmts, DLArg))) DLStmts
  | DLS_Stop SrcLoc
  | DLS_Unreachable SrcLoc [SLCtxtFrame] String
  | DLS_Only SrcLoc SLPart DLStmts
  | DLS_ToConsensus
      { dls_tc_at :: SrcLoc
      , dls_tc_send :: M.Map SLPart DLSend
      , dls_tc_recv :: DLRecv DLStmts
      , dls_tc_mtime :: Maybe (DLArg, DLStmts)
      }
  | DLS_FromConsensus SrcLoc DLStmts
  | DLS_While
      { dls_w_at :: SrcLoc
      , dls_w_asn :: DLAssignment
      , dls_w_inv :: DLSBlock
      , dls_w_cond :: DLSBlock
      , dls_w_body :: DLStmts
      }
  | DLS_Continue SrcLoc DLAssignment
  | DLS_FluidSet SrcLoc FluidVar DLArg
  | DLS_FluidRef SrcLoc DLVar FluidVar
  | DLS_MapReduce SrcLoc Int DLVar DLMVar DLArg DLVar DLVar DLSBlock
  | DLS_Throw SrcLoc DLArg Bool
  | DLS_Try SrcLoc DLStmts DLVar DLStmts
  | DLS_ViewIs SrcLoc SLPart SLVar (Maybe DLSExportBlock)
  deriving (Eq, Generic)

instance Pretty DLSStmt where
  pretty d =
    case d of
      DLS_Let _ DLV_Eff e -> pretty e <> semi
      DLS_Let _ (DLV_Let _ v) e ->
        "const" <+> pretty v <+> "=" <+> pretty e <> semi
      DLS_ArrayMap _ ans x a f -> prettyMap ans x a f
      DLS_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
      DLS_If _ ca sa ts fs ->
        prettyIf (pretty ca <+> braces (pretty sa)) (render_dls ts) (render_dls fs)
      DLS_Switch _ ov sa csm ->
        prettySwitch (pretty ov <+> braces (pretty sa)) csm
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
      DLS_Unreachable {} ->
        "unreachable;"
      DLS_Only _ who onlys ->
        prettyOnly who (ns onlys)
      DLS_ToConsensus {..} ->
        prettyToConsensus__ dls_tc_send dls_tc_recv dls_tc_mtime
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
      DLS_MapReduce _ _mri ans x z b a f -> prettyReduce ans x z b a f
      DLS_Throw _ dv local -> if local then "local" else "nonlocal" <+> "throw" <+> pretty dv
      DLS_Try _ e hv hs -> "try" <+> ns e <+> "catch" <+> parens (pretty hv) <+> ns hs
      DLS_ViewIs _ v k a -> prettyViewIs v k a
    where
      ns x = render_nest $ render_dls x

render_dls :: DLStmts -> Doc
render_dls ss = concatWith (surround hardline) $ fmap pretty ss

instance SrcLocOf DLSStmt where
  srclocOf = \case
    DLS_Let a _ _ -> a
    DLS_ArrayMap a _ _ _ _ -> a
    DLS_ArrayReduce a _ _ _ _ _ _ -> a
    DLS_If a _ _ _ _ -> a
    DLS_Switch a _ _ _ -> a
    DLS_Return a _ _ -> a
    DLS_Prompt a _ _ -> a
    DLS_Stop a -> a
    DLS_Unreachable a _ _ -> a
    DLS_Only a _ _ -> a
    DLS_ToConsensus {..} -> dls_tc_at
    DLS_FromConsensus a _ -> a
    DLS_While {..} -> dls_w_at
    DLS_Continue a _ -> a
    DLS_FluidSet a _ _ -> a
    DLS_FluidRef a _ _ -> a
    DLS_MapReduce a _ _ _ _ _ _ _ -> a
    DLS_Throw a _ _ -> a
    DLS_Try a _ _ _ -> a
    DLS_ViewIs a _ _ _ -> a

instance HasPurity DLSStmt where
  hasPurity = \case
    DLS_Let _ _ e -> fb $ isPure e
    DLS_ArrayMap {} -> fb $ True
    DLS_ArrayReduce {} -> fb True
    DLS_If _ _ a _ _ -> hasPurity a
    DLS_Switch _ _ a _ -> hasPurity a
    DLS_Return _ ret _ -> ImpureUnless $ S.singleton ret
    DLS_Prompt _ ret ss -> rme ret $ hasPurity ss
    DLS_Stop {} -> fb False
    DLS_Unreachable {} -> fb True
    DLS_Only _ _ ss -> hasPurity ss
    DLS_ToConsensus {} -> fb False
    DLS_FromConsensus {} -> fb False
    DLS_While {} -> fb False
    DLS_Continue {} -> fb False
    DLS_FluidSet {} -> fb False
    DLS_FluidRef {} -> fb True
    DLS_MapReduce {} -> fb True
    DLS_Throw {} -> fb False
    DLS_Try _ b _ h -> hasPurity b <> hasPurity h
    DLS_ViewIs {} -> fb False
    where
      rme = \case
        Left r -> rm r
        Right (DLVar _ _ _ r,_) -> rm r
      rm r = \case
        ImpureUnless rs -> impureUnless $ S.delete r rs
        x -> x
      fb = \case
        True -> Pure
        False -> Impure

instance IsLocal DLSStmt where
  isLocal = \case
    DLS_Let _ _ e -> isLocal e
    DLS_ArrayMap {} -> True
    DLS_ArrayReduce {} -> True
    DLS_If _ _ a _ _ -> isLocal a
    DLS_Switch _ _ a _ -> isLocal a
    DLS_Return {} -> True
    DLS_Prompt _ _ ss -> isLocal ss
    DLS_Stop {} -> False
    DLS_Unreachable {} -> True
    DLS_Only {} -> True
    DLS_ToConsensus {} -> False
    DLS_FromConsensus {} -> False
    DLS_While {} -> False
    DLS_Continue {} -> False
    DLS_FluidSet {} -> True
    DLS_FluidRef {} -> True
    DLS_MapReduce {} -> True
    DLS_Throw _ _ local -> local
    DLS_Try _ b _ h -> isLocal b && isLocal h
    DLS_ViewIs {} -> False

type DLStmts = Seq.Seq DLSStmt

instance Pretty (Seq.Seq DLSStmt) where
  pretty = render_dls

data DLSBlock
  = DLSBlock SrcLoc [SLCtxtFrame] DLStmts DLArg
  deriving (Eq, Generic)

instance Pretty DLSBlock where
  pretty (DLSBlock _at _ ss da) = prettyBlock (render_dls ss) da

data DLOpts = DLOpts
  { dlo_deployMode :: DeployMode
  , dlo_verifyArithmetic :: Bool
  , dlo_verifyPerConnector :: Bool
  , dlo_connectors :: [T.Text]
  , dlo_counter :: Counter
  , dlo_bals :: Int
  , dlo_droppedAsserts :: Int
  }
  deriving (Eq, Generic)

instance Pretty DLOpts where
  pretty = \case
    DLOpts {..} ->
      braces $
        vsep
          [ "deployMode: " <> viaShow dlo_deployMode
          , "verifyArithmetic: " <> viaShow dlo_verifyArithmetic
          , "verifyPerConnector: " <> viaShow dlo_verifyPerConnector
          , "connectors: " <> viaShow dlo_connectors
          ]

instance HasCounter DLOpts where
  getCounter (DLOpts {..}) = dlo_counter

type DLSExportBlock = DLinExportBlock DLSBlock

type DLSExports = M.Map SLVar DLSExportBlock

data DLProg
  = DLProg SrcLoc DLOpts SLParts DLInit DLSExports DLViews DLStmts
  deriving (Generic)

instance HasCounter DLProg where
  getCounter (DLProg _ dlo _ _ _ _ _) = getCounter dlo

instance Pretty DLProg where
  pretty (DLProg _at _ sps dli dex dvs ds) =
    "#lang dl" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> hardline
      <> pretty dex
      <> hardline
      <> pretty dvs
      <> hardline
      <> render_dls ds
