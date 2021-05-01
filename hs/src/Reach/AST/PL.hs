{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.PL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

-- NOTE switch to Maybe DLAssignment and make sure we have a consistent order,
-- like with M.toAscList
data FromInfo
  = FI_Continue [(DLVar, DLArg)]
  | FI_Halt [DLArg]
  deriving (Eq)

instance Pretty FromInfo where
  pretty = \case
    FI_Continue svs -> pform "continue" (pretty svs)
    FI_Halt toks -> pform "halt" (pretty toks)

data ETail
  = ET_Com DLStmt ETail
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg ETail ETail
  | ET_Switch SrcLoc DLVar (SwitchCases ETail)
  | ET_FromConsensus SrcLoc Int ViewSave FromInfo ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_from :: DLVar
      , et_tc_prev :: Int
      , et_tc_last_timev :: Maybe DLVar
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt   when   saved_vs just-me
             Maybe ([DLArg], DLPayAmt, DLArg, [DLVar], Bool)
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_out :: [DLVar]
      , et_tc_from_timev :: DLVar
      , et_tc_from_mtime :: (Maybe ([DLArg], ETail))
      , et_tc_cons :: ETail
      }
  | ET_While
      { et_w_at :: SrcLoc
      , et_w_asn :: DLAssignment
      , et_w_cond :: DLBlock
      , et_w_body :: ETail
      , et_w_k :: ETail
      }
  | ET_Continue SrcLoc DLAssignment
  deriving (Eq)

instance Pretty ETail where
  pretty e =
    case e of
      ET_Com c k -> pretty c <> hardline <> pretty k
      ET_Stop _ -> emptyDoc
      ET_If _ ca t f -> prettyIfp ca t f
      ET_Switch _ ov csm -> prettySwitch ov csm
      ET_FromConsensus _ which vis msvs k ->
        "fromConsensus" <+> whichp <+> pretty vis <+> pretty msvs <+> semi
          <> hardline
          <> pretty k
        where
          whichp = viaShow which
      ET_ToConsensus _ fs prev last_timev which msend msg out timev mtime k ->
        msendp <> recvp <> mtimep <> kp
        where
          recvp =
            "recv"
              <> parens
                (render_obj $
                   M.fromList $
                     [ ("from" :: String, pretty fs)
                     , ("prev", pretty prev)
                     , ("last_time", pretty last_timev)
                     , ("which", pretty which)
                     , ("msg", (cm $ map pretty msg))
                     , ("out", (cm $ map pretty out))
                     , ("timev", pretty timev)
                     ])
              <> hardline
          kp = ns $ pretty k
          msendp =
            case msend of
              Nothing -> mempty
              Just (as, amt, whena, saved, soloSend) ->
                "send"
                  <> parens
                    (render_obj $
                       M.fromList $
                         [ ("which" :: String, pretty which)
                         , ("as", cm $ map pretty as)
                         , ("amt", pretty amt)
                         , ("when", pretty whena)
                         , ("saved", cm $ map pretty saved)
                         , ("soloSend", pretty soloSend)
                         ])
                  <> hardline
          mtimep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) ->
                "timeout" <> (cm [pretty td, (render_nest $ pretty tl)]) <> hardline
      ET_While _ asn cond body k ->
        prettyWhile asn () cond (pretty body) <> hardline <> pretty k
      ET_Continue _ asn -> prettyContinue asn
    where
      ns = render_nest
      cm l = parens (hsep $ punctuate comma $ l)

data EPProg
  = EPProg SrcLoc InteractEnv ETail
  deriving (Eq)

instance Pretty EPProg where
  pretty (EPProg _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

data ViewSave
  = ViewSave Int [(DLVar, DLArg)]
  deriving (Eq)

instance Pretty ViewSave where
  pretty (ViewSave vi svs) =
    pform "viewsave" (pretty vi <> "," <+> pretty svs)

data CTail
  = CT_Com DLStmt CTail
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc Int ViewSave FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq)

instance Pretty CTail where
  pretty (CT_Com e k) = pretty e <> hardline <> pretty k
  pretty (CT_If _ ca tt ft) = prettyIfp ca tt ft
  pretty (CT_Switch _ ov csm) = prettySwitch ov csm
  pretty (CT_From _ which vi fi) = pform "from" $ pretty which <> "," <+> pretty vi <> "," <+> pretty fi
  pretty (CT_Jump _ which vars assignment) = pform "jump!" args
    where
      args = pretty which <+> pretty vars <+> pretty assignment

data CInterval a
  = CBetween [a] [a]
  deriving (Show, Eq)

instance Pretty a => Pretty (CInterval a) where
  pretty (CBetween f t) = pform "between" $ go f <+> go t
    where
      go = brackets . render_das

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval DLArg
      , ch_last_timev :: Maybe DLVar
      , ch_from :: DLVar
      , ch_last :: Int
      , ch_svs :: [DLVar]
      , ch_msg :: [DLVar]
      , ch_timev :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVar]
      , cl_vars :: [DLVar]
      , cl_body :: CTail
      }
  deriving (Eq)

instance Pretty CHandler where
  pretty (C_Handler _ int last_timev fs last_i svs msg timev body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last_timev = " <> pretty last_timev
      , "last = " <> pretty last_i
      , pretty svs
      , pretty (map varType svs)
      , pretty msg
      , pretty (map varType msg)
      , "timev = " <> pretty timev
      , render_nest $ pretty body
      ]
  pretty (C_Loop _ svs vars body) =
    pbrackets
      [ "loop!"
      , pretty svs
      , pretty vars
      , render_nest $ pretty body
      ]

newtype CHandlers = CHandlers (M.Map Int CHandler)
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

instance Pretty CHandlers where
  pretty (CHandlers m) =
    render_obj m

data ColorGraphs = ColorGraph
  { typeGraph :: M.Map DLType Int
  , varGraph :: M.Map DLVar Int
  }
  deriving (Eq)

instance Pretty ColorGraphs where
  pretty = \case
    ColorGraph tg vg ->
      "type graph:" <+> pretty tg <> hardline
        <> "var graph:" <+> prettyVarGraph vg
    where
      prettyVarGraph g =
        let rows =
              map
                (\(v, i) ->
                   "  " <> viaShow v <> ": " <> viaShow (varType v) <> " - " <> viaShow i)
                $ M.toList g
         in braces $ hardline <> vsep rows

type ViewsInfo = M.Map SLPart (M.Map SLVar DLArg)

data ViewInfo = ViewInfo [DLVar] ViewsInfo
  deriving (Eq)

instance Pretty ViewInfo where
  pretty (ViewInfo vs vi) =
    pform "view" (pretty vs <+> pretty vi)

type ViewInfos = M.Map Int ViewInfo

type CPViews = DLViews

data CPProg
  = CPProg SrcLoc (Maybe (CPViews, ViewInfos)) CHandlers
  deriving (Eq)

instance Pretty CPProg where
  pretty (CPProg _ vis chs) =
    "views:" <+> pretty vis <> hardline <> pretty chs

newtype EPPs = EPPs (M.Map SLPart EPProg)
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

instance Pretty EPPs where
  pretty (EPPs m) = render_obj m

data PLOpts = PLOpts
  { plo_deployMode :: DeployMode
  , plo_verifyArithmetic :: Bool
  , plo_counter :: Counter
  }
  deriving (Generic, Eq)

instance HasCounter PLOpts where
  getCounter (PLOpts {..}) = plo_counter

data PLProg
  = PLProg SrcLoc PLOpts DLInit DLExports EPPs CPProg
  deriving (Eq)

instance HasCounter PLProg where
  getCounter (PLProg _ plo _ _ _ _) = getCounter plo

instance Pretty PLProg where
  pretty (PLProg _ _ dli dex ps cp) =
    "#lang pl" <> hardline
      <> pretty dex
      <> hardline
      <> pretty dli
      <> hardline
      <> pretty ps
      <> hardline
      <> hardline
      <> pretty cp
