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
  | ET_FromConsensus SrcLoc Int FromInfo ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_from :: DLVar
      , et_tc_prev :: Int
      , et_tc_lct :: Maybe DLArg
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt   when   saved_vs just-me
             Maybe ([DLArg], DLPayAmt, DLArg, [DLVar], Bool)
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_out :: [DLVar]
      , et_tc_from_timev :: DLVar
      , et_tc_from_secsv :: DLVar
      , et_tc_from_didSendv :: DLVar
      , et_tc_from_mtime :: (Maybe (Maybe DLTimeArg, ETail))
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

instance SrcLocOf ETail where
  srclocOf = \case
    ET_Com c _ -> srclocOf c
    ET_Stop at -> at
    ET_If at _ _ _ -> at
    ET_Switch at _ _ -> at
    ET_FromConsensus at _ _ _ -> at
    ET_ToConsensus {..} -> et_tc_at
    ET_While {..} -> et_w_at
    ET_Continue at _ -> at

instance Pretty ETail where
  pretty e =
    case e of
      ET_Com c k -> pretty c <> hardline <> pretty k
      ET_Stop _ -> emptyDoc
      ET_If _ ca t f -> prettyIfp ca t f
      ET_Switch _ ov csm -> prettySwitch ov csm
      ET_FromConsensus _ which msvs k ->
        "fromConsensus" <+> whichp <+> pretty msvs <+> semi
          <> hardline
          <> pretty k
        where
          whichp = viaShow which
      ET_ToConsensus _ fs prev lct which msend msg out timev secsv didSendv mtime k ->
        msendp <> recvp <> mtimep <> kp
        where
          recvp =
            "recv"
              <> parens
                (render_obj $
                   M.fromList $
                     [ ("from" :: String, pretty fs)
                     , ("prev", pretty prev)
                     , ("lct", pretty lct)
                     , ("which", pretty which)
                     , ("msg", (cm $ map pretty msg))
                     , ("out", (cm $ map pretty out))
                     , ("timev", pretty timev)
                     , ("secsv", pretty secsv)
                     , ("didSendv", pretty didSendv)
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
  = EPProg SrcLoc Bool InteractEnv ETail
  deriving (Eq)

instance Pretty EPProg where
  pretty (EPProg _ _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

data CTail
  = CT_Com DLStmt CTail
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc Int FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq)

instance SrcLocOf CTail where
  srclocOf = \case
    CT_Com s _ -> srclocOf s
    CT_If a _ _ _ -> a
    CT_Switch a _ _ -> a
    CT_From a _ _ -> a
    CT_Jump a _ _ _ -> a

instance Pretty CTail where
  pretty = \case
    CT_Com e k -> pretty e <> hardline <> pretty k
    CT_If _ ca tt ft -> prettyIfp ca tt ft
    CT_Switch _ ov csm -> prettySwitch ov csm
    CT_From _ which fi -> pform "from" $ pretty which <> "," <+> pretty fi
    CT_Jump _ which vars assignment -> pform "jump!" args
      where
        args = pretty which <+> pretty vars <+> pretty assignment

data CInterval a
  = CBetween (Maybe a) (Maybe a)
  deriving (Show, Eq)

instance Pretty a => Pretty (CInterval a) where
  pretty (CBetween f t) = pform "between" $ go f <+> go t
    where
      go = brackets . pretty

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval DLTimeArg
      , ch_from :: DLVar
      , ch_last :: Int
      , ch_svs :: [DLVarLet]
      , ch_msg :: [DLVarLet]
      , ch_timev :: DLVar
      , ch_secsv :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVarLet]
      , cl_vars :: [DLVarLet]
      , cl_body :: CTail
      }
  deriving (Eq)

instance Pretty CHandler where
  pretty (C_Handler _ int fs last_i svs msg timev secsv body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last = " <> pretty last_i
      , pretty svs
      , pretty (map varLetType svs)
      , pretty msg
      , pretty (map varLetType msg)
      , "timev = " <> pretty timev
      , "secsv = " <> pretty secsv
      , render_nest $ pretty body
      ]
  pretty (C_Loop _ svs vars body) =
    pbrackets
      [ "loop!"
      , pretty svs
      , pretty vars
      , render_nest $ pretty body
      ]

instance SrcLocOf CHandler where
  srclocOf = \case
    C_Handler {..} -> ch_at
    C_Loop {..} -> cl_at

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

type ViewsInfo = InterfaceLikeMap DLExportBlock

data ViewInfo = ViewInfo [DLVar] ViewsInfo
  deriving (Eq)

instance Pretty ViewInfo where
  pretty (ViewInfo vs vi) =
    pform "view" (pretty vs <+> pretty vi)

type ViewInfos = M.Map Int ViewInfo

type CPViews = DLViews

type ApiInfos = M.Map SLPart (M.Map Int ApiInfo)

data CPProg
  = CPProg SrcLoc (CPViews, ViewInfos) ApiInfos DLEvents CHandlers
  deriving (Eq)

instance Pretty CPProg where
  pretty (CPProg _ vis ai devts chs) =
    "views:" <+> pretty vis <> hardline
      <> "apiInfo:" <+> pretty ai
      <> hardline
      <> "events:" <+> pretty devts
      <> hardline
      <> pretty chs

data EPPs = EPPs
  { epps_apis :: DLAPIs
  , epps_m :: M.Map (SLPart, Maybe Int) EPProg
  }
  deriving (Eq)

instance Monoid EPPs where
  mempty = EPPs mempty mempty

instance Semigroup EPPs where
  (EPPs x0 x1) <> (EPPs y0 y1) = EPPs (x0 <> y0) (x1 <> y1)

instance Pretty EPPs where
  pretty (EPPs {..}) =
    "APIs:"
      <> pretty epps_apis
      <> hardline
      <> render_obj epps_m
      <> hardline

data PLOpts = PLOpts
  { plo_verifyArithmetic :: Bool
  , plo_untrustworthyMaps :: Bool
  , plo_counter :: Counter
  }
  deriving (Generic, Eq)

instance HasCounter PLOpts where
  getCounter (PLOpts {..}) = plo_counter

type StateSrcMap = M.Map Int (SrcLoc, [SLCtxtFrame])

data PLProg = PLProg
  { plp_at :: SrcLoc
  , plp_opts :: PLOpts
  , plp_init :: DLInit
  , plp_exports :: DLExports
  , plp_stateSrcMap :: StateSrcMap
  , plp_epps :: EPPs
  , plp_cpprog :: CPProg
  }
  deriving (Eq)

instance HasCounter PLProg where
  getCounter (PLProg _ plo _ _ _ _ _) = getCounter plo

instance Pretty PLProg where
  pretty (PLProg _ _ dli dex _ ps cp) =
    "#lang pl" <> hardline
      <> pretty dex
      <> hardline
      <> pretty dli
      <> hardline
      <> pretty ps
      <> hardline
      <> hardline
      <> pretty cp
