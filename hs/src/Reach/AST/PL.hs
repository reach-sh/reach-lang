{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.PL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

data PLLetCat
  = PL_Once
  | PL_Many
  deriving (Eq, Show)

instance Semigroup PLLetCat where
  _ <> _ = PL_Many

instance Pretty PLLetCat where
  pretty PL_Many = "*"
  pretty PL_Once = "!"

data PLVar
  = PV_Eff
  | PV_Let PLLetCat DLVar
  deriving (Eq, Show)

instance Pretty PLVar where
  pretty = \case
    PV_Eff -> "eff"
    PV_Let lc x -> pretty x <> pretty lc

type PLCommon = DLinStmt PLVar

type PLTail = DLinTail PLVar

type PLBlock = DLinBlock PLVar

-- NOTE switch to Maybe DLAssignment and make sure we have a consistent order,
-- like with M.toAscList
type FromInfo = Maybe [(DLVar, DLArg)]

data ETail
  = ET_Com PLCommon ETail
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg ETail ETail
  | ET_Switch SrcLoc DLVar (SwitchCases ETail)
  | ET_FromConsensus SrcLoc Int FromInfo ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_from :: DLVar
      , et_tc_prev :: Int
      , et_tc_last_timev :: Maybe DLVar
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt   when   saved_vs just-me
             Maybe ([DLArg], DLArg, DLArg, [DLVar], Bool)
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_amtv :: DLVar
      , et_tc_from_timev :: DLVar
      , et_tc_from_mtime :: (Maybe ([DLArg], ETail))
      , et_tc_cons :: ETail
      }
  | ET_While
      { et_w_at :: SrcLoc
      , et_w_asn :: DLAssignment
      , et_w_cond :: PLBlock
      , et_w_body :: ETail
      , et_w_k :: ETail
      }
  | ET_Continue SrcLoc DLAssignment
  | ET_ConsensusOnly SrcLoc PLTail ETail
  deriving (Eq, Show)

instance Pretty ETail where
  pretty e =
    case e of
      ET_Com c k -> pretty c <> hardline <> pretty k
      ET_Stop _ -> emptyDoc
      ET_If _ ca t f -> prettyIfp ca t f
      ET_Switch _ ov csm -> prettySwitch ov csm
      ET_FromConsensus _ which msvs k ->
        "fromConsensus" <+> whichp <+> msvs' <+> semi
          <> hardline
          <> pretty k
        where
          msvs' = case msvs of
            Nothing -> emptyDoc
            Just svs -> pretty svs
          whichp = viaShow which
      ET_ToConsensus _ fs prev last_timev which msend msg amtv timev mtime k ->
        "sendrecv" <+> fsp <+> prevp <+> pretty last_timev <+> whichp <+> parens msendp <> (cm $ map pretty msg) <+> pretty amtv <+> pretty timev <> timep <> ns (pretty k)
        where
          fsp = pretty fs
          prevp = viaShow prev
          whichp = viaShow which
          msendp =
            case msend of
              Nothing -> mempty
              Just (as, amt, whena, saved, soloSend) -> ".publish" <> cm [parens (render_das as), pretty amt, pretty whena, cm (map pretty saved), pretty soloSend]
          timep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) -> nest 2 (hardline <> ".timeout" <> (cm [pretty td, (render_nest $ pretty tl)]))
      ET_While _ asn cond body k ->
        prettyWhile asn () cond (pretty body) <> hardline <> pretty k
      ET_Continue _ asn -> prettyContinue asn
      ET_ConsensusOnly _ lt k ->
        pretty lt <> hardline <> pretty k
    where
      ns = render_nest
      cm l = parens (hsep $ punctuate comma $ l)

data EPProg
  = EPProg SrcLoc InteractEnv ETail
  deriving (Eq, Show)

instance Pretty EPProg where
  pretty (EPProg _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

data CTail
  = CT_Com PLCommon CTail
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq, Show)

instance Pretty CTail where
  pretty (CT_Com e k) = pretty e <> hardline <> pretty k
  pretty (CT_If _ ca tt ft) = prettyIfp ca tt ft
  pretty (CT_Switch _ ov csm) = prettySwitch ov csm
  pretty (CT_From _ mvars) =
    case mvars of
      Nothing -> pform_ "halt!"
      Just vars -> pform "wait!" $ pretty vars
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
      , ch_amtv :: DLVar
      , ch_timev :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVar]
      , cl_vars :: [(PLLetCat, DLVar)]
      , cl_body :: CTail
      }
  deriving (Eq, Show)

instance Pretty CHandler where
  pretty (C_Handler _ int last_timev fs last_i svs msg amtv timev body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last_timev = " <> pretty last_timev
      , "last = " <> pretty last_i
      , pretty svs
      , pretty (map varType svs)
      , pretty msg
      , pretty (map varType msg)
      , pretty amtv
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
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty CHandlers where
  pretty (CHandlers m) =
    render_obj m

data CPProg
  = CPProg SrcLoc CHandlers
  deriving (Eq, Show)

instance Pretty CPProg where
  pretty (CPProg _ chs) = pretty chs

newtype EPPs = EPPs (M.Map SLPart EPProg)
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty EPPs where
  pretty (EPPs m) = render_obj m

data PLOpts = PLOpts
  { plo_deployMode :: DeployMode
  , plo_verifyOverflow :: Bool
  , plo_counter :: Counter
  }
  deriving (Generic, Eq)

data PLProg
  = PLProg SrcLoc PLOpts DLInit EPPs CPProg
  deriving (Eq)

instance Pretty PLProg where
  pretty (PLProg _ _ dli ps cp) =
    "#lang pl" <> hardline
      <> pretty dli
      <> hardline
      <> pretty ps
      <> hardline
      <> hardline
      <> pretty cp
