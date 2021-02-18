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

type PILVar = Maybe DLVar

type PLCommon = DLinStmt PLVar
type PLTail = DLinTail PLVar
type PLBlock = DLinBlock PLVar

type PILCommon = DLinStmt PILVar
type PILTail = DLinTail PILVar
type PILBlock = DLinBlock PILVar

-- NOTE switch to Maybe DLAssignment and make sure we have a consistent order,
-- like with M.toAscList
type FromInfo = Maybe [(DLVar, DLArg)]

data ETail_ a
  = ET_Com (DLinStmt a) (ETail_ a)
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg (ETail_ a) (ETail_ a)
  | ET_Switch SrcLoc DLVar (SwitchCases (ETail_ a))
  | ET_FromConsensus SrcLoc Int FromInfo (ETail_ a)
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
      , et_tc_from_mtime :: (Maybe ([DLArg], (ETail_ a)))
      , et_tc_cons :: (ETail_ a)
      }
  | ET_While
      { et_w_at :: SrcLoc
      , et_w_asn :: DLAssignment
      , et_w_cond :: (DLinBlock a)
      , et_w_body :: (ETail_ a)
      , et_w_k :: (ETail_ a)
      }
  | ET_Continue SrcLoc DLAssignment
  | ET_ConsensusOnly SrcLoc (DLinTail a) (ETail_ a)
  deriving (Eq)

type ETail = ETail_ PLVar
type EITail = ETail_ PILVar

instance Pretty a => Pretty (ETail_ a) where
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
        msendp <> recvp <> mtimep <> kp
        where
          recvp = "recv" <> parens (render_obj $ M.fromList $
            [ ("from"::String, pretty fs)
            , ("prev", pretty prev)
            , ("last_time", pretty last_timev)
            , ("which", pretty which)
            , ("msg", (cm $ map pretty msg))
            , ("amtv", pretty amtv)
            , ("timev", pretty timev) ]) <> hardline
          kp = ns $ pretty k
          msendp =
            case msend of
              Nothing -> mempty
              Just (as, amt, whena, saved, soloSend) ->
                "send" <> parens (render_obj $ M.fromList $
                  [ ("which"::String, pretty which)
                  , ("as", cm $ map pretty as)
                  , ("amt", pretty amt)
                  , ("when", pretty whena)
                  , ("saved", cm $ map pretty saved)
                  , ("soloSend", pretty soloSend) ]) <> hardline
          mtimep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) ->
                "timeout" <> (cm [pretty td, (render_nest $ pretty tl)]) <> hardline
      ET_While _ asn cond body k ->
        prettyWhile asn () cond (pretty body) <> hardline <> pretty k
      ET_Continue _ asn -> prettyContinue asn
      ET_ConsensusOnly _ lt k ->
        pretty lt <> hardline <> pretty k
    where
      ns = render_nest
      cm l = parens (hsep $ punctuate comma $ l)

data EPProg_ a
  = EPProg SrcLoc InteractEnv (ETail_ a)
  deriving (Eq)

type EPProg = EPProg_ PLVar

instance Pretty a => Pretty (EPProg_ a) where
  pretty (EPProg _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

data CTail_ a
  = CT_Com (DLinStmt a) (CTail_ a)
  | CT_If SrcLoc DLArg (CTail_ a) (CTail_ a)
  | CT_Switch SrcLoc DLVar (SwitchCases (CTail_ a))
  | CT_From SrcLoc FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq)

instance Pretty a => Pretty (CTail_ a) where
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

type CTail = CTail_ PLVar
type CITail = CTail_ PILVar

data CInterval a
  = CBetween [a] [a]
  deriving (Show, Eq)

instance Pretty a => Pretty (CInterval a) where
  pretty (CBetween f t) = pform "between" $ go f <+> go t
    where
      go = brackets . render_das

data CHandler_ a
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
      , ch_body :: (CTail_ a)
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVar]
      , cl_vars :: [a]
      , cl_body :: (CTail_ a)
      }
  deriving (Eq)

type CHandler = CHandler_ PLVar
type CIHandler = CHandler_ PILVar

instance Pretty a => Pretty (CHandler_ a) where
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

newtype CHandlers_ a = CHandlers (M.Map Int (CHandler_ a))
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

type CHandlers = CHandlers_ PLVar

instance Pretty a => Pretty (CHandlers_ a) where
  pretty (CHandlers m) =
    render_obj m

data CPProg a
  = CPProg SrcLoc (CHandlers_ a)
  deriving (Eq)

instance Pretty a => Pretty (CPProg a) where
  pretty (CPProg _ chs) = pretty chs

newtype EPPs a = EPPs (M.Map SLPart (EPProg_ a))
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

instance Pretty a => Pretty (EPPs a) where
  pretty (EPPs m) = render_obj m

data PLOpts = PLOpts
  { plo_deployMode :: DeployMode
  , plo_verifyOverflow :: Bool
  , plo_counter :: Counter
  }
  deriving (Generic, Eq)

data PLinProg a
  = PLProg SrcLoc PLOpts DLInit (EPPs a) (CPProg a)
  deriving (Eq)

type PLProg = PLinProg PLVar
type PIProg = PLinProg PILVar

instance Pretty a => Pretty (PLinProg a) where
  pretty (PLProg _ _ dli ps cp) =
    "#lang pl" <> hardline
      <> pretty dli
      <> hardline
      <> pretty ps
      <> hardline
      <> hardline
      <> pretty cp
