{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.EP where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

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
      ET_Switch _ ov csm -> pretty $ SwitchCasesUse ov csm
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

data EPart = EPart
  { ep_at :: SrcLoc
  , ep_isApi :: Bool
  , ep_interactEnv :: InteractEnv
  , ep_tail :: ETail
  }
  deriving (Eq)

instance Pretty EPart where
  pretty (EPart _ _ ep_interactEnv ep_tail) =
    pretty ep_interactEnv <> semi <> hardline <> pretty ep_tail

data EPOpts = EPOpts
  { epo_counter :: Counter
  }
  deriving (Generic, Eq)

instance HasCounter EPOpts where
  getCounter (EPOpts {..}) = epo_counter

type StateSrcMap = M.Map Int (SrcLoc, [SLCtxtFrame])

data EPProg = EPProg
  { epp_opts :: EPOpts
  , epp_init :: DLInit
  , epp_exports :: DLExports
  , epp_views :: DLViewsX
  , epp_stateSrcMap :: StateSrcMap
  , epp_apis :: DLAPIs
  , epp_events :: DLEvents
  , epp_m :: M.Map (SLPart, Maybe Int) EPart
  }
  deriving (Eq)

instance Pretty EPProg where
  pretty (EPProg {..}) =
    "EP" <+> render_obj (M.fromList $
      [ (("init"::String), pretty epp_init)
      , ("exports", pretty epp_exports)
      , ("apis", pretty epp_apis)
      , ("m", render_obj epp_m)
      ])

instance HasCounter EPProg where
  getCounter = getCounter . epp_opts
