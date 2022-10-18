{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.CL where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

type CLVar = B.ByteString

data CLSym = CLSym CLVar [DLType] DLType
  deriving (Ord, Eq)

instance Pretty CLSym where
  pretty (CLSym f d r) = pretty f <> parens (render_das d) <> pretty r

instance Show CLSym where
  show = show . pretty

data CLStmt
  = CLDL DLStmt
  | CLTxnBind SrcLoc DLVar DLVar DLVar
  | CLTimeCheck SrcLoc DLVar DLVar
  | CLStateRead SrcLoc DLVar
  | CLStateBind SrcLoc [DLVarLet] Int
  | CLIntervalCheck SrcLoc DLVar (CInterval DLTimeArg)
  | CLStateSet SrcLoc Int [(DLVar, DLArg)]
  | CLTokenUntrack SrcLoc DLArg
  | CLStateDestroy SrcLoc
  | CLReturnSet SrcLoc DLVar DLArg
  deriving (Eq)

instance Pretty CLStmt where
  pretty = \case
    CLDL s -> pretty s
    CLTxnBind _ from timev secsv -> lhs <+> ":=" <+> "txn.metadata"
      where
        lhs = render_obj $ M.fromList
          [ ("from" :: String, from)
          , ("time", timev)
          , ("secs", secsv)
          ]
    CLTimeCheck _ actual given -> "checkTime" <> parens (render_das [actual, given])
    CLStateRead _ v -> pretty v <+> ":=" <+> "state"
    CLStateBind _ svs prev -> pretty svs <+> ":=" <+> "state" <> pretty prev
    CLIntervalCheck _ actual int -> "checkInterval" <> parens (render_das [pretty actual, pretty int])
    CLStateSet _ which svs -> "state" <> pretty which <+> "<-" <+> pretty svs
    CLTokenUntrack _ a -> "Token.untrack" <> parens (pretty a)
    CLReturnSet _ v a -> "return" <+> pretty a <+> "into" <+> pretty v
    CLStateDestroy _ -> "stateDestroy"

data CLTail
  = CL_Com CLStmt CLTail
  | CL_If SrcLoc DLArg CLTail CLTail
  | CL_Switch SrcLoc DLVar (SwitchCases CLTail)
  | CL_Jump SrcLoc CLVar [DLVar] DLVar
  | CL_Halt SrcLoc
  deriving (Eq)

instance Pretty CLTail where
  pretty = \case
    CL_Com e k -> pretty e <> hardline <> pretty k
    CL_If _ ca tt ft -> prettyIfp ca tt ft
    CL_Switch _ ov csm -> prettySwitch ov csm
    CL_Jump _ which args ret -> pretty ret <+> "<-" <+> pretty which <> parens (render_das args)
    CL_Halt _ -> "exit()"

data CLFunMode
  = CLFM_View
  | CLFM_Internal
  | CLFM_External
  deriving (Eq)

instance Pretty CLFunMode where
  pretty = \case
    CLFM_View -> "view"
    CLFM_Internal -> "internal"
    CLFM_External -> "external"

data CLFun = CLFun
  { clf_dom :: [DLVarLet]
  , clf_rng :: DLVar
  , clf_mode :: CLFunMode
  , clf_tail :: CLTail
  }
  deriving (Eq)

instance Pretty CLFun where
  pretty (CLFun {..}) =
    pretty clf_mode <+> parens (render_das clf_dom) <+> "rets" <+> pretty clf_rng <+> "=>" <+> render_nest (pretty clf_tail)

data CLDef
  = CLD_Sto DLType
  | CLD_Map
    { cldm_kt :: DLType
    , cldm_ty :: DLType
    }
  | CLD_Evt [DLType]
  deriving (Eq)

instance Pretty CLDef where
  pretty = \case
    CLD_Sto t -> "sto" <+> pretty t
    CLD_Map k v -> "map" <+> pretty k <+> pretty v
    CLD_Evt t -> "evt" <+> brackets (render_das t)

type CLDefs = M.Map CLVar CLDef

type CLFuns = M.Map CLSym CLFun

data CLOpts = CLOpts
  { clo_untrustworthyMaps :: Bool
  , clo_counter :: Counter
  }
  deriving (Eq)

instance HasCounter CLOpts where
  getCounter (CLOpts {..}) = clo_counter

instance HasUntrustworthyMaps CLOpts where
  getUntrustworthyMaps (CLOpts {..}) = clo_untrustworthyMaps

data CLProg = CLProg
  { clp_at :: SrcLoc
  , clp_opts :: CLOpts
  , clp_defs :: CLDefs
  , clp_funs :: CLFuns
  }
  deriving (Eq)

instance Pretty CLProg where
  pretty (CLProg {..}) = "CL" <> hardline
    <> "// Definitions:" <> hardline
    <> render_obj clp_defs <> hardline
    <> "// Functions:" <> hardline
    <> render_obj clp_funs
