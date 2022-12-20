{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.CL where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty
import Reach.Util

type CLVar = B.ByteString

pclv :: CLVar -> Doc
pclv = pretty . bunpack

data CLSym = CLSym CLVar [DLType] DLType
  deriving (Ord, Eq)

instance Pretty CLSym where
  pretty (CLSym f d r) = pretty f <> parens (render_das d) <> pretty r

instance Show CLSym where
  show = show . pretty

data CLSpecial
  = CLS_TxnFrom
  | CLS_TxnTime
  | CLS_TxnSecs
  | CLS_StorageState
  deriving (Eq)

instance Pretty CLSpecial where
  pretty = \case
    CLS_TxnFrom -> "txn.from"
    CLS_TxnTime -> "txn.time"
    CLS_TxnSecs -> "txn.secs"
    CLS_StorageState -> "storage.state"

data CLStmt
  = CLDL DLStmt
  | CLBindSpecial SrcLoc DLLetVar CLSpecial
  | CLTimeCheck SrcLoc DLVar
  | CLEmitPublish SrcLoc Int [DLVar]
  | CLStateBind SrcLoc Bool [DLVarLet] Int
  | CLIntervalCheck SrcLoc DLVar DLVar (CInterval DLTimeArg)
  | CLStateSet SrcLoc Int [SvsPut]
  | CLTokenUntrack SrcLoc DLArg
  | CLMemorySet SrcLoc CLVar DLArg
  deriving (Eq)

instance Pretty CLStmt where
  pretty = \case
    CLDL s -> pretty s
    CLBindSpecial _ lv s -> pretty lv <+> ":=" <+> pretty s
    CLEmitPublish _ which vars -> "emitPublish" <> parens (render_das [pretty which, pretty vars])
    CLTimeCheck _ given -> "checkTime" <> parens (render_das [given])
    CLStateBind _ isSafe svs prev -> pretty svs <+> ":=" <+> "state" <> pretty prev <+> pretty isSafe
    CLIntervalCheck _ timev secsv int -> "checkInterval" <> parens (render_das [pretty timev, pretty secsv, pretty int])
    CLStateSet _ which svs -> "state" <> pretty which <+> "<-" <+> pretty svs
    CLTokenUntrack _ a -> "Token.untrack" <> parens (pretty a)
    CLMemorySet _ v a -> "mem" <+> pretty v <+> "<-" <+> pretty a

data HaltMode
  = HM_Pure
  | HM_Impure
  | HM_Forever
  deriving (Eq)

instance Pretty HaltMode where
  pretty = \case
    HM_Pure -> "pure"
    HM_Impure -> "impure"
    HM_Forever -> "forever"

data CLTail
  = CL_Com CLStmt CLTail
  | CL_If SrcLoc DLArg CLTail CLTail
  | CL_Switch SrcLoc DLVar (SwitchCases CLTail)
  | CL_Jump SrcLoc CLVar [DLArg] Bool (Maybe (Maybe CLVar))
  | CL_Halt SrcLoc HaltMode
  deriving (Eq)

instance Pretty CLTail where
  pretty = \case
    CL_Com e k -> pretty e <> hardline <> pretty k
    CL_If _ ca tt ft -> prettyIfp ca tt ft
    CL_Switch _ ov csm -> pretty $ SwitchCasesUse ov csm
    CL_Jump _ which args _isApi mmret -> "jump" <+> pretty which <> parens (render_das args) <+> pretty mmret
    CL_Halt _ m -> "halt" <> parens (pretty m)

data CLFun = CLFun
  { clf_at :: SrcLoc
  , clf_dom :: [DLVarLet]
  , clf_view :: Bool
  , clf_tail :: CLTail
  }
  deriving (Eq)

instance Pretty CLFun where
  pretty (CLFun {..}) =
    (if clf_view then "view" else "mut") <+> parens (render_das clf_dom) <+> "=>" <+> render_nest (pretty clf_tail)

data CLIntFun = CLIntFun
  { cif_fun :: CLFun
  , cif_mwhich :: Maybe Int
  }
  deriving (Eq)

instance Pretty CLIntFun where
  pretty (CLIntFun {..}) =
    "internal" <+> pretty cif_fun

data CLExtKind
  = CE_API String
  | CE_View String
  | CE_Publish Int
  deriving (Eq)

instance Pretty CLExtKind where
  pretty = \case
    CE_API n -> "API " <> pretty n
    CE_View n -> "View " <> pretty n
    CE_Publish w -> "Step " <> pretty w

data CLExtFun = CLExtFun
  { cef_rng :: DLType
  , cef_kind :: CLExtKind
  , cef_fun :: CLFun
  }
  deriving (Eq)

instance Pretty CLExtFun where
  pretty (CLExtFun {..}) =
    "external" <+> "returns" <+> pretty cef_rng <+> pretty cef_kind <+> pretty cef_fun

data CLDef
  = CLD_Mem DLType
  | CLD_Map
    { cldm_kt :: DLType
    , cldm_ty :: DLType
    }
  | CLD_Evt [DLType]
  deriving (Eq)

instance Pretty CLDef where
  pretty = \case
    CLD_Mem t -> "mem" <+> pretty t
    CLD_Map k v -> "map" <+> pretty k <+> pretty v
    CLD_Evt t -> "evt" <+> brackets (render_das t)

viewCLD_Mem :: CLDef -> Maybe DLType
viewCLD_Mem = \case
  CLD_Mem x -> Just x
  _ -> Nothing

type CLDefs = M.Map CLVar CLDef

type CLFuns = M.Map CLVar CLIntFun
type CLAPI = M.Map CLSym CLExtFun

data CLOpts = CLOpts
  { clo_counter :: Counter
  , clo_aem :: ALGOExitMode
  }
  deriving (Eq)

instance HasCounter CLOpts where
  getCounter (CLOpts {..}) = clo_counter

instance HasALGOExitMode CLOpts where
  getALGOExitMode = clo_aem

type CLState = M.Map Int [DLVar]

data CLProg = CLProg
  { clp_at :: SrcLoc
  , clp_opts :: CLOpts
  , clp_defs :: CLDefs
  , clp_funs :: CLFuns
  , clp_api :: CLAPI
  , clp_state :: CLState
  }
  deriving (Eq)

instance Pretty CLProg where
  pretty (CLProg {..}) = ""
    <> "// Definitions:" <> hardline
    <> render_obj clp_defs <> hardline
    <> "// Functions:" <> hardline
    <> render_obj clp_funs <> hardline
    <> "// API:" <> hardline
    <> render_obj clp_api <> hardline
    <> "// State:" <> hardline
    <> render_obj clp_state <> hardline

instance HasCounter CLProg where
  getCounter = getCounter . clp_opts

instance HasALGOExitMode CLProg where
  getALGOExitMode = getALGOExitMode . clp_opts

-- HasStateMap
class HasStateMap a where
  getStateMap :: a -> CLState

instance HasStateMap CLProg where
  getStateMap = clp_state

-- HasFunVars
type FunVars = M.Map CLVar [DLVarLet]

class HasFunVars a where
  getFunVars :: a -> FunVars

instance HasFunVars CLProg where
  getFunVars (CLProg {..}) = M.map go clp_funs
    where
      go (CLIntFun {..}) = go' cif_fun
      go' (CLFun {..}) = clf_dom

askFunVars :: (HasFunVars e, Monad m) => CLVar -> ReaderT e m [DLVarLet]
askFunVars f = do
  m <- asks getFunVars
  case M.lookup f m of
    Just x -> return x
    Nothing -> impossible $ "lookupFunVars: not in map " <> show f
