{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Generics
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Reach.JSOrphans ()
import Reach.UnsafeUtil
import Reach.Util
import Generics.Deriving (conNameOf)

--- Source Information
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq, Generic, NFData, Ord)

instance Show ReachSource where
  show ReachStdLib = "reach standard library"
  show (ReachSourceFile fp) = fp

data SrcLoc = SrcLoc (Maybe String) (Maybe TokenPosn) (Maybe ReachSource)
  deriving (Eq, Generic, NFData, Ord)

-- This is a "defaulting" instance where the left info is preferred,
-- but can fall back on the right if info is absent from the left.
instance Semigroup SrcLoc where
  SrcLoc mlab mpos msrc <> SrcLoc mlab' mpos' msrc' =
    SrcLoc (firstJust mlab mlab') (firstJust mpos mpos') (firstJust msrc msrc')
    where
      firstJust (Just x) _ = Just x
      firstJust _ (Just y) = Just y
      firstJust Nothing Nothing = Nothing

instance Monoid SrcLoc where
  mempty = SrcLoc Nothing Nothing Nothing

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ intersperse ":" $ concat [sr, loc, lab]
    where
      lab = case mlab of
        Nothing -> []
        Just s -> [s]
      sr = case mrs of
        Nothing -> []
        Just s -> [show s]
      loc = case mtp of
        Nothing -> []
        Just (TokenPn _ l c) -> [show l, show c]

expect_throw :: Show a => HasCallStack => Maybe ([SLCtxtFrame]) -> SrcLoc -> a -> b
expect_throw mCtx src ce =
  error . T.unpack . unsafeRedactAbs . T.pack $
    "error: " ++ (show src) ++ ": " ++ (take 512 $ show ce) <>
    case concat mCtx of
      [] -> ""
      ctx -> "\nTrace:\n" <> intercalate "\n" (topOfStackTrace ctx)

expect_thrown :: Show a => HasCallStack => SrcLoc -> a -> b
expect_thrown = expect_throw Nothing

topOfStackTrace :: [SLCtxtFrame] -> [String]
topOfStackTrace stack
  | length stackMsgs > 10 = take 10 stackMsgs <> ["  ..."]
  | otherwise = stackMsgs
  where
    stackMsgs = map getStackTraceMessage stack

-- Mimic Node's stack trace message
getStackTraceMessage :: SLCtxtFrame -> String
getStackTraceMessage (SLC_CloApp call_at _ name) =
  "  at " <> maybe "[unknown function]" show name <> " (" <> show call_at <> ")"

srcloc_builtin :: SrcLoc
srcloc_builtin = SrcLoc (Just "<builtin>") Nothing Nothing

srcloc_top :: SrcLoc
srcloc_top = SrcLoc (Just "<top level>") Nothing Nothing

srcloc_src :: ReachSource -> SrcLoc
srcloc_src rs = SrcLoc Nothing Nothing (Just rs)

get_srcloc_src :: SrcLoc -> ReachSource
get_srcloc_src (SrcLoc _ _ (Just rs)) = rs
get_srcloc_src (SrcLoc _ _ Nothing) = ReachSourceFile "src" -- FIXME

srcloc_at :: String -> (Maybe TokenPosn) -> SrcLoc -> SrcLoc
srcloc_at lab mp (SrcLoc _ _ rs) = SrcLoc (Just lab) mp rs

class SrcLocOf a where
  srclocOf :: a -> SrcLoc

--- Security Levels
data SecurityLevel
  = Secret
  | Public
  deriving (Eq, Generic, NFData, Show)

public :: a -> (SecurityLevel, a)
public x = (Public, x)

secret :: a -> (SecurityLevel, a)
secret x = (Secret, x)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

lvlMeet :: SecurityLevel -> (SecurityLevel, a) -> (SecurityLevel, a)
lvlMeet lvl (lvl', x) = (lvl <> lvl', x)

instance Monoid SecurityLevel where
  mempty = Public

--- Static Language
type SLVar = String

data SLType
  = T_Null
  | T_Bool
  | T_UInt
  | T_Bytes Integer
  | T_Digest
  | T_Address
  | T_Fun [SLType] SLType
  | T_Array SLType Integer
  | T_Tuple [SLType]
  | T_Object (M.Map SLVar SLType)
  | T_Data (M.Map SLVar SLType)
  | T_Forall SLVar SLType
  | T_Var SLVar
  | T_Type SLType
  deriving (Eq, Generic, NFData, Ord)

-- | Fold over SLType, doing something special on Fun
funFold
  :: a -- ^ On no SLType inside
  -> ([SLType] -> a) -- ^ On many SLType inside
  -> ([SLType] -> SLType -> a) -- ^ On Fun
  -> SLType -- ^ The type to fold over
  -> a
funFold z k fun = go
  where
    go = \case
      T_Null -> z
      T_Bool -> z
      T_UInt -> z
      T_Bytes _ -> z
      T_Digest -> z
      T_Address -> z
      T_Fun inTys outTy -> fun inTys outTy
      T_Array ty _ -> go ty
      T_Tuple tys -> k tys
      T_Object m -> k $ M.elems m
      T_Data m -> k $ M.elems m
      T_Forall _ ty -> go ty
      T_Var _ -> z
      T_Type _ -> z

-- | True if the type is a Fun, or
-- is a container/forall type with Fun somewhere inside
hasFun :: SLType -> Bool
hasFun = funFold z k fun
  where
    z = False
    k = any hasFun
    fun _ _ = True

-- | True if all Function types within this type
-- do not accept or return functions.
isFirstOrder :: SLType -> Bool
isFirstOrder = funFold z k fun
  where
    z = True
    k = all isFirstOrder
    fun inTys outTy = not $ any hasFun $ outTy : inTys

showTys :: [SLType] -> String
showTys = intercalate ", " . map show

showTyMap :: M.Map SLVar SLType -> String
showTyMap = intercalate ", " . map showPair . M.toList
  where
    showPair (name, ty) = show name <> ": " <> show ty

instance Show SLType where
  show T_Null = "Null"
  show T_Bool = "Bool"
  show T_UInt = "UInt"
  show (T_Bytes sz) = "Bytes(" <> show sz <> ")"
  show T_Digest = "Digest"
  show T_Address = "Address"
  show (T_Fun tys ty) = "Fun([" <> showTys tys <> "], " <> show ty <> ")"
  show (T_Array ty i) = "Array(" <> show ty <> ", " <> show i <> ")"
  show (T_Tuple tys) = "Tuple(" <> showTys tys <> ")"
  show (T_Object tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (T_Data tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (T_Forall x t) = "Forall(" <> show x <> ", " <> show t <> ")"
  show (T_Var x) = show x
  show (T_Type ty) = "Type(" <> show ty <> ")"

infixr 9 -->

(-->) :: [SLType] -> SLType -> SLType
dom --> rng = T_Fun dom rng

type SLPart = B.ByteString

type SLPartEnvs = M.Map SLPart SLEnv

data SLCloEnv
  = SLCloEnv SLEnv SLPartEnvs SLEnv
  deriving (Eq, Generic, NFData, Show)

data SLVal
  = SLV_Null SrcLoc String
  | SLV_Bool SrcLoc Bool
  | SLV_Int SrcLoc Integer
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Array SrcLoc SLType [SLVal]
  | SLV_Tuple SrcLoc [SLVal]
  | SLV_Object SrcLoc (Maybe String) SLEnv
  | SLV_Clo SrcLoc (Maybe SLVar) [JSExpression] JSBlock SLCloEnv
  | SLV_Data SrcLoc (M.Map SLVar SLType) SLVar SLVal
  | SLV_DLC DLConstant
  | SLV_DLVar DLVar
  | SLV_Type SLType
  | SLV_Connector T.Text
  | SLV_Participant SrcLoc SLPart (Maybe SLVar) (Maybe DLVar)
  | SLV_Prim SLPrimitive
  | SLV_Form SLForm
  | SLV_Kwd SLKwd
  deriving (Eq, Generic, NFData, Show)

instance SrcLocOf SLVal where
  srclocOf = \case
    SLV_Null a _ -> a
    SLV_Bool a _ -> a
    SLV_Int a _ -> a
    SLV_Bytes a _ -> a
    SLV_Array a _ _ -> a
    SLV_Tuple a _ -> a
    SLV_Object a _ _ -> a
    SLV_Clo a _ _ _ _ -> a
    SLV_Data a _ _ _ -> a
    SLV_DLVar (DLVar a _ _ _) -> a
    SLV_Participant a _ _ _ -> a
    _ -> srcloc_builtin

isLiteralArray :: SLVal -> Bool
isLiteralArray (SLV_Array {}) = True
isLiteralArray _ = False

data ToConsensusMode
  = TCM_Publish
  | TCM_Pay
  | TCM_Timeout
  deriving (Eq, Generic, NFData, Show)

data ParallelReduceMode
  = PRM_Invariant
  | PRM_Timeout
  | PRM_Until
  | PRM_Case
  deriving (Eq, Generic, NFData, Show)

data SLForm
  = SLForm_App
  | SLForm_each
  | SLForm_EachAns [(SLPart, Maybe SLVar)] SrcLoc SLCloEnv JSExpression
  | SLForm_Part_Only SLPart (Maybe SLVar)
  | SLForm_Part_ToConsensus SrcLoc SLPart (Maybe SLVar) (Maybe ToConsensusMode) (Maybe [SLVar]) (Maybe JSExpression) (Maybe (SrcLoc, JSExpression, JSBlock))
  | SLForm_unknowable
  | SLForm_parallel_reduce
  | SLForm_parallel_reduce_partial
      { slfpr_init :: JSExpression
      , slfpr_mode :: Maybe ParallelReduceMode
      , slfpr_minv :: Maybe JSExpression
      , slfpr_mtimeout :: Maybe JSExpression
      , slfpr_muntil :: Maybe JSExpression
      , slfpr_cases :: [ (SrcLoc, (JSExpression, JSExpression)) ] }
  deriving (Eq, Generic, NFData, Show)

data SLKwd
  = SLK_async
  | SLK_await
  | SLK_break
  | SLK_case
  | SLK_catch
  | SLK_class
  | SLK_const
  | SLK_continue
  | SLK_debugger
  | SLK_default
  | SLK_delete
  | SLK_do
  | SLK_else
  | SLK_enum
  | SLK_export
  | SLK_extends
  | SLK_for
  | SLK_from
  | SLK_function
  | SLK_if
  | SLK_in
  | SLK_import
  | SLK_instanceOf
  | SLK_let
  | SLK_new
  | SLK_of
  | SLK_return
  | SLK_static
  | SLK_switch
  | SLK_this
  | SLK_throw
  | SLK_try
  | SLK_typeof
  | SLK_var
  | SLK_while
  | SLK_with
  | SLK_yield
  deriving (Bounded, Enum, Eq, Generic, NFData)

instance Show SLKwd where
  show k = drop 4 $ conNameOf k

allKeywords :: [SLKwd]
allKeywords = enumFrom minBound

data PrimOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PLT
  | PLE
  | PEQ
  | PGE
  | PGT
  | IF_THEN_ELSE
  | DIGEST_EQ
  | ADDRESS_EQ
  | SELF_ADDRESS
  | LSH
  | RSH
  | BAND
  | BIOR
  | BXOR
  deriving (Eq, Generic, NFData, Ord, Show)

primOpType :: PrimOp -> SLType
primOpType SELF_ADDRESS = impossible "self address"
primOpType ADD = [T_UInt, T_UInt] --> T_UInt
primOpType SUB = [T_UInt, T_UInt] --> T_UInt
primOpType MUL = [T_UInt, T_UInt] --> T_UInt
primOpType DIV = [T_UInt, T_UInt] --> T_UInt
primOpType MOD = [T_UInt, T_UInt] --> T_UInt
primOpType PLT = [T_UInt, T_UInt] --> T_Bool
primOpType PLE = [T_UInt, T_UInt] --> T_Bool
primOpType PEQ = [T_UInt, T_UInt] --> T_Bool
primOpType PGE = [T_UInt, T_UInt] --> T_Bool
primOpType PGT = [T_UInt, T_UInt] --> T_Bool
primOpType IF_THEN_ELSE = T_Forall "b" (T_Forall "a" ([T_Var "b", T_Var "a", T_Var "a"] --> T_Var "a"))
primOpType DIGEST_EQ = ([T_Digest, T_Digest] --> T_Bool)
primOpType ADDRESS_EQ = ([T_Address, T_Address] --> T_Bool)
primOpType LSH = [T_UInt, T_UInt] --> T_UInt
primOpType RSH = [T_UInt, T_UInt] --> T_UInt
primOpType BAND = [T_UInt, T_UInt] --> T_UInt
primOpType BIOR = [T_UInt, T_UInt] --> T_UInt
primOpType BXOR = [T_UInt, T_UInt] --> T_UInt

data FluidVar
  = FV_balance
  deriving (Eq, Generic, NFData, Ord, Show)

fluidVarType :: FluidVar -> SLType
fluidVarType FV_balance = T_UInt

data SLPrimitive
  = SLPrim_makeEnum
  | SLPrim_declassify
  | SLPrim_digest
  | SLPrim_commit
  | SLPrim_committed
  | SLPrim_claim ClaimType
  | SLPrim_interact SrcLoc SLPart String SLType
  | SLPrim_is_type
  | SLPrim_type_eq
  | SLPrim_typeOf
  | SLPrim_Fun
  | SLPrim_Bytes
  | SLPrim_Data
  | SLPrim_Data_variant (M.Map SLVar SLType) SLVar SLType
  | SLPrim_data_match
  | SLPrim_Array
  | SLPrim_Array_iota
  | SLPrim_array
  | SLPrim_array_length
  | SLPrim_array_set
  | SLPrim_array_concat
  | SLPrim_array_map
  | SLPrim_array_reduce
  | SLPrim_array_zip
  | SLPrim_Tuple
  | SLPrim_tuple_length
  | SLPrim_tuple_set
  | SLPrim_Object
  | SLPrim_App_Delay SrcLoc SLEnv [SLVal] [JSExpression] JSStatement SLEnv
  | SLPrim_op PrimOp
  | SLPrim_transfer
  | SLPrim_transfer_amt_to SLVal
  | SLPrim_exit
  | SLPrim_exitted
  | SLPrim_forall
  | SLPrim_PrimDelay SrcLoc SLPrimitive [SLSVal] [SLSVal]
  | SLPrim_part_set
  | SLPrim_part_setted SrcLoc SLPart DLArg
  | SLPrim_wait
  | SLPrim_fluid_read FluidVar
  deriving (Eq, Generic, NFData, Show)

type SLSVal = (SecurityLevel, SLVal)

data SLSSVal = SLSSVal
  { sss_at :: SrcLoc
  , sss_level :: SecurityLevel
  , sss_val :: SLVal
  }
  deriving (Eq, Generic, NFData, Show)

sss_restrict :: SecurityLevel -> SLSSVal -> SLSSVal
sss_restrict lvl1 (SLSSVal at lvl2 val) =
  (SLSSVal at (lvl1 <> lvl2) val)

sss_sls :: SLSSVal -> SLSVal
sss_sls (SLSSVal _ level val) = (level, val)

sls_sss :: SrcLoc -> SLSVal -> SLSSVal
sls_sss at (level, val) = SLSSVal at level val

type SLEnv = M.Map SLVar SLSSVal

mt_env :: SLEnv
mt_env = mempty

m_fromList_public :: SrcLoc -> [(SLVar, SLVal)] -> SLEnv
m_fromList_public at kvs =
  M.fromList $ map go kvs
  where
    go (k, v) = (k, SLSSVal at Public v)

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Ord, Generic, NFData)

instance Show SLCtxtFrame where
  show (SLC_CloApp call_at clo_at mname) =
    "at " ++ show call_at ++ " call to " ++ name ++ " (defined at: " ++ show clo_at ++ ")"
    where
      name = maybe "[unknown function]" show mname

callAt :: SLCtxtFrame -> SrcLoc
callAt (SLC_CloApp at _ _) = at

--- Dynamic Language

data DeployMode
  = DM_constructor
  | DM_firstMsg
  deriving (Eq, Generic, NFData, Show)

newtype InteractEnv
  = InteractEnv (M.Map SLVar SLType)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, NFData, Semigroup)

newtype SLParts
  = SLParts (M.Map SLPart InteractEnv)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, NFData, Semigroup)

data DLConstant
  = DLC_UInt_max
  deriving (Eq, Generic, NFData, Show, Ord)

data DLLiteral
  = DLL_Null
  | DLL_Bool Bool
  | DLL_Int SrcLoc Integer
  | DLL_Bytes B.ByteString
  deriving (Eq, Generic, NFData, Show, Ord)

data DLVar = DLVar SrcLoc String SLType Int
  deriving (Eq, Generic, NFData, Show, Ord)

varType :: DLVar -> SLType
varType (DLVar _ _ t _) = t

data DLArg
  = DLA_Var DLVar
  | DLA_Constant DLConstant
  | DLA_Literal DLLiteral
  | DLA_Interact SLPart String SLType
  deriving (Eq, Ord, Generic, NFData, Show)

data ClaimType
  = --- Verified on all paths
    CT_Assert
  | --- Always assumed true
    CT_Assume
  | --- Verified in honest, assumed in dishonest. (This may sound
    --- backwards, but by verifying it in honest mode, then we are
    --- checking that the other participants fulfill the promise when
    --- acting honestly.)
    CT_Require
  | --- Check if an assignment of variables exists to make
    --- this true.
    CT_Possible
  | --- Check if one part can't know what another party does know
    CT_Unknowable SLPart [DLArg]
  deriving (Eq, Ord, Generic, NFData, Show)

class IsPure a where
  isPure :: a -> Bool

class IsLocal a where
  isLocal :: a -> Bool

data StmtAnnot = StmtAnnot
  { sa_pure :: Bool
  , sa_local :: Bool
  }
  deriving (Eq, Generic, NFData, Show)

instance Semigroup StmtAnnot where
  (StmtAnnot xp xl) <> (StmtAnnot yp yl) = (StmtAnnot (xp && yp) (xl && yl))

instance Monoid StmtAnnot where
  mempty = StmtAnnot True True

instance IsPure StmtAnnot where
  isPure = sa_pure

instance IsLocal StmtAnnot where
  isLocal = sa_local

instance IsPure a => IsPure (Seq.Seq a) where
  isPure = all isPure

instance IsLocal a => IsLocal (Seq.Seq a) where
  isLocal = all isLocal

mkAnnot :: IsPure a => IsLocal a => a -> StmtAnnot
mkAnnot a = StmtAnnot {..}
  where
    sa_pure = isPure a
    sa_local = isLocal a

data DLLargeArg
  = DLLA_Array SLType [DLArg]
  | DLLA_Tuple [DLArg]
  | DLLA_Obj (M.Map String DLArg)
  | DLLA_Data (M.Map SLVar SLType) String DLArg
  deriving (Eq, Ord, Generic, NFData, Show)

data DLExpr
  = DLE_Arg SrcLoc DLArg
  | DLE_LArg SrcLoc DLLargeArg
  | DLE_Impossible SrcLoc String
  | DLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLE_ArrayRef SrcLoc DLArg DLArg
  | DLE_ArraySet SrcLoc DLArg DLArg DLArg
  | DLE_ArrayConcat SrcLoc DLArg DLArg
  | DLE_ArrayZip SrcLoc DLArg DLArg
  | DLE_TupleRef SrcLoc DLArg Integer
  | DLE_ObjectRef SrcLoc DLArg String
  | DLE_Interact SrcLoc [SLCtxtFrame] SLPart String SLType [DLArg]
  | DLE_Digest SrcLoc [DLArg]
  | DLE_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg (Maybe B.ByteString)
  | DLE_Transfer SrcLoc DLArg DLArg
  | DLE_Wait SrcLoc DLArg
  | DLE_PartSet SrcLoc SLPart DLArg
  deriving (Eq, Ord, Generic, NFData, Show)

instance IsPure DLExpr where
  isPure = \case
    DLE_Arg {} -> True
    DLE_LArg {} -> True
    DLE_Impossible {} -> True
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_ArraySet {} -> True
    DLE_ArrayConcat {} -> True
    DLE_ArrayZip {} -> True
    DLE_TupleRef {} -> True
    DLE_ObjectRef {} -> True
    DLE_Interact {} -> False
    DLE_Digest {} -> True
    DLE_Claim _ _ ct _ _ ->
      case ct of
        CT_Assert -> True
        CT_Possible -> True
        CT_Assume -> False
        CT_Require -> False
        CT_Unknowable {} -> True
    DLE_Transfer {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> False

instance IsLocal DLExpr where
  isLocal = \case
    DLE_Arg {} -> True
    DLE_LArg {} -> True
    DLE_Impossible {} -> True
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_ArraySet {} -> True
    DLE_ArrayConcat {} -> True
    DLE_ArrayZip {} -> True
    DLE_TupleRef {} -> True
    DLE_ObjectRef {} -> True
    DLE_Interact {} -> True
    DLE_Digest {} -> True
    DLE_Claim {} -> True
    DLE_Transfer {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> True

newtype DLAssignment
  = DLAssignment (M.Map DLVar DLArg)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, NFData, Semigroup)

assignment_vars :: DLAssignment -> [DLVar]
assignment_vars (DLAssignment m) = M.keys m

data FromSpec
  = FS_Join DLVar
  | FS_Again DLVar
  deriving (Eq, Generic, NFData, Show)

type SwitchCases a =
  --- FIXME at the SrcLoc of the case
  M.Map SLVar (Maybe DLVar, a)

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
      , dls_tc_from :: SLPart
      , dls_tc_fs :: FromSpec
      , dls_tc_from_as :: [DLArg]
      , dls_tc_from_msg :: [DLVar]
      , dls_tc_from_amt :: DLArg
      , dls_tc_from_amtv :: DLVar
      , dls_tc_mtime :: (Maybe (DLArg, DLStmts))
      , dls_tc_cons :: DLStmts
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
  | DLS_ParallelReduce
      { dls_pr_at :: SrcLoc
      , dls_pr_init :: DLAssignment
      , dls_pr_inv :: DLBlock
      , dls_pr_muntil :: Maybe DLBlock
      , dls_pr_mtimeout :: Maybe DLArg
      , dls_pr_cases :: [(SLPart, DLStmts)]
      }
  deriving (Eq, Generic, NFData, Show)

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
    DLS_ParallelReduce a _ _ _ _ _ -> a

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
    DLS_ParallelReduce {} -> False

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
    DLS_Only _ _ ss -> isLocal ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> isLocal ss
    DLS_While {} -> False
    DLS_Continue {} -> False
    DLS_FluidSet {} -> True
    DLS_FluidRef {} -> True
    DLS_ParallelReduce {} -> False

type DLStmts = Seq.Seq DLStmt

data DLBlock
  = DLBlock SrcLoc [SLCtxtFrame] DLStmts DLArg
  deriving (Eq, Generic, NFData, Show)

data DLOpts = DLOpts
  { dlo_deployMode :: DeployMode
  , dlo_verifyOverflow :: Bool
  , dlo_verifyPerConnector :: Bool
  , dlo_connectors :: [T.Text]
  }
  deriving (Eq, Generic, NFData, Show)

data DLProg
  = DLProg SrcLoc DLOpts SLParts DLStmts
  deriving (Generic, NFData)

--- Linear Language
data LLCommon a
  = LL_Return SrcLoc
  | LL_Let SrcLoc (Maybe DLVar) DLExpr a
  | LL_ArrayMap SrcLoc DLVar DLArg DLVar LLBlock a
  | LL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar LLBlock a
  | LL_Var SrcLoc DLVar a
  | LL_Set SrcLoc DLVar DLArg a
  | LL_LocalIf SrcLoc DLArg LLLocal LLLocal a
  | LL_LocalSwitch SrcLoc DLVar (SwitchCases LLLocal) a
  deriving (Eq, Show)

data LLLocal
  = LLL_Com (LLCommon LLLocal)
  deriving (Eq, Show)

data LLBlock
  = LLBlock SrcLoc [SLCtxtFrame] LLLocal DLArg
  deriving (Eq, Show)

data LLConsensus
  = LLC_Com (LLCommon LLConsensus)
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Switch SrcLoc DLVar (SwitchCases LLConsensus)
  | LLC_FromConsensus SrcLoc SrcLoc LLStep
  | --- inv then cond then body then kont
    LLC_While
      { llc_w_at :: SrcLoc
      , llc_w_asn :: DLAssignment
      , llc_w_inv :: LLBlock
      , llc_w_cond :: LLBlock
      , llc_w_body :: LLConsensus
      , llc_w_k :: LLConsensus
      }
  | LLC_Continue SrcLoc DLAssignment
  deriving (Eq, Show)

data LLStep
  = LLS_Com (LLCommon LLStep)
  | LLS_Stop SrcLoc
  | LLS_Only SrcLoc SLPart LLLocal LLStep
  | LLS_ToConsensus
      { lls_tc_at :: SrcLoc
      , lls_tc_from :: SLPart
      , lls_tc_fs :: FromSpec
      , lls_tc_from_as :: [DLArg]
      , lls_tc_from_msg :: [DLVar]
      , lls_tc_from_amt :: DLArg
      , lls_tc_from_amtv :: DLVar
      , lls_tc_mtime :: (Maybe (DLArg, LLStep))
      , lls_tc_cons :: LLConsensus
      }
  | LLS_ParallelReduce
      { lls_pr_at :: SrcLoc
      , lls_pr_iasn :: DLAssignment
      , lls_pr_inv :: LLBlock
      , lls_pr_muntil :: Maybe LLBlock
      , lls_pr_mtimeout :: Maybe DLArg
      , lls_pr_cases :: [(SLPart, LLStep)]
      , lls_pr_con :: LLConsensus
      }
  deriving (Eq, Show)

data LLOpts = LLOpts
  { llo_deployMode :: DeployMode
  , llo_verifyOverflow :: Bool
  }
  deriving (Generic, Eq, Show)

data LLProg
  = LLProg SrcLoc LLOpts SLParts LLStep
  deriving (Eq, Show)

--- Projected Language
data PLLetCat
  = PL_Once
  | PL_Many
  deriving (Eq, Show)

instance Semigroup PLLetCat where
  _ <> _ = PL_Many

data PLCommon a
  = PL_Return SrcLoc
  | PL_Let SrcLoc PLLetCat DLVar DLExpr a
  | PL_ArrayMap SrcLoc DLVar DLArg DLVar PLBlock a
  | PL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar PLBlock a
  | PL_Eff SrcLoc DLExpr a
  | PL_Var SrcLoc DLVar a
  | PL_Set SrcLoc DLVar DLArg a
  | PL_LocalIf SrcLoc DLArg PLTail PLTail a
  | PL_LocalSwitch SrcLoc DLVar (SwitchCases PLTail) a
  deriving (Eq, Show)

data PLTail
  = PLTail (PLCommon PLTail)
  deriving (Eq, Show)

data PLBlock
  = PLBlock SrcLoc PLTail DLArg
  deriving (Eq, Show)

data ETail
  = ET_Com (PLCommon ETail)
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg ETail ETail
  | ET_Switch SrcLoc DLVar (SwitchCases ETail)
  | ET_FromConsensus SrcLoc Int (Maybe [DLVar]) ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_fs :: FromSpec
      , et_tc_prev :: Int
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt    saved_vs
             Maybe ([DLArg], DLArg, [DLVar])
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_amtv :: DLVar
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
  deriving (Eq, Show)

data EPProg
  = EPProg SrcLoc InteractEnv ETail
  deriving (Eq, Show)

data CTail
  = CT_Com (PLCommon CTail)
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc (Maybe [DLVar])
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq, Show)

data CInterval
  = CBetween [DLArg] [DLArg]
  deriving (Show, Eq)

default_interval :: CInterval
default_interval = CBetween [] []

interval_from :: CInterval -> [DLArg]
interval_from (CBetween froml _) = froml

interval_add_from :: CInterval -> DLArg -> CInterval
interval_add_from (CBetween froml tol) x =
  CBetween (x : froml) (x : tol)

interval_add_to :: CInterval -> DLArg -> CInterval
interval_add_to (CBetween froml tol) x =
  CBetween froml (x : tol)

interval_no_to :: CInterval -> CInterval
interval_no_to (CBetween froml _) =
  CBetween froml []

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval
      , ch_fs :: FromSpec
      , ch_last :: Int
      , ch_svs :: [DLVar]
      , ch_msg :: [DLVar]
      , ch_amtv :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVar]
      , cl_vars :: [(PLLetCat, DLVar)]
      , cl_body :: CTail
      }
  deriving (Eq, Show)

newtype CHandlers = CHandlers (M.Map Int CHandler)
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

data CPProg
  = CPProg SrcLoc CHandlers
  deriving (Eq, Show)

newtype EPPs = EPPs (M.Map SLPart EPProg)
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

data PLOpts = PLOpts
  { plo_deployMode :: DeployMode
  , plo_verifyOverflow :: Bool
  }
  deriving (Generic, Eq, Show)

data PLProg
  = PLProg SrcLoc PLOpts EPPs CPProg
  deriving (Eq, Show)
