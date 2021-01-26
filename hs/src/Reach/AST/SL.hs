{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.SL where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics
import GHC.Stack (HasCallStack)
import Generics.Deriving (conNameOf)
import Language.JavaScript.Parser
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.JSOrphans ()
import Reach.Util

-- SL types are a superset of DL types.
-- We copy/paste constructors instead of using `ST_Val DLType`
--  because some things can exist in SL that do not exist in DL,
--  such as an object whose fields are functions.
data SLType
  = ST_Null
  | ST_Bool
  | ST_UInt
  | ST_Bytes Integer
  | ST_Digest
  | ST_Address
  | ST_Array SLType Integer
  | ST_Tuple [SLType]
  | ST_Object (M.Map SLVar SLType)
  | ST_Data (M.Map SLVar SLType)
  | ST_Fun [SLType] SLType
  | ST_Forall SLVar SLType
  | ST_Var SLVar
  | ST_Type SLType
  deriving (Eq, Generic, NFData, Ord)

-- | Fold over SLType, doing something special on Fun
funFold
  :: a -- ^ On no Fun inside
  -> ([SLType] -> a) -- ^ On many DLType inside
  -> ([SLType] -> SLType -> a) -- ^ On Fun
  -> SLType -- ^ The type to fold over
  -> a
funFold z k fun = go
  where
    go = \case
      ST_Null -> z
      ST_Bool -> z
      ST_UInt -> z
      ST_Bytes _ -> z
      ST_Digest -> z
      ST_Address -> z
      ST_Fun inTys outTy -> fun inTys outTy
      ST_Array ty _ -> go ty
      ST_Tuple tys -> k tys
      ST_Object m -> k $ M.elems m
      ST_Data m -> k $ M.elems m
      ST_Forall _ ty -> go ty
      ST_Var _ -> z
      ST_Type _ -> z

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

st2dt :: HasCallStack => SLType -> Maybe DLType
st2dt = \case
  ST_Null -> pure T_Null
  ST_Bool -> pure T_Bool
  ST_UInt -> pure T_UInt
  ST_Bytes i -> pure $ T_Bytes i
  ST_Digest -> pure T_Digest
  ST_Address -> pure T_Address
  ST_Array ty i -> T_Array <$> st2dt ty <*> pure i
  ST_Tuple tys -> T_Tuple <$> traverse st2dt tys
  ST_Object tyMap -> T_Object <$> traverse st2dt tyMap
  ST_Data tyMap -> T_Data <$> traverse st2dt tyMap
  ST_Fun {} -> Nothing
  ST_Forall {} -> Nothing
  ST_Var {} -> Nothing
  ST_Type {} -> Nothing

dt2st :: DLType -> SLType
dt2st = \case
  T_Null -> ST_Null
  T_Bool -> ST_Bool
  T_UInt -> ST_UInt
  T_Bytes i -> ST_Bytes i
  T_Digest -> ST_Digest
  T_Address -> ST_Address
  T_Array ty i -> ST_Array (dt2st ty) i
  T_Tuple tys -> ST_Tuple (map dt2st tys)
  T_Object tyMap -> ST_Object (M.map dt2st tyMap)
  T_Data tyMap -> ST_Data (M.map dt2st tyMap)

st2it :: SLType -> Maybe IType
st2it t = case t of
  ST_Fun dom rng -> IT_Fun <$> traverse st2dt dom <*> st2dt rng
  _ -> IT_Val <$> st2dt t

instance Show SLType where
  show ST_Null = "Null"
  show ST_Bool = "Bool"
  show ST_UInt = "UInt"
  show (ST_Bytes sz) = "Bytes(" <> show sz <> ")"
  show ST_Digest = "Digest"
  show ST_Address = "Address"
  show (ST_Array ty i) = "Array(" <> show ty <> ", " <> show i <> ")"
  show (ST_Tuple tys) = "Tuple(" <> showTys tys <> ")"
  show (ST_Object tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (ST_Data tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (ST_Fun tys ty) = "Fun([" <> showTys tys <> "], " <> show ty <> ")"
  show (ST_Forall x t) = "Forall(" <> show x <> ", " <> show t <> ")"
  show (ST_Var x) = show x
  show (ST_Type ty) = "Type(" <> show ty <> ")"

infixr 9 -->

(-->) :: [SLType] -> SLType -> SLType
dom --> rng = ST_Fun dom rng

type SLPartEnvs = M.Map SLPart SLEnv

data SLCloEnv
  = SLCloEnv SLPartEnvs SLEnv
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
  | -- I really want to remove these two Maybes, but it is hard.
    -- The DLVar is needed so that inside of an `only`, we can read off the
    -- address of the participant without setting it in pdvs
    -- The SLVar is needed to know where to put the above DLVar. This feels
    -- really sloppy. Maybe a better way would be to look at the context when
    -- you're inspecting an object and set the pdvs that gets sent to Type.hs
    -- differently.
    SLV_Participant SrcLoc SLPart (Maybe SLVar) (Maybe DLVar)
  | SLV_RaceParticipant SrcLoc (S.Set SLPart)
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
  | TCM_When
  | TCM_Timeout
  deriving (Eq, Generic, NFData, Show)

data ForkMode
  = FM_Case
  | FM_Timeout
  deriving (Eq, Generic, NFData, Show)

data ParallelReduceMode
  = PRM_Invariant
  | PRM_While
  | PRM_Case
  | PRM_Timeout
  deriving (Eq, Generic, NFData, Show)

data SLForm
  = SLForm_App
  | SLForm_each
  | SLForm_EachAns [(SLPart, Maybe SLVar)] SrcLoc SLCloEnv JSExpression
  | SLForm_Part_Only SLPart (Maybe SLVar)
  | SLForm_Part_ToConsensus
      { slptc_at :: SrcLoc
      , slptc_whos :: S.Set SLPart
      , slptc_mv :: Maybe SLVar
      , slptc_mode :: Maybe ToConsensusMode
      , slptc_msg :: Maybe [SLVar]
      , slptc_amte :: Maybe JSExpression
      , slptc_whene :: Maybe JSExpression
      , slptc_timeout :: Maybe (SrcLoc, JSExpression, JSBlock)
      }
  | SLForm_unknowable
  | SLForm_fork
  | SLForm_fork_partial
      { slf_at :: SrcLoc
      , slf_mode :: Maybe ForkMode
      , slf_cases :: [(SrcLoc, (JSExpression, JSExpression, JSExpression, JSExpression))]
      , slf_mtime :: Maybe (SrcLoc, JSExpression, JSBlock)
      }
  | SLForm_parallel_reduce
  | SLForm_parallel_reduce_partial
      { slpr_at :: SrcLoc
      , slpr_mode :: Maybe ParallelReduceMode
      , slpr_init :: JSExpression
      , slpr_minv :: Maybe JSExpression
      , slpr_mwhile :: Maybe JSExpression
      , slpr_cases :: [(SrcLoc, [JSExpression])]
      , slpr_mtime :: Maybe (SrcLoc, [JSExpression])
      }
  | SLForm_wait
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

primOpType :: PrimOp -> SLType
primOpType SELF_ADDRESS = impossible "self address"
primOpType ADD = [ST_UInt, ST_UInt] --> ST_UInt
primOpType SUB = [ST_UInt, ST_UInt] --> ST_UInt
primOpType MUL = [ST_UInt, ST_UInt] --> ST_UInt
primOpType DIV = [ST_UInt, ST_UInt] --> ST_UInt
primOpType MOD = [ST_UInt, ST_UInt] --> ST_UInt
primOpType PLT = [ST_UInt, ST_UInt] --> ST_Bool
primOpType PLE = [ST_UInt, ST_UInt] --> ST_Bool
primOpType PEQ = ST_Forall "a" ([ST_Var "a", ST_Var "a"] --> ST_Bool)
primOpType PGE = [ST_UInt, ST_UInt] --> ST_Bool
primOpType PGT = [ST_UInt, ST_UInt] --> ST_Bool
primOpType IF_THEN_ELSE = ST_Forall "b" (ST_Forall "a" ([ST_Var "b", ST_Var "a", ST_Var "a"] --> ST_Var "a"))
primOpType DIGEST_EQ = ([ST_Digest, ST_Digest] --> ST_Bool)
primOpType ADDRESS_EQ = ([ST_Address, ST_Address] --> ST_Bool)
primOpType LSH = [ST_UInt, ST_UInt] --> ST_UInt
primOpType RSH = [ST_UInt, ST_UInt] --> ST_UInt
primOpType BAND = [ST_UInt, ST_UInt] --> ST_UInt
primOpType BIOR = [ST_UInt, ST_UInt] --> ST_UInt
primOpType BXOR = [ST_UInt, ST_UInt] --> ST_UInt

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
  | SLPrim_Object_has
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
  | SLPrim_fluid_read FluidVar
  | SLPrim_race
  | SLPrim_lastConsensusTime
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
