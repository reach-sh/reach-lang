{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.SL where

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
import Reach.Texty
import Reach.Util
import Reach.Warning (Deprecation(..))

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
  | ST_Token
  | ST_Array SLType Integer
  | ST_Tuple [SLType]
  | ST_Object (M.Map SLVar SLType)
  | ST_Data (M.Map SLVar SLType)
  | ST_Struct [(SLVar, SLType)]
  | ST_Fun SLTypeFun
  | ST_UDFun SLType
  | ST_Type SLType
  | ST_Refine SLType SLVal (Maybe SLVal)
  deriving (Eq, Generic)

data SLTypeFun = SLTypeFun
  { stf_dom :: [SLType]
  , stf_rng :: SLType
  , stf_pre :: Maybe SLVal
  , stf_post :: Maybe SLVal
  , stf_pre_msg :: Maybe SLVal
  , stf_post_msg :: Maybe SLVal
  }
  deriving (Eq, Generic)

instance Show SLType where
  show = \case
    ST_Null -> "Null"
    ST_Bool -> "Bool"
    ST_UInt -> "UInt"
    ST_Bytes sz -> "Bytes(" <> show sz <> ")"
    ST_Digest -> "Digest"
    ST_Address -> "Address"
    ST_Token -> "Token"
    ST_Array ty i -> "Array(" <> show ty <> ", " <> show i <> ")"
    ST_Tuple tys -> "Tuple(" <> showTys tys <> ")"
    ST_Object tyMap -> "Object({" <> showTyMap tyMap <> "})"
    ST_Data tyMap -> "Data({" <> showTyMap tyMap <> "})"
    ST_Struct tys -> "Struct([" <> showTyList tys <> "])"
    ST_Fun (SLTypeFun tys ty Nothing Nothing Nothing Nothing) ->
      "Fun([" <> showTys tys <> "], " <> show ty <> ")"
    ST_Fun (SLTypeFun tys ty _ _ _ _) ->
      "Refine(Fun([" <> showTys tys <> "], " <> show ty <> "), ...., ....)"
    ST_UDFun rng ->
      "Fun(true, " <> show rng <> ")"
    ST_Type ty -> "Type(" <> show ty <> ")"
    ST_Refine ty _ _ -> "Refine(" <> show ty <> ", ....)"

instance Pretty SLType where
  pretty = viaShow

st2dt :: HasCallStack => SLType -> Maybe DLType
st2dt = \case
  ST_Null -> pure T_Null
  ST_Bool -> pure T_Bool
  ST_UInt -> pure T_UInt
  ST_Bytes i -> pure $ T_Bytes i
  ST_Digest -> pure T_Digest
  ST_Address -> pure T_Address
  ST_Token -> pure T_Token
  ST_Array ty i -> T_Array <$> st2dt ty <*> pure i
  ST_Tuple tys -> T_Tuple <$> traverse st2dt tys
  ST_Object tyMap -> T_Object <$> traverse st2dt tyMap
  ST_Data tyMap -> T_Data <$> traverse st2dt tyMap
  ST_Struct tys -> T_Struct <$> traverse (\(k, t) -> (,) k <$> st2dt t) tys
  ST_Fun {} -> Nothing
  ST_UDFun {} -> Nothing
  ST_Type {} -> Nothing
  ST_Refine t _ _ -> st2dt t

dt2st :: DLType -> SLType
dt2st = \case
  T_Null -> ST_Null
  T_Bool -> ST_Bool
  T_UInt -> ST_UInt
  T_Bytes i -> ST_Bytes i
  T_Digest -> ST_Digest
  T_Address -> ST_Address
  T_Token -> ST_Token
  T_Array ty i -> ST_Array (dt2st ty) i
  T_Tuple tys -> ST_Tuple $ map dt2st tys
  T_Object tyMap -> ST_Object $ M.map dt2st tyMap
  T_Data tyMap -> ST_Data $ M.map dt2st tyMap
  T_Struct tys -> ST_Struct $ map (\(k, t) -> (k, dt2st t)) tys

st2it :: SLType -> Maybe IType
st2it t = case t of
  ST_Fun (SLTypeFun {..}) ->
    IT_Fun <$> traverse st2dt stf_dom <*> st2dt stf_rng
  _ -> IT_Val <$> st2dt t

type SLPartEnvs = M.Map SLPart SLEnv

data SLCloEnv = SLCloEnv
  { clo_penvs :: SLPartEnvs
  , clo_cenv :: SLEnv
  , clo_use_strict :: Bool
  }
  deriving (Eq, Generic)

data SLClo = SLClo (Maybe SLVar) [JSExpression] JSBlock SLCloEnv
  deriving (Eq, Generic)

data SLVal
  = SLV_Null SrcLoc String
  | SLV_Bool SrcLoc Bool
  | SLV_Int SrcLoc Integer
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Array SrcLoc DLType [SLVal]
  | SLV_Tuple SrcLoc [SLVal]
  | SLV_Object SrcLoc (Maybe String) SLEnv
  | SLV_Struct SrcLoc [(SLVar, SLVal)]
  | SLV_Clo SrcLoc (Maybe SLTypeFun) SLClo
  | SLV_Data SrcLoc (M.Map SLVar DLType) SLVar SLVal
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
  | SLV_Anybody
  | SLV_Prim SLPrimitive
  | SLV_Form SLForm
  | SLV_Kwd SLKwd
  | SLV_Map DLMVar
  | SLV_Deprecated Deprecation SLVal
  deriving (Eq, Generic)

-- | Equivalence operation on flattened or simplified structures.
class (Eq a) => Equiv a where
  equiv :: a -> a -> Bool

instance (Equiv a) => Equiv (Maybe a) where
  equiv a b = case (a, b) of
    (Nothing, Nothing) -> True
    (Just a', Just b') -> equiv a' b'
    _ -> False

instance (Equiv a) => Equiv (S.Set a) where
  equiv s1 s2 = equiv (S.toList s1) (S.toList s2)

instance Equiv B.ByteString where
  equiv = (==)

instance Equiv Char where
  equiv = (==)

instance Equiv Int where
  equiv = (==)

instance Equiv T.Text where
  equiv = (==)

instance Equiv Integer where
  equiv = (==)

instance Equiv Bool where
  equiv = (==)

instance (Equiv a, Equiv b) => Equiv (Either a b) where
  equiv x y = case (x,y) of
    (Left a, Left b) -> equiv a b
    (Right a, Right b) -> equiv a b
    _ -> False

instance Equiv PrimOp where
  equiv = (==)

instance Equiv SecurityLevel where
  equiv = (==)

instance Equiv FluidVar where
  equiv = (==)

instance Equiv DLArg where
  equiv a b = case (a,b) of
    (DLA_Var v1, DLA_Var v2) -> equiv v1 v2
    (DLA_Constant c1, DLA_Constant c2) -> equiv c1 c2
    (DLA_Literal lit1, DLA_Literal lit2) -> equiv lit1 lit2
    (DLA_Interact part1 s1 _, DLA_Interact part2 s2 _) -> equiv part1 part2 && equiv s1 s2
    _ -> False

instance Equiv DLLiteral where
  equiv a b = case (a,b) of
    (DLL_Null, DLL_Null) -> True
    (DLL_Bool b1, DLL_Bool b2) -> equiv b1 b2
    (DLL_Int _ x, DLL_Int _ y) -> equiv x y
    (DLL_Bytes b1, DLL_Bytes b2) -> equiv b1 b2
    _ -> False

instance Equiv SLTypeFun where
  equiv (SLTypeFun {stf_pre = pre1, stf_post = post1}) (SLTypeFun {stf_pre = pre2, stf_post = post2}) =
    equiv pre1 pre2 && equiv post1 post2

instance Equiv SLSSVal where
  equiv (SLSSVal {sss_val = v1}) (SLSSVal { sss_val = v2}) = equiv v1 v2

instance Equiv DLMVar where
  equiv (DLMVar a) (DLMVar b) = equiv a b

instance (Equiv a) => Equiv [a] where
  equiv xs ys = all (\(x,y) -> equiv x y) $ zip xs ys

instance (Equiv b, Equiv a) => Equiv (M.Map a b) where
  equiv m1 m2 = equiv (M.toList m1) (M.toList m2)

instance (Equiv a, Equiv b) => Equiv (a, b) where
  equiv (a,b) (a2,b2) = equiv a a2 && equiv b b2

instance Equiv DLType where
  -- this is safe to use Eq with
  equiv = (==)


instance Equiv ClaimType where
  equiv a b = case (a,b) of
    (CT_Assert, CT_Assert) -> True
    (CT_Assume b1, CT_Assume b2) -> equiv b1 b2
    (CT_Require, CT_Require) -> True
    (CT_Possible, CT_Possible) -> True
    (CT_Unknowable part1 args1, CT_Unknowable part2 args2) -> equiv args1 args2 && equiv part1 part2
    _ -> False

instance Equiv Deprecation where
  equiv a b = case (a,b) of
    (D_ParticipantTuples _, D_ParticipantTuples _) -> True
    (D_SnakeToCamelCase _, D_SnakeToCamelCase _ ) -> True
    (D_ReachAppArgs, D_ReachAppArgs) -> True
    (D_UntypedTimeArg, D_UntypedTimeArg) -> True
    _ -> False

instance Equiv SLForm where
  equiv a b = case (a,b) of
    (SLForm_App, SLForm_App) -> True
    (SLForm_each, SLForm_each) -> True
    ((SLForm_EachAns parts _ _ _), (SLForm_EachAns parts2 _ _ _)) -> equiv parts parts2
    ((SLForm_Part_Only p _), (SLForm_Part_Only p2 _)) -> equiv p p2
    ((SLForm_liftInteract p _ _), (SLForm_liftInteract p2 _ _)) -> equiv p p2
    ((SLForm_Part_ToConsensus {slptc_whos = who, slptc_mv = mv}), (SLForm_Part_ToConsensus {slptc_whos = who2, slptc_mv = mv2})) ->
      equiv who who2 && equiv mv mv2
    (SLForm_unknowable, SLForm_unknowable) -> True
    (SLForm_fork, SLForm_fork) -> True
    (SLForm_parallel_reduce, SLForm_parallel_reduce) -> True
    (SLForm_wait, SLForm_wait) -> True
    -- these partials are left unchecked
    (SLForm_parallel_reduce_partial{}, _) -> False
    (SLForm_fork_partial{}, _) -> False
    _ -> False

instance Equiv SLType where
  equiv a b = case (a,b) of
    (ST_Null, ST_Null) -> True
    (ST_Bool, ST_Bool) -> True
    (ST_UInt, ST_UInt) -> True
    (ST_Digest, ST_Digest) -> True
    (ST_Address, ST_Address) -> True
    (ST_Token, ST_Token) -> True
    ((ST_Bytes i), (ST_Bytes i2)) -> equiv i i2
    ((ST_Array s1 _len1), (ST_Array s2 _len2)) -> equiv s1 s2
    ((ST_Tuple xs), (ST_Tuple ys)) -> equiv xs ys
    ((ST_Object m), (ST_Object m2)) -> equiv m m2
    ((ST_Data m), (ST_Data m2)) -> equiv m m2
    ((ST_Struct s), (ST_Struct s2)) -> equiv s s2
    ((ST_Fun f), (ST_Fun f2)) -> equiv f f2
    ((ST_UDFun f), (ST_UDFun f2)) -> equiv f f2
    ((ST_Type s1), (ST_Type s2)) -> equiv s1 s2
    ((ST_Refine _sLType v _m), (ST_Refine _sLType2 v2 _m2)) -> equiv v v2
    _ -> False

instance Equiv DLVar where
  equiv (DLVar _ _ _dl i1) (DLVar _ _ _dl2 i2) = equiv i1 i2

instance Equiv DLConstant where
  equiv DLC_UInt_max DLC_UInt_max = True

instance Equiv SLVal where
  equiv a b = case (a,b) of
    ((SLV_Null _ _), (SLV_Null _ _)) -> True
    ((SLV_Bool _ b1), (SLV_Bool _ b2)) -> equiv b1 b2
    ((SLV_Int _ i1), (SLV_Int _ i2)) -> equiv i1 i2
    ((SLV_Bytes _ v1), (SLV_Bytes _ v2)) -> equiv v1 v2
    ((SLV_Array _ _ xs), (SLV_Array _ _ ys)) -> equiv xs ys
    ((SLV_Tuple _ v1), (SLV_Tuple _ v2)) -> equiv v1 v2
    ((SLV_Struct _ xs), (SLV_Struct _ ys)) -> equiv xs ys
    ((SLV_DLC d1), (SLV_DLC d2)) -> equiv d1 d2
    ((SLV_DLVar d1), (SLV_DLVar d2)) -> equiv d1 d2
    ((SLV_Connector t1), (SLV_Connector t2)) -> equiv t1 t2
    ((SLV_Prim p1), (SLV_Prim p2)) -> equiv p1 p2
    ((SLV_Kwd k1), (SLV_Kwd k2))  -> equiv k1 k2
    ((SLV_Deprecated d1 _), (SLV_Deprecated d2 _)) -> equiv d1 d2
    (SLV_Anybody, SLV_Anybody) -> True
    ((SLV_Participant _ s sl dl), (SLV_Participant _ s2 sl2 dl2)) -> equiv s s2 && equiv sl sl2 && equiv dl dl2
    ((SLV_RaceParticipant _ slSet1), (SLV_RaceParticipant _ slSet2)) -> equiv slSet1 slSet2
    ((SLV_Map v1), (SLV_Map v2)) -> equiv v1 v2
    ((SLV_Data _ m1 var1 val1), (SLV_Data _ m2 var2 val2)) -> equiv m1 m2 && equiv var1 var2 && equiv val1 val2
    ((SLV_Form f1), (SLV_Form f2)) -> equiv f1 f2
    ((SLV_Type t1), (SLV_Type t2)) -> equiv t1 t2
    ((SLV_Object _ _ slenv1), (SLV_Object _ _ slenv2)) -> equiv slenv1 slenv2
    -- Closures are the only uninspected structure
    ((SLV_Clo _ _ _), _) -> False
    _ -> False

instance Show SLVal where
  show = show . pretty

instance Pretty SLVal where
  pretty = \case
    SLV_Null {} -> "null"
    SLV_Bool _ b -> pretty b
    SLV_Int _ i -> pretty i
    SLV_Bytes _ b -> pretty b
    SLV_Array at t as ->
      "array" <> parens (pretty t <> comma <+> pretty (SLV_Tuple at as))
    SLV_Tuple _ as ->
      brackets $ hsep $ punctuate comma $ map pretty as
    SLV_Object _ (Just lab) _ -> pretty lab
    SLV_Object _ _ m -> render_obj m
    SLV_Clo {} -> "<closure>"
    SLV_Data _ _ vn vv -> "<" <> pretty vn <> " " <> pretty vv <> ">"
    SLV_Struct _ kvs ->
      "struct" <> brackets (hsep $ punctuate comma $ map go kvs)
      where
        go (k, v) = brackets $ hsep $ punctuate comma $ [pretty k, pretty v]
    SLV_DLC c -> "<constant: " <> viaShow c <> ">"
    SLV_DLVar v -> pretty v
    SLV_Type t -> "<type: " <> pretty t <> ">"
    SLV_Connector cn -> "<connector: " <> pretty cn <> ">"
    SLV_Participant _ who _ _ ->
      "<participant: " <> pretty who <> ">"
    SLV_RaceParticipant _ whos ->
      "<race: " <> pretty whos <> ">"
    SLV_Prim p -> "<primitive: " <> pretty (conNameOf p) <> ">"
    SLV_Form f -> "<form: " <> pretty (conNameOf f) <> ">"
    SLV_Kwd k -> pretty k
    SLV_Map mv -> "<map: " <> pretty mv <> ">"
    SLV_Anybody -> "Anybody"
    SLV_Deprecated d s -> "<deprecated: " <> viaShow d <> ">(" <> pretty s <> ")"

instance SrcLocOf SLVal where
  srclocOf = \case
    SLV_Null a _ -> a
    SLV_Bool a _ -> a
    SLV_Int a _ -> a
    SLV_Bytes a _ -> a
    SLV_Array a _ _ -> a
    SLV_Tuple a _ -> a
    SLV_Object a _ _ -> a
    SLV_Struct a _ -> a
    SLV_Clo a _ _ -> a
    SLV_Data a _ _ _ -> a
    SLV_DLC _ -> def
    SLV_DLVar (DLVar a _ _ _) -> a
    SLV_Type {} -> def
    SLV_Connector _ -> def
    SLV_Participant a _ _ _ -> a
    SLV_RaceParticipant a _ -> a
    SLV_Anybody -> def
    SLV_Prim _ -> def
    SLV_Form _ -> def
    SLV_Kwd _ -> def
    SLV_Map _ -> def
    SLV_Deprecated _ v -> srclocOf v
    where
      def = srcloc_builtin

isLiteralArray :: SLVal -> Bool
isLiteralArray (SLV_Array {}) = True
isLiteralArray _ = False

newtype SLInterface = SLInterface (M.Map SLVar SLType)
  deriving (Eq, Generic)

instance Pretty SLInterface where
  pretty (SLInterface m) = render_obj m

data SLViewInfo
  = SLViewInfo SrcLoc SLPart SLInterface
  deriving (Eq, Generic)

instance SrcLocOf SLViewInfo where
  srclocOf (SLViewInfo a _ _) = a

instance Pretty SLViewInfo where
  pretty (SLViewInfo _ n v) =
    "View" <> "(" <> pretty n <> ", " <> pretty v <> ")"

data SLLValue
  = SLLV_MapRef SrcLoc DLMVar DLArg

data ToConsensusMode
  = TCM_Publish
  | TCM_Pay
  | TCM_When
  | TCM_Timeout
  | TCM_ThrowTimeout
  | TCM_Fork
  deriving (Eq, Generic, Show)

data ForkMode
  = FM_Case
  | FM_Timeout
  | FM_ThrowTimeout
  | FM_PaySpec
  deriving (Eq, Generic, Show)

data ParallelReduceMode
  = PRM_Invariant
  | PRM_While
  | PRM_Case
  | PRM_Timeout
  | PRM_TimeRemaining
  | PRM_ThrowTimeout
  | PRM_PaySpec
  | PRM_Def
  deriving (Eq, Generic, Show)

data ToConsensusRec = ToConsensusRec
  { slptc_at :: SrcLoc
  , slptc_whos :: S.Set SLPart
  , slptc_mv :: Maybe SLVar
  , slptc_mode :: Maybe ToConsensusMode
  , slptc_msg :: Maybe [SLVar]
  , slptc_amte :: Maybe JSExpression
  , slptc_whene :: Maybe JSExpression
  , slptc_timeout :: Maybe (SrcLoc, JSExpression, Maybe JSBlock)
  , slptc_fork :: Bool
  }
  deriving (Eq, Generic)

data ForkCase = ForkCase
  { fc_at :: SrcLoc
  , fc_who :: JSExpression
  , fc_before :: JSExpression
  , fc_pay :: JSExpression
  , fc_after :: JSExpression
  }
  deriving (Eq, Generic)

data ForkRec = ForkRec
  { slf_at :: SrcLoc
  , slf_mode :: Maybe ForkMode
  , slf_cases :: [ForkCase]
  , slf_mtime :: Maybe (SrcLoc, [JSExpression])
  , slf_mnntpay :: Maybe JSExpression
  }
  deriving (Eq, Generic)

data ParallelReduceRec = ParallelReduceRec
  { slpr_at :: SrcLoc
  , slpr_mode :: Maybe ParallelReduceMode
  , slpr_init :: JSExpression
  , slpr_minv :: Maybe JSExpression
  , slpr_mwhile :: Maybe JSExpression
  , slpr_cases :: [(SrcLoc, [JSExpression])]
  , slpr_mtime :: Maybe (ParallelReduceMode, SrcLoc, [JSExpression])
  , slpr_mpay :: Maybe JSExpression
  , slpr_mdef :: Maybe JSExpression
  }
  deriving (Eq, Generic)

data SLForm
  = SLForm_App
  | SLForm_each
  | SLForm_EachAns [(SLPart, Maybe SLVar)] SrcLoc SLCloEnv JSExpression
  | SLForm_Part_Only SLPart (Maybe SLVar)
  | SLForm_liftInteract SLPart (Maybe SLVar) String
  | SLForm_Part_ToConsensus ToConsensusRec
  | SLForm_unknowable
  | SLForm_fork
  | SLForm_fork_partial ForkRec
  | SLForm_parallel_reduce
  | SLForm_parallel_reduce_partial ParallelReduceRec
  | SLForm_wait
  deriving (Eq, Generic)

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
  deriving (Bounded, Enum, Eq, Generic)

instance Equiv SLKwd where
  equiv = (==)

instance Show SLKwd where
  show k = drop 4 $ conNameOf k

instance Pretty SLKwd where
  pretty = viaShow

allKeywords :: [SLKwd]
allKeywords = enumFrom minBound

primOpType :: PrimOp -> ([DLType], DLType)
primOpType SELF_ADDRESS = impossible "self address"
primOpType ADD = ([T_UInt, T_UInt], T_UInt)
primOpType SUB = ([T_UInt, T_UInt], T_UInt)
primOpType MUL = ([T_UInt, T_UInt], T_UInt)
primOpType DIV = ([T_UInt, T_UInt], T_UInt)
primOpType MOD = ([T_UInt, T_UInt], T_UInt)
primOpType PLT = ([T_UInt, T_UInt], T_Bool)
primOpType PLE = ([T_UInt, T_UInt], T_Bool)
primOpType PEQ = impossible "peq type"
primOpType PGE = ([T_UInt, T_UInt], T_Bool)
primOpType PGT = ([T_UInt, T_UInt], T_Bool)
primOpType BYTES_CONCAT = impossible "pad type"
primOpType IF_THEN_ELSE = impossible "ite type"
primOpType DIGEST_EQ = ([T_Digest, T_Digest], T_Bool)
primOpType ADDRESS_EQ = ([T_Address, T_Address], T_Bool)
primOpType TOKEN_EQ = ([T_Token, T_Token], T_Bool)
primOpType LSH = ([T_UInt, T_UInt], T_UInt)
primOpType RSH = ([T_UInt, T_UInt], T_UInt)
primOpType BAND = ([T_UInt, T_UInt], T_UInt)
primOpType BIOR = ([T_UInt, T_UInt], T_UInt)
primOpType BXOR = ([T_UInt, T_UInt], T_UInt)

data RemoteFunMode
  = RFM_Pay
  | RFM_Bill
  | RFM_WithBill
  deriving (Eq, Generic, Show)

data SLPrimitive
  = SLPrim_makeEnum
  | SLPrim_declassify
  | SLPrim_digest
  | SLPrim_commit
  | SLPrim_committed
  | SLPrim_claim ClaimType
  | SLPrim_localf SrcLoc SLPart String (Either SLTypeFun SLType)
  | SLPrim_is_type
  | SLPrim_type_eq
  | SLPrim_typeOf
  | SLPrim_Fun
  | SLPrim_Refine
  | SLPrim_Bytes
  | SLPrim_Data
  | SLPrim_Data_variant (M.Map SLVar SLType) SLVar SLType
  | SLPrim_data_match
  | SLPrim_Array
  | SLPrim_Array_iota
  | SLPrim_array
  | SLPrim_array_elemType
  | SLPrim_array_length
  | SLPrim_array_set
  | SLPrim_array_concat
  | SLPrim_array_map
  | SLPrim_array_reduce
  | SLPrim_array_zip
  | SLPrim_Struct
  | SLPrim_Struct_fromTuple [(SLVar, SLType)]
  | SLPrim_Struct_fromObject [(SLVar, SLType)]
  | SLPrim_Struct_toTuple
  | SLPrim_Struct_toObject
  | SLPrim_Tuple
  | SLPrim_tuple_length
  | SLPrim_tuple_set
  | SLPrim_Object
  | SLPrim_Object_has
  | SLPrim_App_Delay SrcLoc JSStatement (SLEnv, Bool)
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
  | SLPrim_fluid_read_canWait FluidVar
  | SLPrim_race
  | SLPrim_Map
  | SLPrim_Map_new
  | SLPrim_Map_reduce
  | SLPrim_Participant
  | SLPrim_ParticipantClass
  | SLPrim_View
  | SLPrim_Foldable
  | SLPrim_is
  | SLPrim_remote
  | SLPrim_remotef SrcLoc DLArg String SLTypeFun (Maybe SLVal) (Maybe (Either SLVal SLVal)) (Maybe RemoteFunMode)
  | SLPrim_balance
  | SLPrim_Token_supply
  | SLPrim_viewis SrcLoc SLPart SLVar SLType
  | SLPrim_deploy
  | SLPrim_setOptions
  | SLPrim_adaptReachAppTupleArgs
  | SLPrim_padTo Integer
  | SLPrim_Token_new
  | SLPrim_Token_burn
  | SLPrim_Token_destroy
  | SLPrim_Token_destroyed
  deriving (Eq, Generic)

instance Equiv SLPrimitive where
  equiv a b = case (a,b) of
    -- Ignore every SLPrim except ops
    (SLPrim_op p1, SLPrim_op p2) -> equiv p1 p2
    _ -> False

type SLSVal = (SecurityLevel, SLVal)

data SLSSVal = SLSSVal
  { sss_at :: SrcLoc
  , sss_level :: SecurityLevel
  , sss_val :: SLVal
  }
  deriving (Eq, Generic)

instance Pretty SLSSVal where
  pretty (SLSSVal _ level val) = pretty (level, val) -- <> "@" <> pretty at
  -- TODO: incorporate srcloc in pretty?

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
