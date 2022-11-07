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
import Reach.Connector
import Reach.JSOrphans ()
import Reach.Texty
import Reach.Warning (Deprecation (..))
import Data.IORef

-- SL types are a superset of DL types.
-- We copy/paste constructors instead of using `ST_Val DLType`
--  because some things can exist in SL that do not exist in DL,
--  such as an object whose fields are functions.
data SLType
  = ST_Null
  | ST_Bool
  | ST_UInt UIntTy
  | ST_Bytes Integer
  | ST_BytesDyn
  | ST_StringDyn
  | ST_Digest
  | ST_Address
  | ST_Contract
  | ST_Token
  | ST_Array SLType Integer
  | ST_Tuple [SLType]
  | ST_Object (M.Map SLVar SLType)
  | ST_Data MSrcLoc (M.Map SLVar SLType)
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
  deriving (Eq, Generic, Show)

instance Show SLType where
  show = \case
    ST_Null -> "Null"
    ST_Bool -> "Bool"
    ST_UInt UI_Word  -> "UInt"
    ST_UInt UI_256 -> "UInt256"
    ST_Bytes sz -> "Bytes(" <> show sz <> ")"
    ST_BytesDyn -> "BytesDyn"
    ST_StringDyn -> "StringDyn"
    ST_Digest -> "Digest"
    ST_Address -> "Address"
    ST_Contract -> "Contract"
    ST_Token -> "Token"
    ST_Array ty i -> "Array(" <> show ty <> ", " <> show i <> ")"
    ST_Tuple tys -> "Tuple(" <> showTys tys <> ")"
    ST_Object tyMap -> "Object({" <> showTyMap tyMap <> "})"
    ST_Data _ tyMap -> "Data({" <> showTyMap tyMap <> "})"
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
  ST_UInt t -> pure $ T_UInt t
  ST_Bytes i -> pure $ T_Bytes i
  ST_BytesDyn -> pure $ T_BytesDyn
  ST_StringDyn -> pure $ T_StringDyn
  ST_Digest -> pure T_Digest
  ST_Address -> pure T_Address
  ST_Contract -> pure T_Contract
  ST_Token -> pure T_Token
  ST_Array ty i -> T_Array <$> st2dt ty <*> pure i
  ST_Tuple tys -> T_Tuple <$> traverse st2dt tys
  ST_Object tyMap -> T_Object <$> traverse st2dt tyMap
  ST_Data _ tyMap -> T_Data <$> traverse st2dt tyMap
  ST_Struct tys -> T_Struct <$> traverse (\(k, t) -> (,) k <$> st2dt t) tys
  ST_Fun {} -> Nothing
  ST_UDFun {} -> Nothing
  ST_Type {} -> Nothing
  ST_Refine t _ _ -> st2dt t

dt2st :: DLType -> SLType
dt2st = \case
  T_Null -> ST_Null
  T_Bool -> ST_Bool
  T_UInt t -> ST_UInt t
  T_Bytes i -> ST_Bytes i
  T_BytesDyn -> ST_BytesDyn
  T_StringDyn -> ST_StringDyn
  T_Digest -> ST_Digest
  T_Address -> ST_Address
  T_Contract -> ST_Contract
  T_Token -> ST_Token
  T_Array ty i -> ST_Array (dt2st ty) i
  T_Tuple tys -> ST_Tuple $ map dt2st tys
  T_Object tyMap -> ST_Object $ M.map dt2st tyMap
  T_Data tyMap -> ST_Data Nothing $ M.map dt2st tyMap
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
  | SLV_Int SrcLoc (Maybe UIntTy) Integer
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_BytesDyn SrcLoc B.ByteString
  | SLV_String SrcLoc T.Text
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
  | SLV_ContractCode SrcLoc ConnectorObject
  deriving (Eq, Generic)

class IsDynamic a where
  isDynamic :: a -> Bool

instance IsDynamic ClaimType where
  isDynamic = \case
    CT_Unknowable {} -> True
    _ -> False

instance IsDynamic SLPrimitive where
  isDynamic = \case
    SLPrim_makeEnum -> False
    SLPrim_declassify -> False
    SLPrim_digest -> False
    SLPrim_commit -> False
    SLPrim_committed -> False
    SLPrim_is_type -> False
    SLPrim_type_eq -> False
    SLPrim_typeOf  -> False
    SLPrim_Fun     -> False
    SLPrim_Refine  -> False
    SLPrim_Bytes   -> False
    SLPrim_BytesDynCast -> False
    SLPrim_Data    -> False
    SLPrim_data_match -> False
    SLPrim_Array -> False
    SLPrim_Array_iota -> False
    SLPrim_array -> False
    SLPrim_array_elemType -> False
    SLPrim_array_length -> False
    SLPrim_array_set -> False
    SLPrim_array_concat -> False
    SLPrim_array_map {} -> False
    SLPrim_array_reduce {} -> False
    SLPrim_array_zip -> False
    SLPrim_Struct -> False
    SLPrim_Struct_fromTuple {} -> False
    SLPrim_Struct_fromObject {} -> False
    SLPrim_Struct_toTuple -> False
    SLPrim_Struct_toObject -> False
    SLPrim_Struct_fields -> False
    SLPrim_Tuple -> False
    SLPrim_tuple_includes -> False
    SLPrim_tuple_length -> False
    SLPrim_Data_variant {} -> False
    SLPrim_tuple_set -> False
    SLPrim_Object -> False
    SLPrim_Object_has -> False
    SLPrim_Object_fields -> False
    SLPrim_Object_set -> False
    SLPrim_op {} -> False
    SLPrim_transfer -> False
    SLPrim_isDataVariant -> False
    SLPrim_fromSome -> False
    SLPrim_currentMode -> False
    SLPrim_distinct -> False
    SLPrim_xor -> False
    SLPrim_mod -> False
    SLPrim_toStringDyn -> False
    SLPrim_Bytes_fromHex -> False
    SLPrim_Contract_fromAddress -> False
    SLPrim_App_Delay {} -> False
    SLPrim_exit -> False
    SLPrim_exitted -> False
    SLPrim_forall -> False
    SLPrim_part_set -> False
    SLPrim_race -> False
    SLPrim_Map -> False
    SLPrim_Map_new -> False
    SLPrim_Map_reduce -> False
    SLPrim_Participant -> False
    SLPrim_ParticipantClass -> False
    SLPrim_View -> False
    SLPrim_API -> False
    SLPrim_Foldable -> False
    SLPrim_is -> False
    SLPrim_remote -> False
    SLPrim_balance -> False
    SLPrim_Token_accepted -> False
    SLPrim_Token_supply -> False
    SLPrim_init -> False
    SLPrim_inited -> False
    SLPrim_setOptions -> False
    SLPrim_adaptReachAppTupleArgs -> False
    SLPrim_padTo {} -> False
    SLPrim_Token_new -> False
    SLPrim_Token_burn -> False
    SLPrim_Token_destroy -> False
    SLPrim_Token_destroyed -> False
    SLPrim_Token_track -> False
    SLPrim_didPublish -> False
    SLPrim_unstrict -> False
    SLPrim_polyNeq -> False
    SLPrim_getContract -> False
    SLPrim_getAddress -> False
    SLPrim_getCompanion -> False
    SLPrim_EmitLog -> False
    SLPrim_Event -> False
    SLPrim_verifyMuldiv -> False
    SLPrim_getUntrackedFunds -> False
    SLPrim_castOrTrunc {} -> False
    SLPrim_ContractCode -> False
    SLPrim_Contract_new -> False
    SLPrim_Contract_new_ctor {} -> False
    -- These either involve dynamic values or
    -- are the result of instantiating a prim with app specific values
    SLPrim_claim ct -> isDynamic ct
    SLPrim_PrimDelay _ sp x0 x1 -> isDynamic sp && and (map (isDynamic . snd) $ x0 <> x1)
    SLPrim_localf {} -> True
    SLPrim_transfer_amt_to {} -> True
    SLPrim_part_setted {} -> True
    SLPrim_fluid_read {} -> True
    SLPrim_fluid_read_didPublish {} -> True
    SLPrim_fluid_read_canWait {} -> True
    SLPrim_remotef {} -> True
    SLPrim_viewis {} -> True
    SLPrim_event_is {} -> True

instance IsDynamic SLForm where
  isDynamic = \case
    SLForm_App -> False
    SLForm_each -> False
    SLForm_EachAns {} -> True
    SLForm_Part_Only {} -> True
    SLForm_liftInteract {} -> True
    SLForm_Part_ToConsensus {} -> True
    SLForm_unknowable -> False
    SLForm_fork -> False
    SLForm_fork_partial {} -> True
    SLForm_parallel_reduce -> False
    SLForm_parallel_reduce_partial {} -> True
    SLForm_apiCall -> False
    SLForm_apiCall_partial {} -> True
    SLForm_wait -> False
    SLForm_setApiDetails -> True

instance IsDynamic SLVal where
  isDynamic = \case
    SLV_Null {} -> False
    SLV_Bool {} -> False
    SLV_Int {} -> False
    SLV_Bytes {} -> False
    SLV_BytesDyn {} -> False
    SLV_String {} -> False
    SLV_DLC _ -> False
    SLV_Type {} -> False
    SLV_Connector {} -> False
    SLV_Kwd {} -> False
    SLV_ContractCode {} -> False
    SLV_Clo {} -> False
    SLV_Prim p -> isDynamic p
    SLV_Form f -> isDynamic f
    SLV_DLVar {} -> True
    SLV_Participant {} -> True
    SLV_RaceParticipant {} -> True
    SLV_Anybody -> True
    SLV_Map {} -> True
    SLV_Array _ _ svs -> any isDynamic svs
    SLV_Tuple _ svs -> any isDynamic svs
    SLV_Object _ _ svs -> any (isDynamic . sss_val . snd) $ M.toAscList svs
    SLV_Struct _ svs -> any (isDynamic . snd) svs
    SLV_Data _ _ _ sv -> isDynamic sv
    SLV_Deprecated _ sv -> isDynamic sv

uintTyM :: SLVal -> Maybe UIntTy
uintTyM = \case
  SLV_Int _ mt _ -> mt
  SLV_DLVar (DLVar _ _ (T_UInt t) _) -> Just t
  SLV_DLC (DLC_UInt_max) -> Just UI_Word
  _ -> Nothing

mtJoin :: Maybe UIntTy -> Maybe UIntTy -> Maybe UIntTy
mtJoin = \case
  Nothing -> id
  x@(Just _) -> const x

class NewerVar a where
  containsVarNewerThan :: Int -> a -> Bool

instance (NewerVar a) => NewerVar [a] where
  containsVarNewerThan ret = all (containsVarNewerThan ret)

instance NewerVar SLVal where
  containsVarNewerThan ret c = case c of
    (SLV_DLVar (DLVar _ _ _ i)) -> ret < i
    (SLV_Participant _ _ _ (Just (DLVar _ _ _ i))) -> ret < i
    (SLV_Array _ _ vs) -> containsVarNewerThan ret vs
    (SLV_Tuple _ vs) -> containsVarNewerThan ret vs
    (SLV_Struct _ vs) -> containsVarNewerThan ret $ map snd vs
    (SLV_Data _ _ _ v) -> containsVarNewerThan ret v
    (SLV_Deprecated _ v) -> containsVarNewerThan ret v
    _ -> False

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

instance Equiv UIntTy where
  equiv = (==)

instance (Equiv a, Equiv b) => Equiv (Either a b) where
  equiv x y = case (x, y) of
    (Left a, Left b) -> equiv a b
    (Right a, Right b) -> equiv a b
    _ -> False

instance Equiv SPrimOp where
  equiv = (==)

instance Equiv PrimOp where
  equiv = (==)

instance Equiv SecurityLevel where
  equiv = (==)

instance Equiv FluidVar where
  equiv = (==)

instance Equiv DLArg where
  equiv a b = case (a, b) of
    (DLA_Var v1, DLA_Var v2) -> equiv v1 v2
    (DLA_Constant c1, DLA_Constant c2) -> equiv c1 c2
    (DLA_Literal lit1, DLA_Literal lit2) -> equiv lit1 lit2
    (DLA_Interact part1 s1 _, DLA_Interact part2 s2 _) -> equiv part1 part2 && equiv s1 s2
    _ -> False

instance Equiv DLLiteral where
  equiv a b = case (a, b) of
    (DLL_Null, DLL_Null) -> True
    (DLL_Bool b1, DLL_Bool b2) -> equiv b1 b2
    (DLL_Int _ a1 b1, DLL_Int _ a2 b2) -> equiv a1 a2 && equiv b1 b2
    (DLL_TokenZero, DLL_TokenZero) -> True
    _ -> False

instance Equiv SLTypeFun where
  equiv (SLTypeFun {stf_pre = pre1, stf_post = post1}) (SLTypeFun {stf_pre = pre2, stf_post = post2}) =
    equiv pre1 pre2 && equiv post1 post2

instance Equiv SLSSVal where
  equiv (SLSSVal {sss_val = v1}) (SLSSVal {sss_val = v2}) = equiv v1 v2

instance Equiv DLMVar where
  equiv (DLMVar a) (DLMVar b) = equiv a b

instance (Equiv a) => Equiv [a] where
  equiv xs ys = length xs == length ys && all (uncurry equiv) (zip xs ys)

instance (Equiv b, Equiv a) => Equiv (M.Map a b) where
  equiv m1 m2 = equiv (M.toAscList m1) (M.toAscList m2)

instance (Equiv a, Equiv b) => Equiv (a, b) where
  equiv (a, b) (a2, b2) = equiv a a2 && equiv b b2

instance Equiv DLType where
  -- this is safe to use Eq with
  equiv = (==)

instance Equiv ClaimType where
  equiv a b = case (a, b) of
    (CT_Assert, CT_Assert) -> True
    (CT_Enforce, CT_Enforce) -> True
    (CT_Assume, CT_Assume) -> True
    (CT_Require, CT_Require) -> True
    (CT_Possible, CT_Possible) -> True
    (CT_Unknowable part1 args1, CT_Unknowable part2 args2) -> equiv args1 args2 && equiv part1 part2
    _ -> False

instance Equiv Deprecation where
  equiv a b = case (a, b) of
    (D_ParticipantTuples, D_ParticipantTuples) -> True
    (D_SnakeToCamelCase _, D_SnakeToCamelCase _) -> True
    (D_ReachAppArgs, D_ReachAppArgs) -> True
    (D_UntypedTimeArg, D_UntypedTimeArg) -> True
    _ -> False

instance Equiv SLForm where
  equiv a b = case (a, b) of
    (SLForm_App, SLForm_App) -> True
    (SLForm_each, SLForm_each) -> True
    ((SLForm_EachAns parts _ _ _), (SLForm_EachAns parts2 _ _ _)) -> equiv parts parts2
    ((SLForm_Part_Only p _), (SLForm_Part_Only p2 _)) -> equiv p p2
    ((SLForm_liftInteract p _ _), (SLForm_liftInteract p2 _ _)) -> equiv p p2
    (SLForm_unknowable, SLForm_unknowable) -> True
    (SLForm_fork, SLForm_fork) -> True
    (SLForm_parallel_reduce, SLForm_parallel_reduce) -> True
    (SLForm_wait, SLForm_wait) -> True
    -- these partials are left unchecked
    _ -> False

instance Equiv SLType where
  equiv a b = case (a, b) of
    (ST_Null, ST_Null) -> True
    (ST_Bool, ST_Bool) -> True
    (ST_UInt t1, ST_UInt t2) -> equiv t1 t2
    (ST_Digest, ST_Digest) -> True
    (ST_Address, ST_Address) -> True
    (ST_Contract, ST_Contract) -> True
    (ST_Token, ST_Token) -> True
    ((ST_Bytes i), (ST_Bytes i2)) -> equiv i i2
    ((ST_Array s1 _len1), (ST_Array s2 _len2)) -> equiv s1 s2
    ((ST_Tuple xs), (ST_Tuple ys)) -> equiv xs ys
    ((ST_Object m), (ST_Object m2)) -> equiv m m2
    ((ST_Data _ m), (ST_Data _ m2)) -> equiv m m2
    ((ST_Struct s), (ST_Struct s2)) -> equiv s s2
    ((ST_Fun f), (ST_Fun f2)) -> equiv f f2
    ((ST_UDFun f), (ST_UDFun f2)) -> equiv f f2
    ((ST_Type s1), (ST_Type s2)) -> equiv s1 s2
    ((ST_Refine _sLType v _m), (ST_Refine _sLType2 v2 _m2)) -> equiv v v2
    _ -> False

instance Equiv DLVar where
  equiv (DLVar _ _ _dl i1) (DLVar _ _ _dl2 i2) = equiv i1 i2

instance Equiv DLConstant where
  equiv a b = case (a, b) of
    (DLC_UInt_max, DLC_UInt_max) -> True
    (DLC_Token_zero, DLC_Token_zero) -> True
    _ -> False


instance Equiv SLVal where
  equiv a b = case (a, b) of
    ((SLV_Null _ _), (SLV_Null _ _)) -> True
    ((SLV_Bool _ b1), (SLV_Bool _ b2)) -> equiv b1 b2
    ((SLV_Int _ t1 i1), (SLV_Int _ t2 i2)) -> equiv t1 t2 && equiv i1 i2
    ((SLV_Bytes _ v1), (SLV_Bytes _ v2)) -> equiv v1 v2
    ((SLV_Array _ _ xs), (SLV_Array _ _ ys)) -> equiv xs ys
    ((SLV_Tuple _ v1), (SLV_Tuple _ v2)) -> equiv v1 v2
    ((SLV_Struct _ xs), (SLV_Struct _ ys)) -> equiv xs ys
    ((SLV_DLC d1), (SLV_DLC d2)) -> equiv d1 d2
    ((SLV_DLVar d1), (SLV_DLVar d2)) -> equiv d1 d2
    ((SLV_Connector t1), (SLV_Connector t2)) -> equiv t1 t2
    ((SLV_Prim p1), (SLV_Prim p2)) -> equiv p1 p2
    ((SLV_Kwd k1), (SLV_Kwd k2)) -> equiv k1 k2
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
    SLV_Int _ _ i -> pretty i
    SLV_Bytes _ b -> pretty b
    SLV_BytesDyn _ b -> pretty b
    SLV_String _ t -> pretty t
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
    SLV_ContractCode {} -> "<contractCode>"

instance SrcLocOf SLVal where
  srclocOf = \case
    SLV_Null a _ -> a
    SLV_Bool a _ -> a
    SLV_Int a _ _ -> a
    SLV_Bytes a _ -> a
    SLV_BytesDyn a _ -> a
    SLV_String a _ -> a
    SLV_Array a _ _ -> a
    SLV_Tuple a _ -> a
    SLV_Object a _ _ -> a
    SLV_Struct a _ -> a
    SLV_Clo a _ _ -> a
    SLV_Data a _ _ _ -> a
    SLV_DLC _ -> sb
    SLV_DLVar (DLVar a _ _ _) -> a
    SLV_Type {} -> sb
    SLV_Connector _ -> sb
    SLV_Participant a _ _ _ -> a
    SLV_RaceParticipant a _ -> a
    SLV_Anybody -> sb
    SLV_Prim _ -> sb
    SLV_Form _ -> sb
    SLV_Kwd _ -> sb
    SLV_Map _ -> sb
    SLV_Deprecated _ v -> srclocOf v
    SLV_ContractCode a _ -> a

isSmallLiteralArray :: SLVal -> Bool
isSmallLiteralArray = \case
  SLV_Array _ _ _l ->
    True
    -- Why this number!?
    -- length l <= 2
  _ -> False

newtype SLInterface = SLInterface (M.Map SLVar (SrcLoc, SLType))
  deriving (Eq, Generic)

instance Pretty SLInterface where
  pretty (SLInterface m) = render_obj $ M.map snd m

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
  | TCM_Api
  | TCM_Check
  deriving (Eq, Generic, Show)

data ForkMode
  = FM_Case
  | FM_API
  | FM_API_
  | FM_Timeout
  | FM_ThrowTimeout
  | FM_PaySpec
  deriving (Eq, Generic, Show)

data ParallelReduceMode
  = PRM_Invariant
  | PRM_While
  | PRM_Case
  | PRM_API
  | PRM_API_
  | PRM_Timeout
  | PRM_TimeRemaining
  | PRM_ThrowTimeout
  | PRM_PaySpec
  | PRM_Def
  deriving (Eq, Generic, Show)

data ApiCallMode
  = AC_Pay
  | AC_ThrowTimeout
  | AC_Assume
  | AC_Check
  deriving (Eq, Generic, Show)

data ToConsensusRec = ToConsensusRec
  { slptc_at :: SrcLoc
  , slptc_whos :: S.Set SLPart
  , slptc_mv :: Maybe SLVar
  , slptc_mode :: Maybe ToConsensusMode
  , slptc_msg :: Maybe [SLVar]
  , slptc_amte :: Maybe JSExpression
  , slptc_amt_req :: Maybe JSExpression
  , slptc_whene :: Maybe JSExpression
  , slptc_timeout :: Maybe (SrcLoc, JSExpression, Maybe JSBlock)
  , slptc_fork :: Bool
  , slptc_api :: Bool
  , slptc_check :: Maybe JSExpression
  }
  deriving (Eq, Generic)

data ForkCase = ForkCase
  { fc_at :: SrcLoc
  , fc_who_e :: JSExpression
  , fc_who :: String
  , fc_before :: JSExpression
  , fc_check :: JSExpression
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
  , slpr_minv :: [(JSExpression, Maybe JSExpression)]
  , slpr_mwhile :: Maybe JSExpression
  , slpr_cases :: [(SrcLoc, [JSExpression])]
  , slpr_apis :: [(SrcLoc, [JSExpression])]
  , slpr_api_s :: [(SrcLoc, [JSExpression])]
  , slpr_mtime :: Maybe (ParallelReduceMode, SrcLoc, [JSExpression])
  , slpr_mpay :: Maybe JSExpression
  , slpr_defs :: [JSExpression]
  }
  deriving (Eq, Generic)

data ApiCallRec = ApiCallRec
  { slac_at :: SrcLoc
  , slac_who :: JSExpression
  , slac_mode :: Maybe ApiCallMode
  , slac_mpay :: Maybe JSExpression
  , slac_massume :: Maybe JSExpression
  , slac_mtime :: Maybe (JSExpression, JSExpression)
  , slac_mcheck :: Maybe JSExpression
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
  | SLForm_apiCall
  | SLForm_apiCall_partial ApiCallRec
  | SLForm_wait
  | SLForm_setApiDetails
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

data SPrimOp
  = S_ADD (Maybe PrimVM)
  | S_SUB (Maybe PrimVM)
  | S_MUL (Maybe PrimVM)
  | S_DIV (Maybe PrimVM)
  | S_MOD (Maybe PrimVM)
  | S_PLT
  | S_PLE
  | S_PEQ
  | S_PGE
  | S_PGT
  | S_SQRT
  | S_UCAST UIntTy Bool (Maybe PrimVM)
  | S_IF_THEN_ELSE
  | S_DIGEST_EQ
  | S_ADDRESS_EQ
  | S_TOKEN_EQ
  | S_LSH
  | S_RSH
  | S_BAND
  | S_BIOR
  | S_BXOR
  | S_BYTES_ZPAD Integer
  | S_STRINGDYN_CONCAT
  | S_UINT_TO_STRINGDYN UIntTy
  | S_MUL_DIV (Maybe PrimVM)
  | S_DIGEST_XOR
  | S_BYTES_XOR
  | S_BTOI_LAST8 Bool
  | S_CTC_ADDR_EQ
  deriving (Eq, Generic, Ord, Show)

sprimToPrim :: UIntTy -> UIntTy -> Bool -> SPrimOp -> PrimOp
sprimToPrim dom rng useVerifyArith = \case
  S_ADD mpv -> ADD rng $ getPV mpv
  S_SUB mpv -> SUB rng $ getPV mpv
  S_MUL mpv -> MUL rng $ getPV mpv
  S_DIV mpv -> DIV rng $ getPV mpv
  S_MOD mpv -> MOD rng $ getPV mpv
  S_PLT -> PLT dom
  S_PLE -> PLE dom
  S_PEQ -> PEQ dom
  S_PGE -> PGE dom
  S_PGT -> PGT dom
  S_SQRT -> SQRT dom
  S_UCAST _ trunc mpv -> UCAST dom rng trunc $ getPV mpv
  S_IF_THEN_ELSE -> IF_THEN_ELSE
  S_DIGEST_EQ -> DIGEST_EQ
  S_ADDRESS_EQ -> ADDRESS_EQ
  S_TOKEN_EQ -> TOKEN_EQ
  S_LSH -> LSH
  S_RSH -> RSH
  S_BAND -> BAND rng
  S_BIOR -> BIOR rng
  S_BXOR -> BXOR rng
  S_BYTES_ZPAD n -> BYTES_ZPAD n
  S_MUL_DIV mpv -> MUL_DIV $ getPV mpv
  S_DIGEST_XOR -> DIGEST_XOR
  S_BYTES_XOR -> BYTES_XOR
  S_BTOI_LAST8 b -> BTOI_LAST8 b
  S_CTC_ADDR_EQ -> CTC_ADDR_EQ
  S_STRINGDYN_CONCAT -> STRINGDYN_CONCAT
  S_UINT_TO_STRINGDYN _dom -> UINT_TO_STRINGDYN dom
  where
    getPV = \case
      Just pv -> pv
      Nothing -> if useVerifyArith then PV_Veri else PV_Safe

data RemoteFunMode
  = RFM_Pay
  | RFM_Bill
  | RFM_WithBill
  | RFM_ALGO
  deriving (Eq, Generic, Show)

-- Primitive/special forms
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
  | SLPrim_BytesDynCast
  | SLPrim_Data
  | SLPrim_Data_variant MSrcLoc (M.Map SLVar SLType) SLVar SLType
  | SLPrim_data_match
  | SLPrim_Array
  | SLPrim_Array_iota
  | SLPrim_array
  | SLPrim_array_elemType
  | SLPrim_array_length
  | SLPrim_array_set
  | SLPrim_array_concat
  | SLPrim_array_map Bool
  | SLPrim_array_reduce Bool
  | SLPrim_array_zip
  | SLPrim_Struct
  | SLPrim_Struct_fromTuple [(SLVar, SLType)]
  | SLPrim_Struct_fromObject [(SLVar, SLType)]
  | SLPrim_Struct_toTuple
  | SLPrim_Struct_toObject
  | SLPrim_Struct_fields
  | SLPrim_Tuple
  | SLPrim_tuple_includes
  | SLPrim_tuple_length
  | SLPrim_tuple_set
  | SLPrim_Object
  | SLPrim_Object_has
  | SLPrim_Object_fields
  | SLPrim_Object_set
  | SLPrim_App_Delay SrcLoc JSStatement (SLEnv, Bool) (IORef ConnectorObject) (Maybe SLVar)
  | SLPrim_op SPrimOp
  | SLPrim_transfer
  | SLPrim_transfer_amt_to SLVal
  | SLPrim_exit
  | SLPrim_exitted
  | SLPrim_forall
  | SLPrim_PrimDelay SrcLoc SLPrimitive [SLSVal] [SLSVal]
  | SLPrim_part_set
  | SLPrim_part_setted SrcLoc SLPart DLArg
  | SLPrim_fluid_read FluidVar
  | SLPrim_fluid_read_didPublish FluidVar
  | SLPrim_fluid_read_canWait FluidVar
  | SLPrim_race
  | SLPrim_Map
  | SLPrim_Map_new
  | SLPrim_Map_reduce
  | SLPrim_Participant
  | SLPrim_ParticipantClass
  | SLPrim_View
  | SLPrim_API
  | SLPrim_Foldable
  | SLPrim_is
  | SLPrim_remote
  | SLPrim_remotef SrcLoc DLArg (Maybe String) SLTypeFun (Maybe SLVal) (Maybe (Either SLVal SLVal)) (Maybe DLRemoteALGO) (Maybe RemoteFunMode)
  | SLPrim_balance
  | SLPrim_Token_accepted
  | SLPrim_Token_supply
  | SLPrim_viewis SrcLoc (Maybe SLPart) SLVar SLType
  | SLPrim_init
  | SLPrim_inited
  | SLPrim_setOptions
  | SLPrim_adaptReachAppTupleArgs
  | SLPrim_padTo Integer
  | SLPrim_Token_new
  | SLPrim_Token_burn
  | SLPrim_Token_destroy
  | SLPrim_Token_destroyed
  | SLPrim_Token_track
  | SLPrim_didPublish
  | SLPrim_unstrict
  | SLPrim_polyNeq
  | SLPrim_getContract
  | SLPrim_getAddress
  | SLPrim_getCompanion
  | SLPrim_EmitLog
  | SLPrim_Event
  | SLPrim_event_is (Maybe SLPart) SLVar [SLType]
  | SLPrim_verifyMuldiv
  | SLPrim_getUntrackedFunds
  | SLPrim_isDataVariant
  | SLPrim_fromSome
  | SLPrim_currentMode
  | SLPrim_distinct
  | SLPrim_xor
  | SLPrim_mod
  | SLPrim_castOrTrunc UIntTy
  | SLPrim_ContractCode
  | SLPrim_Contract_new
  | SLPrim_Contract_new_ctor DLContractNews
  | SLPrim_toStringDyn
  | SLPrim_Bytes_fromHex
  | SLPrim_Contract_fromAddress
  deriving (Eq, Generic)

instance Equiv SLPrimitive where
  equiv a b = case (a, b) of
    -- Ignore every SLPrim except ops
    (SLPrim_op p1, SLPrim_op p2) -> equiv p1 p2
    _ -> False

type SLSVal = (SecurityLevel, SLVal)

type Universe = Int

data SLSSVal = SLSSVal
  { sss_at :: SrcLoc
  , sss_level :: SecurityLevel
  , sss_universe :: Universe
  , sss_val :: SLVal
  }
  deriving (Eq, Generic, Show)

instance Pretty SLSSVal where
  pretty (SLSSVal _ level uni val) = pretty (level, uni, val) -- <> "@" <> pretty at
  -- TODO: incorporate srcloc in pretty?

sss_restrict :: SecurityLevel -> SLSSVal -> SLSSVal
sss_restrict lvl1 (SLSSVal at lvl2 uni val) =
  (SLSSVal at (lvl1 <> lvl2) uni val)

sss_sls :: SLSSVal -> SLSVal
sss_sls (SLSSVal _ level _ val) = (level, val)

sls_sss :: SrcLoc -> Universe -> SLSVal -> SLSSVal
sls_sss at uni (level, val) = SLSSVal at level uni val

type SLEnv = M.Map SLVar SLSSVal

mt_env :: SLEnv
mt_env = mempty

m_fromList_public :: SrcLoc -> Universe -> [(SLVar, SLVal)] -> SLEnv
m_fromList_public at uni kvs =
  M.fromList $ map go kvs
  where
    go (k, v) = (k, SLSSVal at Public uni v)
