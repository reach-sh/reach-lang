module Reach.CollectTypes (cts) where

import qualified Data.Map as M
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL

class CollectsTypes a where
  cts :: a -> S.Set DLType

instance CollectsTypes Bool where
  cts _ = mempty

instance CollectsTypes a => CollectsTypes [a] where
  cts l = foldMap cts l

instance CollectsTypes a => CollectsTypes (M.Map k a) where
  cts m = foldMap cts m

instance CollectsTypes a => CollectsTypes (Maybe a) where
  cts (Nothing) = mempty
  cts (Just x) = cts x

instance (CollectsTypes a, CollectsTypes b) => CollectsTypes (a, b) where
  cts (x, y) = cts x <> cts y

instance (CollectsTypes a, CollectsTypes b, CollectsTypes c) => CollectsTypes (a, b, c) where
  cts (x, y, z) = cts x <> cts y <> cts z

instance (CollectsTypes a, CollectsTypes b, CollectsTypes c, CollectsTypes d) => CollectsTypes (a, b, c, d) where
  cts (x, y, z, a) = cts x <> cts y <> cts z <> cts a

instance (CollectsTypes a, CollectsTypes b, CollectsTypes c, CollectsTypes d, CollectsTypes e) => CollectsTypes (a, b, c, d, e) where
  cts (x, y, z, a, b) = cts x <> cts y <> cts z <> cts a <> cts b

instance (CollectsTypes a, CollectsTypes b, CollectsTypes c, CollectsTypes d, CollectsTypes e, CollectsTypes f) => CollectsTypes (a, b, c, d, e, f) where
  cts (x, y, z, a, b, c) = cts x <> cts y <> cts z <> cts a <> cts b <> cts c

instance CollectsTypes DLType where
  cts t =
    S.singleton t
      <> case t of
        T_Null -> mempty
        T_Bool -> mempty
        T_UInt -> mempty
        T_Bytes _ -> mempty
        T_Digest -> mempty
        T_Address -> mempty
        T_Token -> mempty
        T_Array e _ -> cts e
        T_Tuple elems -> cts elems
        T_Object m -> cts m
        T_Data m -> cts m
        T_Struct elems -> cts $ map snd elems

instance CollectsTypes IType where
  cts (IT_Fun dom rng) = cts dom <> cts rng
  cts (IT_Val v) = cts v

instance CollectsTypes InteractEnv where
  cts (InteractEnv m) = cts m

instance CollectsTypes SLParts where
  cts (SLParts m) = cts m

instance CollectsTypes DLVar where
  cts (DLVar _ _ t _) = cts t

instance CollectsTypes DLArg where
  cts a = cts (argTypeOf a)

instance CollectsTypes DLLargeArg where
  cts a = cts (largeArgTypeOf a)

instance CollectsTypes DLExpr where
  cts (DLE_Arg _ a) = cts a
  cts (DLE_LArg _ la) = cts $ largeArgTypeOf la
  cts (DLE_Impossible _ _) = mempty
  cts (DLE_PrimOp _ _ as) = cts as
  cts (DLE_ArrayRef _ a i) = cts a <> cts i
  cts (DLE_ArraySet _ a i v) = cts a <> cts i <> cts v
  cts (DLE_ArrayConcat _ x y) = cts x <> cts y
  cts (DLE_ArrayZip _ x y) = cts x <> cts y
  cts (DLE_TupleRef _ t _) = cts t
  cts (DLE_ObjectRef _ a _) = cts a
  cts (DLE_Interact _ _ _ _ t as) = cts t <> cts as
  cts (DLE_Digest _ as) = cts as
  cts (DLE_Claim _ _ _ a _) = cts a
  cts (DLE_Transfer _ x y z) = cts x <> cts y <> cts z
  cts (DLE_TokenInit _ x) = cts x
  cts (DLE_CheckPay _ _ y z) = cts y <> cts z
  cts (DLE_Wait _ a) = cts a
  cts (DLE_PartSet _ _ a) = cts a
  cts (DLE_MapRef _ _ fa) = cts fa
  cts (DLE_MapSet _ _ fa na) = cts [fa, na]
  cts (DLE_MapDel _ _ fa) = cts fa
  cts (DLE_Remote _ _ av _ pamt as _) = cts (av : as) <> cts pamt
  cts (DLE_ViewIs _ _ _ a) = cts $ a

instance CollectsTypes DLAssignment where
  cts (DLAssignment m) = cts m

instance CollectsTypes DLMapInfo where
  cts (DLMapInfo {..}) = cts $ maybeT dlmi_ty

instance CollectsTypes DLInit where
  cts (DLInit {..}) = cts dli_ctimem <> cts dli_maps

instance CollectsTypes a => CollectsTypes (DLinStmt a) where
  cts (DL_Nop _) = mempty
  cts (DL_Let _ v e) = cts v <> cts e
  cts (DL_ArrayMap _ ans x a f) = cts ans <> cts x <> cts a <> cts f
  cts (DL_ArrayReduce _ ans x z b a f) = cts ans <> cts x <> cts z <> cts b <> cts a <> cts f
  cts (DL_Var _ v) = cts v
  cts (DL_Set _ v a) = cts v <> cts a
  cts (DL_LocalIf _ a t f) = cts a <> cts t <> cts f
  cts (DL_LocalSwitch _ v csm) = cts v <> cts csm
  cts (DL_MapReduce _ _ ans _ z b a f) = cts ans <> cts z <> cts b <> cts a <> cts f

instance CollectsTypes a => CollectsTypes (DLinTail a) where
  cts (DT_Return _) = mempty
  cts (DT_Com a k) = cts a <> cts k

instance CollectsTypes a => CollectsTypes (DLinBlock a) where
  cts (DLinBlock _ _ k a) = cts k <> cts a

instance CollectsTypes (DLinExportVal LLBlock) where
  cts = \case
    DLEV_Arg _ a -> cts a
    DLEV_Fun _ a b -> cts a <> cts b

instance CollectsTypes LLConsensus where
  cts (LLC_Com m k) = cts m <> cts k
  cts (LLC_If _ c t f) = cts c <> cts t <> cts f
  cts (LLC_Switch _ v csm) = cts v <> cts csm
  cts (LLC_FromConsensus _ _ k) = cts k
  cts (LLC_While _ asn inv cond body k) = cts asn <> cts inv <> cts cond <> cts body <> cts k
  cts (LLC_Continue _ asn) = cts asn
  cts (LLC_Only _ _ l s) = cts l <> cts s

instance CollectsTypes DLPayAmt where
  cts (DLPayAmt {..}) = cts pa_net <> cts pa_ks

instance CollectsTypes DLSend where
  cts (DLSend {..}) = cts ds_msg <> cts ds_pay <> cts ds_when

instance CollectsTypes a => CollectsTypes (DLRecv a) where
  cts (DLRecv {..}) = cts dr_from <> cts dr_msg <> cts dr_time <> cts dr_k

instance CollectsTypes LLStep where
  cts (LLS_Com m k) = cts m <> cts k
  cts (LLS_Stop _) = mempty
  cts (LLS_Only _ _ l s) = cts l <> cts s
  cts (LLS_ToConsensus _ send recv mtime) = cts send <> cts recv <> cts mtime

instance CollectsTypes (DLExportinBlock LLVar) where
  cts (DLExportinBlock s r) = cts s <> cts r

instance CollectsTypes LLProg where
  cts (LLProg _ _ ps dli dex dvs s) =
    cts ps <> cts dli <> cts dex <> cts dvs <> cts s

instance CollectsTypes PLVar where
  cts (PV_Eff) = mempty
  cts (PV_Let _ x) = cts x

instance CollectsTypes FromInfo where
  cts = \case
    FI_Continue svs -> cts svs
    FI_Halt toks -> cts toks

instance CollectsTypes ViewSave where
  cts = \case
    ViewSave _ svs -> cts svs

instance CollectsTypes CTail where
  cts (CT_Com m k) = cts m <> cts k
  cts (CT_If _ ca t f) = cts ca <> cts t <> cts f
  cts (CT_Switch _ v csm) = cts v <> cts csm
  cts (CT_From _ _ vs msvs) = cts vs <> cts msvs
  cts (CT_Jump _ _ svs asn) = cts svs <> cts asn

instance CollectsTypes a => CollectsTypes (CInterval a) where
  cts (CBetween from to) = cts from <> cts to

instance CollectsTypes PLLetCat where
  cts _ = mempty

instance CollectsTypes CHandler where
  cts (C_Handler _ int last_timev fs _ svs msg timev body) = cts int <> cts last_timev <> cts fs <> cts svs <> cts msg <> cts timev <> cts body
  cts (C_Loop _ svs vars body) = cts svs <> cts vars <> cts body

instance CollectsTypes CHandlers where
  cts (CHandlers m) = cts m
