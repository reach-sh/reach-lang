module Reach.CollectTypes (cts) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL

class CollectsTypes a where
  cts :: a -> S.Set DLType

instance CollectsTypes Bool where
  cts _ = mempty

instance CollectsTypes B.ByteString where
  cts _ = mempty

instance CollectsTypes a => CollectsTypes [a] where
  cts l = foldMap cts l

instance CollectsTypes a => CollectsTypes (M.Map k a) where
  cts m = foldMap cts m

instance CollectsTypes a => CollectsTypes (Maybe a) where
  cts = \case
    Nothing -> mempty
    Just x -> cts x

instance (CollectsTypes a, CollectsTypes b) => CollectsTypes (a, b) where
  cts (x, y) = cts x <> cts y

instance (CollectsTypes a, CollectsTypes b) => CollectsTypes (Either a b) where
  cts = \case
    Left x -> cts x
    Right x -> cts x

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
        T_Contract -> mempty
        T_Token -> mempty
        T_Array e _ -> cts e
        T_Tuple elems -> cts elems
        T_Object m -> cts m
        T_Data m -> cts m
        T_Struct elems -> cts $ map snd elems

instance CollectsTypes IType where
  cts = \case
    IT_Fun dom rng -> cts dom <> cts rng
    IT_Val v -> cts v
    IT_UDFun rng -> cts rng

instance CollectsTypes InteractEnv where
  cts (InteractEnv m) = cts m

instance CollectsTypes SLParts where
  cts (SLParts {..}) = cts sps_ies

instance CollectsTypes DLVar where
  cts (DLVar _ _ t _) = cts t

instance CollectsTypes DLArg where
  cts a = cts (argTypeOf a)

instance CollectsTypes DLLargeArg where
  cts a = cts (largeArgTypeOf a)

instance CollectsTypes DLTokenNew where
  cts (DLTokenNew {..}) =
    cts dtn_name
    <> cts dtn_sym
    <> cts dtn_url
    <> cts dtn_metadata
    <> cts dtn_supply

instance CollectsTypes DLWithBill where
  cts (DLWithBill y z) = cts y <> cts z

instance CollectsTypes PrimOp where
  cts = \case
    BYTES_ZPAD l -> S.singleton $ T_Bytes l
    _ -> mempty

instance CollectsTypes DLExpr where
  cts = \case
    DLE_Arg _ a -> cts a
    DLE_LArg _ la -> cts $ largeArgTypeOf la
    DLE_Impossible {} -> mempty
    DLE_VerifyMuldiv _ _ _ as _ -> cts as
    DLE_PrimOp _ p as -> cts p <> cts as
    DLE_ArrayRef _ a i -> cts a <> cts i
    DLE_ArraySet _ a i v -> cts a <> cts i <> cts v
    DLE_ArrayConcat _ x y -> cts x <> cts y
    DLE_ArrayZip _ x y -> cts x <> cts y
    DLE_TupleRef _ t _ -> cts t
    DLE_ObjectRef _ a _ -> cts a
    DLE_Interact _ _ _ _ t as -> cts t <> cts as
    DLE_Digest _ as -> cts as
    DLE_Claim _ _ _ a _ -> cts a
    DLE_Transfer _ x y z -> cts x <> cts y <> cts z
    DLE_TokenInit _ x -> cts x
    DLE_CheckPay _ _ y z -> cts y <> cts z
    DLE_Wait _ a -> cts a
    DLE_PartSet _ _ a -> cts a
    DLE_MapRef _ _ fa -> cts fa
    DLE_MapSet _ _ fa na -> cts fa <> cts na
    DLE_Remote _ _ av _ pamt as y -> cts (av : as) <> cts pamt <> cts y
    DLE_TokenNew _ tns -> cts tns
    DLE_TokenBurn _ a b -> cts [ a, b ]
    DLE_TokenDestroy _ a -> cts a
    DLE_TimeOrder _ tos -> cts tos
    DLE_GetContract _ -> mempty
    DLE_GetAddress _ -> mempty
    DLE_EmitLog _ _ a -> cts a
    DLE_setApiDetails {} -> mempty
    DLE_GetUntrackedFunds _ mt tb -> cts mt <> cts tb

instance CollectsTypes DLAssignment where
  cts (DLAssignment m) = cts m

instance CollectsTypes DLMapInfo where
  cts = cts . dlmi_tym

instance CollectsTypes DLInit where
  cts (DLInit {..}) = cts dli_maps

instance CollectsTypes DLStmt where
  cts (DL_Nop _) = mempty
  cts (DL_Let _ v e) = cts v <> cts e
  cts (DL_ArrayMap _ ans x a f) = cts ans <> cts x <> cts a <> cts f
  cts (DL_ArrayReduce _ ans x z b a f) = cts ans <> cts x <> cts z <> cts b <> cts a <> cts f
  cts (DL_Var _ v) = cts v
  cts (DL_Set _ v a) = cts v <> cts a
  cts (DL_LocalIf _ a t f) = cts a <> cts t <> cts f
  cts (DL_LocalSwitch _ v csm) = cts v <> cts csm
  cts (DL_Only _ _ b) = cts b
  cts (DL_MapReduce _ _ ans _ z b a f) = cts ans <> cts z <> cts b <> cts a <> cts f
  cts (DL_LocalDo _ t) = cts t

instance CollectsTypes DLTail where
  cts (DT_Return _) = mempty
  cts (DT_Com a k) = cts a <> cts k

instance CollectsTypes DLBlock where
  cts (DLBlock _ _ k a) = cts k <> cts a

instance CollectsTypes LLConsensus where
  cts (LLC_Com m k) = cts m <> cts k
  cts (LLC_If _ c t f) = cts c <> cts t <> cts f
  cts (LLC_Switch _ v csm) = cts v <> cts csm
  cts (LLC_FromConsensus _ _ k) = cts k
  cts (LLC_While _ asn inv cond body k) = cts asn <> cts inv <> cts cond <> cts body <> cts k
  cts (LLC_Continue _ asn) = cts asn
  cts (LLC_ViewIs _ _ _ a k) = cts a <> cts k

instance CollectsTypes DLPayAmt where
  cts (DLPayAmt {..}) = cts pa_net <> cts pa_ks

instance CollectsTypes DLSend where
  cts (DLSend {..}) = cts ds_msg <> cts ds_pay <> cts ds_when

instance CollectsTypes a => CollectsTypes (DLRecv a) where
  cts (DLRecv {..}) = cts dr_from <> cts dr_msg <> cts dr_time <> cts dr_secs <> cts dr_k

instance CollectsTypes LLStep where
  cts (LLS_Com m k) = cts m <> cts k
  cts (LLS_Stop _) = mempty
  cts (LLS_ToConsensus _ lct send recv mtime) = cts lct <> cts send <> cts recv <> cts mtime

instance CollectsTypes a => CollectsTypes (DLinExportBlock a) where
  cts (DLinExportBlock _ vs r) = cts vs <> cts r

instance CollectsTypes Int where
  cts _ = mempty

instance CollectsTypes LLProg where
  cts (LLProg _ _ ps dli dex dvs das devts s) =
    cts ps <> cts dli <> cts dex <> cts dvs <> cts das <> cts devts <> cts s

instance CollectsTypes DLLetVar where
  cts (DLV_Eff) = mempty
  cts (DLV_Let _ x) = cts x

instance CollectsTypes FromInfo where
  cts = \case
    FI_Continue svs -> cts svs
    FI_Halt toks -> cts toks

instance CollectsTypes CTail where
  cts (CT_Com m k) = cts m <> cts k
  cts (CT_If _ ca t f) = cts ca <> cts t <> cts f
  cts (CT_Switch _ v csm) = cts v <> cts csm
  cts (CT_From _ _ msvs) = cts msvs
  cts (CT_Jump _ _ svs asn) = cts svs <> cts asn

instance CollectsTypes a => CollectsTypes (CInterval a) where
  cts (CBetween from to) = cts from <> cts to

instance CollectsTypes DLVarCat where
  cts _ = mempty

instance CollectsTypes CHandler where
  cts (C_Handler _ int fs _ svs msg timev secsv body) = cts int <> cts fs <> cts svs <> cts msg <> cts timev <> cts secsv <> cts body
  cts (C_Loop _ svs vars body) = cts svs <> cts vars <> cts body

instance CollectsTypes CHandlers where
  cts (CHandlers m) = cts m
