module Reach.Sanitize
  ( sani
  , Sanitize
  )
where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL

class Sanitize a where
  sani :: a -> a

instance (Functor f, Sanitize a) => Sanitize (f a) where
  sani = fmap sani

instance Sanitize DLVar where
  sani = id

instance Sanitize Bool where
  sani = id

instance Sanitize DLLiteral where
  sani l =
    case l of
      DLL_Null -> l
      DLL_Bool {} -> l
      DLL_Int _ i -> DLL_Int sb i

instance Sanitize DLArg where
  sani a =
    case a of
      DLA_Var {} -> a
      DLA_Constant {} -> a
      DLA_Literal l -> DLA_Literal $ sani l
      DLA_Interact {} -> a

instance Sanitize DLLargeArg where
  sani = \case
    DLLA_Array t as -> DLLA_Array t (sani as)
    DLLA_Tuple as -> DLLA_Tuple (sani as)
    DLLA_Obj m -> DLLA_Obj (sani m)
    DLLA_Data t v va -> DLLA_Data t v (sani va)
    DLLA_Struct kvs -> DLLA_Struct $ map go kvs
    DLLA_Bytes b -> DLLA_Bytes b
    where
      go (k, v) = (,) k (sani v)

instance Sanitize DLTokenNew where
  sani (DLTokenNew {..}) = DLTokenNew (sani dtn_name) (sani dtn_sym) (sani dtn_url) (sani dtn_metadata) (sani dtn_supply) (sani dtn_decimals)

instance Sanitize DLWithBill where
  sani (DLWithBill y z) = DLWithBill (sani y) (sani z)

instance Sanitize DLExpr where
  sani = \case
    DLE_Arg _ a -> DLE_Arg sb $ sani a
    DLE_LArg _ a -> DLE_LArg sb $ sani a
    DLE_Impossible _ t m -> DLE_Impossible sb t m
    DLE_VerifyMuldiv _ f cl as err -> DLE_VerifyMuldiv sb f cl (sani as) err
    DLE_PrimOp _ f as -> DLE_PrimOp sb f (sani as)
    DLE_ArrayRef _ a i -> DLE_ArrayRef sb (sani a) (sani i)
    DLE_ArraySet _ a i v -> DLE_ArraySet sb (sani a) (sani i) (sani v)
    DLE_ArrayConcat _ x y -> DLE_ArrayConcat sb (sani x) (sani y)
    DLE_ArrayZip _ x y -> DLE_ArrayZip sb (sani x) (sani y)
    DLE_TupleRef _ a i -> DLE_TupleRef sb (sani a) i
    DLE_ObjectRef _ a f -> DLE_ObjectRef sb (sani a) f
    DLE_Interact _ fs p m t as -> DLE_Interact sb fs p m t (sani as)
    DLE_Digest _ as -> DLE_Digest sb (sani as)
    DLE_Claim _ fs ct a mm -> DLE_Claim sb fs ct (sani a) mm
    DLE_Transfer _ x y z -> DLE_Transfer sb (sani x) (sani y) (sani z)
    DLE_TokenInit _ x -> DLE_TokenInit sb (sani x)
    DLE_CheckPay _ x y z -> DLE_CheckPay sb x (sani y) (sani z)
    DLE_Wait _ x -> DLE_Wait sb (sani x)
    DLE_PartSet _ p x -> DLE_PartSet sb p (sani x)
    DLE_MapRef _ mv fa -> DLE_MapRef sb mv (sani fa)
    DLE_MapSet _ mv fa na -> DLE_MapSet sb mv (sani fa) (sani na)
    DLE_Remote _ fs av m amta as wbill -> DLE_Remote sb fs (sani av) m (sani amta) (sani as) (sani wbill)
    DLE_TokenNew _ tns -> DLE_TokenNew sb (sani tns)
    DLE_TokenBurn _ tok amt -> DLE_TokenBurn sb (sani tok) (sani amt)
    DLE_TokenDestroy _ tok -> DLE_TokenDestroy sb (sani tok)
    DLE_TimeOrder _ tos -> DLE_TimeOrder sb (sani tos)
    DLE_GetContract _ -> DLE_GetContract sb
    DLE_GetAddress _ -> DLE_GetAddress sb
    DLE_EmitLog _ k a -> DLE_EmitLog sb k (sani a)
    DLE_setApiDetails _ w d c f -> DLE_setApiDetails sb w d c f
    DLE_GetUntrackedFunds _ mt tb -> DLE_GetUntrackedFunds sb (sani mt) (sani tb)
    DLE_FromSome _ mo da -> DLE_FromSome sb (sani mo) (sani da)
    DLE_BalanceInit i v -> DLE_BalanceInit i (sani v)

instance Sanitize DLAssignment where
  sani (DLAssignment m) = DLAssignment $ sani m

instance Sanitize DLStmt where
  sani = \case
    DL_Nop _ -> DL_Nop sb
    DL_Let _ x e -> DL_Let sb (sani x) (sani e)
    DL_ArrayMap _ a b c d e ->
      DL_ArrayMap sb a (sani b) c d (sani e)
    DL_ArrayReduce _ a b c d e f g ->
      DL_ArrayReduce sb a (sani b) (sani c) d e f (sani g)
    DL_Var _ v -> DL_Var sb v
    DL_Set _ v a -> DL_Set sb v (sani a)
    DL_LocalIf _ a b c -> DL_LocalIf sb (sani a) (sani b) (sani c)
    DL_LocalSwitch _ a b -> DL_LocalSwitch sb a (sani b)
    DL_Only _ a b -> DL_Only sb a (sani b)
    DL_MapReduce _ mri a b c d e f ->
      DL_MapReduce sb mri a b (sani c) d e (sani f)
    DL_LocalDo _ t -> DL_LocalDo sb (sani t)

instance Sanitize DLTail where
  sani = \case
    DT_Return _ -> DT_Return sb
    DT_Com c t -> DT_Com (sani c) (sani t)

instance Sanitize DLBlock where
  sani (DLBlock _ _ t a) = DLBlock sb mempty (sani t) (sani a)

instance Sanitize DLLetVar where
  sani x = x

instance {-# OVERLAPS #-} Sanitize a => Sanitize (DLinExportBlock a) where
  sani = \case
    DLinExportBlock _ vs b ->
      DLinExportBlock sb vs (sani b)

instance Sanitize LLConsensus where
  sani = \case
    LLC_Com m k -> LLC_Com (sani m) (sani k)
    LLC_If _ c t f -> LLC_If sb (sani c) (sani t) (sani f)
    LLC_Switch _ ov csm -> LLC_Switch sb ov (sani csm)
    LLC_While _ asn inv cond body k -> LLC_While sb (sani asn) (sani inv) (sani cond) (sani body) (sani k)
    LLC_Continue _ asn -> LLC_Continue sb (sani asn)
    LLC_FromConsensus _ _ fs s -> LLC_FromConsensus sb sb fs (sani s)
    LLC_ViewIs _ vn vk a k -> LLC_ViewIs sb vn vk (sani a) (sani k)

instance Sanitize DLPayAmt where
  sani (DLPayAmt {..}) = DLPayAmt pa_net (sani pa_ks)

instance Sanitize DLSend where
  sani (DLSend {..}) = DLSend ds_isClass (sani ds_msg) (sani ds_pay) (sani ds_when)

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (DLRecv a) where
  sani (DLRecv {..}) = DLRecv dr_from dr_msg dr_time dr_secs dr_didSend (sani dr_k)

instance Sanitize LLStep where
  sani = \case
    LLS_Com m k -> LLS_Com (sani m) (sani k)
    LLS_Stop _ -> LLS_Stop sb
    LLS_ToConsensus _ lct send recv mtime -> LLS_ToConsensus sb (sani lct) (sani send) (sani recv) (sani mtime)

instance Sanitize FromInfo where
  sani = \case
    FI_Continue svs -> FI_Continue $ sani svs
    FI_Halt toks -> FI_Halt $ sani toks

instance {-# OVERLAPPING #-} (Sanitize a, Sanitize b, Sanitize c, Sanitize d, Sanitize e) => Sanitize (a, b, c, d, e) where
  sani (a, b, c, d, e) = (sani a, sani b, sani c, sani d, sani e)

instance Sanitize ETail where
  sani = \case
    ET_Com m k -> ET_Com (sani m) (sani k)
    ET_Stop _ -> ET_Stop sb
    ET_If _ c t f -> ET_If sb (sani c) (sani t) (sani f)
    ET_Switch _ x b -> ET_Switch sb x (sani b)
    ET_FromConsensus _ x y z -> ET_FromConsensus sb x (sani y) (sani z)
    ET_ToConsensus _ from prev lct which me msg out timev secsv didSendv mtime cons ->
      ET_ToConsensus sb from prev (sani lct) which (sani me) msg out timev secsv didSendv (sani mtime) (sani cons)
    ET_While _ asn cond body k -> ET_While sb (sani asn) (sani cond) (sani body) (sani k)
    ET_Continue _ asn -> ET_Continue sb (sani asn)

instance Sanitize CTail where
  sani = \case
    CT_Com m k -> CT_Com (sani m) (sani k)
    CT_If _ c t f -> CT_If sb (sani c) (sani t) (sani f)
    CT_Switch _ x b -> CT_Switch sb x (sani b)
    CT_From _ w vs -> CT_From sb w (sani vs)
    CT_Jump _ a b c -> CT_Jump sb a b (sani c)
