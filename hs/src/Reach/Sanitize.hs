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

sb :: SrcLoc
sb = srcloc_builtin

instance (Functor f, Sanitize a) => Sanitize (f a) where
  sani = fmap sani

instance Sanitize DLVar where
  sani = id

instance Sanitize DLLiteral where
  sani l =
    case l of
      DLL_Null -> l
      DLL_Bool {} -> l
      DLL_Int _ i -> DLL_Int sb i
      DLL_Bytes {} -> l

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
      where
        go (k, v) = (,) k (sani v)

instance Sanitize DLExpr where
  sani = \case
    DLE_Arg _ a -> DLE_Arg sb $ sani a
    DLE_LArg _ a -> DLE_LArg sb $ sani a
    DLE_Impossible _ m -> DLE_Impossible sb m
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
    DLE_MapDel _ mv fa -> DLE_MapDel sb mv (sani fa)
    DLE_Remote _ fs av m amta as wbill -> DLE_Remote sb fs (sani av) m (sani amta) (sani as) wbill
    DLE_ViewIs _ v k a -> DLE_ViewIs sb v k (sani a)

instance Sanitize DLAssignment where
  sani (DLAssignment m) = DLAssignment $ sani m

instance Sanitize DLStmt where
  sani = \case
    DL_Nop _ -> DL_Nop sb
    DL_Let _ x e -> DL_Let sb (sani x) (sani e)
    DL_ArrayMap _ a b c d ->
      DL_ArrayMap sb a (sani b) c (sani d)
    DL_ArrayReduce _ a b c d e f ->
      DL_ArrayReduce sb a (sani b) (sani c) d e (sani f)
    DL_Var _ v -> DL_Var sb v
    DL_Set _ v a -> DL_Set sb v (sani a)
    DL_LocalIf _ a b c -> DL_LocalIf sb (sani a) (sani b) (sani c)
    DL_LocalSwitch _ a b -> DL_LocalSwitch sb a (sani b)
    DL_Only _ a b -> DL_Only sb a (sani b)
    DL_MapReduce _ mri a b c d e f ->
      DL_MapReduce sb mri a b (sani c) d e (sani f)

instance Sanitize DLTail where
  sani = \case
    DT_Return _ -> DT_Return sb
    DT_Com c t -> DT_Com (sani c) (sani t)

instance Sanitize DLBlock where
  sani (DLBlock _ _ t a) = DLBlock sb mempty (sani t) (sani a)

instance Sanitize DLLetVar where
  sani x = x

instance Sanitize LLConsensus where
  sani = \case
    LLC_Com m k -> LLC_Com (sani m) (sani k)
    LLC_If _ c t f -> LLC_If sb (sani c) (sani t) (sani f)
    LLC_Switch _ ov csm -> LLC_Switch sb ov (sani csm)
    LLC_While _ asn inv cond body k -> LLC_While sb (sani asn) (sani inv) (sani cond) (sani body) (sani k)
    LLC_Continue _ asn -> LLC_Continue sb (sani asn)
    LLC_FromConsensus _ _ s -> LLC_FromConsensus sb sb (sani s)

instance Sanitize DLPayAmt where
  sani (DLPayAmt {..}) = DLPayAmt pa_net (sani pa_ks)

instance Sanitize DLSend where
  sani (DLSend {..}) = DLSend ds_isClass (sani ds_msg) (sani ds_pay) (sani ds_when)

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (DLRecv a) where
  sani (DLRecv {..}) = DLRecv dr_from dr_msg dr_time (sani dr_k)

instance Sanitize LLStep where
  sani = \case
    LLS_Com m k -> LLS_Com (sani m) (sani k)
    LLS_Stop _ -> LLS_Stop sb
    LLS_ToConsensus _ send recv mtime -> LLS_ToConsensus sb (sani send) (sani recv) (sani mtime)

instance Sanitize FromInfo where
  sani = \case
    FI_Continue svs -> FI_Continue $ sani svs
    FI_Halt toks -> FI_Halt $ sani toks

instance Sanitize ViewSave where
  sani = \case
    ViewSave i svs -> ViewSave i $ sani svs

instance Sanitize CTail where
  sani = \case
    CT_Com m k -> CT_Com (sani m) (sani k)
    CT_If _ c t f -> CT_If sb (sani c) (sani t) (sani f)
    CT_Switch _ x b -> CT_Switch sb x (sani b)
    CT_From _ w vi vs -> CT_From sb w (sani vi) (sani vs)
    CT_Jump _ a b c -> CT_Jump sb a b (sani c)
