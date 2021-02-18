module Reach.Sanitize (sani) where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL

class Sanitize a where
  sani :: a -> a

sb :: SrcLoc
sb = srcloc_builtin

instance (Functor f, Sanitize a) => Sanitize (f a) where
  sani = fmap sani

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
    DLE_Transfer _ x y -> DLE_Transfer sb (sani x) (sani y)
    DLE_Wait _ x -> DLE_Wait sb (sani x)
    DLE_PartSet _ p x -> DLE_PartSet sb p (sani x)
    DLE_MapRef _ mv fa -> DLE_MapRef sb mv (sani fa)
    DLE_MapSet _ mv fa na -> DLE_MapSet sb mv (sani fa) (sani na)
    DLE_MapDel _ mv fa -> DLE_MapDel sb mv (sani fa)

instance Sanitize DLAssignment where
  sani (DLAssignment m) = DLAssignment $ sani m

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (DLinStmt a) where
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

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (DLinTail a) where
  sani = \case
    DT_Return _ -> DT_Return sb
    DT_Com c t -> DT_Com (sani c) (sani t)

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (DLinBlock a) where
  sani (DLinBlock _ _ t a) = DLinBlock sb mempty (sani t) (sani a)

instance Sanitize PLVar where
  sani x = x

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (CTail_ a) where
  sani = \case
    CT_Com m k -> CT_Com (sani m) (sani k)
    CT_If _ c t f -> CT_If sb (sani c) (sani t) (sani f)
    CT_Switch _ x b -> CT_Switch sb x (sani b)
    CT_From _ vs -> CT_From sb vs
    CT_Jump _ a b c -> CT_Jump sb a b (sani c)
