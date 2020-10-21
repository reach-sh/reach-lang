module Reach.Sanitize (sani) where

import Reach.AST

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
      DLA_Array t as -> DLA_Array t (sani as)
      DLA_Tuple as -> DLA_Tuple (sani as)
      DLA_Obj m -> DLA_Obj (sani m)
      DLA_Data t v va -> DLA_Data t v (sani va)
      DLA_Interact {} -> a

instance Sanitize DLExpr where
  sani = \case
    DLE_Arg _ a -> DLE_Arg sb $ sani a
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

instance Sanitize DLAssignment where
  sani (DLAssignment m) = DLAssignment $ sani m

instance {-# OVERLAPPING #-} Sanitize a => Sanitize (PLCommon a) where
  sani = \case
    PL_Return _ -> PL_Return sb
    PL_Let _ lc x e k -> PL_Let sb lc x (sani e) (sani k)
    PL_ArrayMap _ a b c d e f ->
      PL_ArrayMap sb a (sani b) c (sani d) (sani e) (sani f)
    PL_ArrayReduce _ a b c d e f g h ->
      PL_ArrayReduce sb a (sani b) (sani c) d e (sani f) (sani g) (sani h)
    PL_Eff _ e k -> PL_Eff sb (sani e) (sani k)
    PL_Var _ v k -> PL_Var sb v (sani k)
    PL_Set _ v a k -> PL_Set sb v (sani a) (sani k)
    PL_LocalIf _ a b c d -> PL_LocalIf sb (sani a) (sani b) (sani c) (sani d)
    PL_LocalSwitch _ a b k -> PL_LocalSwitch sb a (sani b) (sani k)

instance Sanitize PLTail where
  sani (PLTail m) = PLTail (sani m)

instance Sanitize PLBlock where
  sani (PLBlock _ t a) = PLBlock sb (sani t) (sani a)

instance Sanitize CTail where
  sani = \case
    CT_Com m -> CT_Com (sani m)
    CT_If _ c t f -> CT_If sb (sani c) (sani t) (sani f)
    CT_Switch _ x b -> CT_Switch sb x (sani b)
    CT_From _ vs -> CT_From sb vs
    CT_Jump _ a b c -> CT_Jump sb a b (sani c)
