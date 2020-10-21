module Reach.CollectTypes (cts) where

import qualified Data.Map as M
import qualified Data.Set as S
import Reach.AST
import Reach.Type

class CollectsTypes a where
  cts :: a -> S.Set SLType

instance CollectsTypes a => CollectsTypes [a] where
  cts l = foldMap cts l

instance CollectsTypes a => CollectsTypes (M.Map k a) where
  cts m = foldMap cts m

instance CollectsTypes a => CollectsTypes (Maybe a) where
  cts (Nothing) = mempty
  cts (Just x) = cts x

instance (CollectsTypes a, CollectsTypes b) => CollectsTypes (a, b) where
  cts (x, y) = cts x <> cts y

instance CollectsTypes SLType where
  cts t =
    S.singleton t
      <> case t of
        T_Null -> mempty
        T_Bool -> mempty
        T_UInt -> mempty
        T_Bytes -> mempty
        T_Digest -> mempty
        T_Address -> mempty
        T_Fun dom rng -> cts dom <> cts rng
        T_Array e _ -> cts e
        T_Tuple elems -> cts elems
        T_Object m -> cts m
        T_Data m -> cts m
        T_Forall _ t' -> cts t'
        T_Var _ -> mempty
        T_Type _ -> mempty

instance CollectsTypes InteractEnv where
  cts (InteractEnv m) = cts m

instance CollectsTypes SLParts where
  cts (SLParts m) = cts m

instance CollectsTypes DLVar where
  cts (DLVar _ _ t _) = cts t

instance CollectsTypes DLArg where
  cts a = cts (argTypeOf a)

instance CollectsTypes DLExpr where
  cts (DLE_Arg _ a) = cts a
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
  cts (DLE_Transfer _ x y) = cts x <> cts y
  cts (DLE_Wait _ a) = cts a
  cts (DLE_PartSet _ _ a) = cts a

instance CollectsTypes DLAssignment where
  cts (DLAssignment m) = cts m

instance CollectsTypes FromSpec where
  cts (FS_Join v) = cts v
  cts (FS_Again v) = cts v

instance CollectsTypes a => CollectsTypes (LLCommon a) where
  cts (LL_Return _) = mempty
  cts (LL_Let _ v e k) = cts v <> cts e <> cts k
  cts (LL_ArrayMap _ ans x a f k) = cts ans <> cts x <> cts a <> cts f <> cts k
  cts (LL_ArrayReduce _ ans x z b a f k) = cts ans <> cts x <> cts z <> cts b <> cts a <> cts f <> cts k
  cts (LL_Var _ v k) = cts v <> cts k
  cts (LL_Set _ v a k) = cts v <> cts a <> cts k
  cts (LL_LocalIf _ a t f k) = cts a <> cts t <> cts f <> cts k
  cts (LL_LocalSwitch _ v csm k) = cts v <> cts csm <> cts k

instance CollectsTypes LLLocal where
  cts (LLL_Com a) = cts a

instance CollectsTypes LLBlock where
  cts (LLBlock _ _ k a) = cts k <> cts a

instance CollectsTypes LLConsensus where
  cts (LLC_Com k) = cts k
  cts (LLC_If _ c t f) = cts c <> cts t <> cts f
  cts (LLC_Switch _ v csm) = cts v <> cts csm
  cts (LLC_FromConsensus _ _ k) = cts k
  cts (LLC_While _ asn inv cond body k) = cts asn <> cts inv <> cts cond <> cts body <> cts k
  cts (LLC_Continue _ asn) = cts asn

instance CollectsTypes LLStep where
  cts (LLS_Com k) = cts k
  cts (LLS_Stop _) = mempty
  cts (LLS_Only _ _ l s) = cts l <> cts s
  cts (LLS_ToConsensus _ _ fs as msg amt amtv mtime c) =
    cts fs <> cts as <> cts msg <> cts amt <> cts amtv <> cts mtime <> cts c

instance CollectsTypes LLProg where
  cts (LLProg _ _ ps s) = cts ps <> cts s

instance CollectsTypes a => CollectsTypes (PLCommon a) where
  cts (PL_Return _) = mempty
  cts (PL_Let _ _ dv de k) = cts dv <> cts de <> cts k
  cts (PL_ArrayMap _ ans x a f k) = cts ans <> cts x <> cts a <> cts f <> cts k
  cts (PL_ArrayReduce _ ans x z b a f k) = cts ans <> cts x <> cts z <> cts b <> cts a <> cts f <> cts k
  cts (PL_Eff _ de k) = cts de <> cts k
  cts (PL_Var _ dv k) = cts dv <> cts k
  cts (PL_Set _ dv da k) = cts dv <> cts da <> cts k
  cts (PL_LocalIf _ ca t f k) = cts ca <> cts t <> cts f <> cts k
  cts (PL_LocalSwitch _ v csm k) = cts v <> cts csm <> cts k

instance CollectsTypes PLTail where
  cts (PLTail m) = cts m

instance CollectsTypes PLBlock where
  cts (PLBlock _ t a) = cts t <> cts a

instance CollectsTypes CTail where
  cts (CT_Com m) = cts m
  cts (CT_If _ ca t f) = cts ca <> cts t <> cts f
  cts (CT_Switch _ v csm) = cts v <> cts csm
  cts (CT_From _ msvs) = cts msvs
  cts (CT_Jump _ _ svs asn) = cts svs <> cts asn

instance CollectsTypes CInterval where
  cts (CBetween from to) = cts from <> cts to

instance CollectsTypes PLLetCat where
  cts _ = mempty

instance CollectsTypes CHandler where
  cts (C_Handler _ int fs _ svs msg amtv body) = cts int <> cts fs <> cts svs <> cts msg <> cts amtv <> cts body
  cts (C_Loop _ svs vars body) = cts svs <> cts vars <> cts body

instance CollectsTypes CHandlers where
  cts (CHandlers m) = cts m
