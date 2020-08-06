module Reach.CollectTypes where

import Reach.NL_AST
import qualified Data.Map as M
import qualified Data.Set as S

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
  cts t = S.singleton t

instance CollectsTypes InteractEnv where
  cts (InteractEnv m) = cts m

instance CollectsTypes SLParts where
  cts (SLParts m) = cts m

instance CollectsTypes DLVar where
  cts (DLVar _ _ t _) = cts t

instance CollectsTypes DLArg where
  cts (DLA_Var v) = cts v
  cts (DLA_Con {}) = mempty
  cts (DLA_Array as) = cts as
  cts (DLA_Obj m) = cts m
  cts (DLA_Interact _ t) = cts t

instance CollectsTypes DLExpr where
  cts (DLE_PrimOp _ _ as) = cts as
  cts (DLE_ArrayRef _ a i) = cts a <> cts i
  cts (DLE_Interact _ _ as) = cts as
  cts (DLE_Digest _ as) = cts as

instance CollectsTypes DLAssignment where
  cts (DLAssignment m) = cts m

instance CollectsTypes FromSpec where
  cts (FS_Join v) = cts v
  cts (FS_Again v) = cts v

instance CollectsTypes a => CollectsTypes (LLCommon a) where
  cts (LL_Return _) = mempty
  cts (LL_Let _ v e k) = cts v <> cts e <> cts k
  cts (LL_Var _ v k) = cts v <> cts k
  cts (LL_Set _ v a k) = cts v <> cts a <> cts k
  cts (LL_Claim _ _ _ a k) = cts a <> cts k
  cts (LL_LocalIf _ a t f k) = cts a <> cts t <> cts f <> cts k

instance CollectsTypes LLLocal where
  cts (LLL_Com a) = cts a

instance CollectsTypes a => CollectsTypes (LLBlock a) where
  cts (LLBlock _ k a) = cts k <> cts a

instance CollectsTypes LLConsensus where
  cts (LLC_Com k) = cts k
  cts (LLC_If _ c t f) = cts c <> cts t <> cts f
  cts (LLC_Transfer _ to amt k) = cts to <> cts amt <> cts k
  cts (LLC_FromConsensus _ _ k) = cts k
  cts (LLC_While _ asn inv cond body k) = cts asn <> cts inv <> cts cond <> cts body <> cts k
  cts (LLC_Continue _ asn) = cts asn

instance CollectsTypes LLStep where
  cts (LLS_Com k) = cts k
  cts (LLS_Stop _ a) = cts a
  cts (LLS_Only _ _ l s) = cts l <> cts s
  cts (LLS_ToConsensus _ _ fs as msg amt mtime c) =
    cts fs <> cts as <> cts msg <> cts amt <> cts mtime <> cts c

instance CollectsTypes LLProg where
  cts (LLProg _ ps s) = cts ps <> cts s
