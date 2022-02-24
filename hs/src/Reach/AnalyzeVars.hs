{-# LANGUAGE MagicHash, UnboxedTuples                 #-}
module Reach.AnalyzeVars
  ( freeVars
  , boundVars
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Reach.AST.DLBase

-- These functions are unusable because they are exponential because they
-- repeat so much work. It would be better to do something like storing the
-- bound variables/etc in a field and then update it.

-- https://reachsh.slack.com/archives/C01769UAGTZ/p1642849017019300

import System.IO.Unsafe
import Data.IORef
import Reach.Texty

type MMap a = IORef (M.Map String a)

bvMap :: MMap (S.Set DLVar)
bvMap = unsafePerformIO $ newIORef $ mempty
{-# NOINLINE bvMap #-}

fvMap :: MMap (S.Set DLVar)
fvMap = unsafePerformIO $ newIORef $ mempty
{-# NOINLINE fvMap #-}

readMMap :: (Pretty a) => MMap b -> (a -> b) -> a -> b
readMMap mr f x = unsafePerformIO $ do
  m <- readIORef mr
  let k = show $ pretty x
  case M.lookup k m of
    Just y -> do
      --putStrLn $ "hit " <> (take 64 $ k)
      return y
    Nothing -> do
      let !y = f x
      putStrLn $ "miss " <> (take 64 $ k)
      modifyIORef mr $ M.insert k y
      return y
{-# NOINLINE readMMap #-}

-- </XXX>

class FreeVars a where
  freeVars :: a -> S.Set DLVar

class BoundVars a where
  boundVars :: a -> S.Set DLVar

instance FreeVars DLVar where
  freeVars = S.singleton
instance BoundVars DLVar where
  boundVars = S.singleton

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = \case
    Nothing -> mempty
    Just a -> freeVars a

instance FreeVars v => FreeVars (M.Map k v) where
  freeVars m = freeVars $ M.elems m

instance (FreeVars a, FreeVars b) => FreeVars (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b

instance (FreeVars a, FreeVars b) => FreeVars (Either a b) where
  freeVars = \case
    Left a -> freeVars a
    Right b -> freeVars b

instance FreeVars DLArg where
  freeVars = \case
    DLA_Var v -> freeVars v
    _ -> mempty

instance FreeVars DLLargeArg where
  freeVars = \case
    DLLA_Array _ a -> freeVars a
    DLLA_Tuple a -> freeVars a
    DLLA_Obj m -> freeVars m
    DLLA_Data _ _ a -> freeVars a
    DLLA_Struct m -> freeVars $ map snd m
    DLLA_Bytes {} -> mempty

instance FreeVars DLPayAmt where
  freeVars (DLPayAmt a b) = freeVars a <> freeVars b

instance FreeVars DLWithBill where
  freeVars (DLWithBill a b) = freeVars [a, b]

instance FreeVars DLTokenNew where
  freeVars (DLTokenNew a b c d e f) = freeVars [a, b, c, d, e] <> freeVars f

instance FreeVars DLExpr where
  freeVars = \case
    DLE_Arg _ a -> freeVars a
    DLE_LArg _ a -> freeVars a
    DLE_Impossible {} -> mempty
    DLE_VerifyMuldiv _ _ _ a _ -> freeVars a
    DLE_PrimOp _ _ a -> freeVars a
    DLE_ArrayRef _ a b -> freeVars [a, b]
    DLE_ArraySet _ a b c -> freeVars [a, b, c]
    DLE_ArrayConcat _ a b -> freeVars [a, b]
    DLE_ArrayZip _ a b -> freeVars [a, b]
    DLE_TupleRef _ a _ -> freeVars a
    DLE_ObjectRef _ a _ -> freeVars a
    DLE_Interact _ _ _ _ _ a -> freeVars a
    DLE_Digest _ a -> freeVars a
    DLE_Claim _ _ _ a _ -> freeVars a
    DLE_Transfer _ a b c -> freeVars [a, b] <> freeVars c
    DLE_TokenInit _ a -> freeVars a
    DLE_CheckPay _ _ a b -> freeVars a <> freeVars b
    DLE_Wait _ a -> freeVars a
    DLE_PartSet _ _ a -> freeVars a
    DLE_MapRef _ _ a -> freeVars a
    DLE_MapSet _ _ a b -> freeVars a <> freeVars b
    DLE_Remote _ _ a _ b c d -> freeVars a <> freeVars b <> freeVars c <> freeVars d
    DLE_TokenNew _ a -> freeVars a
    DLE_TokenBurn _ a b -> freeVars [a, b]
    DLE_TokenDestroy _ a -> freeVars a
    DLE_TimeOrder _ a -> freeVars a
    DLE_GetContract {} -> mempty
    DLE_GetAddress {} -> mempty
    DLE_EmitLog _ _ a -> freeVars a
    DLE_setApiDetails {} -> mempty
    DLE_GetUntrackedFunds _ a b -> freeVars a <> freeVars b
    DLE_FromSome _ a b -> freeVars [a, b]

instance FreeVars DLLetVar where
  freeVars = \case
    DLV_Eff -> mempty
    DLV_Let _ v -> freeVars v
instance BoundVars DLLetVar where
  boundVars = \case
    DLV_Eff -> mempty
    DLV_Let _ v -> boundVars v

instance FreeVars v => FreeVars [v] where
  freeVars = mconcat . map freeVars
instance BoundVars v => BoundVars [v] where
  boundVars = mconcat . map boundVars

instance {-# OVERLAPS #-} FreeVars k => FreeVars (SwitchCases k) where
  freeVars m = mconcat $ map (\(v, _, k) -> S.difference (freeVars k) (boundVars v)) $ M.elems m
instance {-# OVERLAPS #-} BoundVars k => BoundVars (SwitchCases k) where
  boundVars m = mconcat $ map (\(v, _, k) -> (boundVars k) <> (boundVars v)) $ M.elems m

bindsFor :: (FreeVars a, BoundVars a, FreeVars b) => a -> b -> S.Set DLVar
bindsFor o i = S.difference (freeVars i) (boundVars o) <> freeVars o

instance FreeVars DLTail where
  freeVars = \case
    DT_Return {} -> mempty
    DT_Com m t -> bindsFor m t

instance BoundVars DLTail where
  boundVars = \case
    DT_Return {} -> mempty
    DT_Com m t -> boundVars m <> boundVars t

instance FreeVars DLBlock where
  freeVars (DLBlock _ _ t a) = bindsFor t a
instance BoundVars DLBlock where
  boundVars (DLBlock _ _ t _) = boundVars t

instance FreeVars DLStmt where
  freeVars = readMMap fvMap $ \case
    DL_Nop {} -> mempty
    DL_Let _ _ e -> freeVars e
    DL_ArrayMap _ _ x a i f -> freeVars [x] <> bindsFor [a, i] f
    DL_ArrayReduce _ _ x z a b i f -> freeVars [x, z] <> bindsFor [a, b, i] f
    DL_Var {} -> mempty
    DL_Set _ v a -> freeVars v <> freeVars a
    DL_LocalIf _ c t f -> freeVars c <> freeVars [t, f]
    DL_LocalSwitch _ v csm -> freeVars v <> freeVars csm
    DL_Only _ _ t -> freeVars t
    DL_MapReduce _ _ _ _ z a b f -> freeVars z <> bindsFor [a, b] f
    DL_LocalDo _ t -> freeVars t

instance BoundVars DLStmt where
  boundVars = readMMap bvMap $ \case
    DL_Nop {} -> mempty
    DL_Let _ lv _ -> boundVars lv
    DL_ArrayMap _ ans _ a i f -> boundVars [ans, a, i] <> boundVars f
    DL_ArrayReduce _ ans _ _ a b i f -> boundVars [ans, a, b, i] <> boundVars f
    DL_Var _ v -> boundVars v
    DL_Set {} -> mempty
    DL_LocalIf _ _ t f -> boundVars [t, f]
    DL_LocalSwitch _ _ csm -> boundVars csm
    DL_Only _ _ t -> boundVars t
    DL_MapReduce _ _ ans _ _ a b f -> boundVars [ans, a, b] <> boundVars f
    DL_LocalDo _ t -> boundVars t
