module Reach.CollectCounts
  ( Count (..)
  , Counts (..)
  , Countable
  , counts
  , countsl
  , countsS
  , get_count
  , count_rms
  , count_rmm
  , counts_nzs
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.PL

newtype Count = Count (Maybe DLVarCat)
  deriving (Show, Eq)
  deriving newtype (Semigroup)

newtype Counts = Counts (M.Map DLVar DLVarCat)
  deriving (Show, Eq)

instance Monoid Counts where
  mempty = Counts mempty

instance Semigroup Counts where
  (Counts m1) <> (Counts m2) = Counts m3
    where
      m3 = M.unionWith (<>) m1 m2

get_count :: DLVar -> Counts -> Count
get_count v (Counts m) = Count $ M.lookup v m

count_rms :: [DLVar] -> Counts -> Counts
count_rms vs (Counts cs) = Counts $ foldr M.delete cs vs

count_rmm :: Maybe DLVar -> Counts -> Counts
count_rmm = count_rms . maybeToList

counts_nzs :: Counts -> [DLVar]
counts_nzs (Counts cs) = M.keys cs

countsl :: Countable a => a -> [DLVar]
countsl = counts_nzs . counts

countsS :: Countable a => a -> S.Set DLVar
countsS = S.fromList . countsl

class Countable a where
  counts :: a -> Counts

instance Countable Bool where
  counts = const mempty

instance (Countable x, Countable y) => Countable (x, y) where
  counts (a, b) = counts a <> counts b

instance (Countable x, Countable y, Countable z) => Countable (x, y, z) where
  counts (a, b, c) = counts a <> counts b <> counts c

instance (Countable x, Countable y, Countable z, Countable a) => Countable (x, y, z, a) where
  counts (a, b, c, d) = counts a <> counts b <> counts c <> counts d

instance (Countable x, Countable y, Countable z, Countable a, Countable b) => Countable (x, y, z, a, b) where
  counts (a, b, c, d, e) = counts a <> counts b <> counts c <> counts d <> counts e

instance Countable v => Countable (Maybe v) where
  counts Nothing = mempty
  counts (Just x) = counts x

instance Countable v => Countable [v] where
  counts l = mconcat $ map counts l

instance Countable v => Countable (M.Map k v) where
  counts m = counts $ M.elems m

instance Countable DLVar where
  counts dv = Counts $ M.singleton dv DVC_Once

instance Countable DLLetVar where
  counts = \case
    DLV_Eff -> mempty
    DLV_Let _ v -> counts v

instance Countable DLArg where
  counts = \case
    DLA_Var v -> counts v
    DLA_Constant {} -> mempty
    DLA_Literal {} -> mempty
    DLA_Interact {} -> mempty

instance Countable DLLargeArg where
  counts = \case
    DLLA_Array _ as -> counts as
    DLLA_Tuple as -> counts as
    DLLA_Obj as -> counts as
    DLLA_Data _ _ v -> counts v
    DLLA_Struct kvs -> counts $ map snd kvs

instance Countable DLExpr where
  counts = \case
    DLE_Arg _ a -> counts a
    DLE_LArg _ a -> counts a
    DLE_Impossible _ _ -> mempty
    DLE_PrimOp _ _ as -> counts as
    DLE_ArrayRef _ aa ea -> counts [aa, ea]
    DLE_ArraySet _ aa ia va -> counts [aa, ia, va]
    DLE_ArrayConcat _ x y -> counts x <> counts y
    DLE_ArrayZip _ x y -> counts x <> counts y
    DLE_TupleRef _ t _ -> counts t
    DLE_ObjectRef _ aa _ -> counts aa
    DLE_Interact _ _ _ _ _ as -> counts as
    DLE_Digest _ as -> counts as
    DLE_Claim _ _ _ a _ -> counts a
    DLE_Transfer _ x y z -> counts [x, y] <> counts z
    DLE_TokenInit _ x -> counts x
    DLE_CheckPay _ _ y z -> counts y <> counts z
    DLE_Wait _ a -> counts a
    DLE_PartSet _ _ a -> counts a
    DLE_MapRef _ _ fa -> counts fa
    DLE_MapSet _ _ fa na -> counts [fa, na]
    DLE_MapDel _ _ fa -> counts fa
    DLE_Remote _ _ av _ pamt as _ -> counts (av : as) <> counts pamt

instance Countable DLAssignment where
  counts (DLAssignment m) = counts m

instance Countable a => Countable (CInterval a) where
  counts (CBetween from to) = counts from <> counts to

instance Countable DLPayAmt where
  counts (DLPayAmt {..}) = counts pa_net <> counts pa_ks

instance Countable DLSend where
  counts (DLSend {..}) = counts ds_msg <> counts ds_pay <> counts ds_when

instance Countable FromInfo where
  counts = \case
    FI_Continue vis svs -> counts vis <> counts svs
    FI_Halt toks -> counts toks

instance Countable ViewSave where
  counts = \case
    ViewSave _ svs -> counts svs

instance {-# OVERLAPS #-} Countable a => Countable (DLinExportBlock a) where
  counts = \case
    DLinExportBlock _ vs b ->
      count_rms (countsl vs) $ counts b

instance Countable DLBlock where
  counts = \case
    DLBlock _ _ t a -> countsk (counts a) t

class CountableK a where
  countsk :: Counts -> a -> Counts

instance CountableK DLTail where
  countsk kcs = \case
    DT_Return {} -> kcs
    DT_Com m t -> countsk (countsk kcs t) m

instance Countable DLTail where
  counts = countsk mempty

instance CountableK DLLetVar where
  countsk kcs = \case
    DLV_Eff -> kcs
    DLV_Let _ v -> count_rms [v] kcs

instance CountableK DLStmt where
  countsk kcs = \case
    DL_Nop {} -> kcs
    DL_Let _ lv e -> countsk (counts e <> kcs) lv
    DL_ArrayMap _ ans x a f ->
      count_rms [ans, a] (counts f <> kcs) <> counts x
    DL_ArrayReduce _ ans x z b a f ->
      count_rms [ans, b, a] (counts f <> kcs) <> counts [x, z]
    DL_Var _ v -> count_rms [v] kcs
    DL_Set _ v a -> counts v <> counts a <> kcs
    DL_LocalIf _ c t f -> counts c <> counts [t, f] <> kcs
    DL_LocalSwitch _ v csm -> counts v <> counts csm <> kcs
    DL_Only _ _ t -> counts t <> kcs
    DL_MapReduce _ _ ans _ z b a f ->
      count_rms [ans, b, a] (counts f <> kcs) <> counts z
    DL_LocalDo _ t -> countsk kcs t
