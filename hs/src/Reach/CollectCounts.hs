module Reach.CollectCounts (Count (..), Counts (..), counts, get_count, count_rms, counts_nzs) where

import qualified Data.Map.Strict as M
import Reach.AST

data Count
  = C_Never
  | C_Once
  | C_Many
  deriving (Show, Eq)

instance Monoid Count where
  mempty = C_Never

instance Semigroup Count where
  C_Never <> x = x
  x <> C_Never = x
  C_Once <> C_Once = C_Many
  _ <> _ = C_Many

newtype Counts = Counts (M.Map DLVar Count)
  deriving (Show, Eq)

instance Monoid Counts where
  mempty = Counts mempty

instance Semigroup Counts where
  (Counts m1) <> (Counts m2) = Counts m3
    where
      m3 = M.unionWith (<>) m1 m2

get_count :: DLVar -> Counts -> Count
get_count v (Counts m) =
  case M.lookup v m of
    Nothing -> mempty
    Just x -> x

count_rms :: [DLVar] -> Counts -> Counts
count_rms vs (Counts cs) = Counts $ foldr M.delete cs vs

counts_nzs :: Counts -> [DLVar]
counts_nzs (Counts cs) =
  map fst $ filter (\(_, c) -> c /= C_Never) $ M.toList cs

class Countable a where
  counts :: a -> Counts

instance (Countable x, Countable y) => Countable (x, y) where
  counts (a, b) = counts a <> counts b

instance (Countable x, Countable y, Countable z) => Countable (x, y, z) where
  counts (a, b, c) = counts a <> counts b <> counts c

instance Countable v => Countable (Maybe v) where
  counts Nothing = mempty
  counts (Just x) = counts x

instance Countable v => Countable [v] where
  counts l = mconcat $ map counts l

instance Countable v => Countable (M.Map k v) where
  counts m = counts $ M.elems m

instance Countable DLVar where
  counts dv = Counts $ M.singleton dv C_Once

instance Countable DLArg where
  counts a =
    case a of
      DLA_Var v -> counts v
      DLA_Con {} -> mempty
      DLA_Array _ as -> counts as
      DLA_Tuple as -> counts as
      DLA_Obj as -> counts as
      DLA_Interact {} -> mempty

instance Countable DLExpr where
  counts e =
    case e of
      DLE_PrimOp _ _ as -> counts as
      DLE_ArrayRef _ _ aa _ ea -> counts [aa, ea]
      DLE_TupleRef _ t _ -> counts t
      DLE_ObjectRef _ aa _ -> counts aa
      DLE_Interact _ _ _ _ as -> counts as
      DLE_Digest _ as -> counts as

instance Countable DLAssignment where
  counts (DLAssignment m) = counts m
