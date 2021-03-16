module Reach.CollectCounts
  ( Count (..)
  , Counts (..)
  , Countable
  , counts
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

newtype Count = Count (Maybe PLLetCat)
  deriving (Show, Eq)
  deriving newtype (Semigroup)

newtype Counts = Counts (M.Map DLVar PLLetCat)
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

countsS :: Countable a => a -> S.Set DLVar
countsS = S.fromList . counts_nzs . counts

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
  counts dv = Counts $ M.singleton dv PL_Once

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
    DLE_Transfer _ x y -> counts [x, y]
    DLE_Wait _ a -> counts a
    DLE_PartSet _ _ a -> counts a
    DLE_MapRef _ _ fa -> counts fa
    DLE_MapSet _ _ fa na -> counts [fa, na]
    DLE_MapDel _ _ fa -> counts fa
    DLE_Remote _ _ av _ amta as -> counts $ av : amta : as

instance Countable DLAssignment where
  counts (DLAssignment m) = counts m

instance Countable a => Countable (CInterval a) where
  counts (CBetween from to) = counts from <> counts to
