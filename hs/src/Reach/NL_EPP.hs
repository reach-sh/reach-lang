module Reach.NL_EPP where

import qualified Data.Map.Strict as M
import Reach.NL_AST
--- import Reach.Util

data Count
  --- XXX loc is really per SLPart
  = Count { c_loc :: Int
          , c_vrf :: Int
          , c_con :: Int }
  deriving (Show, Eq)

instance Monoid Count where
  mempty = Count { c_loc = 0
                 , c_vrf = 0
                 , c_con = 0 }

instance Semigroup Count where
  c1 <> c2 = Count { c_loc = c_loc c1 + c_loc c2
                   , c_vrf = c_vrf c1 + c_vrf c2
                   , c_con = c_con c1 + c_con c2 }

newtype Counts = Counts (M.Map DLVar Count)
  deriving (Show, Eq)

instance Monoid Counts where
  mempty = Counts mempty

instance Semigroup Counts where
  (Counts m1) <> (Counts m2) = Counts m3
    where m3 = M.unionWith (<>) m1 m2

get_count :: DLVar -> Counts -> Count
get_count v (Counts m) =
  case M.lookup v m of
    Nothing -> mempty
    Just x -> x

count_rms :: [DLVar] -> Counts -> Counts
count_rms vs (Counts cs) = Counts $ foldr M.delete cs vs

class Countable a where
  counts :: a -> Counts

instance Countable v => Countable [v] where
  counts l = mconcat $ map counts l

instance Countable v => Countable (M.Map k v) where
  counts m = counts $ M.elems m

instance Countable DLVar where
  --- XXX use mode
  counts dv = Counts $
    M.singleton dv (Count { c_loc = 1
                          , c_vrf = 1
                          , c_con = 1 })

instance Countable DLArg where
  counts a =
    case a of
      DLA_Var v -> counts v
      DLA_Con {} -> mempty
      DLA_Array as -> counts as
      DLA_Obj as -> counts as
      DLA_Interact {} -> mempty

instance Countable DLExpr where
  counts e =
    case e of
      DLE_PrimOp _ _ as -> counts as
      DLE_ArrayRef _ aa ea -> counts [aa, ea]
      DLE_Interact _ _ as -> counts as
      DLE_Digest _ as -> counts as

instance Countable DLAssignment where
  counts (DLAssignment m) = counts m
