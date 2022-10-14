module Reach.BinaryLeafTree
  ( bltM
  , bltL
  , BLT (..)
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

data BLT i a
  = Empty
  | Leaf i Bool a
  | Branch i (BLT i a) (BLT i a)

instance Show i => Show (BLT i a) where
  show = \case
    Empty -> "."
    Leaf i _ _ -> show i
    Branch i l r -> "(" <> show l <> ") " <> show i <> " (" <> show r <> ")"

bltM :: Integral i => M.Map i a -> BLT i a
bltM = bltL . M.toAscList

bltL :: Integral i => [(i, a)] -> BLT i a
bltL = blt . S.fromList

blt :: Integral i => S.Seq (i, a) -> BLT i a
blt s =
  case len <= 1 of
    True ->
      case s of
        S.Empty -> Empty
        -- XXX incorporate into this the logic of when you need to check a leaf
        -- and when you don't if you know the low/hi
        (x, v) S.:<| _ -> Leaf x True v
    False ->
      let midl = len `div` 2
          (l, r) = S.splitAt midl s
          cl = blt l
          cr = blt r
       in case r of
            S.Empty -> cl
            (midv, _) S.:<| _ ->
              Branch midv cl cr
  where
    len = S.length s
