module Reach.BinaryLeafTree
  ( bltM
  , bltL
  , BLT(..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

data BLT i a
  = Empty
  | Leaf i a
  | Branch i (BLT i a) (BLT i a)

bltM :: Integral i => M.Map i a -> BLT i a
bltM = bltL . M.toAscList

bltL :: Integral i => [(i, a)] -> BLT i a
bltL = blt . S.fromList

blt :: Integral i => S.Seq (i, a)  -> BLT i a
blt s =
  case len <= 1 of
    True ->
      case s of
        S.Empty -> Empty
        (x, v) S.:<| _ -> Leaf x v
    False ->
      let midl = len `div` 2
          (l, r) = S.splitAt midl s
          cl = blt l
          cr = blt r
      in
        case r of
          S.Empty -> cl
          (midv, _) S.:<| _ ->
            Branch midv cl cr
  where
    len = S.length s
