module Reach.FixedPoint
  ( fixedPoint
  , fixedPoint_
  ) where

fixedPoint_ :: forall a. Eq a => a -> (a -> a) -> a
fixedPoint_ x0 f = h x0
  where
    h :: a -> a
    h x =
      let x' = f x
       in case x == x' of
            True -> x
            False -> h x'

fixedPoint :: (Eq a, Monoid a) => (a -> a) -> a
fixedPoint = fixedPoint_ mempty
