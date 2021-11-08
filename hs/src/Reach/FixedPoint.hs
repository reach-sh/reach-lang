module Reach.FixedPoint
  ( fixedPoint
  , fixedPoint_
  ) where

fixedPoint_ :: forall a. Eq a => a -> (a -> IO a) -> IO a
fixedPoint_ x0 f = h x0
  where
    h :: a -> IO a
    h x = do
      x' <- f x
      case x == x' of
        True -> return x
        False -> h x'

fixedPoint :: (Eq a, Monoid a) => (a -> IO a) -> IO a
fixedPoint = fixedPoint_ mempty
