module Reach.FixedPoint
  ( fixedPoint
  , fixedPoint_
  )
where

fixedPoint_ :: forall a. Eq a => a -> (Integer -> a -> IO a) -> IO a
fixedPoint_ x0 f = h 0 x0
  where
    h :: Integer -> a -> IO a
    h i x = do
      x' <- f i x
      case x == x' of
        True -> return x
        False -> h (i + 1) x'

fixedPoint :: (Eq a, Monoid a) => (Integer -> a -> IO a) -> IO a
fixedPoint = fixedPoint_ mempty
