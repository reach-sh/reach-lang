module Reach.STCounter where

import Control.Monad.ST
import Data.STRef

newtype STCounter s = STCounter (STRef s Int)
  deriving (Eq)

newSTCounter :: ST s (STCounter s)
newSTCounter = do
  r <- newSTRef $ 0
  return $ STCounter r

incSTCounter :: STCounter s -> ST s Int
incSTCounter (STCounter r) = do
  i <- readSTRef r
  writeSTRef r $ i + 1
  return i
