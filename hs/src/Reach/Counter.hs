module Reach.Counter (Counter, newCounter, incCounter) where

import Data.IORef

newtype Counter = Counter (IORef Int)
  deriving (Eq)

newCounter :: Int -> IO Counter
newCounter i = Counter <$> newIORef i

incCounter :: Counter -> IO Int
incCounter (Counter r) = do
  i <- readIORef r
  writeIORef r $ i + 1
  return i
