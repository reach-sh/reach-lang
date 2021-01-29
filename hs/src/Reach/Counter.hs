module Reach.Counter (Counter, newCounter, incCounter, readCounter) where

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

readCounter :: Counter -> IO Int
readCounter (Counter r) = readIORef r
