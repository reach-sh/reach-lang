module Reach.IORefRef (IORefRef, newIORefRef, modifyIORefRef, paramIORefRef, readIORefRef) where

import Data.IORef

type IORefRef a = (IORef (IORef a))

newIORefRef :: a -> IO (IORefRef a)
newIORefRef v = do
  r <- newIORef v
  newIORef r

modifyIORefRef :: (IORefRef a) -> (a -> a) -> IO ()
modifyIORefRef rr f = do
  r <- readIORef rr
  modifyIORef r f

readIORefRef :: IORefRef a -> IO a
readIORefRef rr = do
  r <- readIORef rr
  readIORef r

paramIORefRef :: (IORefRef a) -> IO b -> IO b
paramIORefRef rr m = do
  old_r <- readIORef rr
  old_v <- readIORef old_r
  new_r <- newIORef old_v
  writeIORef rr new_r
  ans <- m
  writeIORef rr old_r
  return $ ans
