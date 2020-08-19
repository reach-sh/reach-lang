{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For advice on writing unsafe functions, see:
-- http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html

module Reach.UnsafeUtil (unsafeRedactAbsStr) where

import Reach.Util
import System.Directory
import System.IO.Unsafe

-- | s/${pwd}/./g
unsafeRedactAbsStr :: String -> String
unsafeRedactAbsStr s = unsafePerformIO $ do
  dir <- getCurrentDirectory
  return $ redactAbsStr dir s
{-# NOINLINE unsafeRedactAbsStr #-}
