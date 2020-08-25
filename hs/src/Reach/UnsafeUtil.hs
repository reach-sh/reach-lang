{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For advice on writing unsafe functions, see:
-- http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html

module Reach.UnsafeUtil (unsafeRedactAbs, unsafeRedactAbsStr) where

import Data.Text (Text)
import qualified Data.Text as T
import Reach.Util
import System.Directory
import System.IO.Unsafe

-- | s/${pwd}/./g
unsafeRedactAbs :: Text -> Text
unsafeRedactAbs s = unsafePerformIO $ do
  dir <- getCurrentDirectory
  return $ redactAbs dir s
{-# NOINLINE unsafeRedactAbs #-}

unsafeRedactAbsStr :: String -> String
unsafeRedactAbsStr = T.unpack . unsafeRedactAbs . T.pack
