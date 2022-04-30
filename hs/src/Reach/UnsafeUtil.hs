{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For advice on writing unsafe functions, see:
-- http://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html

module Reach.UnsafeUtil
  ( unsafeRedactAbs
  , unsafeRedactAbsStr
  , unsafeIsErrorFormatJson
  , unsafeTermSupportsColor
  , unsafeReadFile
  , unsafeHashStr
  , unsafeNub
  , unsafeLoud
  , loud
  )
where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Reach.CommandLine (CompilerToolArgs (cta_errorFormatJson), getCompilerArgs)
import Reach.Util
import System.Console.Pretty (supportsPretty)
import System.Directory
import System.Environment
import System.IO.Unsafe
import Data.List (nub)

-- | s/${pwd}/./g
unsafeRedactAbs :: Text -> Text
unsafeRedactAbs s = unsafePerformIO $ do
  dir <- getCurrentDirectory
  return $ redactAbs dir s
{-# NOINLINE unsafeRedactAbs #-}

unsafeRedactAbsStr :: String -> String
unsafeRedactAbsStr = T.unpack . unsafeRedactAbs . T.pack

unsafeIsErrorFormatJson :: Bool
unsafeIsErrorFormatJson = unsafePerformIO $ do
  args <- getCompilerArgs ""
  return $ cta_errorFormatJson args
{-# NOINLINE unsafeIsErrorFormatJson #-}

unsafeTermSupportsColor :: Bool
unsafeTermSupportsColor = unsafePerformIO supportsPretty
{-# NOINLINE unsafeTermSupportsColor #-}

unsafeReadFile :: FilePath -> [String]
unsafeReadFile s = lines . unsafePerformIO $ readFile s
{-# NOINLINE unsafeReadFile #-}

unsafeHashStr :: String
unsafeHashStr = unsafePerformIO $ do
  let try e fk =
        lookupEnv e >>= \case
          Just hash -> return $ " (" <> hash <> ")"
          Nothing -> fk
  try "REACHC_HASH" (try "REACH_GIT_HASH" $ return "")
{-# NOINLINE unsafeHashStr #-}

unsafeLoud :: Bool
unsafeLoud = unsafePerformIO $ isJust <$> lookupEnv "REACHC_TRACE"
{-# NOINLINE unsafeLoud #-}

loud :: String -> IO ()
loud = when unsafeLoud . putStrLn

unsafeNub :: Eq a => [a] -> [a]
unsafeNub = nub
