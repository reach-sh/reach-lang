module Reach.Util
  ( bpack
  , bunpack
  , lbpack
  , lbunpack
  , impossible
  , trimQuotes
  , fromIntegerMay
  , maybeDie
  , redactAbs
  , redactAbsStr
  , safeInit
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack
import System.Exit

-- | A simple substitute for Data.ByteString.Char8.pack that handles unicode
bpack :: String -> ByteString
bpack = TE.encodeUtf8 . T.pack

bunpack :: ByteString -> String
bunpack = T.unpack . TE.decodeUtf8

lbpack :: String -> BL.ByteString
lbpack = BL.fromStrict . bpack

lbunpack :: BL.ByteString -> String
lbunpack = bunpack . BL.toStrict

maybeDie :: ExitCode -> IO ()
maybeDie ec = do
  unless
    (ec == ExitSuccess)
    (do (exitWith ec))
  return ()

impossible :: HasCallStack => String -> b
impossible msg = error $ "impossible situation (i.e. compiler error): " ++ msg

-- Note: drop 1 is safer than init/tail on empty strings
trimQuotes :: String -> String
trimQuotes = reverse . drop 1 . reverse . drop 1

-- | It's like init, but doesn't crash on empty list
safeInit :: [a] -> [a]
safeInit [] = []
safeInit [_] = []
safeInit (x : xs) = x : safeInit xs

-- | Safe fromInteger with bounds checking
fromIntegerMay :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
fromIntegerMay i
  | i <= fromIntegral (maxBound :: a)
      && i >= fromIntegral (minBound :: a) =
    Just $ fromIntegral i
  | otherwise = Nothing

redactAbs :: FilePath -> Text -> Text
redactAbs dir = T.replace (T.pack dir) "."

redactAbsStr :: FilePath -> String -> String
redactAbsStr dir = T.unpack . redactAbs dir . T.pack
