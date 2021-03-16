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
  , dupeIORef
  , mapWithKeyM
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack
import System.Exit
import Data.IORef (IORef, newIORef, readIORef)

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
impossible msg = error $
  "The compiler has encountered an internal error:\n\n  " <> msg <> "\n\n" <>
  "This error indicates a problem with the Reach compiler, not your program. " <>
  "Please report this error, along with the pertinent program, to the Reach team as soon as possible " <>
  "so we can fix it.\n\nOpen an issue at: https://github.com/reach-sh/reach-lang/issues\n"

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

dupeIORef :: IORef a -> IO (IORef a)
dupeIORef r = newIORef =<< readIORef r

mapWithKeyM :: (Ord k, Monad m) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyM f m = M.fromList <$> (mapM (\(k, x) -> (,) k <$> f k x) $ M.toAscList m)
