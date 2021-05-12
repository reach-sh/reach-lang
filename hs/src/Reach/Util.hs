module Reach.Util
  ( bpack
  , bunpack
  , lbpack
  , lbunpack
  , b2t
  , s2t
  , impossible
  , trimQuotes
  , fromIntegerMay
  , maybeDie
  , redactAbs
  , redactAbsStr
  , safeInit
  , dupeIORef
  , mapWithKeyM
  , hdDie
  , justValues
  , Top(..)
  , uncurry3
  , uncurry4
  , uncurry5
  , listDirectoriesRecursive
  )
where

import Control.Monad
import Control.Monad.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldr')
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack
import System.Directory.Extra
import System.Exit

-- | A simple substitute for Data.ByteString.Char8.pack that handles unicode
bpack :: String -> ByteString
bpack = TE.encodeUtf8 . s2t

s2t :: String -> T.Text
s2t = T.pack

bunpack :: ByteString -> String
bunpack = T.unpack . b2t

b2t :: ByteString -> T.Text
b2t = TE.decodeUtf8

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

hdDie :: [p] -> p
hdDie (h : _) = h
hdDie [] = impossible "hdDie"

impossible :: HasCallStack => String -> b
impossible msg =
  error $
    "The compiler has encountered an internal error:\n\n  " <> msg <> "\n\n"
      <> "This error indicates a problem with the Reach compiler, not your program. "
      <> "Please report this error, along with the pertinent program, to the Reach team as soon as possible "
      <> "so we can fix it.\n\nOpen an issue at: https://github.com/reach-sh/reach-lang/issues\n"

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

justValues :: [(a, Maybe b)] -> [(a, b)]
justValues = foldr' (\(k, mv) acc -> maybe acc ((: acc) . (k,)) mv) []

data Top
  = CompileAll
  | CompileJust [String]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive dir = do
  (ds, _) <- partitionM doesDirectoryExist =<< listContents dir
  rest    <- concatMapM listDirectoriesRecursive ds
  pure $ ds <> rest
