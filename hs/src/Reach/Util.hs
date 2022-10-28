{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Util where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Except
import qualified Data.Aeson as AS
import qualified Data.Aeson.Types as AS
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldr')
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import GHC.Stack
import System.Directory.Extra
import System.Exit
import qualified Data.Aeson as AE

-- | A simple substitute for Data.ByteString.Char8.pack that handles unicode
bpack :: String -> ByteString
bpack = TE.encodeUtf8 . s2t

s2t :: String -> T.Text
s2t = T.pack

t2s :: T.Text -> String
t2s = T.unpack

s2lt :: String -> LT.Text
s2lt = LT.pack

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

possible :: HasCallStack => String -> b
possible msg =
  error $
    "The compiler has encountered an internal error:\n\n  " <> msg <> "\n\n"

saferMaybe :: String -> Maybe b -> b
saferMaybe s m = (fromMaybe (possible s) m)

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

forWithKeyM :: (Ord k, Monad m) => M.Map k a -> (k -> a -> m b) -> m (M.Map k b)
forWithKeyM = flip mapWithKeyM

forWithKeyM_ :: (Monad m) => M.Map k a -> (k -> a -> m ()) -> m ()
forWithKeyM_ m f = mapM_ (uncurry f) $ M.toAscList m

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
  rest <- concatMapM listDirectoriesRecursive ds
  pure $ ds <> rest

leftPad :: Int -> a -> [a] -> [a]
leftPad n e xs = replicate (n - length xs) e <> xs

rightPad :: Int -> a -> [a] -> [a]
rightPad n e xs = xs <> replicate (n - length xs) e

makeErrCode :: Show a => [Char] -> a -> [Char]
makeErrCode errType errIndex =
  errType <> leftPad 4 '0' (show errIndex)

arraySet :: Int -> a -> [a] -> [a]
arraySet n a arr = take n arr <> [a] <> drop (n + 1) arr

allEqual :: Eq a => [a] -> Either (Maybe (a,a)) a
allEqual [] = Left Nothing
allEqual [x] = Right x
allEqual (x:xs) = case allEqual xs of
  Right y -> if x == y then Right y else Left $ Just (x,y)
  Left diff -> Left diff

maybeAt :: Int -> [a] -> Maybe a
maybeAt 0 (x:_) = Just x
maybeAt _ [] = Nothing
maybeAt n (_:xs) = maybeAt (n-1) xs

-- Source https://en.wikipedia.org/wiki/Integer_square_root#Using_only_integer_division
isqrt :: Integral a => a -> a
isqrt n = go n2 (iter n2)
  where
    n2 = n `div` 2
    iter x = (x + (n `div` x)) `div` 2
    go x0 x1 = if x0 == x1 then x0 else go x1 (iter x1)

kmToM :: KM.KeyMap a -> M.Map T.Text a
kmToM = M.fromList . map (\(k,v) -> (K.toText k, v)) . KM.toList

mToKM :: M.Map T.Text a -> KM.KeyMap a
mToKM = KM.fromList . map (\(k,v) -> (K.fromText k, v)) . M.toList

aesonObject :: [(T.Text, AS.Value)] -> AS.Value
aesonObject = AS.object . map (first K.fromText)

aesonParse :: AS.FromJSON a => AS.Value -> Either String a
aesonParse = AS.parseEither AS.parseJSON

aesonParse' :: (Monad m, AS.FromJSON a) => AS.Value -> ExceptT String m a
aesonParse' = except . aesonParse

eitherGo :: (Monad m) => Either a b -> (b -> m c) -> m (Either a c)
eitherGo e f =
  case e of
    Left x -> return $ Left x
    Right y -> Right <$> f y

eitherP :: (Monad m) => (x -> Either a b) -> (b -> m c) -> x -> m (Either a c)
eitherP p f e = eitherGo (p e) f

replace :: Integral i => i -> a -> [a] -> [a]
replace i v l = hd <> (v : tail tl)
  where
    (hd, tl) = splitAt (fromInteger $ toInteger i) l

startsWith :: Eq a => a -> [a] -> Bool
startsWith x = \case
  xp:_ | x == xp -> True
  _ -> False

mapJsonString :: (Text -> Text) -> AE.Value -> AE.Value
mapJsonString f = \case
  AE.String s -> AE.String $ f s
  _ -> impossible "Expected string"

expectJsonObj :: (AE.Object -> t) -> AE.Value -> t
expectJsonObj f = \case
  AE.Object o -> f o
  _ -> impossible "Expected object"
