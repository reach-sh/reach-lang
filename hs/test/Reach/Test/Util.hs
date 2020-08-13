module Reach.Test.Util
  ( errExample
  , errExampleStripAbs
  , goldenTests
  , mkSpecExamplesCoverCtors
  , stderroutExample
  , tryHard
  )
where

import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.List ((\\))
import Generics.Deriving
import Reach.Util
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec

tryHard :: NFData a => Exception e => IO a -> IO (Either e a)
tryHard m = do
  one <- try m
  case one of
    Left _ -> return one
    Right p -> try $ evaluate $ force p

-- It is dumb that both testSpec and it require descriptive strings
testNotEmpty :: String -> [a] -> IO TestTree
testNotEmpty label xs = testSpec label $
  it "is not empty" $ case xs of
    [] -> expectationFailure "... it is empty =["
    (_ : _) -> pure ()

mkSpecExamplesCoverCtors
  :: forall err proxy.
  (Generic err, ConNames (Rep err))
  => proxy err
  -> [String]
  -> String
  -> FilePath
  -> Spec
mkSpecExamplesCoverCtors _ exceptions = mkSpecExamplesCoverStrs strs
  where
    strs = conNames (error "unused" :: err) \\ exceptions

mkSpecExamplesCoverStrs :: [String] -> String -> FilePath -> Spec
mkSpecExamplesCoverStrs strs ext subdir = describe subdir $
  it "covers all specified examples" $ do
    curDir <- getCurrentDirectory
    let dir = curDir </> "test-examples" </> subdir
    doesDirectoryExist dir `shouldReturn` True
    sources <- findByExtension [ext] dir
    let missing = strs \\ map takeBaseName sources
    missing `shouldBe` []

goldenTests' :: (FilePath -> TestTree) -> String -> FilePath -> IO ([FilePath], [TestTree])
goldenTests' mkTest ext subdir = do
  curDir <- getCurrentDirectory
  let dir = curDir </> "test-examples" </> subdir
  sources <- findByExtension [ext] dir
  testNe <- testNotEmpty "this subdir" sources
  let testSources = testGroup ("testing each " <> ext <> " file") $ map mkTest sources
  let tests = [testNe, testSources]
  return (sources, tests)

goldenTests :: (FilePath -> TestTree) -> String -> FilePath -> IO TestTree
goldenTests mkTest ext subdir = do
  (_, gTests) <- goldenTests' mkTest ext subdir
  let groupLabel = subdir
  return $ testGroup groupLabel gTests

errExampleBs :: (Show a, NFData a) => (FilePath -> IO a) -> FilePath -> IO LB.ByteString
errExampleBs k fp =
  withCurrentDirectory dir (tryHard (k fpRel)) >>= \case
    Right r ->
      fail $ "expected a failure, but got: " ++ show r
    Left (ErrorCall e) ->
      return $ LB.fromStrict $ bpack e
  where
    dir = takeDirectory fp
    fpRel = takeFileName fp

-- | Replace all instances of findThis in inThis with replaceWithThis
replaceBs :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceBs findThis replaceWithThis = go
  where
    go inThis = case BS.breakSubstring findThis inThis of
      (bs0, bs1) -> case BS.stripPrefix findThis bs1 of
        Just bs1' -> bs0 <> replaceWithThis <> go bs1'
        Nothing -> inThis

errExampleBsStripAbs :: (Show a, NFData a) => (FilePath -> IO a) -> FilePath -> IO LB.ByteString
errExampleBsStripAbs k fp = do
  bs <- errExampleBs k fp
  let bs' = replaceBs dir "." (LB.toStrict bs)
  return $ LB.fromStrict bs'
  where
    dir = bpack $ takeDirectory fp

-- XXX This is a hack to make tests portable.
-- Ideally the compiler errors would print relative paths?
errExampleStripAbs :: (Show a, NFData a) => String -> (FilePath -> IO a) -> FilePath -> TestTree
errExampleStripAbs ext k fp = do
  let goldenFile = replaceExtension fp ext
      fpBase = takeBaseName fp
  goldenVsString fpBase goldenFile (errExampleBsStripAbs k fp)

errExample :: (Show a, NFData a) => String -> (FilePath -> IO a) -> FilePath -> TestTree
errExample ext k fp = do
  let goldenFile = replaceExtension fp ext
      fpBase = takeBaseName fp
  goldenVsString fpBase goldenFile (errExampleBs k fp)

stderroutExampleBs :: (FilePath -> IO (LB.ByteString, LB.ByteString)) -> FilePath -> IO LB.ByteString
stderroutExampleBs k fp = do
  (out, err) <- k fp
  let tag t x = "<" <> t <> ">\n" <> x <> "\n</" <> t <> ">\n"
  let res = (tag "out" out) <> (tag "err" err)
  return res

stderroutExample :: String -> (FilePath -> IO (LB.ByteString, LB.ByteString)) -> FilePath -> TestTree
stderroutExample ext k fp = do
  let goldenFile = replaceExtension fp ext
      fpBase = takeBaseName fp
  goldenVsString fpBase goldenFile (stderroutExampleBs k fp)
