module Reach.Test.Util
  ( goldenTests
  , mkSpecExamplesCoverCtors
  , compileTestSuccess
  , compileTestFail
  , compileTestAny
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (isInfixOf, (\\))
import Generics.Deriving
import Reach.Util
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec

testCompileOut :: FilePath -> IO (Either LB.ByteString LB.ByteString)
testCompileOut fp = do
  cfp <- canonicalizePath fp
  (ec, outs, errs) <-
    withCurrentDirectory (takeDirectory cfp) $ do
      rfp <- makeRelativeToCurrentDirectory cfp
      readProcessWithExitCode "reachc" ["--disable-reporting", rfp] ""
  let out = LC.pack outs
  let err = LC.pack errs
  let fmt = out <> err
  case ec of
    ExitSuccess -> return $ Right fmt
    ExitFailure _ -> return $ Left fmt

testCompileExpectFail :: FilePath -> IO LB.ByteString
testCompileExpectFail fp =
  testCompileOut fp >>= \case
    (Left s) -> return s
    (Right s) ->
      fail . bunpack . LB.toStrict $
        "Expected failure, but did not fail.\n" <> s

testCompileExpectSuccess :: FilePath -> IO LB.ByteString
testCompileExpectSuccess fp =
  testCompileOut fp >>= \case
    (Left s) ->
      fail . bunpack . LB.toStrict $
        "Expected success, but failed.\n" <> s
    (Right s) -> return s

testCompileExpectAny :: FilePath -> IO LB.ByteString
testCompileExpectAny fp = either id id <$> testCompileOut fp

compileTestFail :: FilePath -> IO TestTree
compileTestFail = stdoutStripAbs ".txt" testCompileExpectFail

compileTestSuccess :: FilePath -> IO TestTree
compileTestSuccess = stdoutStripAbs ".txt" testCompileExpectSuccess

compileTestAny :: FilePath -> IO TestTree
compileTestAny = stdoutStripAbs ".txt" testCompileExpectAny

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

findByExtension' :: [FilePath] -> FilePath -> IO [FilePath]
findByExtension' exts dir =
  findByExtension exts dir
    >>= pure . filter (not . (".reach" `isInfixOf`))

mkSpecExamplesCoverStrs :: [String] -> String -> FilePath -> Spec
mkSpecExamplesCoverStrs strs ext subdir = describe subdir $
  it "covers all specified examples" $ do
    curDir <- getCurrentDirectory
    let dir = curDir </> "t" </> subdir
    doesDirectoryExist dir `shouldReturn` True
    sources <- findByExtension' [ext] dir
    let missing = strs \\ map takeBaseName sources
    missing `shouldBe` []

goldenTests' :: (FilePath -> IO TestTree) -> String -> FilePath -> IO ([FilePath], [TestTree])
goldenTests' mkTest ext subdir = do
  curDir <- getCurrentDirectory
  let dir = curDir </> "t" </> subdir
  sources <- findByExtension' [ext] dir
  testNe <- testNotEmpty "this subdir" sources
  testSources <- testGroup ("testing each " <> ext <> " file") <$> mapM mkTest sources
  let tests = [testNe, testSources]
  return (sources, tests)

goldenTests :: (FilePath -> IO TestTree) -> String -> FilePath -> IO TestTree
goldenTests mkTest ext subdir = do
  (_, gTests) <- goldenTests' mkTest ext subdir
  let groupLabel = subdir
  return $ testGroup groupLabel gTests

-- | Replace all instances of findThis in inThis with replaceWithThis
replaceBs :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceBs findThis replaceWithThis = go
  where
    go inThis = case BS.breakSubstring findThis inThis of
      (bs0, bs1) -> case BS.stripPrefix findThis bs1 of
        Just bs1' -> bs0 <> replaceWithThis <> go bs1'
        Nothing -> inThis

stripAbs :: FilePath -> LB.ByteString -> LB.ByteString
stripAbs fp bs = LB.fromStrict bs'
  where
    bs' = replaceBs dir "." bsStrict
    bsStrict = LB.toStrict bs
    dir = bpack $ takeDirectory fp

-- TODO: relax this once the compiler has all relative paths
-- Just a bunch of dumb hacks to get simpler test output
stripAllAbs :: FilePath -> FilePath -> LB.ByteString -> LB.ByteString
stripAllAbs fp cwd = stripAbs fp''' . stripAbs fp'' . stripAbs fp' . stripAbs fp
  where
    fp' = lbunpack $ stripAbs cwd $ lbpack fp
    fp'' = lbunpack $ stripAbs (cwd </> "hs") $ lbpack fp
    fp''' = lbunpack $ "./t/../../examples/"

-- | Drops "CallStack (from HasCallStack):" and everything after
stripCallStack :: LB.ByteString -> LB.ByteString
stripCallStack bs = LB.fromStrict bsStrict'
  where
    (bsStrict', _) = BS.breakSubstring callStackStr bsStrict
    bsStrict = LB.toStrict bs
    callStackStr = "CallStack (from HasCallStack):"

-- TODO better name
stdoutStripAbs :: String -> (FilePath -> IO LB.ByteString) -> FilePath -> IO TestTree
stdoutStripAbs ext k fp = do
  let goldenFile = replaceExtension fp ext
  --    fpBase = takeBaseName fp
  cwd <- getCurrentDirectory
  return $ goldenVsString fp goldenFile (stripCallStack <$> stripAllAbs fp cwd <$> k fp)
