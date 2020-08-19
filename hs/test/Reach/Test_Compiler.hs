module Reach.Test_Compiler
  ( test_docs
  , test_language_non_features
  , test_language_features
  )
where

import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Reach.Compiler
import Reach.Test.Util
import Reach.Util
import System.Directory
import System.IO.Silently
import Test.Tasty

testCompile :: FilePath -> IO (String, (Either SomeException ()))
testCompile fp = capture $
  try $ do
    createDirectoryIfMissing True outDir
    compileNL copts
  where
    outDir = "/tmp/reachc/"
    copts =
      CompilerOpts
        { output = \x -> outDir <> T.unpack x
        , source = fp
        , tops = ["main"]
        , intermediateFiles = False
        }

-- Left = compile fail, Right = compile success
testCompileOut :: FilePath -> IO (Either BL.ByteString BL.ByteString)
testCompileOut fp =
  testCompile fp >>= \case
    (s, Left e) -> return $ Left $ BL.fromStrict $ bpack $ s <> "\n\nFailed with: " <> show e
    (s, Right ()) -> return $ Right $ BL.fromStrict $ bpack $ s

testCompileExpectFail :: FilePath -> IO BL.ByteString
testCompileExpectFail fp =
  testCompileOut fp >>= \case
    (Left s) -> return s
    (Right s) ->
      fail . bunpack . BL.toStrict $
        "Expected failure, but did not fail.\n" <> s

testCompileExpectSuccess :: FilePath -> IO BL.ByteString
testCompileExpectSuccess fp =
  testCompileOut fp >>= \case
    (Left s) ->
      fail . bunpack . BL.toStrict $
        "Expected success, but failed.\n" <> s
    (Right s) -> return s

testCompileExpectAny :: FilePath -> IO BL.ByteString
testCompileExpectAny fp = either id id <$> testCompileOut fp

compileTestFail :: FilePath -> TestTree
compileTestFail = stdoutStripAbs ".txt" testCompileExpectFail

compileTestSuccess :: FilePath -> TestTree
compileTestSuccess = stdoutStripAbs ".txt" testCompileExpectSuccess

compileTestAny :: FilePath -> TestTree
compileTestAny = stdoutStripAbs ".txt" testCompileExpectAny

test_language_features :: IO TestTree
test_language_features = goldenTests compileTestSuccess ".rsh" "features"

test_language_non_features :: IO TestTree
test_language_non_features = goldenTests compileTestFail ".rsh" "non-features"

test_docs :: IO TestTree
test_docs = goldenTests compileTestAny ".rsh" "../../docs-src/x"
