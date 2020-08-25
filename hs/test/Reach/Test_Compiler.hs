module Reach.Test_Compiler
  ( test_examples
  , test_language_non_features
  , test_language_features
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Reach.Compiler
import Reach.Test.Util
import Reach.Util
import System.Directory
-- import System.IO.Capture

import System.IO.Temp
import Test.Main
import Test.Tasty

testCompile :: FilePath -> IO (BL.ByteString, BL.ByteString, BL.ByteString)
testCompile fp = withSystemTempDirectory "reachc-tests" $ \dir -> do
  pr <- captureProcessResult $ do
    -- (out, err, ex, _) <- capture $ do -- System.IO.Capture
    createDirectoryIfMissing True dir
    compile $
      CompilerOpts
        { output = \x -> dir <> T.unpack x
        , source = fp
        , tops = ["main"]
        , intermediateFiles = False
        }
  -- return (out, err, ex) -- System.IO.Capture
  let ex = case prExitCode pr of
        ExitSuccess -> ""
        ec@(ExitFailure {}) ->
          bpack $ show ec <> (maybe "" (("\n" <>) . show) $ prException pr)
  return (BL.fromStrict $ prStdout pr, BL.fromStrict $ prStderr pr, BL.fromStrict $ ex)

-- Left = compile fail, Right = compile success
testCompileOut :: FilePath -> IO (Either BL.ByteString BL.ByteString)
testCompileOut fp =
  testCompile fp >>= \case
    (out, err, "") -> return $ Right $ outErr out err
    (out, err, ex) -> return $ Left $ outErrEx out err ex
  where
    outErr out err =
      "<stdout>\n" <> out <> "\n</stdout>\n" <> "<stderr>\n" <> err <> "</stderr>\n"
    outErrEx out err ex =
      outErr out err <> "<exception>\n" <> ex <> "\n</exception>\n"

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

compileTestFail :: FilePath -> IO TestTree
compileTestFail = stdoutStripAbs ".txt" testCompileExpectFail

compileTestSuccess :: FilePath -> IO TestTree
compileTestSuccess = stdoutStripAbs ".txt" testCompileExpectSuccess

compileTestAny :: FilePath -> IO TestTree
compileTestAny = stdoutStripAbs ".txt" testCompileExpectAny

test_language_features :: IO TestTree
test_language_features = goldenTests compileTestSuccess ".rsh" "features"

test_language_non_features :: IO TestTree
test_language_non_features = goldenTests compileTestFail ".rsh" "non-features"

test_examples :: IO TestTree
test_examples = goldenTests compileTestAny ".rsh" "../../examples/"
