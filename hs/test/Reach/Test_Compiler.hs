module Reach.Test_Compiler
  ( test_language_non_features
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

testCompile :: FilePath -> IO (Either SomeException (String, ()))
testCompile fp = try $
  capture $ do
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

testCompileExpectFail :: FilePath -> IO BL.ByteString
testCompileExpectFail fp =
  testCompile fp >>= \case
    (Right (s, ())) ->
      fail $
        "Expected failure, but did not fail."
          <> ("stdout: \n" <> s)
    (Left e) ->
      return . BL.fromStrict . bpack $
        ("Failed with: " <> show e)

testCompileExpectSuccess :: FilePath -> IO BL.ByteString
testCompileExpectSuccess fp =
  testCompile fp >>= \case
    (Left e) ->
      fail $
        "Expected success, but failed with: " <> show e
    (Right (s, ())) -> return $ BL.fromStrict $ bpack $ s

compileTestFail :: FilePath -> TestTree
compileTestFail = stdoutStripAbs ".txt" testCompileExpectFail

compileTestSuccess :: FilePath -> TestTree
compileTestSuccess = stdoutStripAbs ".txt" testCompileExpectSuccess

test_language_features :: IO TestTree
test_language_features = goldenTests compileTestSuccess ".rsh" "features"

test_language_non_features :: IO TestTree
test_language_non_features = goldenTests compileTestFail ".rsh" "non-features"
