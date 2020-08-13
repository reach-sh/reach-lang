module Reach.Test_Verify
  ( test_verify_errs
  , test_verify_noerrs
  )
where

import qualified Data.ByteString.Lazy as BL
import Reach.Eval
import Reach.Linearize
import Reach.Parser
import Reach.Test.Util
import Reach.Util
import Reach.Verify
import System.Exit
import System.IO.Silently
import Test.Tasty

partialCompile :: FilePath -> IO (BL.ByteString, ExitCode)
partialCompile fp = do
  djp <- gatherDeps_top fp
  let dl = compileBundle djp "main"
      ll = linearize dl
  (s, ec) <- capture $ verify outn ll
  return (BL.fromStrict $ bpack s, ec)
  where
    -- don't really care about this logged output
    outn _ = "/dev/null"

partialCompileExpectSuccess :: FilePath -> IO BL.ByteString
partialCompileExpectSuccess fp = do
  (bs, ec) <- partialCompile fp
  case ec of
    ExitFailure {} ->
      fail $
        ("Expected ExitSuccess, but got " <> show ec <> "\n")
          <> (bunpack $ BL.toStrict bs)
    ExitSuccess -> return bs

partialCompileExpectFail :: FilePath -> IO BL.ByteString
partialCompileExpectFail fp = do
  (bs, ec) <- partialCompile fp
  case ec of
    ExitSuccess -> fail "Expected ExitFailure, got ExitSuccess"
    ExitFailure {} -> return bs

-- Assert that verification fails and produces the given err output
verifyGoldenTestFail :: FilePath -> TestTree
verifyGoldenTestFail = stdoutStripAbs ".txt" partialCompileExpectFail

-- Assert that verification passes and produces the given output
verifyGoldenTestSuccess :: FilePath -> TestTree
verifyGoldenTestSuccess = stdoutStripAbs ".txt" partialCompileExpectSuccess

test_verify_errs :: IO TestTree
test_verify_errs = goldenTests verifyGoldenTestFail ".rsh" "nl-verify-errs"

test_verify_noerrs :: IO TestTree
test_verify_noerrs = goldenTests verifyGoldenTestSuccess ".rsh" "nl-verify-noerrs"
