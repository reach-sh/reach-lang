module Reach.Test_Verify
  ( test_verify_errs
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

partialCompileExpectFail :: FilePath -> IO BL.ByteString
partialCompileExpectFail fp = do
  (bs, ec) <- partialCompile fp
  case ec of
    ExitSuccess -> fail "Expected ExitFailure, got ExitSuccess"
    ExitFailure {} -> return bs

-- Assert that verification fails w/ the given err output
verifyGoldenTestFail :: FilePath -> TestTree
verifyGoldenTestFail = stdoutStripAbs ".txt" partialCompileExpectFail

test_verify_errs :: IO TestTree
test_verify_errs = goldenTests verifyGoldenTestFail ".rsh" "nl-verify-errs"
