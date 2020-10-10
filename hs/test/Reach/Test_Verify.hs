module Reach.Test_Verify
  ( test_verify_errs
  , test_verify_noerrs
  )
where

import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Reach.AST
import Reach.Compiler (all_connectors)
import Reach.Eval
import Reach.Linearize
import Reach.Parser
import Reach.Test.Util
import Reach.Util
import Reach.Verify
import System.Exit
import System.IO.Silently -- TODO: port to System.IO.Capture
import Test.Tasty

partialCompile :: FilePath -> IO (BL.ByteString, Either SomeException ExitCode)
partialCompile fp = do
  djp <- gatherDeps_top fp
  let dl = compileBundle all_connectors djp "main"
  let ll = linearize dl
  let DLProg _ (DLOpts {..}) _ _ = dl
  let connectors = map (all_connectors M.!) dlo_connectors
  let vconnectors =
        case dlo_verifyPerConnector of
          False -> Nothing
          True -> Just connectors
  (s, eec) <- capture $ try $ verify Nothing vconnectors ll
  return (BL.fromStrict $ bpack s, eec)

partialCompileExpectSuccess :: FilePath -> IO BL.ByteString
partialCompileExpectSuccess fp = do
  (bs, ec) <- partialCompile fp
  case ec of
    Left e -> throwIO e
    Right (ExitFailure {}) ->
      fail $
        ("Expected ExitSuccess, but got " <> show ec <> "\n")
          <> (bunpack $ BL.toStrict bs)
    Right ExitSuccess -> return bs

partialCompileExpectFail :: FilePath -> IO BL.ByteString
partialCompileExpectFail fp = do
  (bs, ec) <- partialCompile fp
  case ec of
    Left e -> throwIO e
    Right ExitSuccess -> fail "Expected ExitFailure, got ExitSuccess"
    Right (ExitFailure {}) -> return bs

-- Assert that verification fails and produces the given err output
verifyGoldenTestFail :: FilePath -> IO TestTree
verifyGoldenTestFail = stdoutStripAbs ".txt" partialCompileExpectFail

-- Assert that verification passes and produces the given output
verifyGoldenTestSuccess :: FilePath -> IO TestTree
verifyGoldenTestSuccess = stdoutStripAbs ".txt" partialCompileExpectSuccess

test_verify_errs :: IO TestTree
test_verify_errs = goldenTests verifyGoldenTestFail ".rsh" "nl-verify-errs"

test_verify_noerrs :: IO TestTree
test_verify_noerrs = goldenTests verifyGoldenTestSuccess ".rsh" "nl-verify-noerrs"
