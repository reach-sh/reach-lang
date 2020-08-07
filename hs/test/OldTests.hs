{-# OPTIONS_GHC -fno-warn-orphans #-}

module OldTests
  ( test_readReachFile_errs
  , test_compile_errs
  , -- XXX broken
    no_test_verify_errs
  )
where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Reach.Compiler (CompileErr, compile)
import Reach.CompilerTool
import Reach.ParserInternal
import Reach.Test.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Silently
import System.Process
import Test.Tasty

instance ReachErr ParseErr

instance ReachErr CompileErr

instance NFData TP where
  rnf (TP _) = ()

testCompile :: FilePath -> IO ()
testCompile n = do
  opts <-
    makeCompilerOpts $
      CompilerToolOpts
        { cto_outputDir = "test.out"
        , cto_source = n
        , cto_expCon = False
        , cto_expComp = False
        }
  silence $ compile opts

testCompileVererr :: FilePath -> IO (LB.ByteString, LB.ByteString)
testCompileVererr pf = do
  let examples_dir = "../examples"
  let dest = T.unpack . T.replace "__" "/" . T.pack $ takeFileName pf
  let orig = dropExtension $ dropExtension dest
  let patchCmd = "patch --quiet -d " ++ examples_dir ++ " -i " ++ pf ++ " -o " ++ dest ++ " " ++ orig
  -- putStrLn patchCmd
  ExitSuccess <- system patchCmd
  -- putStrLn "...patch applied"
  let rdest = examples_dir </> dest
  -- putStrLn "compiling..."
  (_, Just hout, Just herr, hP) <- do
    -- TODO: a better way of running the compiler and capturing stdout/stderr
    unsetEnv "REACHC_VERIFIER"
    createProcess
      (proc "stack" ["exec", "--", "reachc", "-o", "test.out", rdest])
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
  outT <- TIO.hGetContents hout
  err <- LB.hGetContents herr
  let out = LB.fromStrict $ TE.encodeUtf8 outT
  -- putStrLn $ "...finished: " ++ show (LB.length out)
  _ <- waitForProcess hP
  removeFile rdest
  return (out, err)

parseErrExample :: FilePath -> TestTree
parseErrExample = errExample ".txt" readReachFile

compileErrExample :: FilePath -> TestTree
compileErrExample = errExample ".txt" testCompile

verifyErrExample :: FilePath -> TestTree
verifyErrExample = stderroutExample ".out" testCompileVererr

test_readReachFile_errs :: IO TestTree
test_readReachFile_errs = testsFor (Proxy :: Proxy ParseErr) parseErrExample ".rsh" "parse-errors"

test_compile_errs :: IO TestTree
test_compile_errs = testsFor (Proxy :: Proxy CompileErr) compileErrExample ".rsh" "compile-errors"

-- XXX broken. Port these tests to NL.
no_test_verify_errs :: IO TestTree
no_test_verify_errs = goldenTests verifyErrExample ".patch" "verification-errors"
