{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as LB
import Data.List (isPrefixOf)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Language.JavaScript.Parser.SrcLocation
import Reach.Compiler (CompileErr, Verifier (Z3), compile)
import Reach.CompilerTool
import Reach.ParserInternal
import Reach.Test.Test_NL_Eval
import Reach.Test.Test_NL_Parser
import Reach.Test.Test_Util
import Reach.Test.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Silently
import System.Process
import Test.Tasty
import Test.Tasty.Runners.AntXML
import Test.Tasty.Runners.Html

instance ReachErr ParseErr

instance ReachErr CompileErr

instance NFData TP where
  rnf (TP _) = ()

instance NFData TokenPosn where
  rnf (TokenPn _ _ _) = ()

dropDefines :: [T.Text] -> [T.Text]
dropDefines = filter (\line -> not $ "(define" `T.isInfixOf` line)

dropHypotheticalInteracts :: [T.Text] -> [T.Text]
dropHypotheticalInteracts = filter (\line -> not $ "... interact" `T.isPrefixOf` line)

dropMoreInfo :: [T.Text] -> [T.Text]
dropMoreInfo = filter (\line -> not $ "... v" `T.isPrefixOf` line)

dropExcess :: [T.Text] -> [T.Text]
dropExcess = id -- dropDefines . dropHypotheticalInteracts . dropMoreInfo

test_compile :: FilePath -> IO ()
test_compile n = do
  opts <-
    makeCompilerOpts $
      CompilerToolOpts
        { cto_outputDir = "test.out"
        , cto_source = n
        , cto_expCon = False
        , cto_expComp = False
        , cto_verifier = Z3
        }
  silence $ compile opts

test_compile_vererr :: FilePath -> IO (LB.ByteString, LB.ByteString)
test_compile_vererr pf = do
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
  -- strip out the defines
  let outT' = T.unlines $ dropExcess $ T.lines outT
  let out = LB.fromStrict $ TE.encodeUtf8 outT'
  -- putStrLn $ "...finished: " ++ show (LB.length out)
  _ <- waitForProcess hP
  removeFile rdest
  return (out, err)

parseErrExample :: FilePath -> TestTree
parseErrExample = errExample ".txt" readReachFile

compileErrExample :: FilePath -> TestTree
compileErrExample = errExample ".txt" test_compile

verifyErrExample :: FilePath -> TestTree
verifyErrExample = stderroutExample ".out" test_compile_vererr

main :: IO ()
main = do
  let parseTests = testsFor (Proxy :: Proxy ParseErr) parseErrExample ".rsh" "parse-errors"
  let compileTests = testsFor (Proxy :: Proxy CompileErr) compileErrExample ".rsh" "compile-errors"
  let verifyTests = goldenTests verifyErrExample ".patch" "verification-errors"
  args <- getArgs
  let -- Note: antXMLRunner isn't very polite when you leave off --xml
      theMain = case any ("--xml" `isPrefixOf`) args of
        True -> defaultMainWithIngredients (htmlRunner : antXMLRunner : defaultIngredients)
        False -> defaultMainWithIngredients (htmlRunner : defaultIngredients)
      -- Note: The tests get current dir mixed up if run in parallel
      theArgs = "--num-threads=1" : args
  tests <-
    sequence
      [ testReachNlParser
      , testReachNlEval
      , testReachUtil
      , parseTests
      , compileTests
      , verifyTests
      ]
  withArgs theArgs $
    theMain $ testGroup "tests" tests
