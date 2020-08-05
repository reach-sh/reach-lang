module Reach.Test_NL_Eval where

import Control.DeepSeq
import Reach.NL_Eval
import Reach.NL_Parser
import Reach.Test.Util
import Test.Tasty

partialCompile :: FilePath -> IO ()
partialCompile fp = do
  bundle <- gatherDeps_top fp
  let prog = compileBundle bundle "main"
  return $! rnf prog

evalGoldenTest :: FilePath -> TestTree
evalGoldenTest = errExampleStripAbs ".txt" partialCompile

test_compileBundle_errs :: IO TestTree
test_compileBundle_errs = goldenTests evalGoldenTest ".rsh" "nl-eval-errors"
