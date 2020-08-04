module Reach.Test.Test_NL_Eval (testReachNlEval) where

import Reach.NL_Eval
import Reach.NL_Parser
import Reach.Test.Util
import Test.Tasty

partialCompile :: FilePath -> IO ()
partialCompile fp = do
  bundle <- gatherDeps_top fp
  let prog = compileBundle bundle "main"
  return $! prog `seq` ()

evalGoldenTest :: FilePath -> TestTree
evalGoldenTest = errExampleStripAbs ".txt" partialCompile

evalTests :: IO TestTree
evalTests = goldenTests evalGoldenTest ".rsh" "nl-eval-errors"

testReachNlEval :: IO TestTree
testReachNlEval = do
  tests <- sequence [evalTests]
  pure $ testGroup "Reach.NL_Eval" tests
