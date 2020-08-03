module Reach.Test.Test_NL_Parser (testReachNlParser) where

import Reach.NL_Parser
import Reach.Test.Util
import Test.Tasty

parseGoldenTest :: FilePath -> TestTree
parseGoldenTest = errExampleStripAbs ".txt" gatherDeps_top

parseTests :: IO TestTree
parseTests = goldenTests parseGoldenTest ".rsh" "nl-parse-errors"

testReachNlParser :: IO TestTree
testReachNlParser = do
  tests <- sequence [parseTests]
  pure $ testGroup "NL parse tests" tests
