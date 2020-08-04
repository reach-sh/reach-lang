module Reach.Test.Test_NL_Parser (testReachNlParser) where

import Reach.NL_Parser
import Reach.Test.Util
import Test.Tasty

gatherDeps_top_ :: FilePath -> IO ()
gatherDeps_top_ fp = do
  bundle <- gatherDeps_top fp
  return $! bundle `seq` ()

parseGoldenTest :: FilePath -> TestTree
parseGoldenTest = errExampleStripAbs ".txt" gatherDeps_top_

parseTests :: IO TestTree
parseTests = goldenTests parseGoldenTest ".rsh" "nl-parse-errors"

testReachNlParser :: IO TestTree
testReachNlParser = do
  tests <- sequence [parseTests]
  pure $ testGroup "Reach.NL_Parser" tests
