module Reach.Test_Compiler
  ( test_examples
  , test_language_non_features
  , test_language_features
  )
where

import Reach.Test.Util
import Test.Tasty

test_language_features :: IO TestTree
test_language_features = goldenTests compileTestSuccess ".rsh" "features"

test_language_non_features :: IO TestTree
test_language_non_features = goldenTests compileTestFail ".rsh" "non-features"

test_examples :: IO TestTree
test_examples = goldenTests compileTestAny ".rsh" "../../examples/"
