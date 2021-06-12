module Reach.Test_Compiler
  ( test_examples
  , test_yes
  , test_no
  )
where

import Reach.Test.Util
import Test.Tasty

test_yes :: IO TestTree
test_yes = goldenTests compileTestSuccess ".rsh" "y"

test_no :: IO TestTree
test_no = goldenTests compileTestFail ".rsh" "n"

test_examples :: IO TestTree
test_examples = goldenTests compileTestAny ".rsh" "../../examples/"
