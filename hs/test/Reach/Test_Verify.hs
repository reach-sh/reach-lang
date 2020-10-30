module Reach.Test_Verify
  ( test_verify_errs
  , test_verify_noerrs
  )
where

import Reach.Test.Util
import Test.Tasty

test_verify_errs :: IO TestTree
test_verify_errs = goldenTests compileTestFail ".rsh" "nl-verify-errs"

test_verify_noerrs :: IO TestTree
test_verify_noerrs = goldenTests compileTestSuccess ".rsh" "nl-verify-noerrs"
