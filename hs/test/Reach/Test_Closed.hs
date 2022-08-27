{-# LANGUAGE CPP #-}
module Reach.Test_Closed (test_closed) where

import Test.Tasty
import Test.Tasty.HUnit

#ifdef REACH_EVEREST
import Reach.Closed.Test_Verify (test_verify)
#endif

test_closed :: TestTree
test_closed = testGroup "closed" $
  [ testCase "null" $ return ()
#ifdef REACH_EVEREST
  , test_verify
#endif
  ]
