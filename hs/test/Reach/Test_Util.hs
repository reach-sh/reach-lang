module Reach.Test_Util where

import Reach.Util
import Test.Tasty
import Test.Tasty.HUnit

test_fromIntegerMay :: TestTree
test_fromIntegerMay =
  testGroup
    "fromIntegerMay"
    [ testCase "does not overflow" $
        let i = fromIntegral (maxBound :: Int) + 1
            mi = fromIntegerMay i :: Maybe Int
         in assertEqual "" mi Nothing
    , testCase "does not underflow" $
        let i = fromIntegral (minBound :: Int) - 1
            mi = fromIntegerMay i :: Maybe Int
         in assertEqual "" mi Nothing
    , testCase "works" $
        let i = 42 :: Integer
            mi = fromIntegerMay i :: Maybe Int
         in assertEqual "" mi (Just 42)
    ]
