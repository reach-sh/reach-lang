module Reach.Test_Util where

import Reach.Util
import Test.Hspec

spec_fromIntegerMay :: Spec
spec_fromIntegerMay = describe "fromIntegerMay" $ do
  it "does not overflow" $ do
    let i = fromIntegral (maxBound :: Int) + 1
        mi = fromIntegerMay i :: Maybe Int
    mi `shouldBe` Nothing
  it "does not underflow" $ do
    let i = fromIntegral (minBound :: Int) - 1
        mi = fromIntegerMay i :: Maybe Int
    mi `shouldBe` Nothing
  it "works" $ do
    let i = 42 :: Integer
        mi = fromIntegerMay i :: Maybe Int
    mi `shouldBe` Just 42

prop_fromIntegerMay_intRoundTrip :: Int -> Bool
prop_fromIntegerMay_intRoundTrip i = fromIntegerMay (fromIntegral i) == Just i
