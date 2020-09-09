module Reach.Test_AST (spec_isFirstOrder) where

import qualified Data.Map as M
import Reach.AST
import Test.Hspec

spec_isFirstOrder :: Spec
spec_isFirstOrder = describe "isFirstOrder" $ do
  it "returns True when obvs" $ do
    isFirstOrder T_UInt256 `shouldBe` True
    isFirstOrder ([] --> T_Null) `shouldBe` True
    isFirstOrder (T_Array T_Bool 3) `shouldBe` True
    isFirstOrder (T_Tuple [T_Address]) `shouldBe` True
    isFirstOrder (T_Object (M.fromList [("x", T_Bytes)]))
      `shouldBe` True
    isFirstOrder (T_Forall "a" $ T_Var "a")
      `shouldBe` True

  it "returns False when obvs" $ do
    isFirstOrder ([[] --> T_Null] --> T_Null)
      `shouldBe` False
    isFirstOrder ([] --> [] --> T_Null)
      `shouldBe` False

  it "returns True when less obvs" $ do
    isFirstOrder (T_Object (M.fromList [("f", [] --> T_Null)]))
      `shouldBe` True
    isFirstOrder (T_Forall "a" $ [] --> T_Var "a")
      `shouldBe` True

  it "returns False when less obvs" $ do
    isFirstOrder ([] --> T_Array ([] --> T_Null) 3)
      `shouldBe` False
