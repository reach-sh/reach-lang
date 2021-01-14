module Reach.Test_AST (spec_isFirstOrder) where

import qualified Data.Map as M
import Reach.AST.Base
import Reach.AST.SL
import Test.Hspec

spec_isFirstOrder :: Spec
spec_isFirstOrder = describe "isFirstOrder" $ do
  it "returns True when obvs" $ do
    isFirstOrder ST_UInt `shouldBe` True
    isFirstOrder ([] --> ST_Null) `shouldBe` True
    isFirstOrder (ST_Array ST_Bool 3) `shouldBe` True
    isFirstOrder (ST_Tuple [ST_Address]) `shouldBe` True
    isFirstOrder (ST_Object (M.fromList [("x", ST_Bytes 16)]))
      `shouldBe` True
    isFirstOrder (ST_Forall "a" $ ST_Var "a")
      `shouldBe` True

  it "returns False when obvs" $ do
    isFirstOrder ([[] --> ST_Null] --> ST_Null)
      `shouldBe` False
    isFirstOrder ([] --> [] --> ST_Null)
      `shouldBe` False

  it "returns True when less obvs" $ do
    isFirstOrder (ST_Object (M.fromList [("f", [] --> ST_Null)]))
      `shouldBe` True
    isFirstOrder (ST_Forall "a" $ [] --> ST_Var "a")
      `shouldBe` True

  it "returns False when less obvs" $ do
    isFirstOrder ([] --> ST_Array ([] --> ST_Null) 3)
      `shouldBe` False
