module Reach.Test_Version (spec_versionMatch) where

import qualified Paths_reach
import Reach.Version
import Test.Hspec

spec_versionMatch :: Spec
spec_versionMatch = describe "version" $ do
  it "is correct" $ version `shouldBe` Paths_reach.version
