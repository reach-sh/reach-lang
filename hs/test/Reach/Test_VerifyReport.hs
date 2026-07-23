module Reach.Test_VerifyReport
  ( spec_verifyReportShape
  , spec_solCommandLine
  )
where

import Data.Aeson (Value, object, toJSON, (.=))
import Reach.CommandLine
import Reach.VerifyReport
import System.Environment (withArgs)
import Test.Hspec

-- The verify.json field names are a public interface consumed by CI
-- pipelines; this spec locks them.
spec_verifyReportShape :: Spec
spec_verifyReportShape = describe "VerifyReport JSON shape" $ do
  it "serializes with stable field names" $ do
    let acc0 = emptyVerifyReportAccum {vra_succ = 5, vra_fail = 1, vra_time = 0, vra_reps = 2}
    let acc1 = vraAddMode "generic connector: ALL participants are honest" acc0
    let f =
          VerifyFailure
            { vf_mode = "ALL participants are honest"
            , vf_theorem = "assert"
            , vf_msg = Just "x is small"
            , vf_at = "./x.rsh:14:9:application"
            , vf_timeout = False
            , vf_witness = "Verification failed: ..."
            }
    let acc = vraAddFailure f acc1
    let vr = mkVerifyReport "x.rsh" "main" False acc
    let expected :: Value
        expected =
          object
            [ "vr_source" .= ("x.rsh" :: String)
            , "vr_app" .= ("main" :: String)
            , "vr_verified" .= False
            , "vr_theoremCount" .= (6 :: Int)
            , "vr_succeeded" .= (5 :: Int)
            , "vr_failed" .= (1 :: Int)
            , "vr_timedOut" .= (0 :: Int)
            , "vr_omittedRepeats" .= (2 :: Int)
            , "vr_modes"
                .= [ object
                       [ "vmr_mode" .= ("generic connector: ALL participants are honest" :: String)
                       , "vmr_failures" .= (1 :: Int)
                       ]
                   ]
            , "vr_failures"
                .= [ object
                       [ "vf_mode" .= ("ALL participants are honest" :: String)
                       , "vf_theorem" .= ("assert" :: String)
                       , "vf_msg" .= ("x is small" :: String)
                       , "vf_at" .= ("./x.rsh:14:9:application" :: String)
                       , "vf_timeout" .= False
                       , "vf_witness" .= ("Verification failed: ..." :: String)
                       ]
                   ]
            ]
    toJSON vr `shouldBe` expected

spec_solCommandLine :: Spec
spec_solCommandLine = describe "reachc --sol flag" $ do
  it "sets co_solOnly and composes with --intermediate-files" $ do
    cta <- withArgs ["--sol", "--intermediate-files", "x.rsh"] $ getCompilerArgs "test"
    let co = cta_co cta
    co_solOnly co `shouldBe` True
    co_intermediateFiles co `shouldBe` True
    co_source co `shouldBe` "x.rsh"
  it "defaults to off" $ do
    cta <- withArgs ["x.rsh"] $ getCompilerArgs "test"
    co_solOnly (cta_co cta) `shouldBe` False
    co_verifyReport (cta_co cta) `shouldBe` False
