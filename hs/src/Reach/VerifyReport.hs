module Reach.VerifyReport
  ( VerifyReport (..)
  , VerifyModeResult (..)
  , VerifyFailure (..)
  , VerifyReportAccum (..)
  , emptyVerifyReportAccum
  , vraAddMode
  , vraAddFailure
  , mkVerifyReport
  , writeVerifyReport
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Reach.OutputUtil

-- The shape of these records is a public interface: CI pipelines consume the
-- emitted verify.json, so field names must stay stable.

data VerifyFailure = VerifyFailure
  { vf_mode :: T.Text
  , vf_theorem :: T.Text
  , vf_msg :: Maybe T.Text
  , vf_at :: T.Text
  , vf_timeout :: Bool
  , vf_witness :: T.Text
  }
  deriving (Eq, Show)

instance ToJSON VerifyFailure where
  toJSON (VerifyFailure {..}) =
    object
      [ "vf_mode" .= vf_mode
      , "vf_theorem" .= vf_theorem
      , "vf_msg" .= vf_msg
      , "vf_at" .= vf_at
      , "vf_timeout" .= vf_timeout
      , "vf_witness" .= vf_witness
      ]

data VerifyModeResult = VerifyModeResult
  { vmr_mode :: T.Text
  , vmr_failures :: Int
  }
  deriving (Eq, Show)

instance ToJSON VerifyModeResult where
  toJSON (VerifyModeResult {..}) =
    object
      [ "vmr_mode" .= vmr_mode
      , "vmr_failures" .= vmr_failures
      ]

data VerifyReportAccum = VerifyReportAccum
  { vra_modes :: [VerifyModeResult] -- newest first
  , vra_failures :: [VerifyFailure] -- newest first
  , vra_succ :: Int
  , vra_fail :: Int
  , vra_time :: Int
  , vra_reps :: Int
  }
  deriving (Eq, Show)

emptyVerifyReportAccum :: VerifyReportAccum
emptyVerifyReportAccum = VerifyReportAccum mempty mempty 0 0 0 0

vraAddMode :: T.Text -> VerifyReportAccum -> VerifyReportAccum
vraAddMode m a = a {vra_modes = VerifyModeResult m 0 : vra_modes a}

vraAddFailure :: VerifyFailure -> VerifyReportAccum -> VerifyReportAccum
vraAddFailure f a =
  a
    { vra_failures = f : vra_failures a
    , vra_modes =
        case vra_modes a of
          [] -> []
          m : ms -> m {vmr_failures = vmr_failures m + 1} : ms
    }

data VerifyReport = VerifyReport
  { vr_source :: T.Text
  , vr_app :: T.Text
  , vr_verified :: Bool
  , vr_theoremCount :: Int
  , vr_succeeded :: Int
  , vr_failed :: Int
  , vr_timedOut :: Int
  , vr_omittedRepeats :: Int
  , vr_modes :: [VerifyModeResult]
  , vr_failures :: [VerifyFailure]
  }
  deriving (Eq, Show)

instance ToJSON VerifyReport where
  toJSON (VerifyReport {..}) =
    object
      [ "vr_source" .= vr_source
      , "vr_app" .= vr_app
      , "vr_verified" .= vr_verified
      , "vr_theoremCount" .= vr_theoremCount
      , "vr_succeeded" .= vr_succeeded
      , "vr_failed" .= vr_failed
      , "vr_timedOut" .= vr_timedOut
      , "vr_omittedRepeats" .= vr_omittedRepeats
      , "vr_modes" .= vr_modes
      , "vr_failures" .= vr_failures
      ]

mkVerifyReport :: T.Text -> T.Text -> Bool -> VerifyReportAccum -> VerifyReport
mkVerifyReport vr_source vr_app vr_verified (VerifyReportAccum {..}) =
  VerifyReport
    { vr_theoremCount = vra_succ + vra_fail + vra_time
    , vr_succeeded = vra_succ
    , vr_failed = vra_fail
    , vr_timedOut = vra_time
    , vr_omittedRepeats = vra_reps
    , vr_modes = reverse vra_modes
    , vr_failures = reverse vra_failures
    , ..
    }

writeVerifyReport :: FilePath -> VerifyReport -> IO ()
writeVerifyReport fp =
  atomicWriteFile BL.writeFile fp
    . encodePretty' (defConfig {confCompare = compare})
