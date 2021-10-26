module Reach.Version
  ( version
  , versionStr
  , versionHashStr
  , compatibleVersion
  , compatibleVersionStr
  , versionHeader
  , major
  , minor
  , patch
  ) where

import Data.Version (Version (..), makeVersion, showVersion)
import System.Environment
import System.IO.Unsafe

major :: Int
major = {{MAJOR}}

minor :: Int
minor = {{MINOR}}

patch :: Int
patch = {{PATCH}}

version :: Version
version = makeVersion [major, minor, patch]

versionStr :: String
versionStr = showVersion version

versionHashStr :: String
versionHashStr = unsafePerformIO $ do
  let ret = return . (versionStr <>)
  let try e fk = lookupEnv e >>= \case
        Just hash -> ret $ " (" <> hash <> ")"
        Nothing -> fk
  try "REACHC_HASH" (try "REACH_GIT_HASH" $ ret "")

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

compatibleVersionStr :: String
compatibleVersionStr = showVersion compatibleVersion

versionHeader :: String
versionHeader = "reach " ++ compatibleVersionStr
