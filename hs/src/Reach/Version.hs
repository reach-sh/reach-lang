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
import Reach.UnsafeUtil

major :: Int
major = 0

minor :: Int
minor = 1

patch :: Int
patch = 6

version :: Version
version = makeVersion [major, minor, patch]

versionStr :: String
versionStr = showVersion version

versionHashStr :: String
versionHashStr = versionStr <> unsafeHashStr

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

compatibleVersionStr :: String
compatibleVersionStr = showVersion compatibleVersion

versionHeader :: String
versionHeader = "reach " ++ compatibleVersionStr
