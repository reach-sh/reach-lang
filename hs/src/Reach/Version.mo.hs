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
  , solcVersionStr
  ) where

import Data.Version (Version (..), makeVersion, showVersion)
import Reach.UnsafeUtil

solcVersionStr :: String
solcVersionStr = "{{SOLC_VERSION}}"

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
versionHashStr = versionStr <> unsafeHashStr

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

compatibleVersionStr :: String
compatibleVersionStr = showVersion compatibleVersion

versionHeader :: String
versionHeader = "reach " ++ compatibleVersionStr
