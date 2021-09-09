module Reach.Version
  ( version
  , versionStr
  , compatibleVersion
  , compatibleVersionStr
  , versionHeader
  , major
  , minor
  , patch
  ) where

import Data.Version (Version (..), makeVersion, showVersion)

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

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

compatibleVersionStr :: String
compatibleVersionStr = showVersion compatibleVersion

versionHeader :: String
versionHeader = "reach " ++ (showVersion compatibleVersion)
