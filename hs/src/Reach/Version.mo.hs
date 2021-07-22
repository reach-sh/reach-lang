module Reach.Version
  ( version
  , versionStr
  , compatibleVersion
  , compatibleVersionStr
  , versionHeader
  )
where

import Data.Version (Version (..), makeVersion, showVersion)

version :: Version
version = makeVersion [{{MAJOR}}, {{MINOR}}, {{PATCH}}]

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
