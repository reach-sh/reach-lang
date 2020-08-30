module Reach.Version
  ( version
  , versionStr
  , compatibleVersion
  , versionHeader
  )
where

import Data.Version (Version (..), makeVersion, showVersion)

--- XXX Move into embedded files and get from /VERSION
version :: Version
version = makeVersion [0, 1, 2]

versionStr :: String
versionStr = showVersion version

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

versionHeader :: String
versionHeader = "reach " ++ (showVersion compatibleVersion)
