module Reach.Version
  ( version
  , versionStr
  , compatibleVersion
  , versionHeader
  )
where

import Data.Version (Version (..), makeVersion, showVersion)

--- FIXME make lemonade of this monstrosity by trying to unify all the
--- version numbers everywhere with some file at the root of the repo
--- named VERSION?
version :: Version
version = makeVersion [0, 1, 0]

versionStr :: String
versionStr = showVersion version

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

versionHeader :: String
versionHeader = "reach " ++ (showVersion compatibleVersion)
