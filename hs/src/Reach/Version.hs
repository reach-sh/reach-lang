module Reach.Version
  ( version
  , versionStr
  , compatibleVersion
  , versionHeader
  )
where

import Data.Version (Version (..), makeVersion, showVersion)

-- Note(Dan): Sorry, this is really dumb, but
-- Paths_reach breaks my flimsy ide tooling
-- See test/Reach/Test_Version where it is asserted that
-- this is up to date.
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
