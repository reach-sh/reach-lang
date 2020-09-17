module GenReach.EmbeddedFiles (usage_txt) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (Text, fromStrict)

usage_txt :: Text
usage_txt =
  fromStrict . decodeUtf8 $
    $(makeRelativeToProject "./templates/USAGE.txt" >>= embedFile)
