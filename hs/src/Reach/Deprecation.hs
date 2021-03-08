module Reach.Deprecation (
  Deprecation(..),
  deprecated_warning
) where

import Reach.AST.Base (SrcLoc)
import System.IO (hPutStrLn)
import System.IO.Extra (stderr)
import Data.List.Extra (splitOn)
import Data.Char (toUpper, toLower)

capitalised :: String -> String
capitalised [] = []
capitalised x = toUpper (head x) : map toLower (tail x)

data Deprecation
  = Deprecated_ParticipantTuples SrcLoc
  | Deprecated_SnakeToCamelCase String
  deriving (Eq)

instance Show Deprecation where
  show = \case
    Deprecated_ParticipantTuples at ->
      "Declaring Participants with a tuple is now deprecated. "
        <> "Please use `Participant(name, interface)` or `ParticipantClass(name, interface)` at "
        <> show at
    Deprecated_SnakeToCamelCase name ->
      let name' = foldl1 (\ acc w -> acc <> capitalised w) $ splitOn "_" name in
      "`" <> name <> "` is now deprecated. It has been renamed from snake case to camel case. Use `" <> name' <> "`"

deprecated_warning :: Deprecation -> IO ()
deprecated_warning d =
  hPutStrLn stderr $ "WARNING: " <> show d
