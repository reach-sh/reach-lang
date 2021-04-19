module Reach.Warning
  ( Warning (..)
  , Deprecation (..)
  , emitWarning
  )
where

import Data.Char (toLower, toUpper)
import Data.List.Extra (splitOn)
import Reach.AST.Base (SrcLoc)
import System.IO (hPutStrLn)
import System.IO.Extra (stderr)

capitalized :: String -> String
capitalized [] = []
capitalized (h : t) = toUpper h : map toLower t

camlCase :: String -> [String] -> String
camlCase _ [] = ""
camlCase acc (h : t) = acc <> capitalized h <> camlCase acc t

data Deprecation
  = D_ParticipantTuples SrcLoc
  | D_SnakeToCamelCase String
  deriving (Eq)

data Warning
  = W_Deprecated Deprecation
  deriving (Eq)

instance Show Deprecation where
  show = \case
    D_ParticipantTuples at ->
      "Declaring Participants with a tuple is now deprecated. "
        <> "Please use `Participant(name, interface)` or `ParticipantClass(name, interface)` at "
        <> show at
    D_SnakeToCamelCase name ->
      let name' = case splitOn "_" name of
            [] -> name
            h : t -> camlCase h t
       in "`" <> name <> "` is now deprecated. It has been renamed from snake case to camel case. Use `" <> name' <> "`"

instance Show Warning where
  show = \case
    W_Deprecated d -> show d

emitWarning :: Warning -> IO ()
emitWarning d =
  hPutStrLn stderr $ "WARNING: " <> show d
