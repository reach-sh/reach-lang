module Reach.Warning
  ( Warning (..)
  , Deprecation (..)
  , emitWarning
  )
where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Reach.AST.Base (SrcLoc)
import System.IO (hPutStrLn)
import System.IO.Extra (stderr)
import Reach.UnsafeUtil (unsafeTermSupportsColor)
import qualified System.Console.Pretty as TC

capitalized :: String -> String
capitalized [] = []
capitalized (h : t) = toUpper h : map toLower t

camlCase :: String -> [String] -> String
camlCase _ [] = ""
camlCase acc (h : t) = acc <> capitalized h <> camlCase acc t

data Deprecation
  = D_ParticipantTuples SrcLoc
  | D_SnakeToCamelCase String
  | D_ReachAppArgs
  | D_UntypedTimeArg
  deriving (Eq)

data Warning
  = W_Deprecated Deprecation
  | W_SolidityOptimizeFailure String
  | W_ALGOUnsupported [String]
  | W_ALGOConservative [String]
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
    D_ReachAppArgs ->
      "Declaring a `Reach.App` with 3 arguments is now deprecated. Please specify one thunk."
    D_UntypedTimeArg ->
      "Using a bare value as a time argument is now deprecated. Please use relativeTime, absoluteTime, relativeSecs, or absoluteSecs."

instance Show Warning where
  show = \case
    W_Deprecated d -> show d
    W_SolidityOptimizeFailure msg ->
      "The Solidity compiler, run with optimization, fails on this program, but succeeds without optimization. This indicates a problem with Solidity that Reach is not working around; typically, because it is not possible to do so. You could report this error to Solidity (or Reach). If you do so, this is the message from Solidity:\n" <> msg
    W_ALGOUnsupported rs ->
      "Compiler instructed to emit for Algorand, but we can statically determine that this program will not work on Algorand, because:\n" <> (intercalate "\n" $ map (" * " <>) rs)
    W_ALGOConservative rs ->
      "Compiler instructed to emit for Algorand, but the conservative analysis found these potential problems:\n" <> (intercalate "\n" $ map (" * " <>) rs)

emitWarning :: Warning -> IO ()
emitWarning d = do
  let hasColor = unsafeTermSupportsColor
  let style s = if hasColor then TC.style s else id
  let color s = if hasColor then TC.color s else id
  hPutStrLn stderr $ style TC.Bold (color TC.Yellow "WARNING") <> ": " <> show d
