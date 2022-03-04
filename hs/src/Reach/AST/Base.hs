{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reach.AST.Base where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics
import GHC.Stack (callStack, HasCallStack, prettyCallStack)
import Language.JavaScript.Parser
import Reach.JSOrphans ()
import Reach.Pretty
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util (makeErrCode)
import Safe (atMay)
import qualified System.Console.Pretty as TC
import Control.Exception (Exception, throw)

--- Source Information
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq, Generic, NFData, Ord)

instance Show ReachSource where
  show ReachStdLib = "reach standard library"
  show (ReachSourceFile fp) = fp

data SrcLoc = SrcLoc (Maybe String) (Maybe TokenPosn) (Maybe ReachSource)
  deriving (Eq, Generic, NFData, Ord)

instance FromJSON TokenPosn

instance ToJSON TokenPosn

instance FromJSON ReachSource

instance ToJSON ReachSource

instance ToJSON SrcLoc where
  toJSON v = toJSON $ show v
instance FromJSON SrcLoc

-- This is a "defaulting" instance where the left info is preferred,
-- but can fall back on the right if info is absent from the left.
instance Semigroup SrcLoc where
  SrcLoc mlab mpos msrc <> SrcLoc mlab' mpos' msrc' =
    SrcLoc (mlab <|> mlab') (mpos <|> mpos') (msrc <|> msrc')

instance Monoid SrcLoc where
  mempty = SrcLoc Nothing Nothing Nothing

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ List.intersperse ":" $ concat [sr, loc, lab]
    where
      lab = case mlab of
        Nothing -> []
        Just s -> [s]
      sr = case mrs of
        Nothing -> []
        Just s -> [show s]
      loc = case mtp of
        Nothing -> []
        Just (TokenPn _ l c) -> [show l, show c]

instance Pretty SrcLoc where
  pretty = viaShow

data ImpossibleError
  = Err_Impossible_Inspect String
  deriving (Eq, Ord, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode ImpossibleError where
  errPrefix = const "RX"

  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If you delete a constructor, do NOT re-allocate the number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_Impossible_Inspect {} -> 0

instance Show ImpossibleError where
  show = \case
    Err_Impossible_Inspect f ->
      "Cannot inspect value from `" <> f <> "`"

instance Pretty ImpossibleError where
  pretty = viaShow

class ErrorMessageForJson a where
  errorMessageForJson :: Show a => a -> String
  errorMessageForJson = show

class ErrorSuggestions a where
  errorSuggestions :: a -> (Maybe String, [String])
  errorSuggestions _ = (Nothing, [])

srcloc_line_col :: SrcLoc -> [Int]
srcloc_line_col (SrcLoc _ (Just (TokenPn _ l c)) _) = [l, c]
srcloc_line_col _ = []

getSrcLine :: Maybe Int -> [String] -> Maybe String
getSrcLine rowNum fl =
  rowNum >>= (\r -> atMay fl $ r - 1)

errorCodeDocUrl :: HasErrorCode a => a -> String
errorCodeDocUrl e =
  "https://docs.reach.sh/rsh/errors/#" <> errCode e

getErrorMessage :: (HasErrorCode a, Show a, Foldable t) => t [SLCtxtFrame] -> SrcLoc -> Bool -> a -> String
getErrorMessage mCtx src isWarning ce = do
  let hasColor = unsafeTermSupportsColor
  let color :: TC.Color -> [Char] -> [Char]
      color c = if hasColor then TC.color c else id
  let style :: TC.Style -> [Char] -> [Char]
      style s = if hasColor then TC.style s else id
  let fileLines = srcloc_file src >>= Just . unsafeReadFile
  let rowNum = case srcloc_line_col src of
        [l, _] -> Just l
        _ -> Nothing
  let rowNumStr = pretty $ maybe "" (style TC.Bold . color TC.Cyan . show) rowNum
  let fileLine =
        maybe "" (\l -> rowNumStr <> "|" <+> (pretty $ style TC.Faint l)) $
          getSrcLine rowNum (fromMaybe [] fileLines)
  let errType = if isWarning then "warning" else "error"
  let errColor = if isWarning then color TC.Yellow else color TC.Red
  let errDesc = pretty (style TC.Bold $ errColor errType) <> brackets (pretty $ style TC.Bold $ errCode ce) <> ":" <+> (pretty $ take 512 $ show ce)
  let srcCodeAt = nest $ hardline <> pretty (style TC.Bold (show src))
  let srcCodeLine = nest $ hardline <> pretty fileLine
  let stackTrace =
        case concat mCtx of
          [] -> ""
          ctx ->
            hardline <> (pretty $ style TC.Bold "Trace") <> ":" <> hardline
              <> concatWith (surround hardline) (map pretty $ topOfStackTrace ctx)
              <> hardline
  let docsUrl = "For further explanation of this " <> pretty errType <> ", see: " <> pretty (style TC.Underline $ errorCodeDocUrl ce) <> hardline
  T.unpack . unsafeRedactAbs . T.pack . show $
    errDesc <> hardline
      <> srcCodeAt
      <> hardline
      <> srcCodeLine
      <> hardline
      <> stackTrace
      <> hardline
      <> docsUrl


data CompilationError = CompilationError
  { ce_suggestions :: [String]
  , ce_errorMessage :: String
  , ce_position :: [Int]
  , ce_offendingToken :: Maybe String
  , ce_errorCode :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

makeCompilationError :: (ErrorSuggestions a, ErrorMessageForJson a, Show a, HasErrorCode a) => SrcLoc -> a -> CompilationError
makeCompilationError src err =
  CompilationError
    { ce_suggestions = snd $ errorSuggestions err
    , ce_offendingToken = fst $ errorSuggestions err
    , ce_errorMessage = errorMessageForJson err
    , ce_position = srcloc_line_col src
    , ce_errorCode = makeErrCode (errPrefix err) (errIndex err)
    }

encodeJSONString :: ToJSON a => a -> String
encodeJSONString = map w2c . LB.unpack . encode

makeErrorJson :: (ErrorSuggestions a, ErrorMessageForJson a, Show a, HasErrorCode a) => SrcLoc -> a -> String
makeErrorJson src err = encodeJSONString $ makeCompilationError src err

data CompileErrorException = CompileErrorException
  { cee_error :: CompilationError
  , cee_pretty :: String
  }

instance Show CompileErrorException where
  show =
    case unsafeIsErrorFormatJson of
      True -> ("error: " ++) . encodeJSONString
      False -> cee_pretty

instance Exception CompileErrorException

instance ToJSON CompileErrorException where
  toJSON = toJSON . cee_error

instance FromJSON CompileErrorException where
  parseJSON = withObject "CompileErrorException" $ \v -> CompileErrorException
    <$> parseJSON (Object v)
    <*> pure ""

expect_throw :: (HasErrorCode a, Show a, ErrorMessageForJson a, ErrorSuggestions a) => HasCallStack => Maybe ([SLCtxtFrame]) -> SrcLoc -> a -> b
expect_throw mCtx src err = throw CompileErrorException {..}
  where
    cee_pretty = getErrorMessage mCtx src False err ++ "\n" ++ prettyCallStack callStack
    cee_error = makeCompilationError src err

expect_thrown :: (HasErrorCode a, Show a, ErrorMessageForJson a, ErrorSuggestions a) => HasCallStack => SrcLoc -> a -> b
expect_thrown = expect_throw Nothing

topOfStackTrace :: [SLCtxtFrame] -> [String]
topOfStackTrace stack
  | length stackMsgs > 10 = take 10 stackMsgs <> ["  ..."]
  | otherwise = stackMsgs
  where
    stackMsgs = map getStackTraceMessage stack

-- Mimic Node's stack trace message
getStackTraceMessage :: SLCtxtFrame -> String
getStackTraceMessage (SLC_CloApp call_at clo_at name) =
  "  in " <> maybe "[unknown function]" show name <> " from (" <> show clo_at <> ")" <> " at (" <> show call_at <> ")"

srcloc_builtin :: SrcLoc
srcloc_builtin = SrcLoc (Just "<builtin>") Nothing Nothing

sb :: SrcLoc
sb = srcloc_builtin

srcloc_top :: SrcLoc
srcloc_top = SrcLoc (Just "<top level>") Nothing Nothing

srcloc_src :: ReachSource -> SrcLoc
srcloc_src rs = SrcLoc Nothing Nothing (Just rs)

get_srcloc_src :: SrcLoc -> ReachSource
get_srcloc_src (SrcLoc _ _ (Just rs)) = rs
get_srcloc_src (SrcLoc _ _ Nothing) = ReachSourceFile "src" -- FIXME

srcloc_at :: String -> (Maybe TokenPosn) -> SrcLoc -> SrcLoc
srcloc_at lab mp (SrcLoc _ _ rs) = SrcLoc (Just lab) mp rs

srcloc_lab :: String -> SrcLoc -> SrcLoc
srcloc_lab lab (SrcLoc _ tp rs) = SrcLoc (Just lab) tp rs

srcloc_file :: SrcLoc -> Maybe FilePath
srcloc_file = \case
  SrcLoc _ _ (Just (ReachSourceFile f)) -> Just f
  _ -> Nothing

class SrcLocOf a where
  srclocOf :: a -> SrcLoc

srclocOf_ :: SrcLocOf a => SrcLoc -> a -> SrcLoc
srclocOf_ def v = a'
  where
    a = srclocOf v
    a' = if a == sb then def else a

--- Security Levels
data SecurityLevel
  = Secret
  | Public
  deriving (Eq, Generic, NFData, Show)

public :: a -> (SecurityLevel, a)
public x = (Public, x)

secret :: a -> (SecurityLevel, a)
secret x = (Secret, x)

instance Pretty SecurityLevel where
  pretty = \case
    Public -> "public"
    Secret -> "secret"

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

lvlMeet :: SecurityLevel -> (SecurityLevel, a) -> (SecurityLevel, a)
lvlMeet lvl (lvl', x) = (lvl <> lvl', x)

instance Monoid SecurityLevel where
  mempty = Public

--- Static Language
type SLVar = String

type SLPart = B.ByteString

render_sp :: SLPart -> Doc
render_sp = viaShow

data PrimOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PLT
  | PLE
  | PEQ
  | PGE
  | PGT
  | IF_THEN_ELSE
  | DIGEST_EQ
  | ADDRESS_EQ
  | TOKEN_EQ
  | SELF_ADDRESS SLPart Bool Int
  | LSH
  | RSH
  | BAND
  | BIOR
  | BXOR
  | BYTES_ZPAD Integer
  | MUL_DIV
  deriving (Eq, Generic, NFData, Ord, Show)

instance Pretty PrimOp where
  pretty = \case
    ADD -> "+"
    SUB -> "-"
    MUL -> "*"
    DIV -> "/"
    MOD -> "%"
    PLT -> "<"
    PLE -> "<="
    PEQ -> "=="
    PGE -> ">="
    PGT -> ">"
    IF_THEN_ELSE -> "ite"
    DIGEST_EQ -> "=="
    ADDRESS_EQ -> "=="
    TOKEN_EQ -> "=="
    SELF_ADDRESS x y z -> "selfAddress" <> parens (render_das [pretty x, pretty y, pretty z])
    LSH -> "<<"
    RSH -> ">>"
    BAND -> "&"
    BIOR -> "|"
    BXOR -> "^"
    BYTES_ZPAD x -> "zpad" <> parens (pretty x)
    MUL_DIV -> "muldiv"

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Ord, Generic, NFData)

instance FromJSON SLCtxtFrame
instance ToJSON SLCtxtFrame where
  toJSON v = toJSON $ show v

instance Show SLCtxtFrame where
  show (SLC_CloApp call_at clo_at mname) =
    "at " ++ show call_at ++ " call to " ++ name ++ " (defined at: " ++ show clo_at ++ ")"
    where
      name = maybe "[unknown function]" show mname

instance SrcLocOf SLCtxtFrame where
  srclocOf (SLC_CloApp at _ _) = at

class Generic a => HasErrorCode a where
  errPrefix :: a -> String
  errIndex :: a -> Int
  errCode :: a -> String
  errCode e = makeErrCode (errPrefix e) (errIndex e)
