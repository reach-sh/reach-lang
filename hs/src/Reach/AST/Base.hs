{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.Base where

import Control.DeepSeq (NFData)
import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as LB
import Data.List
import qualified Data.Text as T
import GHC.Generics
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Reach.JSOrphans ()
import Reach.UnsafeUtil

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

-- This is a "defaulting" instance where the left info is preferred,
-- but can fall back on the right if info is absent from the left.
instance Semigroup SrcLoc where
  SrcLoc mlab mpos msrc <> SrcLoc mlab' mpos' msrc' =
    SrcLoc (firstJust mlab mlab') (firstJust mpos mpos') (firstJust msrc msrc')
    where
      firstJust (Just x) _ = Just x
      firstJust _ (Just y) = Just y
      firstJust Nothing Nothing = Nothing

instance Monoid SrcLoc where
  mempty = SrcLoc Nothing Nothing Nothing

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ intersperse ":" $ concat [sr, loc, lab]
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

data ImpossibleError
  = Err_Impossible String
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance Show ImpossibleError where
  show = \case
    Err_Impossible msg -> msg

data CompilationError = CompilationError
  { ce_suggestions :: [String]
  , ce_errorMessage :: String
  , ce_position :: [Int]
  , ce_offendingToken :: Maybe String
  }
  deriving (Show, Generic, ToJSON)

class ErrorMessageForJson a where
  errorMessageForJson :: Show a => a -> String
  errorMessageForJson = show

class ErrorSuggestions a where
  errorSuggestions :: a -> (Maybe String, [String])
  errorSuggestions _ = (Nothing, [])

srcloc_line_col :: SrcLoc -> [Int]
srcloc_line_col (SrcLoc _ (Just (TokenPn _ l c)) _) = [l, c]
srcloc_line_col _ = []

expect_throw :: (Show a, ErrorMessageForJson a, ErrorSuggestions a) => HasCallStack => Maybe ([SLCtxtFrame]) -> SrcLoc -> a -> b
expect_throw mCtx src ce =
  case unsafeIsErrorFormatJson of
    True ->
      error $
        "error: "
          ++ (map w2c $
                LB.unpack $
                  encode $
                    CompilationError
                      { ce_suggestions = snd $ errorSuggestions ce
                      , ce_offendingToken = fst $ errorSuggestions ce
                      , ce_errorMessage = errorMessageForJson ce
                      , ce_position = srcloc_line_col src
                      })
    False ->
      error . T.unpack . unsafeRedactAbs . T.pack $
        "error: " ++ (show src) ++ ": " ++ (take 512 $ show ce)
          <> case concat mCtx of
            [] -> ""
            ctx -> "\nTrace:\n" <> intercalate "\n" (topOfStackTrace ctx)

expect_thrown :: (Show a, ErrorMessageForJson a, ErrorSuggestions a) => HasCallStack => SrcLoc -> a -> b
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

srcloc_top :: SrcLoc
srcloc_top = SrcLoc (Just "<top level>") Nothing Nothing

srcloc_src :: ReachSource -> SrcLoc
srcloc_src rs = SrcLoc Nothing Nothing (Just rs)

get_srcloc_src :: SrcLoc -> ReachSource
get_srcloc_src (SrcLoc _ _ (Just rs)) = rs
get_srcloc_src (SrcLoc _ _ Nothing) = ReachSourceFile "src" -- FIXME

srcloc_at :: String -> (Maybe TokenPosn) -> SrcLoc -> SrcLoc
srcloc_at lab mp (SrcLoc _ _ rs) = SrcLoc (Just lab) mp rs

class SrcLocOf a where
  srclocOf :: a -> SrcLoc

--- Security Levels
data SecurityLevel
  = Secret
  | Public
  deriving (Eq, Generic, NFData, Show)

public :: a -> (SecurityLevel, a)
public x = (Public, x)

secret :: a -> (SecurityLevel, a)
secret x = (Secret, x)

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
  | SELF_ADDRESS
  | LSH
  | RSH
  | BAND
  | BIOR
  | BXOR
  deriving (Eq, Generic, NFData, Ord, Show)

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Ord, Generic, NFData)

instance Show SLCtxtFrame where
  show (SLC_CloApp call_at clo_at mname) =
    "at " ++ show call_at ++ " call to " ++ name ++ " (defined at: " ++ show clo_at ++ ")"
    where
      name = maybe "[unknown function]" show mname

instance SrcLocOf SLCtxtFrame where
  srclocOf (SLC_CloApp at _ _) = at
