module Reach.Connector
  ( ConnectorInfoMap
  , ConnectorInfo
  , ConnectorResult
  , Connector (..)
  , Connectors
  , checkIntLiteralC
  , conWrite
  , conShowP
  , ConnectorError(..)
  )
where

import Data.Aeson (Object, Value)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Generics.Deriving hiding (conName)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Texty
import Reach.Util

type ConnectorInfoMap = Object

type ConnectorResult = Object

type ConnectorInfo = Value

data Connector = Connector
  { conName :: T.Text
  , conCons :: DLConstant -> DLLiteral
  , conGen :: Maybe (T.Text -> String) -> PLProg -> IO ConnectorInfo
  }

instance Eq Connector where
  l == r = conName l == conName r

instance Show Connector where
  show = T.unpack . conName

data ConnectorError
  = Err_IntLiteralRange Connector Integer Integer Integer
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode ConnectorError where
  errPrefix = const "RC"
  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If you delete a constructor, do NOT re-allocate the number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_IntLiteralRange {} -> 0

instance Show ConnectorError where
  show (Err_IntLiteralRange con rmin x rmax) =
    "integer literal out of range for " <> show con <> ": " <> show x <> " not in [" <> show rmin <> "," <> show rmax <> "]"

conWriteH :: (String -> a -> IO ()) -> Maybe (T.Text -> String) -> T.Text -> a -> IO ()
conWriteH doWrite moutn which c =
  case moutn of
    Nothing -> return ()
    Just outn -> doWrite (outn which) c

conWrite :: Maybe (T.Text -> String) -> T.Text -> T.Text -> IO ()
conWrite = conWriteH TIO.writeFile

conShowP :: Pretty a => Maybe (T.Text -> String) -> T.Text -> a -> IO ()
conShowP moutn which v =
  conWriteH LTIO.writeFile moutn which (render $ pretty v)

checkIntLiteralC :: SrcLoc -> Connector -> Integer -> Integer
checkIntLiteralC at c x =
  case rmin <= x && x <= rmax of
    True -> x
    False -> expect_thrown at $ Err_IntLiteralRange c rmin x rmax
  where
    rmin = 0
    rmax = case conCons c DLC_UInt_max of
      DLL_Int _ uim -> uim
      _ -> impossible "uint_max not int"

type Connectors =
  M.Map T.Text Connector
