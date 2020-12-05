module Reach.Connector
  ( ConnectorInfoMap
  , ConnectorInfo
  , ConnectorResult
  , Connector (..)
  , Connectors
  , checkIntLiteralC
  )
where

import Data.Aeson (Object, Value)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Type
import Reach.Util

type ConnectorInfoMap = Object

type ConnectorResult = Object

type ConnectorInfo = Value

data Connector = Connector
  { conName :: T.Text
  , conCons :: DLConstant -> DLLiteral
  , conGen :: Maybe (T.Text -> String) -> PLProg -> IO ConnectorInfo
  }

checkIntLiteralC :: SrcLoc -> Connector -> Integer -> Integer
checkIntLiteralC at c x = checkIntLiteral at 0 x rmax
  where
    rmax = case conCons c DLC_UInt_max of
      DLL_Int _ uim -> uim
      _ -> impossible "uint_max not int"

type Connectors =
  M.Map T.Text Connector
