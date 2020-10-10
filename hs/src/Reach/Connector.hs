module Reach.Connector
  ( ConnectorInfoMap
  , ConnectorInfo (..)
  , ConnectorResult
  , Connector (..)
  , Connectors
  , checkIntLiteralC
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Reach.AST
import Reach.Type
import Reach.Util

type ConnectorInfoMap =
  M.Map String ConnectorInfo

data ConnectorInfo
  = CI_Null
  | CI_Bool Bool
  | CI_Int Integer
  | CI_Text T.Text
  | CI_Array [ConnectorInfo]
  | CI_Obj ConnectorInfoMap
  deriving (Generic, NFData)

data Connector = Connector
  { conName :: String
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
  M.Map String Connector

type ConnectorResult =
  M.Map String ConnectorInfo
