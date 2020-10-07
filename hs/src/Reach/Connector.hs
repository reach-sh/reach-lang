module Reach.Connector
  ( ConnectorInfoMap
  , ConnectorInfo (..)
  , ConnectorResult
  , Connector (..)
  , Connectors
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.AST

type ConnectorInfoMap =
  M.Map String ConnectorInfo

data ConnectorInfo
  = CI_Null
  | CI_Bool Bool
  | CI_Int Integer
  | CI_Text T.Text
  | CI_Array [ConnectorInfo]
  | CI_Obj ConnectorInfoMap

data Connector = Connector
  { conName :: String
  , conGen :: Maybe (T.Text -> String) -> PLProg -> IO ConnectorInfo
  }

type Connectors =
  M.Map String Connector

type ConnectorResult =
  M.Map String ConnectorInfo
