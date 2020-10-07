module Reach.Connector (ConnectorInfoMap, ConnectorInfo (..), ConnectorResult, Connector) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.AST

type ConnectorInfoMap
  = M.Map String ConnectorInfo

data ConnectorInfo
  = CI_Null
  | CI_Bool Bool
  | CI_Int Integer
  | CI_Text T.Text
  | CI_Array [ConnectorInfo]
  | CI_Obj ConnectorInfoMap

type ConnectorResult =
  M.Map String ConnectorInfo

type Connector = Maybe (T.Text -> String) -> PLProg -> IO ConnectorResult
