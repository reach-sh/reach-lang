module Reach.Connector (ConnectorInfo, ConnectorResult, Connector) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.AST

type ConnectorInfo =
  M.Map String T.Text

type ConnectorResult =
  M.Map String ConnectorInfo

type Connector = Maybe (T.Text -> String) -> PLProg -> IO ConnectorResult
