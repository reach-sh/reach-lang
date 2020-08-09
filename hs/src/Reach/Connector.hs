module Reach.Connector (ConnectorResult, Connector) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.NL_AST

type ConnectorResult =
  M.Map String (M.Map String T.Text)

type Connector = (T.Text -> String) -> PLProg -> IO ConnectorResult
