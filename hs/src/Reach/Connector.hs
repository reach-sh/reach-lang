module Reach.Connector where

import Reach.NL_AST
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type ConnectorResult =
  M.Map String (M.Map String T.Text)

type Connector = (T.Text -> String) -> PLProg -> IO ConnectorResult
