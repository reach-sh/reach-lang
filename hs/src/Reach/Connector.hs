module Reach.Connector where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

type ConnectorResult
  = M.Map String (M.Map String T.Text)
