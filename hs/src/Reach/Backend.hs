module Reach.Backend (Backend) where

import qualified Data.Text as T
import Reach.Connector
import Reach.AST

type Backend = (T.Text -> String) -> ConnectorResult -> PLProg -> IO ()
