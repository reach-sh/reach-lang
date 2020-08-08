module Reach.Backend (Backend) where

import qualified Data.Text as T
import Reach.Connector
import Reach.NL_AST

type Backend = (T.Text -> String) -> ConnectorResult -> PLProg -> IO ()
