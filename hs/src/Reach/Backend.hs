module Reach.Backend (Backend) where

import qualified Data.Text as T
import Reach.AST
import Reach.Connector

type Backend = (T.Text -> String) -> ConnectorResult -> PLProg -> IO ()
