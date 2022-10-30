module Reach.Backend (Backend) where

import qualified Data.Text as T
import Reach.AST.EP
import Reach.Connector

type Backend = (T.Text -> String) -> ConnectorObject -> EPProg -> IO ()
