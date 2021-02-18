module Reach.Backend (Backend) where

import qualified Data.Text as T
import Reach.AST.PL
import Reach.Connector

type Backend = (T.Text -> String) -> ConnectorResult -> PIProg -> IO ()
