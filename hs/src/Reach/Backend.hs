module Reach.Backend (Backend) where

import Reach.AST.EP
import Reach.Connector
import Reach.OutputUtil

type Backend = Outputer -> ConnectorObject -> EPProg -> IO ()
