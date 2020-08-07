module Reach.Connector.ALGO_NL where

import Debug.Trace
--- import Reach.NL_AST
import Reach.Connector

connect_algo :: Connector
connect_algo _outn _pl = do
  traceM $ "XXX connect_algo"
  return $ mempty
