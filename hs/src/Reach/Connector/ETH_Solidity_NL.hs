module Reach.Connector.ETH_Solidity_NL where

import Debug.Trace
--- import Reach.NL_AST
import Reach.Connector

connect_eth :: Connector
connect_eth _outn _pl = do
  traceM $ "XXX connect_eth"
  return $ mempty
