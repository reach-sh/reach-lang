module Reach.BackendConnector.ConsensusNetworkProgram where

import qualified Data.Map.Strict as M
import Reach.AST (ConsensusNetwork)
import Reach.Connector.ETH_Solidity
  ( CompiledSol )
import Reach.Connector.ALGO
  ( CompiledTeal )

-- ^ A specific program compiled to a given Consensus Network
data ConsensusNetworkProgram
  = CNP_ETH CompiledSol  -- Or EVM not compiled from Sol
  | CNP_ALGO CompiledTeal

type CNP_Map = M.Map ConsensusNetwork ConsensusNetworkProgram
