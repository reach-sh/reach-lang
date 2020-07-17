{-# LANGUAGE OverloadedStrings #-}
module Reach.ConsensusNetworkProgram where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Reach.AST (ConsensusNetwork, CNP_TMap)
import Reach.Connector.ETH_Solidity
  ( CompiledSol )
import Reach.Connector.ALGO
  ( CompiledTeal )
import Reach.Util

-- ^ A specific program compiled to a given Consensus Network
data ConsensusNetworkProgram
  = CNP_ETH CompiledSol  -- Or EVM not compiled from Sol
  | CNP_ALGO CompiledTeal

type CNP_Map = M.Map ConsensusNetwork ConsensusNetworkProgram

cnpToFieldMap :: CNP_Map -> CNP_TMap
cnpToFieldMap  = M.mapKeys tshow . fmap cnpToFields where

cnpToFields :: ConsensusNetworkProgram -> M.Map T.Text T.Text
cnpToFields (CNP_ETH (abi, bc)) = M.fromList
  [ ("ABI", T.pack abi)
  , ("Bytecode", "0x" <> T.pack bc) ]
cnpToFields (CNP_ALGO (tc_lsp, tc_ap, tc_csp)) = M.fromList
  [ ("LogicSigProgram", T.pack tc_lsp)
  , ("ApprovalProgram", T.pack tc_ap)
  , ("ClearStateProgram", T.pack tc_csp) ]
