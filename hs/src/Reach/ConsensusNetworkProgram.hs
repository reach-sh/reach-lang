{-# LANGUAGE OverloadedStrings #-}
module Reach.ConsensusNetworkProgram where

import qualified Data.Map.Strict as M
import Reach.AST (ConsensusNetwork, CNP_TMap)
import Reach.Connector.ETH_Solidity
  ( CompiledSol )
import Reach.Connector.ALGO
  ( CompiledTeal )
import qualified Data.Text as T

-- ^ A specific program compiled to a given Consensus Network
data ConsensusNetworkProgram
  = CNP_ETH CompiledSol  -- Or EVM not compiled from Sol
  | CNP_ALGO CompiledTeal

type CNP_Map = M.Map ConsensusNetwork ConsensusNetworkProgram

cnpToFieldMap :: CNP_Map -> CNP_TMap
cnpToFieldMap  = M.mapKeys tshow . fmap cnpToFields where
  tshow = T.pack . show

cnpToFields :: ConsensusNetworkProgram -> M.Map T.Text T.Text
cnpToFields (CNP_ETH (abi, bc)) = M.fromList
  [ ("ABI", T.pack abi)
  , ("Bytecode", ebc_fmt bc) ]
  where
    ebc_fmt x = "\"0x" <> T.pack x <> "\""
cnpToFields (CNP_ALGO (tc_lsp, tc_ap, tc_csp)) = M.fromList
  [ ("LogicSigProgram", tbc_fmt tc_lsp)
  , ("ApprovalProgram", tbc_fmt tc_ap)
  , ("ClearStateProgram", tbc_fmt tc_csp) ]
  where
    tbc_fmt x = "`" <> T.pack x <> "`"
