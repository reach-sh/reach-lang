module Reach.BackendConnector.JS
  ( ConsensusNetworkProgram
  , CNP_Map
  , jsCnp
  , jsObject
  , jsBraces
  , str_as_hex
  ) where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc

import Reach.AST (ConsensusNetwork)
import Reach.BackendConnector.ConsensusNetworkProgram

str_as_hex :: String -> Doc a
str_as_hex x = pretty $ "\"0x" ++ x ++ "\""

teal_code_fmt :: String -> Doc a
teal_code_fmt x = pretty $ "`" ++ x ++ "`"

jsBraces :: Doc a -> Doc a
jsBraces body = braces (nest 2 $ hardline <> body <> space)

jsObject :: [(String, Doc a)] -> Doc a
jsObject kvs = jsBraces $ vsep $ (intersperse (comma <> hardline)) $ map jsObjField kvs
  where jsObjField (k, v) = pretty (k ++ ":") <+> v


-- ^ Render a ConsensusNetworkProgram as a JS object
jsCnpObj :: ConsensusNetworkProgram -> Doc a
jsCnpObj (CNP_ETH (abi, evm_code)) = jsObject
  [ ("ABI", pretty abi)
  , ("Bytecode", str_as_hex evm_code)]
jsCnpObj (CNP_ALGO (tc_lsp, tc_ap, tc_csp)) = jsObject
  [ ("LogicSigProgram", teal_code_fmt tc_lsp)
  , ("ApprovalProgram", teal_code_fmt tc_ap)
  , ("ClearStateProgram", teal_code_fmt tc_csp)]

-- ^ Render an exported variable for the given ConsensusNetworkProgram
jsCnp :: ConsensusNetwork -> ConsensusNetworkProgram -> Doc a
jsCnp name cnp = pretty "export const " <> pretty (show name) <> pretty " = " <> jsCnpObj cnp <> semi
