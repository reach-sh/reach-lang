{-# LANGUAGE OverloadedStrings #-}

module Reach.EmitEVM where

--import qualified Data.Word as W
import qualified Data.HexString as H
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Numeric (showHex)

import Reach.AST
import Reach.EmitSol
  ( CompiledSol )

data EVM
  = EVM_ERR String
  | EVM_STOP
  | EVM_ADD
  | EVM_MUL
  | EVM_SUB
  | EVM_DIV
  | EVM_SDIV
  | EVM_MOD
  | EVM_SMOD
  | EVM_ADDMOD
  | EVM_MULMOD
  | EVM_EXP
  | EVM_SIGNEXTEND
  | EVM_LT
  | EVM_GT
  | EVM_SLT
  | EVM_SGT
  | EVM_EQ
  | EVM_ISZERO
  | EVM_AND
  | EVM_OR
  | EVM_XOR
  | EVM_NOT
  | EVM_BYTE
  | EVM_SHL
  | EVM_SHR
  | EVM_SAR
  | EVM_SHA3
  | EVM_ADDRESS
  | EVM_BALANCE
  | EVM_ORIGIN
  | EVM_CALLER
  | EVM_CALLVALUE
  | EVM_CALLDATALOAD
  | EVM_CALLDATASIZE
  | EVM_CALLDATACOPY
  | EVM_CODESIZE
  | EVM_CODECOPY
  | EVM_GASPRICE
  | EVM_EXTCODESIZE
  | EVM_EXTCODECOPY
  | EVM_RETURNDATASIZE
  | EVM_RETURNDATACOPY
  | EVM_EXTCODEHASH
  | EVM_BLOCKHASH
  | EVM_COINBASE
  | EVM_TIMESTAMP
  | EVM_NUMBER
  | EVM_DIFFICULTY
  | EVM_GASLIMIT
  | EVM_POP
  | EVM_MLOAD
  | EVM_MSTORE
  | EVM_MSTORE8
  | EVM_SLOAD
  | EVM_SSTORE
  | EVM_JUMP
  | EVM_JUMPI
  | EVM_PC
  | EVM_MSIZE
  | EVM_GAS
  | EVM_JUMPDEST
  | EVM_PUSH1 B.ByteString
  | EVM_PUSH2 B.ByteString
  | EVM_PUSH3 B.ByteString
  | EVM_PUSH4 B.ByteString
  | EVM_PUSH5 B.ByteString
  | EVM_PUSH6 B.ByteString
  | EVM_PUSH7 B.ByteString
  | EVM_PUSH8 B.ByteString
  | EVM_PUSH9 B.ByteString
  | EVM_PUSH10 B.ByteString
  | EVM_PUSH11 B.ByteString
  | EVM_PUSH12 B.ByteString
  | EVM_PUSH13 B.ByteString
  | EVM_PUSH14 B.ByteString
  | EVM_PUSH15 B.ByteString
  | EVM_PUSH16 B.ByteString
  | EVM_PUSH17 B.ByteString
  | EVM_PUSH18 B.ByteString
  | EVM_PUSH19 B.ByteString
  | EVM_PUSH20 B.ByteString
  | EVM_PUSH21 B.ByteString
  | EVM_PUSH22 B.ByteString
  | EVM_PUSH23 B.ByteString
  | EVM_PUSH24 B.ByteString
  | EVM_PUSH25 B.ByteString
  | EVM_PUSH26 B.ByteString
  | EVM_PUSH27 B.ByteString
  | EVM_PUSH28 B.ByteString
  | EVM_PUSH29 B.ByteString
  | EVM_PUSH30 B.ByteString
  | EVM_PUSH31 B.ByteString
  | EVM_PUSH32 B.ByteString
  | EVM_DUP1
  | EVM_DUP2
  | EVM_DUP3
  | EVM_DUP4
  | EVM_DUP5
  | EVM_DUP6
  | EVM_DUP7
  | EVM_DUP8
  | EVM_DUP9
  | EVM_DUP10
  | EVM_DUP11
  | EVM_DUP12
  | EVM_DUP13
  | EVM_DUP14
  | EVM_DUP15
  | EVM_DUP16
  | EVM_SWAP1
  | EVM_SWAP2
  | EVM_SWAP3
  | EVM_SWAP4
  | EVM_SWAP5
  | EVM_SWAP6
  | EVM_SWAP7
  | EVM_SWAP8
  | EVM_SWAP9
  | EVM_SWAP10
  | EVM_SWAP11
  | EVM_SWAP12
  | EVM_SWAP13
  | EVM_SWAP14
  | EVM_SWAP15
  | EVM_SWAP16
  | EVM_LOG0
  | EVM_LOG1
  | EVM_LOG2
  | EVM_LOG3
  | EVM_LOG4
  | EVM_CREATE
  | EVM_CALL
  | EVM_CALLCODE
  | EVM_RETURN
  | EVM_DELEGATECALL
  | EVM_CREATE2
  | EVM_STATICCALL
  | EVM_REVERT
  | EVM_SELFDESTRUCT  
  deriving (Eq, Ord, Show)

decode_evm :: B.ByteString -> [EVM]
decode_evm bs =
  if B.null bs then
    []
  else
    case opc of
      0x00 -> EVM_STOP : more
      0x01 -> EVM_ADD : more
      0x02 -> EVM_MUL : more
      0x03 -> EVM_SUB : more
      0x04 -> EVM_DIV : more
      0x05 -> EVM_SDIV : more
      0x06 -> EVM_MOD : more
      0x07 -> EVM_SMOD : more
      0x08 -> EVM_ADDMOD : more
      0x09 -> EVM_MULMOD : more
      0x0A -> EVM_EXP : more
      0x0B -> EVM_SIGNEXTEND : more
      0x10 -> EVM_LT : more
      0x11 -> EVM_GT : more
      0x12 -> EVM_SLT : more
      0x13 -> EVM_SGT : more
      0x14 -> EVM_EQ : more
      0x15 -> EVM_ISZERO : more
      0x16 -> EVM_AND : more
      0x17 -> EVM_OR : more
      0x18 -> EVM_XOR : more
      0x19 -> EVM_NOT : more
      0x1A -> EVM_BYTE : more
      0x1B -> EVM_SHL : more
      0x1C -> EVM_SHR : more
      0x1D -> EVM_SAR : more
      0x20 -> EVM_SHA3 : more
      0x30 -> EVM_ADDRESS : more
      0x31 -> EVM_BALANCE : more
      0x32 -> EVM_ORIGIN : more
      0x33 -> EVM_CALLER : more
      0x34 -> EVM_CALLVALUE : more
      0x35 -> EVM_CALLDATALOAD : more
      0x36 -> EVM_CALLDATASIZE : more
      0x37 -> EVM_CALLDATACOPY : more
      0x38 -> EVM_CODESIZE : more
      0x39 -> EVM_CODECOPY : more
      0x3A -> EVM_GASPRICE : more
      0x3B -> EVM_EXTCODESIZE : more
      0x3C -> EVM_EXTCODECOPY : more
      0x3D -> EVM_RETURNDATASIZE : more
      0x3E -> EVM_RETURNDATACOPY : more
      0x3F -> EVM_EXTCODEHASH : more
      0x40 -> EVM_BLOCKHASH : more
      0x41 -> EVM_COINBASE : more
      0x42 -> EVM_TIMESTAMP : more
      0x43 -> EVM_NUMBER : more
      0x44 -> EVM_DIFFICULTY : more
      0x45 -> EVM_GASLIMIT : more
      0x50 -> EVM_POP : more
      0x51 -> EVM_MLOAD : more
      0x52 -> EVM_MSTORE : more
      0x53 -> EVM_MSTORE8 : more
      0x54 -> EVM_SLOAD : more
      0x55 -> EVM_SSTORE : more
      0x56 -> EVM_JUMP : more
      0x57 -> EVM_JUMPI : more
      0x58 -> EVM_PC : more
      0x59 -> EVM_MSIZE : more
      0x5A -> EVM_GAS : more
      0x5B -> EVM_JUMPDEST : more
      0x60 -> next EVM_PUSH1 1
      0x61 -> next EVM_PUSH2 2
      0x62 -> next EVM_PUSH3 3
      0x63 -> next EVM_PUSH4 4
      0x64 -> next EVM_PUSH5 5
      0x65 -> next EVM_PUSH6 6
      0x66 -> next EVM_PUSH7 7
      0x67 -> next EVM_PUSH8 8
      0x68 -> next EVM_PUSH9 9
      0x69 -> next EVM_PUSH10 10
      0x6A -> next EVM_PUSH11 11
      0x6B -> next EVM_PUSH12 12
      0x6C -> next EVM_PUSH13 13
      0x6D -> next EVM_PUSH14 14
      0x6E -> next EVM_PUSH15 15
      0x6F -> next EVM_PUSH16 16
      0x70 -> next EVM_PUSH17 17
      0x71 -> next EVM_PUSH18 18
      0x72 -> next EVM_PUSH19 19
      0x73 -> next EVM_PUSH20 20
      0x74 -> next EVM_PUSH21 21
      0x75 -> next EVM_PUSH22 22
      0x76 -> next EVM_PUSH23 23
      0x77 -> next EVM_PUSH24 24
      0x78 -> next EVM_PUSH25 25
      0x79 -> next EVM_PUSH26 26
      0x7A -> next EVM_PUSH27 27
      0x7B -> next EVM_PUSH28 28
      0x7C -> next EVM_PUSH29 29
      0x7D -> next EVM_PUSH30 30
      0x7E -> next EVM_PUSH31 31
      0x7F -> next EVM_PUSH32 32
      0x80 -> EVM_DUP1 : more
      0x81 -> EVM_DUP2 : more
      0x82 -> EVM_DUP3 : more
      0x83 -> EVM_DUP4 : more
      0x84 -> EVM_DUP5 : more
      0x85 -> EVM_DUP6 : more
      0x86 -> EVM_DUP7 : more
      0x87 -> EVM_DUP8 : more
      0x88 -> EVM_DUP9 : more
      0x89 -> EVM_DUP10 : more
      0x8A -> EVM_DUP11 : more
      0x8B -> EVM_DUP12 : more
      0x8C -> EVM_DUP13 : more
      0x8D -> EVM_DUP14 : more
      0x8E -> EVM_DUP15 : more
      0x8F -> EVM_DUP16 : more
      0x90 -> EVM_SWAP1 : more
      0x91 -> EVM_SWAP2 : more
      0x92 -> EVM_SWAP3 : more
      0x93 -> EVM_SWAP4 : more
      0x94 -> EVM_SWAP5 : more
      0x95 -> EVM_SWAP6 : more
      0x96 -> EVM_SWAP7 : more
      0x97 -> EVM_SWAP8 : more
      0x98 -> EVM_SWAP9 : more
      0x99 -> EVM_SWAP10 : more
      0x9A -> EVM_SWAP11 : more
      0x9B -> EVM_SWAP12 : more
      0x9C -> EVM_SWAP13 : more
      0x9D -> EVM_SWAP14 : more
      0x9E -> EVM_SWAP15 : more
      0x9F -> EVM_SWAP16 : more
      0xA0 -> EVM_LOG0 : more
      0xA1 -> EVM_LOG1 : more
      0xA2 -> EVM_LOG2 : more
      0xA3 -> EVM_LOG3 : more
      0xA4 -> EVM_LOG4 : more
      0xF0 -> EVM_CREATE : more
      0xF1 -> EVM_CALL : more
      0xF2 -> EVM_CALLCODE : more
      0xF3 -> EVM_RETURN : more
      0xF4 -> EVM_DELEGATECALL : more
      0xF5 -> EVM_CREATE2 : more
      0xFA -> EVM_STATICCALL : more
      0xFD -> EVM_REVERT : more
      0xFF -> EVM_SELFDESTRUCT : more
      _ -> EVM_ERR (showHex opc "") : more
  where opc = B.head bs
        r = B.tail bs
        more = decode_evm r
        next o k = o ks : decode_evm rks
          where (ks, rks) = B.splitAt k r

emit_evm :: FilePath -> BLProgram a -> CompiledSol -> IO ()
emit_evm _ _ (_, code) = do
  let bs = H.toBytes $ H.hexString $ BC.pack code
  mapM_ (\o -> putStrLn $ show o) $ decode_evm bs
  return ()
