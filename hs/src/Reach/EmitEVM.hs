{-# LANGUAGE OverloadedStrings #-}

module Reach.EmitEVM where

--import qualified Data.Word as W
import qualified Data.HexString as H
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified EVM.Bytecode as EVM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import Data.Foldable

import Reach.AST
import Reach.EmitSol
  ( CompiledSol )

data LabelInfo = LabelInfo Int Int
data ASMOp a b
  = Op b
  | LabelRef a Int (LabelInfo -> b)
type ASMSeq a b = [ASMOp a b]
data ASMProg a b = ASMProg (M.Map a (ASMSeq a b)) (ASMSeq a b)

evm_op_len :: EVM.Opcode -> Int
evm_op_len o = length $ EVM.encode [o]

assemble :: Show l => Ord l => (o -> Int) -> ASMProg l o -> [o]
assemble op_len (ASMProg defs main) = map asmfix $ toList image
  where asmfix ao =
          case ao of
            Op o -> o
            LabelRef l _ f ->
              case M.lookup l locs of
                Nothing -> error $ "Undefined label in assembly: " ++ show l
                Just li -> f li
        seq_length = sum . (map op_length)
        op_length (Op o) = op_len o
        op_length (LabelRef _ l _) = l
        (_, locs, image) = M.foldrWithKey loc1 (main_sz, M.empty, Q.fromList main) defs
        main_sz = seq_length main
        loc1 label body (start, locs0, before) = (end, locs1, after)
          where sz = seq_length body
                body_seq = Q.fromList body
                end = start + sz
                locs1 = M.insert label (LabelInfo start sz) locs0
                after = before Q.>< body_seq

push_label_offset :: a -> ASMOp a EVM.Opcode
push_label_offset l = LabelRef l 2 (\ (LabelInfo off _) -> (EVM.PUSH1 [fromIntegral off]))
push_label_size :: a -> ASMOp a EVM.Opcode
push_label_size l = LabelRef l 2 (\ (LabelInfo _ sz) -> (EVM.PUSH1 [fromIntegral sz]))

cp_to_evm :: CProgram a -> [EVM.Opcode]
cp_to_evm (C_Prog _ hs) = con_bc
  where con_bc = assemble evm_op_len $ ASMProg (M.singleton ins_l ins_as) con_as
        ins_l :: Int
        ins_l = 0
        con_as = [ --- XXX initialize state
                   push_label_size ins_l
                 , push_label_offset ins_l
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.CODECOPY)
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.RETURN)
                 , Op (EVM.INVALID 254) ]
        ins_as = map Op $ assemble evm_op_len $ ASMProg hdefs dis_as
        dis_as = [ --- XXX Do something real 
                   Op (EVM.PUSH1 [0])
                 , Op (EVM.RETURN)
                 , Op (EVM.INVALID 254) ]
        hdefs = foldl hcomp1 M.empty hs
        hcomp1 hdefs0 h = M.insert hi h_as hdefs0
          --- XXX do something real
          where hi = case h of (C_Handler _ _ _ _ _ _ _ i) -> i
                               (C_Loop _ _ _ _ _ i) -> i
                h_as = dis_as

emit_evm :: FilePath -> BLProgram a -> CompiledSol -> IO ()
emit_evm _ (BL_Prog _ _ cp) (_, code) =
  if False then
    do
      let bs = H.toBytes $ H.hexString $ BC.pack code
      mapM_ (\o -> putStrLn $ show o) $ EVM.decode $ B.unpack bs
      return ()
  else
    if True then
      do
        mapM_ (\o -> putStrLn $ show o) $ cp_to_evm cp
        return ()
    else
      return ()
