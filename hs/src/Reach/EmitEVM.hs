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
type ASMLabels a b = (M.Map a (ASMSeq a b))
data ASMProg a b = ASMProg (ASMLabels a b) a (ASMSeq a b)

asm_union :: Ord a => ASMProg a b -> ASMProg a b -> ASMProg a b
asm_union main extra = ASMProg combined main_lab main_as
  where ASMProg main_defs main_lab main_as = main
        ASMProg extra_defs extra_lab extra_as = extra
        combined = M.union main_defs $ M.insert extra_lab extra_as extra_defs

asm_cat :: Ord a => ASMProg a b -> ASMProg a b -> ASMProg a b
asm_cat main extra = ASMProg combined main_lab combined_as
  where ASMProg main_defs main_lab main_as = main
        ASMProg extra_defs _ extra_as = extra
        combined_as = main_as ++ extra_as
        combined = M.union main_defs extra_defs

--- XXX Make an "encodedLength" type-class to do this more efficiently
evm_op_len :: EVM.Opcode -> Int
evm_op_len o = length $ EVM.encode [o]

assemble :: Show l => Ord l => (o -> Int) -> ASMProg l o -> [o]
assemble op_len (ASMProg defs _ main) = map asmfix $ toList image
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

data EVMLabel
  = EL_Handler Int
  | EL_New
  | EL_Dispatch
  | EL_Cond EVMLabel
  | EL_Arg
  deriving (Show, Eq, Ord)

push_label_offset :: a -> ASMOp a EVM.Opcode
push_label_offset l = LabelRef l 2 (\ (LabelInfo off _) -> (EVM.PUSH1 [fromIntegral off]))
push_label_size :: a -> ASMOp a EVM.Opcode
push_label_size l = LabelRef l 2 (\ (LabelInfo _ sz) -> (EVM.PUSH1 [fromIntegral sz]))

type CompileState = ()
--- XXX This needs to help use figure out how to read a variable from
--- the stack/memory and ensure the stack doesn't get too deep
compile_state :: [BLVar] -> CompileState
compile_state _xxx = ()

comp_blarg :: CompileState -> BLArg a -> ASMProg EVMLabel EVM.Opcode
comp_blarg _cs _a =
  --- XXX real
  ASMProg M.empty EL_Arg [ Op (EVM.INVALID 254) ]

comp_ctail :: CompileState -> EVMLabel -> CTail a -> ASMProg EVMLabel EVM.Opcode
comp_ctail cs lab t =
  case t of
    C_Halt _ ->
      blk [ Op (EVM.INVALID 254)
            --- XXX ^- current_state = 0x0
            --- XXX v-- selfdestruct to sender
          , Op (EVM.INVALID 254)]
    C_Wait _ _last_i _svs ->
      blk [ Op (EVM.INVALID 254)
            --- XXX ^-- current_state = hash
          ]
    C_If _ ca tt ft ->
      asm_cat cap $ asm_cat (blk [ push_label_offset tlab
                                 , Op EVM.JUMPI ]) (asm_union ttp ftp)
      where tlab = EL_Cond lab
            cap = comp_blarg cs ca
            ttp = comp_ctail cs tlab tt
            ftp = comp_ctail cs lab ft
    _ ->
      --- XXX other cases
      blk [ Op (EVM.INVALID 254) ]
  where blk os = ASMProg M.empty lab os

comp_ctail_top :: Maybe (FromSpec, Bool, Int, (BLArg a)) -> CompileState -> Int -> CTail a -> ASMProg EVMLabel EVM.Opcode
comp_ctail_top _handler_info cs i t =
  --- XXX Use handler_info to check states
  comp_ctail cs (EL_Handler i) t
  
comp_chandler :: CHandler a -> ASMProg EVMLabel EVM.Opcode
comp_chandler (C_Handler _ from_spec is_timeout (last_i, svs) msg delay body i) =
  comp_ctail_top (Just (from_spec, is_timeout, last_i, delay)) (compile_state $ svs ++ msg) i body
comp_chandler (C_Loop _ svs args _inv body i) =
  comp_ctail_top Nothing (compile_state $ svs ++ args) i body

cp_to_evm :: CProgram a -> [EVM.Opcode]
cp_to_evm (C_Prog _ hs) = con_bc
  where con_bc = assemble evm_op_len $ ASMProg (M.singleton EL_Dispatch ins_as) EL_New con_as
        con_as = [ --- XXX initialize state
                   push_label_size EL_Dispatch
                 , push_label_offset EL_Dispatch
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.CODECOPY)
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.RETURN)
                 , Op (EVM.INVALID 254) ]
        ins_as = map Op $ assemble evm_op_len dis_p
        dis_as = [ --- XXX Do something real 
                   Op (EVM.PUSH1 [0])
                 , Op (EVM.RETURN)
                 , Op (EVM.INVALID 254) ]
        dis_p = foldl hcomp1 (ASMProg M.empty EL_Dispatch dis_as) hs
        hcomp1 hdefs0 h = asm_union hdefs0 (comp_chandler h)

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
