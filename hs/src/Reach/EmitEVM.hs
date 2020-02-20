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

m_insertSafe :: Show k => Ord k => k -> a -> M.Map k a -> M.Map k a
m_insertSafe k v m =
  if M.notMember k m then
    M.insert k v m
  else
    error $ "m_insertSafe: key already exists: " ++ show k

asm_union :: Show a => Ord a => ASMProg a b -> ASMProg a b -> ASMProg a b
asm_union main extra = ASMProg combined main_lab main_as
  where ASMProg main_defs main_lab main_as = main
        ASMProg extra_defs extra_lab extra_as = extra
        combined = M.unionWithKey safe_combine main_defs $ m_insertSafe extra_lab extra_as extra_defs
        safe_combine k _ _ = error $ "m_unionWithKey: key already exists: " ++ show k

asm_cat :: Ord a => ASMProg a b -> ASMProg a b -> ASMProg a b
asm_cat main extra = ASMProg combined main_lab combined_as
  where ASMProg main_defs main_lab main_as = main
        ASMProg extra_defs _ extra_as = extra
        combined_as = main_as ++ extra_as
        combined = M.union main_defs extra_defs

--- FIXME Make an "encodedLength" type-class to do this more efficiently
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
                locs1 = m_insertSafe label (LabelInfo start sz) locs0
                after = before Q.>< body_seq

data EVMLabel
  = EL_Handler Int
  | EL_New
  | EL_Dispatch
  | EL_Cond EVMLabel
  | EL_End
  | EL_Revert
  | EL_Halt
  deriving (Show, Eq, Ord)

push_label_offset :: a -> ASMOp a EVM.Opcode
push_label_offset l = LabelRef l 2 (\ (LabelInfo off _) -> (EVM.PUSH1 [fromIntegral off]))
push_label_size :: a -> ASMOp a EVM.Opcode
push_label_size l = LabelRef l 2 (\ (LabelInfo _ sz) -> (EVM.PUSH1 [fromIntegral sz]))

--- XXX Maybe make an assembler monad to track stuff in here, such as the stack depth
type CompileState = ()
--- XXX This needs to help use figure out how to read a variable from
--- the stack/memory and ensure the stack doesn't get too deep
compile_state :: [BLVar] -> CompileState
compile_state _xxx = ()

comp_con :: CompileState -> EVMLabel -> Constant -> ASMProg EVMLabel EVM.Opcode
comp_con _cs lab c =
  case c of
    Con_I i ->
      --- XXX Be sensitive to size of int
      blk [ Op (EVM.PUSH1 [ fromIntegral $ i ]) ]
    Con_B t ->
      blk [ Op (EVM.PUSH1 [ if t then 1 else 0 ]) ]
    Con_BS _bs ->
      blk [ Op (EVM.INVALID 0xFE "XXX comp_con BS") ]
  where blk os = ASMProg M.empty lab os

comp_blarg :: CompileState -> EVMLabel -> BLArg a -> ASMProg EVMLabel EVM.Opcode
comp_blarg cs lab a =
  case a of
    BL_Con _ c -> comp_con cs lab c
    BL_Var _ v ->
      ASMProg M.empty lab [ Op (EVM.INVALID 0xFE ("XXX comp_blag var: " ++ show v)) ]

comp_cexpr :: CompileState -> EVMLabel -> CExpr a -> ASMProg EVMLabel EVM.Opcode
comp_cexpr cs lab e =
  case e of
    C_PrimApp _ cp as ->
      case cp of
        ADD -> op1 EVM.ADD
        SUB -> op1 EVM.SUB
        MUL -> op1 EVM.MUL
        DIV -> op1 EVM.DIV
        MOD -> op1 EVM.MOD
        PLT -> op1 EVM.LT
        PLE -> blk [ Op EVM.GT, Op EVM.NOT ]
        PEQ -> op1 EVM.EQ
        PGT -> op1 EVM.GT
        PGE -> blk [ Op EVM.LT, Op EVM.NOT ]
        IF_THEN_ELSE -> blk [ Op EVM.SWAP1, Op EVM.NOT, Op EVM.OR ]
        BALANCE -> op1 EVM.BALANCE
        TXN_VALUE -> op1 EVM.CALLVALUE
        _ ->
          asm_cat asp $ blk [ Op (EVM.INVALID 0xFE ("XXX comp_cexpr C_PrimApp " ++ show cp)) ]
      where asp = foldl (\p a -> asm_cat p $ comp_blarg cs lab a) (blk []) as
  where blk os = ASMProg M.empty lab os
        op1 o = blk [ Op o ]

comp_cstmt :: CompileState -> EVMLabel -> CStmt a -> ASMProg EVMLabel EVM.Opcode
comp_cstmt cs lab s =
  case s of
    C_Claim _ CT_Possible _ -> blk []
    C_Claim _ CT_Assert _ -> blk []
    C_Claim _ _ a -> 
      asm_cat (comp_blarg cs lab a) $ blk [ Op (EVM.NOT)
                                          , push_label_offset EL_Revert
                                          , Op (EVM.JUMPI) ]
    C_Transfer _ _p a -> 
      asm_cat (comp_blarg cs lab a) $ blk [ Op (EVM.INVALID 0xFE "XXX C_Transfer") ]
  where blk os = ASMProg M.empty lab os

--- current_state is key 0
comp_ctail :: CompileState -> EVMLabel -> CTail a -> ASMProg EVMLabel EVM.Opcode
comp_ctail cs lab t =
  case t of
    C_Halt _ ->
      blk [ push_label_offset EL_Halt
          , Op EVM.JUMP ]
    C_Wait _ _last_i _svs ->
      blk [ Op (EVM.INVALID 0xFE "XXX C_Wait") -- current_state = hash
          ]
    C_If _ ca tt ft ->
      asm_cat cap $ asm_cat (blk [ push_label_offset tlab
                                 , Op EVM.JUMPI ]) (asm_union ttp ftp)
      where tlab = EL_Cond lab --- XXX New to label these with which branch
            cap = comp_blarg cs lab ca
            ttp = comp_ctail cs tlab tt
            ftp = comp_ctail cs lab ft
    C_Let _ _bv ce kt ->
      --- FIXME use ccs from EmitSol and store in cs if count is low
      asm_cat cep ktp
      where cep = comp_cexpr cs lab ce
            --- XXX ^-- need to communicate where to store
            ktp = comp_ctail cs' lab kt
            --- XXX need to record where the value gets stored
            cs' = cs
    C_Do _ ds kt ->
      asm_cat dsp ktp
      where dsp = comp_cstmt cs lab ds
            ktp = comp_ctail cs lab kt
    C_Jump _ which _vs _ _as ->
      --- XXX prepare argument (vs & as)
      (blk [ push_label_offset (EL_Handler which)
           , Op EVM.JUMP ])
  where blk os = ASMProg M.empty lab os

comp_ctail_top :: Maybe (FromSpec, Bool, Int, (BLArg a)) -> CompileState -> Int -> CTail a -> ASMProg EVMLabel EVM.Opcode
comp_ctail_top _handler_info cs i t =
  --- XXX Use handler_info to check states
  add_end_block ("end Handler " ++ show i) $ comp_ctail cs (EL_Handler i) t
  
comp_chandler :: CHandler a -> ASMProg EVMLabel EVM.Opcode
comp_chandler (C_Handler _ from_spec is_timeout (last_i, svs) msg delay body i) =
  comp_ctail_top (Just (from_spec, is_timeout, last_i, delay)) (compile_state $ svs ++ msg) i body
comp_chandler (C_Loop _ svs args _inv body i) =
  comp_ctail_top Nothing (compile_state $ svs ++ args) i body

end_block_op :: String -> ASMOp EVMLabel EVM.Opcode
end_block_op dbg = Op $ EVM.INVALID 0xFE dbg

end_block_p :: String -> ASMProg EVMLabel EVM.Opcode
end_block_p dbg = ASMProg M.empty EL_End [ end_block_op dbg ]

add_end_block :: String -> ASMProg EVMLabel EVM.Opcode -> ASMProg EVMLabel EVM.Opcode
add_end_block dbg p = asm_cat p $ end_block_p dbg

cp_to_evm :: CProgram a -> [EVM.Opcode]
cp_to_evm (C_Prog _ hs) = con_bc
  where con_bc = assemble evm_op_len $ add_end_block "end Constructor" $ ASMProg (M.singleton EL_Dispatch ins_as) EL_New con_as
        con_as = [ --- XXX initialize state
                   push_label_size EL_Dispatch
                 , push_label_offset EL_Dispatch
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.CODECOPY)
                 , Op (EVM.PUSH1 [0])
                 , Op (EVM.DUP1)
                 , Op (EVM.RETURN) ]
        ins_as = map Op $ assemble evm_op_len dis_p
        dis_as = [ --- XXX Do something real 
                   Op (EVM.PUSH1 [0])
                 , Op (EVM.DUP1)
                 , Op (EVM.RETURN) ]
        halt_as = [ Op (EVM.PUSH1 [0])
                  , Op (EVM.DUP1)
                  , Op (EVM.SSTORE) --- current_state = 0x0
                  , Op (EVM.CALLER)
                  , Op (EVM.SELFDESTRUCT)
                  , end_block_op "end HALT" ] --- selfdestruct(msg.sender)
        rev_as = [ Op (EVM.PUSH1 [0])
                  , Op (EVM.DUP1)
                  , Op (EVM.REVERT)
                  , end_block_op "end REVERT" ] --- revert
        base_defs = M.insert EL_Revert rev_as $ M.insert EL_Halt halt_as $ M.empty
        dis_p = foldl' hcomp1 (add_end_block "end Dispatch" (ASMProg base_defs EL_Dispatch dis_as)) hs
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
