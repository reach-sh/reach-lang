{-# LANGUAGE OverloadedStrings #-}

module Reach.EmitEVM where

import Control.Monad.State.Lazy
import qualified Data.Word as W
--import qualified Data.HexString as H
--import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified EVM.Bytecode as EVM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import Data.Foldable
import Data.Monoid

import Reach.AST
import Reach.Util
import Reach.EmitSol
  ( CompiledSol
  , CCounts
  , usesCTail )

m_insertSafe :: Show k => Ord k => k -> v -> M.Map k v -> M.Map k v
m_insertSafe k v m =
  case M.lookup k m of
    Nothing -> M.insert k v m
    Just _ ->
      error $ "m_insertSafe: Key already set: " ++ show k ++ " inside: " ++ (show $ map (\(x,_)->x) $ M.toAscList m)

type ASMMonad ann a = State ASMSt a
type Label = Int
data LabelInfo = LabelInfo Int Int
  deriving (Show, Eq, Ord)
data ASMOp
  = Op EVM.Opcode
  | LabelRef Label Int (LabelInfo -> EVM.Opcode)
type ASMSt =
  ( LinkerSt
  , CompileSt )
type ASMSeq =
  Q.Seq ASMOp
type LinkerSt =
  ( Label --- next
  , M.Map Label ASMSeq --- label to code map
  , LabelSt -- standard labels
  , ASMSeq --- code
  )
type LabelSt =
  ( Label --- halt
  , Label --- revert
  , M.Map Int Label --- handlers
  )
data VarRHS
  = VR_Mem Int
  | VR_Op EVM.Opcode
type CompileSt =
  ( Int --- stack depth
  , Int --- mem free pointer
  , M.Map BLVar VarRHS --- variable to mem addr
  )

--- FIXME Add JUMPDEST

empty_asm_st :: ASMSt
empty_asm_st = ( empty_linker_st, empty_compile_st )
empty_linker_st :: LinkerSt
empty_linker_st = ( 0, M.empty, empty_label_st, Q.empty )
empty_label_st :: LabelSt
empty_label_st = ( 0, 0, M.empty )
empty_compile_st :: CompileSt
empty_compile_st = ( 0, 0, M.empty )

asm_fresh_label_obs :: (Label -> ASMMonad ann a) -> ASMMonad ann a
asm_fresh_label_obs obs_and_code = do
  ( ls, cs ) <- get
  let ( this_label, defs, lst, orig_code ) = ls
  let next' = this_label + 1
  let fresh_code = Q.empty
  let ls' = ( next', defs, lst, fresh_code )
  put ( ls', cs )
  res <- obs_and_code this_label
  ( after_ls, after_cs ) <- get
  let ( after_next, after_defs, after_lst, after_code ) = after_ls
  let after_defs' = m_insertSafe this_label after_code after_defs
  let after_ls' = ( after_next, after_defs', after_lst, orig_code )
  put ( after_ls', after_cs )
  return res

asm_fresh_label :: ASMMonad ann () -> ASMMonad ann Label
asm_fresh_label new_code = asm_fresh_label_obs (\lab -> do new_code ; return lab)

asm_with_label_handler :: Int -> ASMMonad ann () -> ASMMonad ann Label
asm_with_label_handler which hand_code = asm_fresh_label_obs $ \label -> do
  asm_labels_set_handler which label
  hand_code
  return label

-- FIXME Have asm_push/pop/stack be inside of asm_op, or maybe as some sort of pass during assembly
asm_stack :: Int -> Int -> ASMMonad ann ()
asm_stack outs ins = do
  ( ls, cs ) <- get
  let ( stack, mp, vmap ) = cs
  let stack' = stack - outs + ins
  if stack' > 1023 then
    error "asm_stack: Depth is too deep"
  else do
    let cs' = ( stack', mp, vmap )
    put ( ls, cs' )

asm_rawop :: ASMOp -> ASMMonad ann ()
asm_rawop aop = do
  ( ls, cs ) <- get
  let ( next_label, label_seq, label_refs, code ) = ls
  let ls' = ( next_label, label_seq, label_refs, code Q.|> aop )
  put ( ls', cs )

asm_labels :: ASMMonad ann LabelSt
asm_labels = do
  ( ls, _ ) <- get
  let ( _, _, l, _ ) = ls
  return l

asm_labels_set :: Label -> Label -> ASMMonad ann ()
asm_labels_set halt_lab rev_lab = do
  ( ls, cs ) <- get
  let ( next, defs, l, code ) = ls
  let ( _, _, hs ) = l
  let l' = ( halt_lab, rev_lab, hs )
  let ls' = ( next, defs, l', code )
  put ( ls', cs )

asm_labels_set_handler :: Int -> Label -> ASMMonad ann ()
asm_labels_set_handler which lab = do
  ( ls, cs ) <- get
  let ( next, defs, l, code ) = ls
  let ( h, r, hs ) = l
  let hs' = m_insertSafe which lab hs
  let l' = ( h, r, hs' )
  let ls' = ( next, defs, l', code )
  put ( ls', cs )

asm_label_halt :: ASMMonad ann Label
asm_label_halt = do
  ( l, _, _ ) <- asm_labels
  return l
asm_label_revert :: ASMMonad ann Label
asm_label_revert = do
  ( _, l, _ ) <- asm_labels
  return l
asm_label_handler :: Int -> ASMMonad ann Label
asm_label_handler i = do
  ( _, _, hs ) <- asm_labels
  case M.lookup i hs of
    Just l -> return l
    Nothing ->
      error $ "Handler not defined --- " ++ show i ++ " --- in " ++ show hs

asm_op :: EVM.Opcode -> ASMMonad ann ()
asm_op op = asm_rawop $ Op op

asm_pop :: Int -> ASMMonad ann ()
asm_pop k = asm_stack k 0
asm_push :: Int -> ASMMonad ann ()
asm_push k = asm_stack 0 k

--- FIXME Make an "encodedLength" type-class to do this more efficiently
evm_op_len :: EVM.Opcode -> Int
evm_op_len o = length $ EVM.encode [o]

assemble :: ASMMonad ann () -> [EVM.Opcode]
assemble am = map asmfix $ toList image
  where ((), ((_, defs, _, main), _)) = runState am empty_asm_st
        asmfix ao =
          case ao of
            Op o -> o
            LabelRef l _ f ->
              case M.lookup l locs of
                Nothing -> error $ "Undefined label in assembly: " ++ show l
                Just li -> f li
        op_length (Op o) = evm_op_len o
        op_length (LabelRef _ l _) = l
        (_, locs, image) = M.foldlWithKey loc1 (main_sz, M.empty, main) defs
        seq_length = getSum . foldMap (Sum . op_length)
        main_sz = seq_length main
        loc1 (start, locs0, before) label body_seq = (end, locs1, after)
          where sz = seq_length body_seq
                end = start + sz
                locs1 = m_insertSafe label (LabelInfo start sz) locs0
                after = before Q.>< body_seq

asm_push_label_offset :: Label -> ASMMonad ann ()
asm_push_label_offset l = asm_rawop $ LabelRef l 2 (\ (LabelInfo off _) -> (EVM.PUSH1 [fromIntegral off]))
asm_push_label_size :: Label -> ASMMonad ann ()
asm_push_label_size l = asm_rawop $ LabelRef l 2 (\ (LabelInfo _ sz) -> (EVM.PUSH1 [fromIntegral sz]))

asm_mem_ptr :: ASMMonad ann Int
asm_mem_ptr = do
  ( _, cs ) <- get
  let ( _,mp, _ ) = cs
  return mp
asm_mem_reset :: Int -> ASMMonad ann ()
asm_mem_reset mp' = do
  ( ls, cs ) <- get
  let ( stack, _, vmap ) = cs
  let cs' = ( stack, mp', vmap )
  put ( ls, cs' )

is_pointer :: BaseType -> Bool
is_pointer t = case t of
  BT_UInt256 -> False
  BT_Bool -> False
  BT_Bytes -> True
  BT_Address -> False

size_of_var :: BLVar -> Int
size_of_var (_, (_, t)) = case t of
  BT_UInt256 -> 32
  BT_Bool -> 32 --- FIXME In the future, make this smaller by coallescing
  BT_Bytes -> 32
  BT_Address -> 32

asm_vmap_set :: BLVar -> VarRHS -> ASMMonad ann ()
asm_vmap_set v vr = do
  ( ls, cs ) <- get
  let ( sd,  mp, vmap ) = cs
  let vmap' = m_insertSafe v vr vmap
  let cs' = ( sd, mp, vmap' )
  put ( ls, cs' )

asm_malloc :: BLVar -> ASMMonad ann ()
asm_malloc v = do
  tp <- mem_bump_and_store $ size_of_var v
  asm_vmap_set v (VR_Mem tp)

mem_bump_and_store :: Int -> ASMMonad ann Int
mem_bump_and_store sz = do
  ( ls, cs ) <- get
  let ( sd, mp, vmap ) = cs
  let mp' = mp + sz
  let cs' = ( sd, mp', vmap )
  put ( ls, cs' )
  return mp

comp_bump_and_store :: Int -> ASMMonad ann ()
comp_bump_and_store sz = do
  tp <- mem_bump_and_store sz
  comp_store tp

comp_bump_and_store_var :: BLVar -> ASMMonad ann ()
comp_bump_and_store_var v =
  comp_bump_and_store $ size_of_var v

asm_free_all :: ASMMonad ann ()
asm_free_all = do
  ( ls, cs ) <- get
  let ( sd, _mp, _vmap ) = cs
  if sd == 0 then
    put ( ls, empty_compile_st )
  else
    put ( ls, empty_compile_st )
    --- XXX error $ "asm_free_all: stack is not empty at end of handler: " ++ show sd

asm_malloc_args :: [BLVar] -> ASMMonad ann ()
asm_malloc_args args = mapM_ asm_malloc args

asm_vmap_ref :: BLVar -> ASMMonad ann VarRHS
asm_vmap_ref v = do
  ( _, cs ) <- get
  let ( _, _, vmap ) = cs
  case M.lookup v vmap of
    Nothing -> error $ "asm_vmap_ref: Variable not in memory: " ++ show v
    Just addr -> do
      return addr

comp_jump_to_label :: Label -> Bool -> ASMMonad ann ()
comp_jump_to_label lab cond = do
  asm_push_label_offset lab
  if cond then
    do asm_op EVM.JUMPI
       asm_pop 2
  else
    do asm_op EVM.JUMP
       asm_pop 1

comp_jump_to_handler :: Int -> ASMMonad ann ()
comp_jump_to_handler which = do
  lab <- asm_label_handler which
  comp_jump_to_label lab False

evm_pushN :: [ W.Word8 ] -> EVM.Opcode
evm_pushN args = op args
  where
    op = case length args of
           0 -> \ _ -> EVM.PUSH1 [ 0 ]
           1 -> EVM.PUSH1
           2 -> EVM.PUSH2
           3 -> EVM.PUSH3
           4 -> EVM.PUSH4
           5 -> EVM.PUSH5
           6 -> EVM.PUSH6
           7 -> EVM.PUSH7
           8 -> EVM.PUSH8
           9 -> EVM.PUSH9
           10 -> EVM.PUSH10
           11 -> EVM.PUSH11
           12 -> EVM.PUSH12
           13 -> EVM.PUSH13
           14 -> EVM.PUSH14
           15 -> EVM.PUSH15
           16 -> EVM.PUSH16
           17 -> EVM.PUSH17
           18 -> EVM.PUSH18
           19 -> EVM.PUSH19
           20 -> EVM.PUSH20
           21 -> EVM.PUSH21
           22 -> EVM.PUSH22
           23 -> EVM.PUSH23
           24 -> EVM.PUSH24
           25 -> EVM.PUSH25
           26 -> EVM.PUSH26
           27 -> EVM.PUSH27
           28 -> EVM.PUSH28
           29 -> EVM.PUSH29
           30 -> EVM.PUSH30
           31 -> EVM.PUSH31
           32 -> EVM.PUSH32
           _ -> error $ "Constant is too large: " ++ show args

integerToWords :: Integer -> [ W.Word8 ]
integerToWords i = if i == 0 then [] else (fromIntegral $ i `mod` 256) : (integerToWords $ i `div` 256 )

asm_constant_bytes_lookup :: B.ByteString -> ASMMonad ann Int
asm_constant_bytes_lookup _bs = return 0 --- XXX

comp_con :: Constant -> ASMMonad ann ()
comp_con c =
  case c of
    Con_I i -> do
      asm_op $ evm_pushN $ integerToWords i
      asm_push 1
    Con_B t -> do
      asm_op (EVM.PUSH1 [ if t then 1 else 0 ])
      asm_push 1
    Con_BS bs -> do
      bs_addr <- asm_constant_bytes_lookup bs
      comp_con $ Con_I $ fromIntegral bs_addr

comp_memread :: Int -> ASMMonad ann ()
comp_memread addr = do
  asm_op $ EVM.PUSH1 [ fromIntegral $ addr ]
  asm_push 1
  asm_op $ EVM.MLOAD
  asm_stack 1 1

comp_cdread :: Int -> ASMMonad ann ()
comp_cdread addr = do
  asm_op $ EVM.PUSH1 [ fromIntegral $ addr ]
  asm_push 1
  asm_op $ EVM.CALLDATALOAD
  asm_stack 1 1

comp_blvar :: BLVar -> ASMMonad ann ()
comp_blvar v = do
  let (_, (_, bt)) = v
  vrhs <- asm_vmap_ref v
  case vrhs of
    VR_Mem addr ->
      if is_pointer bt then
        comp_con $ Con_I $ fromIntegral addr
      else
        comp_memread addr
    VR_Op o -> do
      asm_op o
      asm_push 1

comp_blarg :: BLArg a -> ASMMonad ann ()
comp_blarg a =
  case a of
    BL_Con _ c -> comp_con c
    BL_Var _ v -> comp_blvar v

comp_cexpr :: CExpr a -> ASMMonad ann () -> ASMMonad ann ()
comp_cexpr e km =
  case e of
    C_PrimApp _ cp as -> do
      --- Arguments are reversed, because we store (a / b) as (C_Prim
      --- _ DIV [a, b]) but the EVM expects [ a b ... ] on the stack
      --- when you run DIV, so we need to push on B first and then A.
      forM_ (reverse as) comp_blarg
      case cp of
        ADD -> p_op EVM.ADD 2
        SUB -> p_op EVM.SUB 2
        MUL -> p_op EVM.MUL 2
        DIV -> p_op EVM.DIV 2
        MOD -> p_op EVM.MOD 2
        PLT -> p_op EVM.LT 2
        PLE -> p_op_not EVM.GT 2
        PEQ -> p_op EVM.EQ 2
        PGT -> p_op EVM.GT 2
        PGE -> p_op_not EVM.LT 2
        IF_THEN_ELSE -> do
          after <- asm_fresh_label km
          true <- asm_fresh_label $ do
            asm_op $ EVM.SWAP1
            asm_op $ EVM.POP
            asm_pop 1
            comp_jump_to_label after False
          comp_jump_to_label true True
          asm_op $ EVM.POP
          asm_pop 1
          comp_jump_to_label after False
        UINT256_TO_BYTES -> unimplemented_xxx
        DIGEST -> unimplemented_xxx
        BYTES_EQ -> unimplemented_xxx
        BYTES_LEN -> unimplemented_xxx
        BCAT -> unimplemented_xxx
        BCAT_LEFT -> unimplemented_xxx
        BCAT_RIGHT -> unimplemented_xxx
        BALANCE -> p_op EVM.BALANCE 0
        TXN_VALUE -> p_op EVM.CALLVALUE 0
      where unimplemented_xxx = do end_block_op $ "XXX comp_cexpr C_PrimApp " ++ show cp
                                   km
  where p_op o amt = do asm_op o
                        asm_stack amt 1
                        km
        p_op_not o amt = do asm_op o
                            asm_stack amt 1
                            asm_op EVM.NOT
                            asm_stack 1 1
                            km

callStipend :: Integer
callStipend = 2300

comp_cstmt :: CStmt a -> ASMMonad ann  ()
comp_cstmt s =
  case s of
    C_Claim _ CT_Possible _ -> return ()
    C_Claim _ CT_Assert _ -> return ()
    C_Claim _ _ a -> do
      comp_blarg a
      asm_op $ EVM.NOT
      asm_stack 1 1
      revert_lab <- asm_label_revert
      comp_jump_to_label revert_lab True
    C_Transfer _ p a -> do
      asm_op $ EVM.PUSH1 [0] --- retlength
      asm_push 1
      asm_op $ EVM.DUP1 --- retoffset
      asm_stack 1 2
      asm_op $ EVM.DUP1 --- argslength
      asm_stack 1 2
      asm_op $ EVM.DUP1 --- argsoffset
      asm_stack 1 2
      comp_blarg a --- value
      comp_blvar p --- addr
      comp_con $ Con_I $ callStipend --- gas
      --- Why does Solidity do... gas <- gas * !value
      asm_op $ EVM.CALL
      asm_stack 7 1
      --- check if zero for failure
      asm_op $ EVM.NOT
      asm_stack 1 1
      revert_lab <- asm_label_revert
      comp_jump_to_label revert_lab True

comp_state_compute :: Int -> Bool -> [BLVar] -> ASMMonad ann ()
comp_state_compute i use_this_block svs = do
  before_free_ptr <- asm_mem_ptr
  --- push i into hash args
  comp_con $ Con_I $ fromIntegral i
  comp_bump_and_store 32
  --- push block number in hash args
  if use_this_block then
    asm_op $ EVM.NUMBER
  else
    comp_memread last_time_offset
  comp_bump_and_store 32
  --- push variables
  mapM_ comp_bump_and_store_var svs
  after_free_ptr <- asm_mem_ptr
  --- compute hash its
  comp_con $ Con_I $ fromIntegral before_free_ptr --- hash args offset
  comp_con $ Con_I $ fromIntegral $ after_free_ptr - before_free_ptr --- hash args length
  asm_op $ EVM.SHA3
  asm_stack 2 1
  asm_mem_reset before_free_ptr

comp_state_check :: Int -> [BLVar] -> ASMMonad ann ()
comp_state_check last_i svs = do
  comp_state_compute last_i False svs
  asm_op $ EVM.PUSH1 [0]
  asm_push 1
  asm_op $ EVM.SLOAD
  asm_stack 1 1
  asm_op $ EVM.EQ
  asm_pop 2
  asm_op $ EVM.NOT
  asm_stack 1 1
  revert_lab <- asm_label_revert
  comp_jump_to_label revert_lab True

comp_state_set :: Int -> [BLVar] -> ASMMonad ann ()
comp_state_set i svs = do
  comp_state_compute i True svs
  asm_op $ EVM.PUSH1 [0]
  asm_push 1
  asm_op $ EVM.SSTORE
  asm_pop 2

comp_set_args :: [BLArg a] -> ASMMonad ann ()
comp_set_args _args =
  end_block_op $ "XXX comp_set_args"

comp_store :: Int -> ASMMonad ann ()
comp_store addr = do
  asm_op $ EVM.PUSH1 [ fromIntegral addr ]
  asm_push 1
  asm_op $ EVM.MSTORE
  asm_pop 2

comp_malloc_and_store :: BLVar -> ASMMonad ann ()
comp_malloc_and_store v = do
  asm_malloc v
  vmap <- asm_vmap_ref v
  case vmap of
    VR_Mem v_addr ->
      comp_store v_addr
    VR_Op _ -> impossible "malloc always puts variable in memory"

--- current_state is key 0
comp_ctail :: CCounts -> CTail a -> ASMMonad ann () -> ASMMonad ann ()
comp_ctail ccs t km =
  case t of
    C_Halt _ -> do
      halt_lab <- asm_label_halt
      comp_jump_to_label halt_lab False
      km
    C_Wait _ i svs -> do
      comp_state_set i svs
      km
    C_If _ ca tt ft -> do
      comp_blarg ca
      after <- asm_fresh_label km
      let goto_after = comp_jump_to_label after False
      tlab <- asm_fresh_label $ comp_ctail ccs tt goto_after
      comp_jump_to_label tlab True
      comp_ctail ccs ft goto_after
    C_Let _ bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> comp_ctail ccs kt km
        --- XXX if is 1, then use stack
        _ -> comp_cexpr ce $ do
          comp_malloc_and_store bv
          comp_ctail ccs kt km
    C_Do _ ds kt -> do
      comp_cstmt ds
      comp_ctail ccs kt km
    C_Jump loc which vs _ as -> do
      comp_set_args $ (map (BL_Var loc) vs) ++ as
      comp_jump_to_handler which
      km

abiTag :: [ BLVar ] -> Integer
abiTag _vs = 0 --- XXX

tag_offset :: Int
tag_offset = 0
last_time_offset :: Int
last_time_offset = tag_offset + 4
arg_start_offset :: Int
arg_start_offset = last_time_offset + 32

comp_chandler :: Label -> CHandler a -> ASMMonad ann Label
comp_chandler next_lab (C_Handler _ from_spec is_timeout (last_i, svs) msg delay body i) = asm_with_label_handler i $ do
  revert_lab <- asm_label_revert
  asm_free_all
  --- check tag and parse args
  asm_malloc_args $ svs
  mem_after_svs <- asm_mem_ptr
  asm_malloc_args $ msg
  comp_cdread tag_offset
  comp_con $ Con_I $ abiTag $ svs ++ msg
  asm_op $ EVM.EQ
  asm_stack 2 1
  asm_op $ EVM.CALLDATASIZE
  asm_push 1
  mem_after_args <- asm_mem_ptr
  comp_con $ Con_I $ fromIntegral $ arg_start_offset + mem_after_args
  asm_op $ EVM.EQ
  asm_stack 2 1
  asm_op $ EVM.AND
  asm_stack 2 1
  asm_op $ EVM.NOT
  asm_stack 1 1
  comp_jump_to_label next_lab True
  --- copy calldata args to memory
  comp_con $ Con_I $ fromIntegral $ mem_after_args
  comp_con $ Con_I $ fromIntegral $ arg_start_offset
  comp_con $ Con_I $ 0
  asm_op $ EVM.CALLDATACOPY
  --- parse args
  --- check state
  comp_state_check last_i svs
  --- check sender
  case from_spec of
    FS_Join from ->
      asm_vmap_set from $ VR_Op EVM.CALLER
    FS_From from -> do
      comp_blvar from
      asm_op $ EVM.CALLER
      asm_push 1
      asm_op $ EVM.EQ
      asm_stack 2 1
      asm_op $ EVM.NOT
      asm_stack 1 1      
      comp_jump_to_label revert_lab True
    FS_Any -> return ()
  --- check timeout
  comp_cdread last_time_offset
  comp_blarg delay
  asm_op $ EVM.ADD
  asm_stack 2 1
  asm_op $ EVM.NUMBER
  asm_push 1
  asm_op $ EVM.LT
  asm_stack 2 1
  if is_timeout then
    return ()
  else
    asm_op $ EVM.NOT
  comp_jump_to_label revert_lab True
  --- emit the event
  let msg_length = mem_after_args - mem_after_svs
  let msg_offset = mem_after_svs
  comp_con $ Con_I $ abiTag $ msg --- topic
  comp_con $ Con_I $ fromIntegral msg_length
  comp_con $ Con_I $ fromIntegral msg_offset
  asm_op $ EVM.LOG1
  --- do the thing
  --- XXX load constant strings into memory
  comp_ctail (usesCTail body) body $ do
    end_block_op ("</Handler " ++ show i ++ ">")
comp_chandler next_lab (C_Loop _ _svs _args _inv _body _i) = return next_lab

comp_cloop :: CHandler a -> ASMMonad ann ()
comp_cloop (C_Handler _ _from_spec _is_timeout _ _msg _delay _body _i) = return ()
comp_cloop (C_Loop _ svs args _inv body i) = void $ asm_with_label_handler i $ do
  asm_free_all
  asm_malloc_args $ svs ++ args
  comp_ctail (usesCTail body) body $ do
    end_block_op ("</Loop " ++ show i ++ ">")
  
end_block_op :: String -> ASMMonad ann ()
end_block_op dbg = asm_op $ EVM.INVALID 0xFE dbg

cp_to_evm :: CProgram a -> [EVM.Opcode]
cp_to_evm (C_Prog _ hs) = assemble $ do
  comp_state_set 0 []
  dis_lab <- asm_fresh_label $ mapM_ asm_op $ assemble dis
  asm_push_label_size dis_lab
  asm_push_label_offset dis_lab
  asm_op $ EVM.PUSH1 [0]
  asm_push 1
  asm_op $ EVM.CODECOPY
  asm_pop 3
  asm_op $ EVM.PUSH1 [0]
  asm_push 1
  asm_op $ EVM.DUP1
  asm_push 1
  asm_op $ EVM.RETURN
  asm_pop 2
  end_block_op "</Constructor>"
  where dis = do
          halt_lab <- asm_fresh_label $ halt
          rev_lab <- asm_fresh_label $ revert
          asm_labels_set halt_lab rev_lab
          mapM_ comp_cloop hs
          foldM_ (\next_lab h -> do this_lab <- comp_chandler next_lab h
                                    return this_lab)
                 rev_lab (reverse hs)
          comp_jump_to_handler 1
          end_block_op "</Dispatch>"
        halt = do
          --- current_state = 0x0
          asm_op $ EVM.PUSH1 [0]
          asm_push 1
          asm_op $ EVM.DUP1
          asm_push 1
          asm_op $ EVM.SSTORE
          asm_pop 2
          --- selfdestruct(msg.sender)
          asm_op $ EVM.CALLER
          asm_push 1
          asm_op $ EVM.SELFDESTRUCT
          asm_pop 1
          end_block_op "</HALT>"
        revert = do
          asm_op $ EVM.PUSH1 [0]
          asm_push 1
          asm_op $ EVM.DUP1
          asm_push 1
          asm_op $ EVM.REVERT
          asm_pop 2
          end_block_op "</REVERT>"

emit_evm :: FilePath -> BLProgram a -> CompiledSol -> IO ()
emit_evm _ (BL_Prog _ _ cp) (_, _code) = do
  mapM_ (\o -> putStrLn $ show o) $ cp_to_evm cp
  return ()
