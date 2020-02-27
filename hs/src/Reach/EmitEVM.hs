{-# LANGUAGE OverloadedStrings #-}

module Reach.EmitEVM where

import Control.Monad.State.Lazy
--import qualified Data.Word as W
import qualified Data.HexString as H
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified EVM.Bytecode as EVM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import Data.Foldable
import Data.Monoid

import Reach.AST
import Reach.EmitSol
  ( CompiledSol )

type ASMMonad ann op a = State (ASMSt op) a
type Label = Int
data LabelInfo = LabelInfo Int Int
data ASMOp op
  = Op op
  | LabelRef Label Int (LabelInfo -> op)
type ASMSt op =
  ( LinkerSt op
  , CompileSt )
type ASMSeq op =
  Q.Seq (ASMOp op)
type LinkerSt op =
  ( Label --- next
  , M.Map Label (ASMSeq op) --- label to code map
  , LabelSt -- standard labels
  , ASMSeq op --- code
  )
type LabelSt =
  ( Label --- halt
  , Label --- revert
  , M.Map Int Label --- handlers
  )
type CompileSt =
  ( Int --- stack depth
  )
--- XXX This needs to help us figure out how to read a variable

empty_asm_st :: ASMSt op
empty_asm_st = ( empty_linker_st, empty_compile_st )
empty_linker_st :: LinkerSt op
empty_linker_st = ( 0, M.empty, empty_label_st, Q.empty )
empty_label_st :: LabelSt
empty_label_st = ( 0, 0, M.empty )
empty_compile_st :: CompileSt
empty_compile_st = ( 0 )

asm_fresh_label_obs :: (Label -> ASMMonad ann o a) -> ASMMonad ann o a
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
  let after_defs' = M.insert this_label after_code after_defs
  let after_ls' = ( after_next, after_defs', after_lst, orig_code )
  put ( after_ls', after_cs )
  return res

asm_fresh_label :: ASMMonad ann o () -> ASMMonad ann o Label
asm_fresh_label new_code = asm_fresh_label_obs (\lab -> do new_code ; return lab)

asm_with_label_handler :: Int -> ASMMonad ann o () -> ASMMonad ann o ()
asm_with_label_handler which hand_code = asm_fresh_label_obs $ \label -> do
  asm_labels_set_handler which label
  hand_code

-- XXX Have asm_push/pop/stack be inside of asm_op
asm_stack :: Int -> Int -> ASMMonad ann o ()
asm_stack outs ins = do
  ( ls, cs ) <- get
  let ( cur_stack ) = cs
  let cs' = ( cur_stack - outs + ins )
  --- XXX error if stack limit exceeded
  put ( ls, cs' )

asm_rawop :: ASMOp o -> ASMMonad ann o ()
asm_rawop aop = do
  ( ls, cs ) <- get
  let ( next_label, label_seq, label_refs, code ) = ls
  let ls' = ( next_label, label_seq, label_refs, code Q.|> aop )
  put ( ls', cs )

asm_labels :: ASMMonad ann o LabelSt
asm_labels = do
  ( ls, _ ) <- get
  let ( _, _, l, _ ) = ls
  return l

asm_labels_set :: Label -> Label -> ASMMonad ann o ()
asm_labels_set halt_lab rev_lab = do
  ( ls, cs ) <- get
  let ( next, defs, l, code ) = ls
  let ( _, _, hs ) = l
  let l' = ( halt_lab, rev_lab, hs )
  let ls' = ( next, defs, l', code )
  put ( ls', cs )

asm_labels_set_handler :: Int -> Label -> ASMMonad ann o ()
asm_labels_set_handler which lab = do
  ( ls, cs ) <- get
  let ( next, defs, l, code ) = ls
  let ( h, r, hs ) = l
  let hs' = M.insert which lab hs
  let l' = ( h, r, hs' )
  let ls' = ( next, defs, l', code )
  put ( ls', cs )

asm_label_halt :: ASMMonad ann o Label
asm_label_halt = do
  ( l, _, _ ) <- asm_labels
  return l
asm_label_revert :: ASMMonad ann o Label
asm_label_revert = do
  ( _, l, _ ) <- asm_labels
  return l
asm_label_handler :: Int -> ASMMonad ann o Label
asm_label_handler i = do
  ( _, _, hs ) <- asm_labels
  case M.lookup i hs of
    Just l -> return l
    Nothing ->
      error $ "Handler not defined --- " ++ show i ++ " --- in " ++ show hs

asm_op :: o -> ASMMonad ann o ()
asm_op op = asm_rawop $ Op op

asm_pop :: Int -> ASMMonad ann o ()
asm_pop k = asm_stack k 0
asm_push :: Int -> ASMMonad ann o ()
asm_push k = asm_stack 0 k

--- FIXME Make an "encodedLength" type-class to do this more efficiently
evm_op_len :: EVM.Opcode -> Int
evm_op_len o = length $ EVM.encode [o]

assemble :: (o -> Int) -> ASMMonad ann o () -> [o]
assemble op_len am = map asmfix $ toList image
  where ((), ((_, defs, _, main), _)) = runState am empty_asm_st
        asmfix ao =
          case ao of
            Op o -> o
            LabelRef l _ f ->
              case M.lookup l locs of
                Nothing -> error $ "Undefined label in assembly: " ++ show l
                Just li -> f li
        op_length (Op o) = op_len o
        op_length (LabelRef _ l _) = l
        (_, locs, image) = M.foldlWithKey loc1 (main_sz, M.empty, main) defs
        seq_length = getSum . foldMap (Sum . op_length)
        main_sz = seq_length main
        loc1 (start, locs0, before) label body_seq = (end, locs1, after)
          where sz = seq_length body_seq
                end = start + sz
                locs1 = M.insert label (LabelInfo start sz) locs0
                after = before Q.>< body_seq

asm_push_label_offset :: Label -> ASMMonad ann EVM.Opcode ()
asm_push_label_offset l = asm_rawop $ LabelRef l 2 (\ (LabelInfo off _) -> (EVM.PUSH1 [fromIntegral off]))
asm_push_label_size :: Label -> ASMMonad ann EVM.Opcode ()
asm_push_label_size l = asm_rawop $ LabelRef l 2 (\ (LabelInfo _ sz) -> (EVM.PUSH1 [fromIntegral sz]))

comp_jump_to_label :: Label -> Bool -> ASMMonad ann EVM.Opcode ()
comp_jump_to_label lab cond = do
  asm_push_label_offset lab
  if cond then
    do asm_op EVM.JUMPI
       asm_pop 2
  else
    do asm_op EVM.JUMP
       asm_pop 1

comp_jump_to_handler :: Int -> ASMMonad ann EVM.Opcode ()
comp_jump_to_handler which = do
  lab <- asm_label_handler which
  comp_jump_to_label lab False

comp_con :: Constant -> ASMMonad ann EVM.Opcode ()
comp_con c =
  case c of
    Con_I i -> do
      --- XXX Be sensitive to size of int
      asm_op (EVM.PUSH1 [ fromIntegral $ i ])
      asm_push 1
    Con_B t -> do
      asm_op (EVM.PUSH1 [ if t then 1 else 0 ])
      asm_push 1
    Con_BS _bs ->
      end_block_op "XXX comp_con BS"

comp_blarg :: BLArg a -> ASMMonad ann EVM.Opcode ()
comp_blarg a =
  case a of
    BL_Con _ c -> comp_con c
    BL_Var _ v -> end_block_op $ "XXX comp_blag var: " ++ show v

comp_cexpr :: CExpr a -> ASMMonad ann EVM.Opcode ()
comp_cexpr e =
  case e of
    C_PrimApp _ cp as -> do
      forM_ as comp_blarg
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
        BALANCE -> p_op EVM.BALANCE 0
        TXN_VALUE -> p_op EVM.CALLVALUE 0
        _ -> end_block_op $ "XXX comp_cexpr C_PrimApp " ++ show cp
  where p_op o amt = do asm_op o
                        asm_stack amt 1
        p_op_not o amt = do asm_op o
                            asm_stack amt 1
                            asm_op EVM.NOT

comp_cstmt :: CStmt a -> ASMMonad ann EVM.Opcode ()
comp_cstmt s =
  case s of
    C_Claim _ CT_Possible _ -> return ()
    C_Claim _ CT_Assert _ -> return ()
    C_Claim _ _ a -> do
      comp_blarg a
      asm_op EVM.NOT
      revert_lab <- asm_label_revert
      comp_jump_to_label revert_lab True
    C_Transfer _ _p a -> do
      end_block_op $ "XXX C_Transfer p"
      comp_blarg a
      end_block_op $ "XXX C_Transfer effect"

--- current_state is key 0
comp_ctail :: CTail a -> ASMMonad ann EVM.Opcode ()
comp_ctail t =
  case t of
    C_Halt _ -> do
      halt_lab <- asm_label_halt
      comp_jump_to_label halt_lab False
    C_Wait _ _last_i _svs -> do
      end_block_op $ "XXX C_Wait" -- current_state = hash
    C_If _ ca tt ft -> do
      comp_blarg ca
      tlab <- asm_fresh_label $ comp_ctail tt
      comp_jump_to_label tlab True
      comp_ctail ft
    C_Let _ _bv ce kt -> do
      --- FIXME use ccs from EmitSol and store in cs if count is low
      comp_cexpr ce
      --- XXX ^-- need to store someone and save in monad state
      comp_ctail kt
    C_Do _ ds kt -> do
      comp_cstmt ds
      comp_ctail kt
    C_Jump _ which _vs _ _as -> do
      end_block_op "XXX prepare C_Jump args"
      comp_jump_to_handler which

comp_ctail_top :: Maybe (FromSpec, Bool, Int, (BLArg a)) -> [BLVar] -> Int -> CTail a -> ASMMonad ann EVM.Opcode ()
comp_ctail_top _handler_info _args i t = asm_with_label_handler i $ do
  end_block_op "XXX Check state and initialize arguments"
  comp_ctail t
  end_block_op ("end Handler " ++ show i)
  
comp_chandler :: CHandler a -> ASMMonad ann EVM.Opcode ()
comp_chandler (C_Handler _ from_spec is_timeout (last_i, svs) msg delay body i) =
  comp_ctail_top (Just (from_spec, is_timeout, last_i, delay)) (svs ++ msg) i body
comp_chandler (C_Loop _ svs args _inv body i) =
  comp_ctail_top Nothing (svs ++ args) i body

end_block_op :: String -> ASMMonad ann EVM.Opcode ()
end_block_op dbg = asm_op $ EVM.INVALID 0xFE dbg

cp_to_evm :: CProgram a -> [EVM.Opcode]
cp_to_evm (C_Prog _ hs) = assemble evm_op_len $ do
  end_block_op "XXX Initialize state"
  dis_lab <- asm_fresh_label $ mapM_ asm_op $ assemble evm_op_len dis
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
  end_block_op "end Constructor"
  where dis = do
          halt_lab <- asm_fresh_label $ halt
          rev_lab <- asm_fresh_label $ revert
          asm_labels_set halt_lab rev_lab
          mapM_ comp_chandler hs
          comp_jump_to_handler 1
          end_block_op "end Dispatch"
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
          end_block_op "end HALT"
        revert = do
          asm_op $ EVM.PUSH1 [0]
          asm_push 1
          asm_op $ EVM.DUP1
          asm_push 1
          asm_op $ EVM.REVERT
          asm_pop 2
          end_block_op "end REVERT"

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
