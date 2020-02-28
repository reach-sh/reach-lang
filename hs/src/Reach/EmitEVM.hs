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
  ( CompiledSol
  , CCounts
  , usesCTail )

m_insertSafe :: Show k => Ord k => k -> v -> M.Map k v -> M.Map k v
m_insertSafe k v m =
  case M.lookup k m of
    Nothing -> M.insert k v m
    Just _ ->
      error $ "m_insertSafe: Key already set: " ++ show k

type ASMMonad ann a = State ASMSt a
type Label = Int
data LabelInfo = LabelInfo Int Int
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
data EVMRegion
  = ER_Mem
  | ER_CallData
  deriving (Show, Eq, Ord)
type CompileSt =
  ( Int --- stack depth
  , Int --- call_data_ptr
  , Int --- mem free pointer
  , M.Map BLVar (EVMRegion, Int) --- variable to mem addr
  )

--- FIXME Add JUMPDEST

empty_asm_st :: ASMSt
empty_asm_st = ( empty_linker_st, empty_compile_st )
empty_linker_st :: LinkerSt
empty_linker_st = ( 0, M.empty, empty_label_st, Q.empty )
empty_label_st :: LabelSt
empty_label_st = ( 0, 0, M.empty )
empty_compile_st :: CompileSt
empty_compile_st = ( 0, 0, 0, M.empty )

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

asm_with_label_handler :: Int -> ASMMonad ann () -> ASMMonad ann ()
asm_with_label_handler which hand_code = asm_fresh_label_obs $ \label -> do
  asm_labels_set_handler which label
  hand_code

-- FIXME Have asm_push/pop/stack be inside of asm_op, or maybe as some sort of pass during assembly
asm_stack :: Int -> Int -> ASMMonad ann ()
asm_stack outs ins = do
  ( ls, cs ) <- get
  let ( stack, cdp, mp, vmap ) = cs
  let stack' = stack - outs + ins
  if stack' > 1023 then
    error "asm_stack: Depth is too deep"
  else do
    let cs' = ( stack', cdp, mp, vmap )
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

asm_malloc :: EVMRegion -> BLVar -> ASMMonad ann ()
asm_malloc reg v = do
  let sz = 32 --- XXX Be sensitive to the actual size of things in memory
  ( ls, cs ) <- get
  let ( sd, cdp, mp, vmap ) = cs
  let ( cdp', mp', tp ) =
        case reg of
          ER_Mem -> ( cdp, mp + sz, mp )
          ER_CallData -> ( cdp + sz, mp, cdp )
  let vmap' = m_insertSafe v (reg, tp) vmap
  let cs' = ( sd, cdp', mp', vmap' )
  put ( ls, cs' )

asm_free_all :: ASMMonad ann ()
asm_free_all = do
  ( ls, cs ) <- get
  let ( sd, _cdp, _mp, _vmap ) = cs
  let cs' = ( sd, 0, 0, M.empty )
  put ( ls, cs' )

asm_malloc_args :: EVMRegion -> [BLVar] -> ASMMonad ann ()
asm_malloc_args which args = mapM_ (asm_malloc which) args

asm_vmap_ref :: BLVar -> ASMMonad ann (EVMRegion, Int)
asm_vmap_ref v = do
  ( _, cs ) <- get
  let ( _, _, _, vmap ) = cs
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

comp_con :: Constant -> ASMMonad ann ()
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

comp_blvar :: BLVar -> ASMMonad ann ()
comp_blvar v = do
  (reg, addr) <- asm_vmap_ref v
  --- XXX If v is a byte-string, then just hold the pointer
  asm_op $ EVM.PUSH1 [ fromIntegral $ addr ]
  case reg of
    ER_Mem -> asm_op $ EVM.MLOAD
    ER_CallData -> asm_op $ EVM.CALLDATALOAD
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
        BALANCE -> p_op EVM.BALANCE 0
        TXN_VALUE -> p_op EVM.CALLVALUE 0
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
        _ -> do end_block_op $ "XXX comp_cexpr C_PrimApp " ++ show cp
                km
  where p_op o amt = do asm_op o
                        asm_stack amt 1
                        km
        p_op_not o amt = do asm_op o
                            asm_stack amt 1
                            asm_op EVM.NOT
                            km

comp_cstmt :: CStmt a -> ASMMonad ann  ()
comp_cstmt s =
  case s of
    C_Claim _ CT_Possible _ -> return ()
    C_Claim _ CT_Assert _ -> return ()
    C_Claim _ _ a -> do
      comp_blarg a
      asm_op EVM.NOT
      revert_lab <- asm_label_revert
      comp_jump_to_label revert_lab True
    C_Transfer _ p a -> do
      comp_blvar p
      comp_blarg a
      end_block_op $ "XXX C_Transfer effect"

comp_state_check :: FromSpec -> Bool -> Int -> BLArg a -> [BLVar] -> ASMMonad ann ()
comp_state_check _from_spec _is_timeout _last_i _delay _svs =
  end_block_op $ "XXX comp_state_check"

comp_state_set :: Int -> [BLVar] -> ASMMonad ann ()
comp_state_set _i _svs =
  end_block_op $ "XXX comp_state_set"

comp_set_args :: [BLArg a] -> ASMMonad ann ()
comp_set_args _args =
  end_block_op $ "XXX comp_set_args"

comp_malloc_and_store :: BLVar -> ASMMonad ann ()
comp_malloc_and_store v = do
  asm_malloc ER_Mem v
  (_, v_addr) <- asm_vmap_ref v
  asm_op $ EVM.PUSH1 [ fromIntegral v_addr ]
  asm_push 1
  asm_op $ EVM.MSTORE
  asm_pop 2

--- current_state is key 0
comp_ctail :: CCounts -> CTail a -> ASMMonad ann ()
comp_ctail ccs t =
  case t of
    C_Halt _ -> do
      halt_lab <- asm_label_halt
      comp_jump_to_label halt_lab False
    C_Wait _ i svs -> do
      comp_state_set i svs
    C_If _ ca tt ft -> do
      comp_blarg ca
      tlab <- asm_fresh_label $ comp_ctail ccs tt
      comp_jump_to_label tlab True
      comp_ctail ccs ft
    C_Let _ bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> comp_ctail ccs kt
        _ -> comp_cexpr ce $ do
          comp_malloc_and_store bv
          comp_ctail ccs kt
    C_Do _ ds kt -> do
      comp_cstmt ds
      comp_ctail ccs kt
    C_Jump loc which vs _ as -> do
      comp_set_args $ (map (BL_Var loc) vs) ++ as
      comp_jump_to_handler which

comp_chandler :: CHandler a -> ASMMonad ann  ()
comp_chandler (C_Handler _ from_spec is_timeout (last_i, svs) msg delay body i) = asm_with_label_handler i $ do
  end_block_op $ "XXX from_spec"
  asm_malloc_args ER_CallData $ svs ++ msg
  comp_state_check from_spec is_timeout last_i delay svs
  comp_ctail (usesCTail body) body
  end_block_op ("</Handler " ++ show i ++ ">")
  asm_free_all
comp_chandler (C_Loop _ svs args _inv body i) = asm_with_label_handler i $ do
  asm_malloc_args ER_Mem $ svs ++ args
  comp_ctail (usesCTail body) body
  end_block_op ("</Loop " ++ show i ++ ">")
  asm_free_all

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
          mapM_ comp_chandler hs
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
