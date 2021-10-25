{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader
-- import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util
-- import Data.Either
import Data.Bits
-- import Control.Concurrent

type ConsensusEnv = M.Map DLVar DLVal
type Store = ConsensusEnv

type Balance = Integer

type Token = Integer

data Ledger = Ledger
  { nw_ledger :: M.Map Account (M.Map Token Balance)
  , nw_next_acc :: Integer
  , nw_next_token :: Integer
  }
  deriving (Eq)

ledgerNewToken :: Integer -> App DLVal -> App DLVal
ledgerNewToken supply f  = do
  -- TODO QUESTION: which account? 0 is placeholder
  ledger <- asks e_ledger
  let token_id = nw_next_token ledger
  let new_nw_ledger = M.insert 0 (M.singleton token_id supply) (nw_ledger ledger)
  let new_ledger = ledger { nw_ledger = new_nw_ledger, nw_next_token = token_id + 1 }
  local (\e -> e {e_ledger = new_ledger}) f

type Frontend = ( Account, SLPart, M.Map DLVar DLVal, [ DLTail ] )

type Frontends = [ Frontend ]

type NewPartActions = M.Map SLPart [ DLTail ]

-- type State = (ConsensusNetworkState, ConsensusEnv, Frontends, NewPartActions, DAppCode)

data DAppCode
  = DAppLLCons LLConsensus
  | DAppLLStep LLStep

data UIAction
  = ContinueAction -- run until breakpoint or error
  | NextAction Integer -- proceeds through the next N computation steps
  | BTAction -- print the backtrace
  | ShowAction String -- print the variable described by this string

data Action
  = TieBreakAction
  | NewAccAction
  | NewPartAction
  | ImitateFrontendAction

type Account = Integer

type LinearState = M.Map DLMVar (M.Map Account DLVal)

data DLVal
  = V_Null
  | V_Bool Bool
  | V_UInt Integer
  | V_Token Int
  | V_Bytes String
  | V_Digest DLVal
  | V_Address Account
  | V_Array [DLVal]
  | V_Tuple [DLVal]
  | V_Object (M.Map SLVar DLVal)
  | V_Data (M.Map SLVar DLVal)
  | V_Struct [(SLVar, DLVal)]
  deriving (Eq, Ord, Show)

type App = ReaderT Env IO

-- data Env = Env
--   { e_state :: IORef Session
--   }

-- state
data Env = Env
  {  e_store :: Store
   , e_ledger :: Ledger
   , e_linstate :: LinearState
  }

-- Identify program states
type Id = Integer

-- All of the states visited
-- type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value


-- ## INTERPRETER ## --

class Interp a where
  interp :: a -> App DLVal

interpPrim :: (PrimOp, [DLVal]) -> App DLVal
interpPrim = \case
  (ADD, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (+) lhs rhs
  (SUB, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (-) lhs rhs
  (MUL, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (*) lhs rhs
  (DIV, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ div lhs rhs
  (MOD, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ mod lhs rhs
  (PLT, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (<) lhs rhs
  (PLE, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (<=) lhs rhs
  (PEQ, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (==) lhs rhs
  (PGE, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (>=) lhs rhs
  (PGT, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (>) lhs rhs
  (IF_THEN_ELSE, [V_Bool cond, cond_val, alt]) -> do
    return $ if cond then cond_val else alt
  (DIGEST_EQ,  [V_Digest lhs,V_Digest rhs]) -> return $ V_Bool $ (==) lhs rhs
  (ADDRESS_EQ,  [V_Address lhs,V_Address rhs]) -> return $ V_Bool $ (==) lhs rhs
  (TOKEN_EQ, [V_Token lhs,V_Token rhs]) -> return $ V_Bool $ (==) lhs rhs
  -- TODO QUESTION
  (SELF_ADDRESS, _) -> undefined
  (LSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftL lhs (fromIntegral rhs)
  (RSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftR lhs (fromIntegral rhs)
  (BAND, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.&.) lhs rhs
  (BIOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.|.) lhs rhs
  (BXOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ xor lhs rhs
  (BYTES_CONCAT, [V_Bytes lhs,V_Bytes rhs]) -> return $ V_Bytes $ (++) lhs rhs

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      st <- asks e_store
      return $ st M.! dlvar
    -- TODO: fake constants per connector
    DLA_Constant _dlconst -> return $ V_UInt $ toInteger (maxBound :: Int)
    DLA_Literal dllit -> interp dllit
    -- TODO: handle interact case: needs participant frontend state
    DLA_Interact _slpart _string _dltype -> undefined

instance Interp DLLiteral where
  interp = \case
    DLL_Null -> return $ V_Null
    DLL_Bool bool -> return $ V_Bool bool
    DLL_Int _at int -> return $ V_UInt int
    DLL_Bytes bytes -> return $ V_Bytes $ show bytes

instance Interp DLLargeArg where
  interp = \case
    DLLA_Array _dltype dlargs -> V_Array <$> mapM interp dlargs
    DLLA_Tuple dlargs -> V_Tuple <$> mapM interp dlargs
    DLLA_Obj map_strs_to_dlargs -> V_Object <$> mapM interp map_strs_to_dlargs
    DLLA_Data _map_slvars_to_dltypes string dlarg -> do
      evd_arg <- interp $ dlarg
      return $ V_Data $ M.singleton string evd_arg
    DLLA_Struct assoc_slvars_dlargs -> do
      evd_args <- mapM (\arg -> interp arg) $ M.fromList assoc_slvars_dlargs
      return $ V_Struct $ M.toList evd_args

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> interp dllargearg
    DLE_Impossible at err -> expect_thrown at err
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- mapM interp dlargs
      interpPrim (primop,evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> return $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      ev3 <- interp dlarg3
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> do
          let n' = fromIntegral n
          return $ V_Array $ take n' arr ++ [ev3] ++ drop (n' + 1) arr
        _ -> impossible "expression interpreter"
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> return $ V_Array $ arr1 ++ arr2
        _ -> impossible "expression interpreter"
    DLE_ArrayZip _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> do
          return $ V_Array $ map (\(l,r)-> V_Tuple $ [l,r]) $ zip arr1 arr2
        _ -> impossible "expression interpreter"
    DLE_TupleRef _at dlarg n -> do
      ev <- interp dlarg
      case ev of
        V_Tuple arr -> return $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ObjectRef _at dlarg str -> do
      ev <- interp dlarg
      case ev of
        V_Object obj -> return $ obj M.! str
        _ -> impossible "expression interpreter"
    DLE_Interact _at _slcxtframes _slpart _string _dltype _dlargs -> undefined
    DLE_Digest _at dlargs -> V_Digest <$> V_Tuple <$> mapM interp dlargs
    -- TODO
    DLE_Claim _at _slcxtframes claimtype dlarg _maybe_bytestring -> case claimtype of
      CT_Assert -> undefined
      CT_Assume _bool -> undefined
      CT_Require -> interp dlarg
      CT_Possible -> return $ V_Null
      CT_Unknowable _slpart _dlargs -> return $ V_Null
    DLE_Transfer _at dlarg1 dlarg2 _maybe_dlarg -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        -- TODO: needs ledger\account state
        (V_UInt _n, V_Address _acc) -> undefined
        _ -> impossible "expression interpreter"
    DLE_TokenInit _at _dlarg -> return V_Null
    -- QUESTION: checkCommitment?
    DLE_CheckPay _at _slcxtframes _dlarg _maybe_dlarg -> undefined
    -- TODO requires Time state
    DLE_Wait _at dltimearg -> case dltimearg of
      Left _dlarg -> do
        return $ V_Null
      Right _dlarg -> do
        return $ V_Null
    DLE_PartSet _at _slpart dlarg -> interp dlarg
    DLE_MapRef _at dlmvar dlarg -> do
      linstate <- asks e_linstate
      ev <- interp dlarg
      case ev of
        V_Address acc -> do
          return $ flip (M.!) acc $ (M.!) linstate dlmvar
    DLE_MapSet _at dlmvar dlarg maybe_dlarg -> do
      linst <- asks e_linstate
      ev <- interp dlarg
      case ev of
        V_Address acc -> case maybe_dlarg of
          Nothing -> do
            let m = M.delete acc ((M.!) linst dlmvar)
            local (\e -> e {e_linstate = M.insert dlmvar m linst}) $ return V_Null
          Just dlarg' -> do
            ev' <- interp dlarg'
            let m = M.insert acc ev' ((M.!) linst dlmvar)
            local (\e -> e {e_linstate = M.insert dlmvar m linst}) $ return V_Null
    -- TODO QUESTION
    DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill -> undefined
    DLE_TokenNew _at dltokennew -> do
      supply <- interp $ dtn_supply dltokennew
      case supply of
        V_UInt supply' -> do
          ledgerNewToken supply' $ return V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_UInt tok, V_UInt burn_amt) -> do
          ledger <- asks e_ledger
          let map_ledger = nw_ledger ledger
          let prev_amt = flip (M.!) tok $ (M.!) map_ledger 0
          let new_amt = prev_amt - burn_amt
          let new_nw_ledger = M.insert 0 (M.insert tok new_amt ((M.!) map_ledger 0)) map_ledger
          local (\e -> e {e_ledger = ledger { nw_ledger = new_nw_ledger }}) $ return V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenDestroy _at dlarg -> do
      ev <- interp dlarg
      case ev of
        V_UInt tok -> do
          ledger <- asks e_ledger
          let map_ledger = nw_ledger ledger
          let new_nw_ledger = M.insert 0 (M.delete tok ((M.!) map_ledger 0)) map_ledger
          local (\e -> e {e_ledger = ledger { nw_ledger = new_nw_ledger }}) $ return V_Null
        _ -> impossible "expression interpreter"

instance Interp DLStmt where
  interp = \case
    DL_Nop _at -> return V_Null
    DL_Let _at let_var expr -> case let_var of
      DLV_Eff -> undefined
      DLV_Let _ var -> do
        st <- asks e_store
        ev <- interp expr
        local (\e -> e {e_store = M.insert var ev st}) $ return V_Null
    DL_ArrayMap _at var1 arg var2 block -> do
      st <- asks e_store
      let f var x = local (\e -> e {e_store = M.insert var x st}) $ interp block
      arr <- interp arg
      case arr of
        V_Array arr' -> do
          res <- V_Array <$> mapM (\x -> f var2 x) arr'
          local (\e -> e {e_store = M.insert var2 res st}) $ return V_Null
        _ -> impossible "statement interpreter"
    DL_ArrayReduce _at _var1 _arg1 _arg2 _var2 _var3 _block -> undefined
    DL_Var _at _var -> return $ V_Null
    DL_Set _at var arg -> do
      st <- asks e_store
      ev <- interp arg
      local (\e -> e {e_store = M.insert var ev st}) $ return V_Null
    DL_LocalDo _at dltail -> interp dltail
    DL_LocalIf _at arg tail1 tail2 -> do
      ev <- interp arg
      case ev of
        V_Bool True -> interp tail1
        V_Bool False -> interp tail2
        _ -> impossible "statement interpreter"
    DL_LocalSwitch _at var switch_cases -> do
      st <- asks e_store
      ev <- interp $ DLA_Var var
      case ev of
        V_Data v -> do
          let switch_key = flip (!!) 0 $ M.keys v
          let (switch_binding, dltail) = (M.!) switch_cases switch_key
          case switch_binding of
            Nothing -> interp dltail
            Just ident -> local (\e -> e {e_store = M.insert ident ev st}) $ interp dltail
    -- QUESTION: what does this Either represent?
    DL_Only _at _either_part dltail -> interp dltail
    DL_MapReduce _at _int _var1 _dlm_var _arg _var2 _var3 _block -> undefined

instance Interp DLTail where
  interp = \case
    DT_Return _at -> return V_Null
    DT_Com stmt dltail -> do
      _ <- interp stmt
      interp dltail

instance Interp DLBlock where
  interp = \case
    DLBlock _at _frame dltail dlarg -> do
      interp dltail
      interp dlarg

interpCons :: LLConsensus -> App ()
interpCons = \case
  LLC_Com _stmt _cons -> undefined
  LLC_If _at _arg _cons1 _cons2 -> undefined
  LLC_Switch _at _var _switch_cases -> undefined
  LLC_FromConsensus _at1 _at2 _step -> undefined
  LLC_While _at _asn _inv _cond _body _k -> undefined
  LLC_Continue _at _asn -> undefined
  LLC_ViewIs _at _part _var _export _cons -> undefined

interpStep :: (LLStep -> LLProg) -> LLStep -> App ()
interpStep pmeta = \case
  LLS_Com stmt step -> do
    _ <- interp stmt
    interpStep pmeta step
  LLS_Stop _loc -> return ()
  LLS_ToConsensus _at _tc_send _tc_recv _tc_mtime -> undefined

-- evaluate a linear Reach program
interpProgram :: LLProg -> App ()
interpProgram (LLProg at llo ps dli dex dvs step) = interpStep (LLProg at llo ps dli dex dvs) step

-- interpM :: Store -> LLProg -> App (Store)
-- interpM st mprog = do
--   prog <- mprog
--   interp st prog

-- evaluate the next N steps
-- interpN :: Store -> Integer -> LLProg -> App (Store)
-- interpN st 0 prog = return st
-- interpN st n prog = do
--   st' <- interp st prog
--   interpN st' (n-1) ???

-- creates the first state and returns its id
init :: LLProg -> App Id
init (LLProg _at _llo _ps _dli _dex _dvs _s) = return 0

-- returns the set of possible reductions
actions  :: Id -> App [ Action ]
actions = undefined

-- applies an action (with its parameters) and returns a new state. Memoizes
apply :: Id -> Action -> Params -> App Id
apply = undefined

-- returns the children and how they can be derived
children :: Id -> App (M.Map Id [ (Action, Params) ])
children = undefined

-- returns the parent
parent :: Id -> App Id
parent = undefined

-- returns the state
-- inspect :: Id -> App State
-- inspect = undefined
