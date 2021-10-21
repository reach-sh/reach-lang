{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader
-- import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
-- import Data.Either
import Data.Bits
import Control.Exception

type ConsensusEnv = M.Map DLVar DLVal
type Store = ConsensusEnv

type Balance = Integer

type Token = Integer

type Ledger = M.Map Account (M.Map Token Balance)

type Frontend = ( Account, SLPart, M.Map DLVar DLVal, [ DLTail ] )

type Frontends = [ Frontend ]

type NewPartActions = M.Map SLPart [ DLTail ]

data ConsensusNetworkState = ConsensusNetworkState
  { nw_ledger :: Ledger
  , nw_next_acc :: Integer
  , nw_next_token :: Integer
  }
  deriving (Eq)

type State = (ConsensusNetworkState, ConsensusEnv, Frontends, NewPartActions, DAppCode)

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

data DLVal
  = V_Null
  | V_Bool Bool
  | V_UInt Integer
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

data Env = Env
  { e_store :: Store
  }

-- Identify program states
type Id = Integer

-- All of the states visited
type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value


-- ## INTERPRETER ## --

class Interp a where
  interp :: a -> App DLVal

data InterpreterException = InterpreterException
  deriving Show

instance Exception InterpreterException

type PrimExpr = (PrimOp, [DLVal])

instance Interp PrimExpr where
  interp = \case
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
    (TOKEN_EQ, [V_Bytes lhs,V_Bytes rhs]) -> return $ V_Bool $ (==) lhs rhs
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
      stt <- asks e_store
      return $ stt M.! dlvar
    DLA_Constant _dlconst -> return $ V_UInt $ toInteger (maxBound :: Int)
    DLA_Literal dllit -> interp dllit
    -- TODO: handle interact case
    DLA_Interact _slpart _string _dltype -> undefined

instance Interp DLLiteral where
  interp = \case
    DLL_Null -> return $ V_Null
    DLL_Bool bool -> return $ V_Bool bool
    DLL_Int _at int -> return $ V_UInt int
    DLL_Bytes bytes -> return $ V_Bytes $ show bytes

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> case dllargearg of
      DLLA_Array _dltype dlargs -> V_Array <$> mapM interp dlargs
      DLLA_Tuple dlargs -> V_Tuple <$> mapM interp dlargs
      DLLA_Obj map_strs_to_dlargs -> V_Object <$> mapM interp map_strs_to_dlargs
      DLLA_Data _map_slvars_to_dltypes string dlarg -> do
        evd_arg <- interp $ dlarg
        return $ V_Data $ M.singleton string evd_arg
      DLLA_Struct assoc_slvars_dlargs -> do
        evd_args <- mapM (\arg -> interp arg) $ M.fromList assoc_slvars_dlargs
        return $ V_Struct $ M.toList evd_args
    DLE_Impossible _at _impossible_error -> error "ImpossibleError"
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- mapM interp dlargs
      interp $ (primop,evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> return $ arr !! (fromIntegral n)
        _ -> throw InterpreterException
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      ev3 <- interp dlarg3
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> do
          let n' = fromIntegral n
          return $ V_Array $ take n' arr ++ [ev3] ++ drop (n' + 1) arr
        _ -> throw InterpreterException
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> return $ V_Array $ arr1 ++ arr2
        _ -> throw InterpreterException
    DLE_ArrayZip _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> do
          return $ V_Array $ map (\(l,r)-> V_Array $ [l,r]) $ zip arr1 arr2
        _ -> throw InterpreterException
    DLE_TupleRef _at dlarg n -> do
      ev <- interp dlarg
      case ev of
        V_Tuple arr -> return $ arr !! (fromIntegral n)
        _ -> throw InterpreterException
    DLE_ObjectRef _at dlarg str -> do
      ev <- interp dlarg
      case ev of
        V_Object obj -> return $ obj M.! str
        _ -> throw InterpreterException
    DLE_Interact _at _slcxtframes _slpart _string _dltype _dlargs -> undefined
    DLE_Digest _at dlargs -> V_Digest <$> V_Array <$> mapM interp dlargs
    DLE_Claim _at _slcxtframes _claimtype _dlarg _maybe_bytestring -> undefined
    DLE_Transfer _at _dlarg1 _dlarg2 _maybe_dlarg -> undefined
    DLE_TokenInit _at _dlarg -> undefined
    DLE_CheckPay _at _slcxtframes _dlarg _maybe_dlarg -> undefined
    DLE_Wait _at _dltimearg -> undefined
    DLE_PartSet _at _slpart _dlarg -> undefined
    DLE_MapRef _at _dlm_var _dlarg -> undefined
    DLE_MapSet _at _dlm_var _dlarg _maybe_dlarg -> undefined
    DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill -> undefined
    DLE_TokenNew _at _dltokennew -> undefined
    DLE_TokenBurn _at _dlarg1 _dlarg2 -> undefined
    DLE_TokenDestroy _at _dlarg -> undefined

instance Interp DLStmt where
  interp = \case
    DL_Nop _at -> return V_Null
    DL_Let _at _let_var _expr -> undefined
    DL_ArrayMap _at _var1 _arg _var2 _block -> undefined
    DL_ArrayReduce _at _var1 _arg1 _arg2 _var2 _var3 _block -> undefined
    DL_Var _at _var -> undefined
    DL_Set _at _var _arg -> undefined
    DL_LocalDo _at _tail -> undefined
    DL_LocalIf _at _arg _tail1 _tail2 -> undefined
    DL_LocalSwitch _at _var _switch_cases -> undefined
    DL_Only _at _either_part _tail -> undefined
    DL_MapReduce _at _int _var1 _dlm_var _arg _var2 _var3 _block -> undefined

interpTailStmt :: DLTail -> App ()
interpTailStmt = \case
  DT_Return _at -> return ()
  DT_Com stmt dltail -> do
    _ <- interp stmt
    interpTailStmt dltail

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
interp1 :: LLProg -> App ()
interp1 (LLProg at llo ps dli dex dvs step) = interpStep (LLProg at llo ps dli dex dvs) step

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
inspect :: Id -> App State
inspect = undefined
