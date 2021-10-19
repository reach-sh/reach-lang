{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
-- import Data.Either

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

data Env = Env
  { e_state :: IORef Session
  }

-- Identify program states
type Id = Integer

-- All of the states visited
type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value

-- ## interpreter ## --

interpExpr :: Store -> DLExpr -> App DLVal
interpExpr st = \case
  (DLE_Arg _at _dlarg) -> undefined
  (DLE_LArg _at _dllargearg) -> undefined
  (DLE_Impossible _at _impossibleerror) -> undefined
  (DLE_PrimOp _at _primop _dlargs) -> undefined
  (DLE_ArrayRef _at _dlarg1 _dlarg2) -> undefined
  (DLE_ArraySet _at _dlarg1 _dlarg2 _dlarg3) -> undefined
  (DLE_ArrayConcat _at _dlarg1 _dlarg2) -> undefined
  (DLE_ArrayZip _at _dlarg1 _dlarg2) -> undefined
  (DLE_TupleRef _at _dlarg _integer) -> undefined
  (DLE_ObjectRef _at _dlarg _string) -> undefined
  (DLE_Interact _at _slcxtframes _slpart _string _dltype _dlargs) -> undefined
  (DLE_Digest _at _dlargs) -> undefined
  (DLE_Claim _at _slcxtframes _claimtype _dlarg _maybe_bytestring) -> undefined
  (DLE_Transfer _at _dlarg1 _dlarg2 _maybe_dlarg) -> undefined
  (DLE_TokenInit _at _dlarg) -> undefined
  (DLE_CheckPay _at _slcxtframes _dlarg _maybe_dlarg) -> undefined
  (DLE_Wait _at _dltimearg) -> undefined
  (DLE_PartSet _at _slpart _dlarg) -> undefined
  (DLE_MapRef _at _dlm_var _dlarg) -> undefined
  (DLE_MapSet _at _dlm_var _dlarg _maybe_dlarg) -> undefined
  (DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill) -> undefined
  (DLE_TokenNew _at _dltokennew) -> undefined
  (DLE_TokenBurn _at _dlarg1 _dlarg2) -> undefined
  (DLE_TokenDestroy _at _dlarg) -> undefined

interpStmt :: Store -> DLStmt -> App Store
interpStmt st = \case
  (DL_Nop _at) -> return st
  (DL_Let _at _let_var _expr) -> undefined
  (DL_ArrayMap _at _var1 _arg _var2 _block) -> undefined
  (DL_ArrayReduce _at _var1 _arg1 _arg2 _var2 _var3 _block) -> undefined
  (DL_Var _at _var) -> undefined
  (DL_Set _at _var _arg) -> undefined
  (DL_LocalDo _at _tail) -> undefined
  (DL_LocalIf _at _arg _tail1 _tail2) -> undefined
  (DL_LocalSwitch _at _var _switch_cases) -> undefined
  (DL_Only _at _either_part _tail) -> undefined
  (DL_MapReduce _at _int _var1 _dlm_var _arg _var2 _var3 _block) -> undefined

interpTailStmt :: Store -> DLTail -> App Store
interpTailStmt st = \case
  (DT_Return _at) -> return st
  (DT_Com stmt dltail) -> do
    st' <- interpStmt st stmt
    interpTailStmt st' dltail

interpCons :: LLConsensus -> App ()
interpCons = \case
  (LLC_Com _stmt _cons) -> undefined
  (LLC_If _at _arg _cons1 _cons2) -> undefined
  (LLC_Switch _at _var _switch_cases) -> undefined
  (LLC_FromConsensus _at1 _at2 _step) -> undefined
  (LLC_While _at _asn _inv _cond _body _k) -> undefined
  (LLC_Continue _at _asn) -> undefined
  (LLC_ViewIs _at _part _var _export _cons) -> undefined

interpStep :: Store -> (LLStep -> LLProg) -> LLStep -> App (Store)
interpStep st pmeta = \case
  (LLS_Com stmt step) -> do
    st' <- interpStmt st stmt
    interpStep st' pmeta step
  (LLS_Stop loc) -> return st
  (LLS_ToConsensus _at _tc_send _tc_recv _tc_mtime) -> undefined

-- evaluate a linear Reach program
interp :: Store -> LLProg -> App (Store)
interp st (LLProg at llo ps dli dex dvs step) = interpStep st (LLProg at llo ps dli dex dvs) step

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
