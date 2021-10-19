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

interpExpr :: DLExpr -> App DLArg
interpExpr = \case
  (DLE_Arg _loc _dlarg) -> undefined
  (DLE_LArg _loc _dllargearg) -> undefined
  (DLE_Impossible _loc _impossibleerror) -> undefined
  (DLE_PrimOp _loc _primop _dlargs) -> undefined
  (DLE_ArrayRef _loc _dlarg1 _dlarg2) -> undefined
  (DLE_ArraySet _loc _dlarg1 _dlarg2 _dlarg3) -> undefined
  (DLE_ArrayConcat _loc _dlarg1 _dlarg2) -> undefined
  (DLE_ArrayZip _loc _dlarg1 _dlarg2) -> undefined
  (DLE_TupleRef _loc _dlarg _integer) -> undefined
  (DLE_ObjectRef _loc _dlarg _string) -> undefined
  (DLE_Interact _loc _slcxtframes _slpart _string _dltype _dlargs) -> undefined
  (DLE_Digest _loc _dlargs) -> undefined
  (DLE_Claim _loc _slcxtframes _claimtype _dlarg _maybe_bytestring) -> undefined
  (DLE_Transfer _loc _dlarg1 _dlarg2 _maybe_dlarg) -> undefined
  (DLE_TokenInit _loc _dlarg) -> undefined
  (DLE_CheckPay _loc _slcxtframes _dlarg _maybe_dlarg) -> undefined
  (DLE_Wait _loc _dltimearg) -> undefined
  (DLE_PartSet _loc _slpart _dlarg) -> undefined
  (DLE_MapRef _loc _dlm_var _dlarg) -> undefined
  (DLE_MapSet _loc _dlm_var _dlarg _maybe_dlarg) -> undefined
  (DLE_Remote _loc _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill) -> undefined
  (DLE_TokenNew _loc _dltokennew) -> undefined
  (DLE_TokenBurn _loc _dlarg1 _dlarg2) -> undefined
  (DLE_TokenDestroy _loc _dlarg) -> undefined

interpStmt :: DLStmt -> App ()
interpStmt = \case
  (DL_Nop _loc) -> return ()
  (DL_Let _loc _let_var _expr) -> undefined
  (DL_ArrayMap _loc _var1 _arg _var2 _block) -> undefined
  (DL_ArrayReduce _loc _var1 _arg1 _arg2 _var2 _var3 _block) -> undefined
  (DL_Var _loc _var) -> undefined
  (DL_Set _loc _var _arg) -> undefined
  (DL_LocalDo _loc _tail) -> undefined
  (DL_LocalIf _loc _arg _tail1 _tail2) -> undefined
  (DL_LocalSwitch _loc _var _switch_cases) -> undefined
  (DL_Only _loc _either_part _tail) -> undefined
  (DL_MapReduce _loc _int _var1 _dlm_var _arg _var2 _var3 _block) -> undefined

interpTailStmt :: DLTail -> App ()
interpTailStmt = \case
  (DT_Return _loc) -> return ()
  (DT_Com stmt dltail) -> do
    _ <- interpStmt stmt
    interpTailStmt dltail

interpCons :: LLConsensus -> App ()
interpCons = \case
  (LLC_Com _stmt _cons) -> undefined
  (LLC_If _loc _arg _cons1 _cons2) -> undefined
  (LLC_Switch _loc _var _switch_cases) -> undefined
  (LLC_FromConsensus _loc1 _loc2 _step) -> undefined
  (LLC_While _loc _asn _inv _cond _body _k) -> undefined
  (LLC_Continue _loc _asn) -> undefined
  (LLC_ViewIs _loc _part _var _export _cons) -> undefined

interpStep :: (LLStep -> LLProg) -> LLStep -> App LLProg
interpStep pmeta = \case
  (LLS_Com stmt st') -> do
    _ <- interpStmt stmt
    interpStep pmeta st'
  (LLS_Stop loc) -> return $ pmeta (LLS_Stop loc)
  (LLS_ToConsensus _loc _tc_send _tc_recv _tc_mtime) -> undefined

-- evaluate a linear Reach program
interp :: LLProg -> App LLProg
interp (LLProg at llo ps dli dex dvs st) = interpStep (LLProg at llo ps dli dex dvs) st

interpM :: App LLProg -> App LLProg
interpM mprog = do
  prog <- mprog
  interp prog

-- evaluate the next N steps
interpN :: Integer -> LLProg -> App LLProg
interpN n prog = foldr (.) id (replicate (fromInteger n) interpM) $ return prog

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
