{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Natural
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL

type ConsensusEnv = M.Map DLVar DLVal

type Balance = Natural

type Token = Natural

type Ledger = M.Map Account (M.Map Token Balance)

type Frontend = ( Account, SLPart, M.Map DLVar DLVal, [ DLTail ] )

type Frontends = [ Frontend ]

type NewPartActions = M.Map SLPart [ DLTail ]

data ConsensusNetworkState = ConsensusNetworkState
  { nw_ledger :: Ledger
  , nw_next_acc :: Natural
  , nw_next_token :: Natural
  }
  deriving (Eq)

type State = (ConsensusNetworkState, ConsensusEnv, Frontends, NewPartActions, DAppCode)

data DAppCode
  = DAppLLCons LLConsensus
  | DAppLLStep LLStep

data UIAction
  = ContinueAction -- run until breakpoint or error
  | NextAction Natural -- proceeds through the next N computation steps
  | BTAction -- print the backtrace
  | ShowAction String -- print the variable described by this string

data Action
  = TieBreakAction
  | NewAccAction
  | NewPartAction
  | ImitateFrontendAction

type Account = Natural

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
type Id = Natural

-- All of the states visited
type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value

-- interpreter
type Gas = Natural

-- determine if program is already a value
get_prog_val :: LLProg -> Maybe DLVal
get_prog_val = undefined

interp :: Gas -> LLProg -> App (Maybe DLVal)
interp 0 prog = return $ get_prog_val prog
interp _n _prog = undefined

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
