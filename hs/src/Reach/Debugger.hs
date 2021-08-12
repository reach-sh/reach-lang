{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL

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

-- interpreter
interpStmt :: DLStmt -> App LLProg
interpStmt = undefined

interpStep ::  (LLStep -> LLProg) -> LLStep -> App LLProg
interpStep pmeta = \case
  (LLS_Com stmt st') -> do
    _ <- interpStmt stmt
    interpStep pmeta st'
  (LLS_Stop loc) -> return $ pmeta (LLS_Stop loc)
  (LLS_ToConsensus _tc_at _tc_send _tc_recv _tc_mtime) -> undefined

interp :: LLProg -> App LLProg
interp (LLProg at llo ps dli dex dvs st) = interpStep (LLProg at llo ps dli dex dvs) st

interpM :: App LLProg -> App LLProg
interpM mprog = do
  prog <- mprog
  interp prog

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
