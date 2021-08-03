{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Debugger where

import Control.Monad.Reader

import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL

-- NOTE: let several base types be Integer/String as a placeholder for now

type ConsensusEnv = M.Map DLVar DLVal

type Balances = [Double]

type Ledger = M.Map Account Balances

type Frontend = ( Account, SLPart, M.Map DLVar DLVal, [ DLTail ] )

type Frontends = [ Frontend ]

type NewPartActions = M.Map SLPart [ DLTail ]

type State = (Ledger, ConsensusEnv, Frontends, NewPartActions, Action)

-- possible backend actions
data Action
  = ConsAction LLConsensus
  | StepAction LLStep

data Contract = Contract
  { contr_contract_info :: Integer
  , address :: String
  , contr_token :: String
  }

data Account = Account
  { nw_acc :: Integer
  , backend :: LLProg
  , contract :: Contract
  , acc_contract_info :: Integer
  , acc_token :: String
  }

data DLVal
  = V_Null
  | V_Bool Bool
  | V_UInt Integer
  | V_Bytes Integer
  -- TODO
  -- | V_Digest
  -- | V_Address
  -- | V_Token
  | V_Array [Integer]
  | V_Tuple [DLVal]
  | V_Object (M.Map SLVar DLVal)
  | V_Data (M.Map SLVar DLVal)
  | V_Struct [(SLVar, DLVal)]
  deriving (Eq, Ord)

type App = ReaderT Env IO

data Env = Env
  { e_id :: Integer
  , e_state :: State
  }

-- Identify program states
type Id = Int

-- All of the states visited
type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value

-- creates the first state and returns its id
init :: LLProg -> App Id
init = undefined

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
