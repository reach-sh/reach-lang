{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.SimClient where

import Network.Curl

x = curlGet "www.reach.sh" []

-- Identify program states
-- type Id = Integer

-- creates the first state and returns its id
-- init :: LLProg -> App Id
-- init (LLProg _at _llo _ps _dli _dex _dvs _apis _s) = return 0

-- returns the set of possible reductions
-- actions  :: Id -> App [ Action ]
-- actions = undefined

-- applies an action (with its parameters) and returns a new state
-- apply :: Id -> Action -> Params -> App Id
-- apply = undefined

-- returns the children and how they can be derived
-- children :: Id -> App (M.Map Id [ (Action, Params) ])
-- children = undefined

-- returns the parent
-- parent :: Id -> App Id
-- parent = undefined

-- returns the state
-- inspect :: Id -> App State
-- inspect = undefined
