{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}

module Reach.Simulator.Server where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Reach.AST.LL
import Control.Monad.Reader
import qualified Reach.Simulator.Core as S
-- import qualified Data.Map.Strict as M

type StateId = Int
type ActionId = Int

data State = State
  { state_id :: StateId
  }
  deriving (Show, Generic)

data Action = Action
  { action_id :: ActionId
  , action_type :: S.Action
  }
  deriving (Generic)

portNumber :: Int
portNumber = 3000

instance ToJSON State
instance FromJSON State

instance ToJSON Action
instance FromJSON Action

type Actions = [[Action]]

-- state
data Session = Session
  {  e_actions :: Actions
  ,  e_nsid :: Int -- next state id
  ,  e_naid :: Int -- next action id
  ,  e_pthread :: S.PartState
  ,  e_res :: S.DLVal
  }

initSession :: Session
initSession = Session
  { e_actions = [[]]
  , e_nsid = 0
  , e_naid = 0
  , e_pthread = S.initPartState
  , e_res = S.V_Null
  }

type App a = ReaderT Session IO a

initProgSim :: LLProg -> App S.PartState
initProgSim ll = do
  let st = S.initState
  ps <- local (\_ -> initSession) $ return $ S.initApp ll st
  sid <- asks e_nsid
  actions <- asks e_actions
  let new_actions =
        case ps of
          S.PS_Done _ _ -> [registerAction S.A_None]
          S.PS_Suspend a _ _ -> [registerAction a]
  let new_res =
        case ps of
          S.PS_Done _ v -> v
          S.PS_Suspend _ _ _ -> S.V_Null
  local (\e ->
    e {e_actions = actions ++ [new_actions]}
      {e_nsid = sid+1}
      {e_pthread = ps}
      {e_res = new_res})
      $ return ps

registerAction :: S.Action -> Action
registerAction = undefined

-- TODO: which program to unblock?
-- NOTE: need to store cont at each step
unblockProg :: StateId -> ActionId -> S.DLVal -> ()
unblockProg = undefined

allStates :: [State]
allStates = undefined

computeActions :: StateId -> [Action]
computeActions = undefined

matchesId :: Int -> (a -> Int) -> a -> Bool
matchesId i f a = f a == i

main :: IO ()
main = scotty portNumber $ do
  get "/states" $ do
    json $ allStates

  get "/states/:s" $ do
    s <- param "s"
    json (filter (matchesId s state_id) $ allStates)

  get "/states/:s/actions" $ do
    s <- param "s"
    json $ computeActions s

  post "/states/:s/actions/:a/?data=post_val" $ do
    s <- param "s"
    a <- param "a"
    v <- param "post_val"
    return $ unblockProg s a $ S.V_UInt v
