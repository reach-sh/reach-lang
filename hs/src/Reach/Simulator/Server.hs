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

type Session = [[Action]]

-- state
data Env = Env
  {  e_session :: Session
  ,  e_state :: Int
  }

type App a = ReaderT Env IO a

initProgSim :: LLProg -> App (S.App S.DLVal)
initProgSim ll = do
  local (\e -> e {e_session = [[]], e_state = 0}) $ return $ S.interp ll

unblockProg :: StateId -> ActionId -> S.DLVal -> ()
unblockProg = undefined

allStates :: [State]
allStates = undefined

actions :: StateId -> [Action]
actions = undefined

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
    json $ actions s

  post "/states/:s/actions/:a/?data=post_val" $ do
    s <- param "s"
    a <- param "a"
    v <- param "post_val"
    return $ unblockProg s a $ S.V_UInt v
