{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}

module Reach.SimServer where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Reach.Simulator as S

data State = State
  { state_id :: Int
  }
  deriving (Show, Generic)

data Action = Action
  { action_id :: Int
  , action_type :: S.Action
  }
  deriving (Generic)

type Program = Int

instance ToJSON State
instance FromJSON State

instance ToJSON Action
instance FromJSON Action

p :: Program
p = -1

allStates :: Program -> [State]
allStates = undefined

actions :: Program -> State -> [Action]
actions = undefined

matchesId :: Int -> (a -> Int) -> a -> Bool
matchesId i f a = f a == i

main :: IO ()
main = scotty 3000 $ do
  get "/states" $ do
    json $ allStates p

  get "/states/:id" $ do
    i <- param "id"
    json (filter (matchesId i state_id) $ allStates p)

  -- get "/states/:id/actions" $ do
  --   i <- param "id"
  --   json $ actions p i

  -- get "/states/:id1/actions/:id2" $ do
  --   i1 <- param "id1"
  --   i2 <- param "id2"
  --   json (filter (matchesId i2 action_id) $ actions p i1)


-- Identify program states
-- type Id = Integer

-- All of the states visited
-- type Session = M.Map Id State

-- free-form & untyped parameters for the action
-- type Params = String -- JSON.Value
