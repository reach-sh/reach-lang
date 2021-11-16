{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}

module Reach.SimServer where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Reach.Simulator as S

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

type Program = Int

instance ToJSON State
instance FromJSON State

instance ToJSON Action
instance FromJSON Action

-- TODO: need a way to load real programs from parser
p :: Program
p = -1

initProg :: Program -> ()
initProg = undefined

unblockProg :: Program -> StateId -> ActionId -> S.DLVal -> ()
unblockProg = undefined

allStates :: Program -> [State]
allStates = undefined

actions :: Program -> StateId -> [Action]
actions = undefined

matchesId :: Int -> (a -> Int) -> a -> Bool
matchesId i f a = f a == i

main :: IO ()
main = scotty 3000 $ do
  get "/states" $ do
    json $ allStates p

  get "/states/:s" $ do
    s <- param "s"
    json (filter (matchesId s state_id) $ allStates p)

  get "/states/:s/actions" $ do
    s <- param "s"
    json $ actions p s

  post "/states/:s/actions/:a/?data=post_val" $ do
    s <- param "s"
    a <- param "a"
    v <- param "post_val"
    return $ unblockProg p s a $ S.V_UInt v
