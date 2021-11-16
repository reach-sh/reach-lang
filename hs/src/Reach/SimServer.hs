{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}

module Reach.SimServer where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Reach.AST.LL
import qualified Reach.Simulator as S
import qualified Data.Map.Strict as M

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

type Program = Int

instance ToJSON State
instance FromJSON State

instance ToJSON Action
instance FromJSON Action

-- TODO: need a way to load real programs from parser
p :: Program
p = -1

type Session = M.Map Program (M.Map Action State)

fetchProgSrc :: Program -> LLProg
fetchProgSrc = undefined

initProg :: Program -> S.App S.DLVal
initProg p' = do
  -- TODO: init session
  -- NOTE: monad with session state
  let ll = fetchProgSrc p'
  S.interp ll

unblockProg :: Program -> StateId -> ActionId -> S.DLVal -> ()
unblockProg = undefined

allStates :: Program -> [State]
allStates = undefined

actions :: Program -> StateId -> [Action]
actions = undefined

matchesId :: Int -> (a -> Int) -> a -> Bool
matchesId i f a = f a == i

main :: IO ()
main = scotty portNumber $ do
  post "/init/:s" $ do
    s <- param "s"
    _ <- return $ initProg s
    return ()

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
