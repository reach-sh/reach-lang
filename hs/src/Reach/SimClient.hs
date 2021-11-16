{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.SimClient where

import Network.Curl

hostname :: [Char]
hostname = "http://localhost:3000"

initProg :: Int -> IO ()
initProg s = curlPost (hostname ++ "/init/" ++ show s) []

getProgStates :: IO ()
getProgStates = curlGet (hostname ++ "/states") []

getProgramState :: Int -> IO ()
getProgramState s = curlGet (hostname ++ "/states/" ++ show s) []

getStateActions :: Int -> IO ()
getStateActions s = curlGet (hostname ++ "/states/" ++ show s ++ "/actions") []

respondWithVal :: Int -> Int -> Int -> IO ()
respondWithVal s a v = do
  curlPost (hostname ++ "/states/" ++ show s ++ "/actions/" ++ show a ++ "/?data=" ++ show v) []

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
