{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Reach.Simulator.Server where

import Reach.AST.LL
import Control.Monad.Reader
import qualified Reach.Simulator.Core as S
import Control.Concurrent.STM
import Data.Default.Class
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans
-- import GHC.Generics
-- import qualified Data.Map.Strict as M
-- import Data.Aeson (FromJSON, ToJSON)

instance Default Session where
  def = initSession

newtype WebM a = WebM { runWebM :: ReaderT (TVar Session) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar Session))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (Session -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (Session -> Session) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

type State = Int
type Action = Int

portNumber :: Int
portNumber = 3000

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

initProgSim :: LLProg -> WebM S.PartState
initProgSim ll = do
  let initSt = S.initState
  ps <- return $ S.initApp ll initSt
  sid <- gets e_nsid
  actions <- gets e_actions
  let newActionsM =
        case ps of
          S.PS_Done _ _ -> [registerAction S.A_None]
          S.PS_Suspend a _ _ -> [registerAction a]
  let new_res =
        case ps of
          S.PS_Done _ v -> v
          S.PS_Suspend _ _ _ -> S.V_Null
  newActions <- mapM id newActionsM
  modify $ \ st -> st {e_actions = actions ++ [newActions]}
    {e_nsid = sid+1}
    {e_pthread = ps}
    {e_res = new_res}
  return ps

registerAction :: S.Action -> WebM Action
registerAction = undefined

-- TODO: which program to unblock?
-- NOTE: need to store cont at each step
unblockProg :: State -> Action -> S.DLVal -> ()
unblockProg = undefined

allStates :: WebM [State]
allStates = do
  a <- gets e_nsid
  return [0..(a-1)]

computeActions :: State -> WebM [Action]
computeActions = undefined

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync
    scottyT portNumber runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/states" $ do
      ss <- webM $ allStates
      json $ ss

    get "/states/:s" $ do
      s <- param "s"
      ss <- webM $ allStates
      json (filter ((==) s) $ ss)

    get "/states/:s/actions" $ do
      s <- param "s"
      as <- webM $ computeActions s
      json $ as

    post "/states/:s/actions/:a/?data=post_val" $ do
      s <- param "s"
      a <- param "a"
      v <- param "post_val"
      return $ unblockProg s a $ S.V_UInt v
