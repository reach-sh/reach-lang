{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Reach.Simulator.Server where

import Reach.AST.LL
import Control.Monad.Reader
import qualified Reach.Simulator.Core as C
import Control.Concurrent.STM
import Data.Default.Class
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans
import qualified Data.Map.Strict as M
-- import GHC.Generics
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

type StateId = Int
type ActionId = Int

portNumber :: Int
portNumber = 3000

-- state
data Session = Session
  {  e_states_actions :: M.Map StateId [ActionId]
  ,  e_nsid :: Int -- next state id
  ,  e_naid :: Int -- next action id
  ,  e_res :: C.DLVal
  ,  e_ids_actions :: M.Map ActionId C.Action
  ,  e_states_ks :: M.Map StateId C.PartState
  }

initSession :: Session
initSession = Session
  { e_states_actions = M.empty
  , e_nsid = 0
  , e_naid = 0
  , e_res = C.V_Null
  , e_ids_actions = M.empty
  , e_states_ks = M.empty
  }

initProgSim :: LLProg -> WebM C.PartState
initProgSim ll = do
  let initSt = C.initState
  ps <- return $ C.initApp ll initSt
  processNewState ps

processNewState :: C.PartState -> WebM C.PartState
processNewState ps = do
  sid <- gets e_nsid
  _ <- case ps of
    C.PS_Done _ _ -> do
      _ <- return $ putStrLn "EVAL DONE"
      registerAction sid C.A_None
    C.PS_Suspend a _ _ -> registerAction sid a
  let new_res =
        case ps of
          C.PS_Done _ v -> v
          C.PS_Suspend _ _ _ -> C.V_Null
  modify $ \ st -> st
    {e_nsid = sid + 1}
    {e_res = new_res}
    {e_states_ks = M.singleton sid ps}
  return ps

registerAction :: StateId -> C.Action -> WebM ActionId
registerAction sid act = do
  aid <- gets e_naid
  modify $ \ st -> st {e_naid = aid + 1}
  stacts <- gets e_states_actions
  idacts <- gets e_ids_actions
  modify $ \ st -> st {e_ids_actions = M.insert aid act idacts}
  case M.lookup sid stacts of
    Nothing -> modify $ \ st -> st {e_states_actions = M.insert sid [aid] stacts }
    Just acts -> modify $ \ st -> st {e_states_actions = M.insert sid (aid:acts) stacts }
  return aid

unblockProg :: StateId -> ActionId -> C.DLVal -> WebM ()
unblockProg sid aid v = do
  stks <- gets e_states_ks
  av_actions <- gets e_ids_actions
  case M.lookup sid stks of
    Nothing -> do
      _ <- return $ putStrLn "previous state not found"
      return ()
    Just (C.PS_Suspend _a cst k) -> do
      case M.lookup aid av_actions of
        Just (C.A_ChangePart part_id) -> do
          let ps = k cst{C.e_partid = part_id} v
          _ <- processNewState ps
          return ()
        -- TODO
        _ -> do
          let ps = k cst v
          _ <- processNewState ps
          return ()
    Just (C.PS_Done _ _) -> do
      _ <- return $ putStrLn "previous state already terminated"
      return ()

allStates :: WebM [StateId]
allStates = do
  a <- gets e_nsid
  return [0..(a-1)]

computeActions :: StateId -> WebM [ActionId]
computeActions sid = do
  stacts <- gets e_states_actions
  case M.lookup sid stacts of
    Nothing -> return []
    Just acts -> return acts

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
    webM $ unblockProg s a $ C.V_UInt v
    return ()
