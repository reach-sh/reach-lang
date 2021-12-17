{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Server where

import Reach.AST.LL
import Reach.Util
import Control.Monad.Reader
import qualified Reach.Simulator.Core as C
import Control.Concurrent.STM
import Data.Default.Class
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans
import qualified Data.Map.Strict as M
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

instance Default Session where
  def = initSession

newtype WebM a = WebM { runWebM :: ReaderT (TVar Session) IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (TVar Session))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (Session -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (Session -> Session) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

type StateId = Int
type ActionId = Int

portNumber :: Int
portNumber = 3001

type Graph = M.Map StateId C.State

data Status = Initial | Running | Done
  deriving (Show, Generic)

instance ToJSON Status
instance FromJSON Status

data Session = Session
  { e_states_actions :: M.Map StateId [ActionId]
  , e_nsid :: Int
  , e_naid :: Int
  , e_ids_actions :: M.Map ActionId C.Action
  , e_actors_states_ks :: M.Map C.ActorId (M.Map StateId C.PartState)
  , e_actorid :: C.ActorId
  , e_graph :: Graph
  , e_src :: Maybe LLProg
  , e_status :: Status
  }

initSession :: Session
initSession = Session
  { e_states_actions = mempty
  , e_nsid = 0
  , e_naid = 0
  , e_ids_actions = mempty
  , e_actors_states_ks = mempty
  , e_actorid = fromIntegral C.consensusId
  , e_graph = mempty
  , e_src = Nothing
  , e_status = Initial
  }

processNewState :: C.PartState -> WebM ()
processNewState ps = do
  sid <- gets e_nsid
  _ <- case ps of
    C.PS_Done _ _ -> do
      _ <- return $ putStrLn "EVAL DONE"
      registerAction sid C.A_None
    C.PS_Suspend a _ _ -> registerAction sid a
  let (new_st, stat) =
        case ps of
          C.PS_Done s _ -> (s, Done)
          C.PS_Suspend _ s _ -> (s, Running)
  graph <- gets e_graph
  astks <- gets e_actors_states_ks
  actorId <- gets e_actorid
  let sks = case M.lookup actorId astks of
        Nothing -> mempty
        Just m -> m
  modify $ \ st -> st
    {e_nsid = sid + 1}
    {e_actors_states_ks = M.insert actorId (M.insert sid ps sks) astks}
    {e_status = stat}
    {e_graph = M.insert sid new_st graph}

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
  astks <- gets e_actors_states_ks
  actorId <- gets e_actorid
  avActions <- gets e_ids_actions
  case M.lookup actorId astks of
    Nothing -> do
      possible "actor states not found"
    Just stks -> do
      case M.lookup sid stks of
        Nothing -> do
          possible "previous state not found"
        Just (C.PS_Suspend _a (g,l) k) -> do
          case M.lookup aid avActions of
            Just (C.A_ChangeActor actor_id) -> do
              let ps = k (g,l{C.e_curr_actorid = actor_id}) v
              processNewState ps
            Just (C.A_Interact _at _slcxtframes _part _str _dltype _args) -> do
              let ps = k (g,l) v
              processNewState ps
            Just (C.A_InteractV _part _str _dltype) -> do
              let ps = k (g,l) v
              processNewState ps
            Just (C.A_TieBreak _poolid _parts) -> do
              let pacts = C.e_partacts g
              case v of
                C.V_Bytes s -> do
                  case M.lookup s pacts of
                    Nothing -> do
                      possible $ "A_TieBreak: participant not found in " ++ show pacts
                    Just actid -> do
                      let ps = k (g,l) $ C.V_UInt $ fromIntegral actid
                      processNewState ps
                _ -> impossible "A_TieBreak: expected string value"
            Just C.A_NewActor -> do
              case v of
                C.V_Bytes s -> do
                  let (g',l') = C.registerPart (g,l) s
                  let ps = k (g',l') v
                  processNewState ps
                _ -> possible "A_NewActor: expected string value"
            Just C.A_None -> return ()
            Just (C.A_AdvanceTime n)  -> do
              case ((C.e_nwtime g) < n) of
                True -> do
                  let ps = k (g{C.e_nwtime = n},l) v
                  processNewState ps
                False -> do
                  let ps = k (g,l) v
                  processNewState ps
            Just (C.A_AdvanceSeconds n)  -> do
              case ((C.e_nwsecs g) < n) of
                True -> do
                  let ps = k (g{C.e_nwsecs = n},l) v
                  processNewState ps
                False -> do
                  let ps = k (g,l) v
                  processNewState ps
            Nothing -> possible "action not found"
        Just (C.PS_Done _ _) -> do
          possible "previous state already terminated"

allStates :: WebM [StateId]
allStates = do
  a <- gets e_nsid
  return [0..(a-1)]

getStatus :: WebM Status
getStatus = do
  s <- gets e_status
  return s

getProgState :: StateId -> WebM (Maybe C.State)
getProgState sid = do
  s <- gets e_graph
  case M.lookup sid s of
    Nothing -> return Nothing
    Just st -> return $ Just st

computeActions :: StateId -> WebM [C.Action]
computeActions sid = do
  stacts <- gets e_states_actions
  idacts <- gets e_ids_actions
  case M.lookup sid stacts of
    Nothing -> return []
    Just acts -> return $ map (\x -> saferMapRef "computeActions" $ M.lookup x idacts) acts

initProgSim :: LLProg -> WebM ()
initProgSim ll = do
  let initSt = C.initState
  ps <- return $ C.initApp ll initSt
  processNewState ps

startServer :: LLProg -> IO ()
startServer p = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebM m) sync
  putStrLn "Starting Sim Server..."
  scottyT portNumber runActionToIO (app p)

app :: LLProg -> ScottyT Text WebM ()
app p = do
  middleware logStdoutDev

  post "/load" $ do
    webM $ modify $ \ st -> st {e_src = Just p}
    json $ ("OK" :: String)

  post "/init" $ do
    ll <- webM $ gets e_src
    case ll of
      Nothing -> json $ ("No Program" :: String)
      Just ll' -> do
        webM $ initProgSim ll'
        json $ ("OK" :: String)

  get "/states" $ do
    ss <- webM $ allStates
    json ss

  get "/global/:s" $ do
    s <- param "s"
    g' <- webM $ getProgState s
    case g' of
      Nothing -> json $ ("Not Found" :: String)
      Just (g,_) -> json g

  get "/local/:s" $ do
    s <- param "s"
    l' <- webM $ getProgState s
    case l' of
      Nothing -> json $ ("Not Found" :: String)
      Just (_,l) -> json l

  get "/status" $ do
    ss <- webM $ getStatus
    json ss

  get "/states/:s" $ do
    s <- param "s"
    ss <- webM $ allStates
    json (filter ((==) s) $ ss)

  get "/states/:s/actions" $ do
    s <- param "s"
    as <- webM $ computeActions s
    json as

  post "/states/:s/actions/:a/" $ do
    s <- param "s"
    a <- param "a"
    t :: String <- param "type"
    case t of
      "String" -> do
        v :: String <- param "data"
        webM $ unblockProg s a $ C.V_Bytes v
      "Number" -> do
        v :: Integer <- param "data"
        webM $ unblockProg s a $ C.V_UInt v
      _ -> impossible "unsupported type"
    json ("OK" :: String)

  get "/ping" $ do
    json ("Hello World" :: String)
