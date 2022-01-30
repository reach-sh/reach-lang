{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Server where

import Reach.AST.Base
import Reach.AST.LL
import Reach.Util
import qualified Reach.Simulator.Core as C
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class
import qualified Data.Map.Strict as M
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans

instance Default Session where
  def = initSession

newtype WebM a = WebM {runWebM :: ReaderT (TVar Session) IO a}
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
  { e_actors_actions :: M.Map C.ActorId (M.Map StateId ActionId)
  , e_last_acts :: M.Map C.ActorId StateId
  , e_nsid :: Int
  , e_naid :: Int
  , e_ids_actions :: M.Map ActionId C.Action
  , e_actor_id :: C.ActorId
  , e_graph :: Graph
  , e_src :: Maybe LLProg
  , e_status :: Status
  , e_edges :: [(StateId,StateId)]
  , e_locs :: M.Map StateId (Maybe SrcLoc)
  , e_src_txt :: String
  }

initSession :: Session
initSession = Session
  { e_actors_actions = mempty
  , e_last_acts = mempty
  , e_nsid = 0
  , e_naid = 0
  , e_ids_actions = mempty
  , e_actor_id = C.consensusId
  , e_graph = mempty
  , e_src = Nothing
  , e_status = Initial
  , e_edges = mempty
  , e_locs = mempty
  , e_src_txt = mempty
  }

processNewState :: Maybe (StateId) -> C.PartState -> WebM ()
processNewState psid ps = do
  sid <- gets e_nsid
  actorId <- gets e_actor_id
  edges <- gets e_edges
  _ <- case ps of
    C.PS_Done _ _ -> do
      _ <- return $ putStrLn "EVAL DONE"
      registerAction sid actorId C.A_None
    C.PS_Suspend _ a _ _ -> registerAction sid actorId a
  let ((g,l), stat, loc) =
        case ps of
          C.PS_Done s _ -> do
            (s, Done, Nothing)
          C.PS_Suspend at _ s _ -> do
            (s, Running, at)
  graph <- gets e_graph
  last_acts <- gets e_last_acts
  locs <- gets e_locs
  let locals = C.l_locals l
  let lcl = saferMapRef "processNewState" $ M.lookup actorId locals
  let lcl' = lcl { C.l_ks = Just ps }
  let l' = l { C.l_locals = M.insert actorId lcl' locals }
  modify $ \ st -> st
    {e_nsid = sid + 1}
    {e_status = stat}
    {e_graph = M.insert sid (g,l') graph}
    {e_locs = M.insert sid loc locs}
    {e_last_acts = M.insert actorId sid last_acts}
  case psid of
    Nothing -> return ()
    Just psid' -> modify $ \st ->
      st
        { e_edges = (psid', sid) : edges
        }

registerAction :: StateId -> C.ActorId -> C.Action -> WebM ActionId
registerAction sid actorId act = do
  actId <- gets e_naid
  modify $ \ st -> st {e_naid = actId + 1}
  actacts <- gets e_actors_actions
  idacts <- gets e_ids_actions
  modify $ \ st -> st {e_ids_actions = M.insert actId act idacts}
  case M.lookup actorId actacts of
    Nothing -> modify $ \ st -> st {e_actors_actions = M.insert actorId (M.singleton sid actId) actacts }
    Just acts -> modify $ \ st -> st {e_actors_actions = M.insert actorId (M.insert sid actId acts) actacts }
  return actId

newAccount :: StateId -> WebM (C.AccountId)
newAccount sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "newAccount: state not found"
    Just (g, l) -> do
      let aid = C.e_naccid g
      let newAccId = aid + 1
      let g' = g { C.e_naccid = newAccId }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return newAccId

newTok :: StateId -> WebM (C.Token)
newTok sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "newTok: state not found"
    Just (g, l) -> do
      let tokId = C.e_ntok g
      let nTokId = tokId + 1
      let g' = g { C.e_ntok = nTokId }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return tokId

updateLedger :: C.Ledger -> C.Account -> C.Token -> (Integer -> Integer) -> C.Ledger
updateLedger map_ledger acc tok f = do
  let m = saferMapRef "updateLedger" $ M.lookup acc map_ledger
  let prev_amt = saferMapRef "updateLedger1" $ M.lookup tok m
  let new_amt = f prev_amt
  M.insert acc (M.insert tok new_amt m) map_ledger

transfer :: StateId -> C.Account -> C.Account -> C.Token -> Integer -> WebM ()
transfer sid fromAcc toAcc tok amt = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "newAccount: state not found"
    Just (g, l) -> do
      let ledger = C.e_ledger g
      let ledger' = updateLedger ledger fromAcc tok (\x -> x - amt)
      let ledger'' = updateLedger ledger' toAcc tok (+amt)
      let g' = g { C.e_ledger = ledger'' }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return ()

unblockProg :: StateId -> ActionId -> C.DLVal -> WebM ()
unblockProg sid aid v = do
  graph <- gets e_graph
  actorId <- gets e_actor_id
  avActions <- gets e_ids_actions
  case M.lookup sid graph of
    Nothing -> do
      possible "previous state not found"
    Just (g, l') -> do
      let locals = C.l_locals l'
      case C.l_ks <$> M.lookup actorId locals of
        Nothing -> do
          possible "actor not found"
        Just Nothing -> do
          possible $ "partstate not found for actor "
            <> show actorId
            <> " in: "
            <> (show $ M.keys locals)
        Just (Just (C.PS_Suspend _ _a (_g,_l) k)) -> do
          let l = l' {C.l_curr_actor_id = actorId}
          case M.lookup aid avActions of
            Just (C.A_Interact _slcxtframes _part _str _dltype _args) -> do
              let ps = k (g,l) v
              processNewState (Just sid) ps
            Just (C.A_Remote _slcxtframes _str _args1 _args2) -> do
              let ps = k (g,l) v
              processNewState (Just sid) ps
            Just (C.A_InteractV _part _str _dltype) -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps
            Just (C.A_Contest _phid) -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps
            Just (C.A_TieBreak _poolid _parts) -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps
            Just C.A_None -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps
            Just (C.A_AdvanceTime n) -> do
              case ((C.e_nwtime g) < n) of
                True -> do
                  let ps = k (g {C.e_nwtime = n}, l) v
                  processNewState (Just sid) ps
                False -> do
                  let ps = k (g, l) v
                  processNewState (Just sid) ps
            Just (C.A_AdvanceSeconds n) -> do
              case ((C.e_nwsecs g) < n) of
                True -> do
                  let ps = k (g {C.e_nwsecs = n}, l) v
                  processNewState (Just sid) ps
                False -> do
                  let ps = k (g, l) v
                  processNewState (Just sid) ps
            Nothing -> possible "action not found"
        Just (Just (C.PS_Done _ _)) -> do
          possible "previous state already terminated"

allStates :: WebM [StateId]
allStates = do
  a <- gets e_nsid
  return [0 .. (a -1)]

getStatus :: WebM Status
getStatus = do
  s <- gets e_status
  return s

getEdges :: WebM [(StateId, StateId)]
getEdges = do
  es <- gets e_edges
  return es

resetServer :: WebM ()
resetServer = do
  modify $ \_ -> initSession

getProgState :: StateId -> WebM (Maybe C.State)
getProgState sid = do
  s <- gets e_graph
  case M.lookup sid s of
    Nothing -> return Nothing
    Just st -> return $ Just st

getLoc :: StateId -> WebM (Maybe SrcLoc)
getLoc sid = do
  locs <- gets e_locs
  return $ join $ M.lookup sid locs

changeActor :: C.ActorId -> WebM ()
changeActor actId = do
  modify $ \st -> st {e_actor_id = actId}

computeActions :: C.ActorId -> WebM (Maybe (ActionId,C.Action))
computeActions actorId = do
  actacts <- gets e_actors_actions
  idacts <- gets e_ids_actions
  last_acts <- gets e_last_acts
  case (M.lookup actorId actacts, M.lookup actorId last_acts)  of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just acts, Just sid) -> do
      case M.lookup sid acts of
        Nothing -> return Nothing
        Just actId -> do
          let act = saferMapRef "computeActions actId" $ M.lookup actId idacts
          return $ Just (actId,act)

initProgSim :: LLProg -> WebM ()
initProgSim ll = do
  let initSt = C.initState
  ps <- return $ C.initApp ll initSt
  processNewState Nothing ps

initProgSimFor :: C.ActorId -> StateId -> LLProg -> WebM ()
initProgSimFor actId sid (LLProg _ _ _ _ _ _ _ _ step) = do
  graph <- gets e_graph
  modify $ \st -> st {e_actor_id = actId}
  let (g, l) = saferMapRef "initProgSimFor" $ M.lookup sid graph
  let l' = l {C.l_curr_actor_id = actId}
  ps <- return $ C.initAppFromStep step (g, l')
  processNewState (Just sid) ps

startServer :: LLProg -> String -> IO ()
startServer p srcTxt = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebM m) sync
  putStrLn "Starting Sim Server..."
  scottyT portNumber runActionToIO (app p srcTxt)

setHeaders :: ActionT Text WebM ()
setHeaders = do
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Credentials" "true"
  setHeader "Access-Control-Allow-Methods" "GET, POST, PUT"
  setHeader "Access-Control-Allow-Headers" "Content-Type"

app :: LLProg -> String -> ScottyT Text WebM ()
app p srcTxt = do
  middleware logStdoutDev

  post "/load" $ do
    setHeaders
    webM $ modify $ \st -> st {e_src = Just p, e_src_txt = srcTxt}
    json srcTxt

  post "/init" $ do
    setHeaders
    ll <- webM $ gets e_src
    case ll of
      Nothing -> json $ ("No Program" :: String)
      Just ll' -> do
        webM $ initProgSim ll'
        json $ ("OK" :: String)

  post "/init/:a/:s" $ do
    setHeaders
    a <- param "a"
    s <- param "s"
    ll <- webM $ gets e_src
    case ll of
      Nothing -> json $ ("No Program" :: String)
      Just ll' -> do
        webM $ initProgSimFor a s ll'
        json $ ("OK" :: String)

  get "/states" $ do
    setHeaders
    ss <- webM $ allStates
    json ss

  get "/edges" $ do
    setHeaders
    es <- webM $ getEdges
    json es

  get "/global/:s" $ do
    setHeaders
    s <- param "s"
    g' <- webM $ getProgState s
    case g' of
      Nothing -> json $ ("Not Found" :: String)
      Just (g, _) -> json g

  get "/local/:s" $ do
    setHeaders
    s <- param "s"
    l' <- webM $ getProgState s
    case l' of
      Nothing -> json $ ("Not Found" :: String)
      Just (_, l) -> json l

  get "/locs/:s" $ do
    setHeaders
    s <- param "s"
    loc <- webM $ getLoc s
    json loc

  get "/status" $ do
    setHeaders
    ss <- webM $ getStatus
    json ss

  get "/states/:s" $ do
    setHeaders
    s <- param "s"
    ss <- webM $ allStates
    json (filter ((==) s) $ ss)

  get "/actions/:a/" $ do
    setHeaders
    a <- param "a"
    act <- webM $ computeActions a
    json act

  post "/reset" $ do
    setHeaders
    _ <- webM $ resetServer
    json ("OK" :: String)

  post "/accounts/new/:s" $ do
    setHeaders
    s <- param "s"
    accId <- webM $ newAccount s
    json accId

  post "/tokens/new/:s" $ do
    setHeaders
    s <- param "s"
    tokId <- webM $ newTok s
    json tokId

  post "/transfer/:s" $ do
    setHeaders
    s <- param "s"
    fAcc <- param "from"
    tAcc <- param "to"
    token <- param "token"
    amount <- param "amount"
    webM $ transfer s fAcc tAcc token amount
    json ("OK" :: String)

  post "/states/:s/actions/:a/" $ do
    setHeaders
    s <- param "s"
    a <- param "a"
    t :: String <- param "type"
    ps <- M.fromList <$> params
    case M.lookup "who" ps of
      Nothing -> return ()
      Just prm -> do
        case (parseParam prm) :: Either Text C.ActorId of
          Left e -> possible $ show e
          Right w -> webM $ changeActor $ fromIntegral w
    case t of
      "number" -> do
        v :: Integer <- param "data"
        webM $ unblockProg s a $ C.V_UInt v
      "string" -> do
        v :: String <- param "data"
        webM $ unblockProg s a $ C.V_Bytes v
      _ -> possible "Unexpected value type"
    json ("OK" :: String)

  get "/ping" $ do
    setHeaders
    json ("Hello World" :: String)

  options (regex ".*") $ do
    setHeaders
    json ("OK" :: String)
