{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Server where

import Reach.AST.DLBase
import Reach.AST.Base
import Reach.AST.LL
import Reach.Util
import qualified Reach.Simulator.Core as C
import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Default.Class
import qualified Data.Map.Strict as M
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)
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
  , e_nsid :: Int
  , e_naid :: Int
  , e_ids_actions :: M.Map ActionId C.Action
  , e_actor_id :: C.ActorId
  , e_graph :: Graph
  , e_src :: Maybe LLProg
  , e_status :: Status
  , e_edges :: [(StateId,StateId)]
  , e_parents :: [(StateId,StateId)]
  , e_locs :: M.Map C.ActorId (M.Map StateId SrcLoc)
  , e_src_txt :: String
  }

initSession :: Session
initSession = Session
  { e_actors_actions = mempty
  , e_nsid = 0
  , e_naid = 0
  , e_ids_actions = mempty
  , e_actor_id = C.consensusId
  , e_graph = mempty
  , e_src = Nothing
  , e_status = Initial
  , e_edges = mempty
  , e_parents = mempty
  , e_locs = mempty
  , e_src_txt = mempty
  }

processNewState :: Maybe (StateId) -> C.PartState -> WebM ()
processNewState psid ps = do
  sid <- gets e_nsid
  actorId <- gets e_actor_id
  edges <- gets e_edges
  parents <- gets e_parents
  _ <- case ps of
    C.PS_Done _ _ -> do
      registerAction sid actorId C.A_None
    C.PS_Suspend at a _ _ -> do
      registerAction sid actorId a
      case at of
        Nothing -> return ()
        Just at' -> do
          registerLoc sid actorId at'
  let ((g,l), stat) =
        case ps of
          C.PS_Done s _ -> do
            (s, Done)
          C.PS_Suspend _ _ s _ -> do
            (s, Running)
  graph <- gets e_graph
  let locals = C.l_locals l
  let lcl = saferMaybe "processNewState" $ M.lookup actorId locals
  let lcl' = lcl { C.l_ks = Just ps }
  let l' = l { C.l_locals = M.insert actorId lcl' locals }
  modify $ \ st -> st
    {e_nsid = sid + 1}
    {e_status = stat}
    {e_graph = M.insert sid (g,l') graph}
  case psid of
    Nothing -> return ()
    Just psid' -> modify $ \st ->
      st
        { e_edges = (psid', sid) : edges
        , e_parents = (sid, psid') : parents
        }

registerLoc :: StateId -> C.ActorId -> SrcLoc -> WebM ()
registerLoc sid actorId at = do
  locs <- gets e_locs
  case M.lookup actorId locs of
    Nothing -> modify $ \ st -> st {e_locs = M.insert actorId (M.singleton sid at) locs }
    Just locs' -> modify $ \ st -> st {e_locs = M.insert actorId (M.insert sid at locs') locs }
  return ()

registerAction :: StateId -> C.ActorId -> C.Action -> WebM ()
registerAction sid actorId act = do
  actId <- gets e_naid
  modify $ \ st -> st {e_naid = actId + 1}
  actacts <- gets e_actors_actions
  idacts <- gets e_ids_actions
  modify $ \ st -> st {e_ids_actions = M.insert actId act idacts}
  case M.lookup actorId actacts of
    Nothing -> modify $ \ st -> st {e_actors_actions = M.insert actorId (M.singleton sid actId) actacts }
    Just acts -> modify $ \ st -> st {e_actors_actions = M.insert actorId (M.insert sid actId acts) actacts }
  return ()

newAccount :: StateId -> WebM (C.Account)
newAccount sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "newAccount: state not found"
    Just (g, l) -> do
      let aid = fromIntegral $ C.e_naccid g
      let newAccId = aid + 1
      let ledger = C.e_ledger g
      let tokenIdMax = (C.e_ntok g) - 1
      let newWallet = initWallets tokenIdMax
      let ledger' = M.insert aid newWallet ledger
      let g' = g { C.e_naccid = newAccId, C.e_ledger = ledger' }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return newAccId

initWallets :: Integer -> C.Wallet
initWallets n = initWallets' n $ M.empty

initWallets' :: Integer -> C.Wallet -> C.Wallet
initWallets' 0 w = w
initWallets' n w = initWallets' (n-1) $ M.insert n 0 w

newTok :: StateId -> WebM (C.Token)
newTok sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "newTok: state not found"
    Just (g, l) -> do
      let tokId = C.e_ntok g
      let nTokId = tokId + 1
      let ledger = C.e_ledger g
      let ledger' = M.map (\wllt -> M.insert tokId 0 wllt) ledger
      let g' = g { C.e_ntok = nTokId, C.e_ledger = ledger' }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return tokId

updateLedger :: C.Ledger -> C.Account -> C.Token -> (Integer -> Integer) -> C.Ledger
updateLedger map_ledger acc tok f = do
  let m = saferMaybe "updateLedger" $ M.lookup acc map_ledger
  let prev_amt = saferMaybe "updateLedger1" $ M.lookup tok m
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
            Just (C.A_Receive _phid) -> do
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


stActHist :: StateId -> WebM (StateId, (C.ActorId, C.Action))
stActHist sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> possible "stActHist failed"
    Just (_g,l) -> do
      let actorId = C.l_curr_actor_id l
      let locals = C.l_locals l
      let k = saferMaybe "stActHist failed (2)" $ C.l_ks $ saferMaybe "stActHist failed (1)" $ M.lookup actorId locals
      case k of
        C.PS_Done _ _ -> return (sid, (actorId, C.A_None))
        C.PS_Suspend _ act _ _ -> return (sid, (actorId, act))

allStates :: WebM (M.Map StateId (C.ActorId, C.Action))
allStates = do
  a <- gets e_nsid
  M.fromList <$> mapM stActHist [0 .. (a - 1)]

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

checkIfValIType :: IType -> Bool
checkIfValIType = \case
  IT_Val _ -> True
  IT_Fun _ _ -> False
  IT_UDFun _ -> False

initVals :: InteractEnv -> WebM (M.Map String String)
initVals (InteractEnv iv'') = do
  return $ M.map show $ M.filter checkIfValIType iv''

initDetails :: C.ActorId -> WebM (M.Map String String)
initDetails actorId = do
  (_,l) <- fromMaybe (possible "initDetails: state not founds") <$> getProgState 0
  case M.lookup actorId (C.l_locals l) of
    Nothing -> possible "initDetails: actor not found"
    Just lcl -> initVals $ C.l_ivd lcl

getLoc :: StateId -> C.ActorId -> WebM (Maybe SrcLoc)
getLoc sid actorId = do
  locs <- gets e_locs
  parents <- M.fromList <$> gets e_parents
  case M.lookup actorId locs of
    Nothing -> return Nothing
    Just locs' -> do
      case M.lookup sid locs' of
        Nothing -> do
          case M.lookup sid parents of
            Nothing -> return Nothing
            Just parent -> do
              getLoc parent actorId
        Just loc -> return $ Just loc

changeActor :: C.ActorId -> WebM ()
changeActor actId = do
  modify $ \st -> st {e_actor_id = actId}

computeActions :: StateId -> C.ActorId -> WebM (Maybe (ActionId,C.Action))
computeActions sid actorId = do
  actacts <- gets e_actors_actions
  idacts <- gets e_ids_actions
  parents <- M.fromList <$> gets e_parents
  case M.lookup actorId actacts of
    Nothing -> return Nothing
    Just acts -> do
      case M.lookup sid acts of
        Nothing -> do
          case M.lookup sid parents of
            Nothing -> return Nothing
            Just parent -> do
              computeActions parent actorId
        Just actId -> do
          let act = saferMaybe "computeActions actId" $ M.lookup actId idacts
          return $ Just (actId,act)

initProgSim :: LLProg -> WebM ()
initProgSim ll = do
  let initSt = C.initState
  ps <- return $ C.initApp ll initSt
  processNewState Nothing ps

initProgSimFor :: C.ActorId -> StateId -> C.LocalInteractEnv -> LLProg -> WebM ()
initProgSimFor actId sid liv (LLProg _ _ _ _ _ _ _ _ step) = do
  graph <- gets e_graph
  modify $ \st -> st {e_actor_id = actId}
  let (g, l) = saferMaybe "initProgSimFor" $ M.lookup sid graph
  let locals = C.l_locals l
  let lcl = saferMaybe "initProgSimFor1" $ M.lookup actId locals
  let lcl' = lcl { C.l_livs = liv }
  let locals' = M.insert actId lcl' locals
  let l' = l {C.l_curr_actor_id = actId, C.l_locals = locals'}
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
    liv :: LB.ByteString <- param "liv"
    ll <- webM $ gets e_src
    let liv' :: Maybe C.LocalInteractEnv = decode liv
    case liv' of
      Nothing -> possible "Init Parse Failure"
      Just liv'' -> do
        case ll of
          Nothing -> json $ ("No Program" :: String)
          Just ll' -> do
            webM $ initProgSimFor a s liv'' ll'
            json $ ("OK" :: String)

  get "/init_details/:a" $ do
    setHeaders
    a <- param "a"
    dets <- webM $ initDetails a
    json dets

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

  get "/locs/:s/:a/" $ do
    setHeaders
    s <- param "s"
    a <- param "a"
    loc <- webM $ getLoc s a
    json loc

  get "/status" $ do
    setHeaders
    ss <- webM $ getStatus
    json ss

  get "/actions/:s/:a/" $ do
    setHeaders
    s <- param "s"
    a <- param "a"
    act <- webM $ computeActions s a
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
      "contract" -> do
        v :: C.Account <- param "data"
        webM $ unblockProg s a $ C.V_Contract v
      "address" -> do
        v :: C.Account <- param "data"
        webM $ unblockProg s a $ C.V_Address v
      "boolean" -> do
        v :: Bool <- param "data"
        webM $ unblockProg s a $ C.V_Bool v
      "tuple" -> do
        v' :: LB.ByteString <- param "data"
        let v = saferMaybe "decode Tuple" $ decode v'
        webM $ unblockProg s a v
      "object" -> do
        v' :: LB.ByteString <- param "data"
        let v = saferMaybe "decode Object" $ decode v'
        webM $ unblockProg s a v
      _ -> possible "Unexpected value type"
    json ("OK" :: String)

  get "/ping" $ do
    setHeaders
    json ("Hello World" :: String)

  options (regex ".*") $ do
    setHeaders
    json ("OK" :: String)
