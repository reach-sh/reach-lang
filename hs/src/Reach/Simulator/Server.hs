{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Server where

import Reach.AST.DLBase
import Reach.AST.Base
import Reach.AST.LL
import Reach.Util
import Reach.Dotty
import Reach.StateDiagram
import Reach.BigOpt
import Reach.EPP
import Reach.FloatAPI
import qualified Data.Text as T
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

type CategoryGraph = M.Map StateId StateCategory

data Status = Initial | Running | Done
  deriving (Show, Generic)

instance ToJSON Status
instance FromJSON Status

data StateCategory = Local | Consensus
  deriving (Show, Generic)

instance ToJSON StateCategory

data Session = Session
  { e_actors_actions :: M.Map C.ActorId (M.Map StateId ActionId)
  , e_nsid :: Int
  , e_naid :: Int
  , e_ids_actions :: M.Map ActionId C.Action
  , e_actor_id :: C.ActorId
  , e_graph :: Graph
  , e_cgraph :: CategoryGraph
  , e_src :: Maybe LLProg
  , e_status :: Status
  , e_edges :: [(StateId,StateId)]
  , e_parents :: [(StateId,StateId)]
  , e_locs :: M.Map C.ActorId (M.Map StateId SrcLoc)
  , e_errors :: [(Maybe StateId, Maybe SrcLoc, String)]
  , e_src_txt :: String
  , e_dotgraph :: DotGraph
  }

initSession :: Session
initSession = Session
  { e_actors_actions = mempty
  , e_nsid = 0
  , e_naid = 0
  , e_ids_actions = mempty
  , e_actor_id = C.consensusId
  , e_graph = mempty
  , e_cgraph = mempty
  , e_src = Nothing
  , e_status = Initial
  , e_edges = mempty
  , e_parents = mempty
  , e_locs = mempty
  , e_errors = mempty
  , e_src_txt = mempty
  , e_dotgraph = mempty
  }

processNewMetaState :: StateId -> C.State -> WebM ()
processNewMetaState psid s = do
  sid <- gets e_nsid
  stat <- gets e_status
  edges <- gets e_edges
  parents <- gets e_parents
  graph <- gets e_graph
  cgraph <- gets e_cgraph
  modify $ \ st -> st
    {e_nsid = sid + 1}
    {e_status = stat}
    {e_graph = M.insert sid s graph}
    {e_cgraph = M.insert sid Consensus cgraph}
    {e_edges = (psid, sid) : edges}
    {e_parents = (sid, psid) : parents}

processNewState :: Maybe (StateId) -> C.PartState -> StateCategory -> WebM (Bool)
processNewState psid ps sc = do
  sid <- gets e_nsid
  actorId <- gets e_actor_id
  edges <- gets e_edges
  parents <- gets e_parents
  void $ case ps of
    C.PS_Done _ _ -> do
      registerAction sid actorId C.A_None
    C.PS_Error at e _ _ -> do
      registerError psid at e
    C.PS_Suspend at a _ _ -> do
      registerAction sid actorId a
      case at of
        Nothing -> return ()
        Just at' -> do
          registerLoc sid actorId at'
  let ((g,l), stat, process) =
        case ps of
          C.PS_Done s _ -> do
            (s, Done, True)
          C.PS_Suspend _ _ s _ -> do
            (s, Running, True)
          C.PS_Error _ _ s _ -> do
            (s, Done, False)
  case process of
    True -> do
      graph <- gets e_graph
      cgraph <- gets e_cgraph
      let locals = C.l_locals l
      let lcl = saferMaybe "processNewState" $ M.lookup actorId locals
      let lcl' = lcl { C.l_ks = Just ps }
      let l' = l { C.l_locals = M.insert actorId lcl' locals }
      modify $ \ st -> st
        {e_nsid = sid + 1}
        {e_status = stat}
        {e_graph = M.insert sid (g,l') graph}
        {e_cgraph = M.insert sid sc cgraph}
      case psid of
        Nothing -> return ()
        Just psid' -> modify $ \st ->
          st
            { e_edges = (psid', sid) : edges
            , e_parents = (sid, psid') : parents
            }
      return True
    False -> return False


registerLoc :: StateId -> C.ActorId -> SrcLoc -> WebM ()
registerLoc sid actorId at = do
  locs <- gets e_locs
  case M.lookup actorId locs of
    Nothing -> modify $ \ st -> st {e_locs = M.insert actorId (M.singleton sid at) locs }
    Just locs' -> modify $ \ st -> st {e_locs = M.insert actorId (M.insert sid at locs') locs }
  return ()

registerError :: Maybe StateId -> Maybe SrcLoc -> String -> WebM ()
registerError sid at err = do
  errs <- gets e_errors
  modify $ \ st -> st {e_errors = (sid,at,err):errs }
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
      let ledger = C.e_ledger g
      let tokenIdMax = (C.e_ntok g) - 1
      let newWallet = initWallets tokenIdMax
      let ledger' = M.insert aid newWallet ledger
      let g' = g { C.e_naccid = aid + 1, C.e_ledger = ledger' }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return aid

initWallets :: Integer -> C.Wallet
initWallets n = initWallets' n $ M.empty

initWallets' :: Integer -> C.Wallet -> C.Wallet
initWallets' (-2) w = w
initWallets' n w = initWallets' (n-1) $ M.insert n 0 w

getAPIs :: WebM (M.Map C.APID C.ReachAPI)
getAPIs = do
  graph <- gets e_graph
  case M.lookup 0 graph of
    Nothing -> do
      possible "getAPIs: no initial state found"
    Just (g, _) -> do
      let apis = C.e_apis g
      return apis

getViews :: StateId -> WebM (M.Map C.VID C.ReachView)
getViews sid = do
  graph <- gets e_graph
  case M.lookup sid graph of
    Nothing -> do
      possible "getViews: state not found"
    Just (g, _) -> do
      let views = C.e_views g
      return views

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
      possible "transfer: state not found"
    Just (g, l) -> do
      let ledger = C.e_ledger g
      let ledger' = updateLedger ledger fromAcc tok (\x -> x - amt)
      let ledger'' = updateLedger ledger' toAcc tok (+amt)
      let g' = g { C.e_ledger = ledger'' }
      let graph' = M.insert sid (g',l) graph
      modify $ \ st -> st {e_graph = graph'}
      return ()

apiCall :: Integer -> C.APID -> C.DLVal -> WebM ()
apiCall sid apid v = do
  graph <- gets e_graph
  case M.lookup (fromIntegral sid) graph of
    Nothing -> do
      possible "apiCall: previous state not found"
    Just (g, l) -> do
      let apis = C.e_apis g
      case M.lookup apid apis of
        Nothing -> possible "apiCall: API not found"
        Just api -> do
          let api' = api { C.a_val = Just v }
          let apis' = M.insert apid api' apis
          let g' = g { C.e_apis = apis' }
          let graph' = M.insert (fromIntegral sid) (g',l) graph
          modify $ \ st -> st {e_graph = graph'}
          return ()

viewCall :: Integer -> C.VID -> C.DLVal -> WebM (C.DLVal)
viewCall sid vid v = do
  graph <- gets e_graph
  case M.lookup (fromIntegral sid) graph of
    Nothing -> do
      possible "viewCall: previous state not found"
    Just (g, l) -> do
      let views = C.e_views g
      case M.lookup vid views of
        Nothing -> possible "viewCall: View not found"
        Just view -> do
          let vbl = C.v_bl view
          case vbl of
            Nothing -> return C.V_Null
            Just (DLinExportBlock _ mbvars a) -> do
              case mbvars of
                Nothing -> do
                  let ps = C.runWithState a (g,l)
                  case ps of
                    C.PS_Done _ val -> return val
                    _ -> possible "expected PS_Done"
                Just vars -> do
                  case v of
                    C.V_Tuple tup -> do
                      l' <- bindToConsensusStore tup (map varLetVar vars) l
                      let ps = C.runWithState a (g,l')
                      case ps of
                        C.PS_Done _ val -> return val
                        _ -> possible "expected PS_Done"
                    _ -> possible "expected Tuple"

bindToConsensusStore :: [C.DLVal] -> [DLVar] -> C.Local -> WebM C.Local
bindToConsensusStore vals lets l = do
  let locals = C.l_locals l
  case M.lookup C.consensusId locals of
    Nothing -> possible "bindToConsensusStore: no local store"
    Just lst -> do
      let st = C.l_store lst
      let lst' = lst {C.l_store = M.union (M.fromList (zip lets vals)) st}
      return $ l {C.l_locals = M.insert C.consensusId lst' locals}

passTime :: StateId -> Integer -> WebM ()
passTime sid' n = do
  graph <- gets e_graph
  let sid = fromIntegral sid'
  case M.lookup sid graph of
    Nothing -> do
      possible "passTime: previous state not found"
    Just (g,l) -> do
      let nwsecs = n + C.e_nwsecs g
      let nwtime = n + C.e_nwtime g
      let g' = g {C.e_nwsecs = nwsecs, C.e_nwtime = nwtime}
      processNewMetaState sid' (g',l)

forceTimeout :: StateId -> WebM ()
forceTimeout sid' = do
  graph <- gets e_graph
  let sid = fromIntegral sid'
  case M.lookup sid graph of
    Nothing -> do
      possible "passTime: previous state not found"
    Just (g,l) -> do
      let phId = C.l_phase $ saferMaybe "forceTimeout" $ M.lookup C.consensusId $ C.l_locals l
      let timeouts = M.insert phId True $ C.e_timeouts g
      let g' = g {C.e_timeouts = timeouts}
      processNewMetaState sid' (g',l)

unblockProg :: Integer -> Integer -> C.DLVal -> WebM (Bool)
unblockProg sid' aid' v = do
  let sid = fromIntegral sid'
  let aid = fromIntegral aid'
  graph <- gets e_graph
  actorId <- gets e_actor_id
  avActions <- gets e_ids_actions
  case M.lookup sid graph of
    Nothing -> do
      registerError (Just sid) Nothing "Previous state not found."
      return False
    Just (g, l') -> do
      let locals = C.l_locals l'
      case C.l_ks <$> M.lookup actorId locals of
        Nothing -> do
          registerError (Just sid) Nothing "Actor not found."
          return False
        Just Nothing -> do
          let err = "partstate not found for actor "
                <> show actorId
                <> " in: "
                <> (show $ M.keys locals)
          registerError (Just sid) Nothing err
          return False
        Just (Just (C.PS_Suspend _ _a (_g,_l) k)) -> do
          let l = l' {C.l_curr_actor_id = actorId}
          case M.lookup aid avActions of
            Just (C.A_Interact _slcxtframes _part _str _dltype _args) -> do
              let ps = k (g,l) v
              processNewState (Just sid) ps Local
            Just (C.A_Remote _slcxtframes _str _args1 _args2) -> do
              let ps = k (g,l) v
              processNewState (Just sid) ps Consensus
            Just (C.A_Receive _phid) -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps Local
            Just (C.A_TieBreak _poolid _parts) -> do
              case v of
                C.V_Data _ _ -> do
                  let ps = k (g, l) v
                  processNewState (Just sid) ps Consensus
                C.V_UInt i -> do
                  let ps = k (g, l) $ C.V_Data "actor" $ C.V_UInt i
                  processNewState (Just sid) ps Consensus
                _ -> do
                  registerError (Just sid) Nothing "Tiebreak: unexpected value."
                  return False
            Just C.A_None -> do
              let ps = k (g, l) v
              processNewState (Just sid) ps Consensus
            Just (C.A_AdvanceTime n) -> do
              case ((C.e_nwtime g) < n) of
                True -> do
                  let ps = k (g {C.e_nwtime = n}, l) v
                  processNewState (Just sid) ps Consensus
                False -> do
                  let ps = k (g, l) v
                  processNewState (Just sid) ps Consensus
            Just (C.A_AdvanceSeconds n) -> do
              case ((C.e_nwsecs g) < n) of
                True -> do
                  let ps = k (g {C.e_nwsecs = n}, l) v
                  processNewState (Just sid) ps Consensus
                False -> do
                  let ps = k (g, l) v
                  processNewState (Just sid) ps Consensus
            Nothing -> do
              registerError (Just sid) Nothing "Action not found."
              return False
        Just (Just _) -> do
          registerError (Just sid) Nothing "Previous state already terminated."
          return False


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
        C.PS_Suspend _ act _ _ -> return (sid, (actorId, act))
        _ -> return (sid, (actorId, C.A_None))

allStates :: WebM (M.Map StateId (C.ActorId, C.Action))
allStates = do
  a <- gets e_nsid
  M.fromList <$> mapM stActHist [0 .. (a - 1)]

getStateGraph :: WebM Graph
getStateGraph = do
  gets e_graph

getStatus :: WebM Status
getStatus = do
  gets e_status

getEdges :: WebM [(StateId, StateId)]
getEdges = do
  gets e_edges

resetServer :: WebM ()
resetServer = do
  modify $ \_ -> initSession

getProgState :: StateId -> WebM (Maybe C.State)
getProgState sid = do
  s <- gets e_graph
  case M.lookup sid s of
    Nothing -> return Nothing
    Just st -> return $ Just st

catGraph :: WebM CategoryGraph
catGraph = do
  gets e_cgraph

dotGraph :: WebM DotGraph
dotGraph = do
  gets e_dotgraph

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

initProgSim :: LLProg -> WebM Bool
initProgSim ll = do
  let initSt = C.initState
  ps <- return $ C.initApp ll initSt
  processNewState Nothing ps Consensus

initProgSimFor :: String ->
                  StateId ->
                  C.LocalInteractEnv ->
                  Maybe C.Account ->
                  Maybe Integer ->
                  LLProg ->
                  WebM (Bool,C.Locals)
initProgSimFor slpart sid liv accId blce (LLProg {..}) = do
  graph <- gets e_graph
  let (g'',l'') = saferMaybe "initProgSimFor" $ M.lookup sid graph
  let iv = saferMaybe "initProgSimFor2" $ M.lookup slpart $ C.e_parts g''
  let ((g, l), actId) = C.registerPart (g'',l'') slpart iv
  modify $ \st -> st {e_actor_id = actId}
  let locals = C.l_locals l
  let lcl = saferMaybe "initProgSimFor1" $ M.lookup actId locals
  let accId' = case accId of
        Nothing -> C.l_acct lcl
        Just accId'' -> accId''
  let blce' = case blce of
        Nothing -> 0
        Just n -> n
  let lcl' = lcl { C.l_livs = liv, C.l_acct = accId' }
  let locals' = M.insert actId lcl' locals
  let l' = l {C.l_curr_actor_id = actId, C.l_locals = locals'}
  let ledger = M.insert accId' (M.singleton C.nwToken blce') $ C.e_ledger g
  let g' = g {C.e_ledger = ledger}
  ps <- return $ C.initAppFromStep llp_step (g', l')
  b <- processNewState (Just sid) ps Local
  return (b, M.singleton actId lcl')

startServer :: LLProg -> String -> IO ()
startServer p srcTxt = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebM m) sync
  let showp :: T.Text -> a -> IO a
      showp _ x = return x
  eol <- bigopt (showp, "eol") p
  flap <- floatAPI eol
  pil <- epp flap
  let (DotGraph_ sg) = stateDiagram pil
  putStrLn "Starting Sim Server..."
  scottyT portNumber runActionToIO (app p srcTxt sg)

setHeaders :: ActionT Text WebM ()
setHeaders = do
  setHeader "Access-Control-Allow-Origin" "*"
  setHeader "Access-Control-Allow-Credentials" "true"
  setHeader "Access-Control-Allow-Methods" "GET, POST, PUT"
  setHeader "Access-Control-Allow-Headers" "Content-Type"

formatError :: (Maybe StateId, Maybe SrcLoc, String) -> String
formatError (msid, mloc, e) = do
  "\n"
    <>
    "\nError in state: " <> (show msid) <>
    "\nOn line: " <> (show mloc) <>
    "\nMessage: " <> show e <>
    "\n"

caseTypes :: (Integer -> Integer -> C.DLVal -> WebM a) -> Integer -> Integer -> String -> ActionT Text WebM a
caseTypes f s a = \case
  "number" -> do
    v :: Integer <- param "data"
    webM $ f s a $ C.V_UInt v
  "token" -> do
    v :: Int <- param "data"
    webM $ f s a $ C.V_Token v
  "string" -> do
    v :: String <- param "data"
    webM $ f s a $ C.V_Bytes v
  "contract" -> do
    v :: C.Account <- param "data"
    webM $ f s a $ C.V_Contract v
  "address" -> do
    v :: C.Account <- param "data"
    webM $ f s a $ C.V_Address v
  "boolean" -> do
    v :: Bool <- param "data"
    webM $ f s a $ C.V_Bool v
  "tuple" -> do
    v' :: LB.ByteString <- param "data"
    let v = saferMaybe "decode Tuple" $ decode v'
    webM $ f s a v
  "object" -> do
    v' :: LB.ByteString <- param "data"
    let v = saferMaybe "decode Object" $ decode v'
    webM $ f s a v
  "data" -> do
    v' :: LB.ByteString <- param "data"
    let v = saferMaybe "decode Data" $ decode v'
    webM $ f s a v
  "struct" -> do
    v' :: LB.ByteString <- param "data"
    let v = saferMaybe "decode Struct" $ decode v'
    webM $ f s a v
  _ -> possible "Unexpected value type"

raiseError :: Bool -> ActionT Text WebM ()
raiseError = \case
  True -> json ("OK" :: String)
  False -> do
    errs <- webM $ gets e_errors
    case errs of
      (e : _) -> raise $ s2lt $ formatError e
      [] -> raise "An impossible error has occurred."

app :: LLProg -> String -> DotGraph -> ScottyT Text WebM ()
app p srcTxt dg = do
  middleware logStdoutDev

  post "/load" $ do
    setHeaders
    webM $ modify $ \st -> st
      { e_src = Just p,
        e_src_txt = srcTxt,
        e_dotgraph = dg
      }
    json srcTxt

  post "/init" $ do
    setHeaders
    ll <- webM $ gets e_src
    case ll of
      Nothing -> json $ ("No Program" :: String)
      Just ll' -> do
        outcome <- webM $ initProgSim ll'
        raiseError outcome

  post "/init/:a/:s" $ do
    setHeaders
    a <- param "a"
    s <- param "s"
    ps <- M.fromList <$> params
    acc <- case M.lookup "accountId" ps of
      Nothing -> return Nothing
      Just prm -> do
        case (parseParam prm) :: Either Text C.Account of
          Left e -> possible $ show e
          Right w -> return $ Just w
    blce <- case M.lookup "startingBalance" ps of
      Nothing -> return Nothing
      Just prm -> do
        case (parseParam prm) :: Either Text Integer of
          Left e -> possible $ show e
          Right w -> return $ Just w
    liv :: LB.ByteString <- param "liv"
    ll <- webM $ gets e_src
    let liv' :: Maybe C.LocalInteractEnv = decode liv
    case liv' of
      Nothing -> possible "Init Parse Failure"
      Just liv'' -> do
        case ll of
          Nothing -> json $ ("No Program" :: String)
          Just ll' -> do
            (outcome,part) <- webM $ initProgSimFor a s liv'' acc blce ll'
            raiseError outcome
            json part

  get "/init_details/:a" $ do
    setHeaders
    a <- param "a"
    dets <- webM $ initDetails a
    json dets

  get "/states" $ do
    setHeaders
    ss <- webM $ allStates
    json ss

  get "/graph" $ do
    setHeaders
    sg <- webM $ getStateGraph
    json sg

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

  get "/catgraph" $ do
    setHeaders
    cat <- webM $ catGraph
    json cat

  get "/dotstategraph" $ do
    setHeaders
    dot <- webM $ dotGraph
    json dot

  get "/apis" $ do
    setHeaders
    a <- webM $ getAPIs
    json a

  get "/views/:s" $ do
    setHeaders
    s <- param "s"
    v <- webM $ getViews s
    json v

  post "/api_call/:a/:s" $ do
    setHeaders
    a <- param "a"
    s <- param "s"
    t :: String <- param "type"
    caseTypes apiCall s a t
    json ("OK" :: String)

  post "/view_call/:a/:s" $ do
    setHeaders
    a <- param "a"
    s <- param "s"
    t :: String <- param "type"
    r <- caseTypes viewCall s a t
    json $ show r

  post "/wait/:s/:n" $ do
    setHeaders
    s <- param "s"
    n <- param "n"
    r <- webM $ passTime s n
    json $ show r

  post "/timeout/:s/" $ do
    setHeaders
    s <- param "s"
    r <- webM $ forceTimeout s
    json $ show r

  get "/actions/:s/:a/" $ do
    setHeaders
    s <- param "s"
    a <- param "a"
    act <- webM $ computeActions s a
    json act

  post "/reset" $ do
    setHeaders
    void $ webM $ resetServer
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
    outcome <- caseTypes unblockProg s a t
    raiseError outcome

  get "/ping" $ do
    setHeaders
    json ("Hello World" :: String)

  options (regex ".*") $ do
    setHeaders
    json ("OK" :: String)
