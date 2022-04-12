{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Core where

import Control.Monad.Reader
import Data.Aeson
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (member)
import Data.List (partition)
import GHC.Generics
import qualified GHC.Stack as G
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util

data SimIdentity = Consensus | Participant Participant
  deriving (Eq, Ord, Show, Generic)

type Participant = String

type ActorId = Int

type PhaseId = Integer

type ConsensusEnv = M.Map DLVar DLVal

type LocalInteractEnv = M.Map String DLVal

type Store = ConsensusEnv

type Balance = Integer

type Token = Integer

type APID = Integer

type VID = Integer

type APIFlag = Bool

consensusId :: ActorId
consensusId = -1

nwToken :: Token
nwToken = -1

simContract :: Account
simContract = fromIntegral consensusId

simContractAmt :: Balance
simContractAmt = 0

type Wallet = M.Map Token Balance

type Ledger = M.Map Account Wallet

data Message = Message
  { m_store :: Store
  , m_pay :: DLPayAmt
  , m_api :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Message where
  toJSON (Message _ _ _) = "Message"

data MessageInfo = NotFixedYet (M.Map ActorId Message) | Fixed (ActorId, Message)
  deriving (Show, Generic)

instance ToJSON MessageInfo

data ReachAPI = ReachAPI
  { a_name :: String
  , a_liv :: InteractEnv
  , a_val :: Maybe DLVal
  , a_acc :: Account
  }
  deriving (Show, Generic)

instance ToJSON ReachAPI

data ReachView = ReachView
  { v_name :: Maybe String
  , v_var :: SLVar
  , v_ty :: IType
  , v_bl :: Maybe DLExportBlock
  }
  deriving (Generic)

instance ToJSON ReachView

data Global = Global
  { e_ledger :: Ledger
  , e_ntok :: Token
  , e_linstate :: LinearState
  , e_nwtime :: Integer
  , e_nwsecs :: Integer
  , e_nactorid :: ActorId
  , e_naccid :: Account
  , e_napid :: Integer
  , e_nvid :: Integer
  , e_apis :: M.Map APID ReachAPI
  , e_views :: M.Map VID ReachView
  , e_viewids :: M.Map String VID
  , e_partacts :: M.Map Participant ActorId
  , e_messages :: M.Map PhaseId MessageInfo
  }
  deriving (Generic)

instance ToJSON Global

data LocalInfo = LocalInfo
  { l_acct :: Account
  , l_who :: Maybe Participant
  , l_store :: Store
  , l_phase :: PhaseId
  , l_ks :: Maybe PartState
  , l_livs :: LocalInteractEnv
  , l_ivd :: InteractEnv
  }
  deriving (Generic)

type Locals = M.Map ActorId LocalInfo

data Local = Local
  { l_locals :: Locals
  , l_curr_actor_id :: ActorId
  }
  deriving (Generic)

instance ToJSON LocalInfo

instance ToJSON Local

type State = (Global, Local)

initState :: State
initState = (initGlobal, initLocal)

initGlobal :: Global
initGlobal =
  Global
    { e_ledger = M.singleton simContract (M.singleton nwToken simContractAmt)
    , e_ntok = 0
    , e_linstate = mempty
    , e_nwtime = 0
    , e_nwsecs = 0
    , e_nactorid = 0
    , e_naccid = 0
    , e_napid = 0
    , e_nvid = 0
    , e_apis = mempty
    , e_views = mempty
    , e_viewids = mempty
    , e_partacts = mempty
    , e_messages = mempty
    }

initLocal :: Local
initLocal =
  Local
    { l_locals =
        M.singleton
          consensusId
          LocalInfo
            { l_acct = simContract
            , l_who = Nothing
            , l_store = mempty
            , l_phase = 0
            , l_ks = Nothing
            , l_livs = mempty
            , l_ivd = mempty
            }
    , l_curr_actor_id = consensusId
    }

type PartCont = State -> DLVal -> PartState

data PartState
  = PS_Done State DLVal
  | PS_Suspend (Maybe SrcLoc) Action State PartCont
  deriving (Generic)

instance ToJSON PartState where
  toJSON (PS_Done _ _) = "PS_Done"
  toJSON (PS_Suspend _ _ _ _) = "PS_Suspend"

initPartState :: PartState
initPartState = PS_Done initState V_Null

newtype App a
  = App (State -> (State -> a -> PartState) -> PartState)

instance Functor App where
  fmap t (App f) = App (\gv k -> f gv (\gv' v -> k gv' (t v)))

instance Applicative App where
  pure v = App (\gv k -> k gv v)
  (App fun) <*> (App val) =
    App
      (\gvTop kAns ->
         fun
           gvTop
           (\gvFun vFun ->
              val
                gvFun
                (\gvVal vVal ->
                   kAns gvVal (vFun vVal))))

instance Monad App where
  (App val) >>= conF =
    App
      (\gvTop kAns ->
         val
           gvTop
           (\gvVal vVal ->
              let App con = conF vVal
               in con gvVal kAns))

suspend :: (State -> PartCont -> PartState) -> App DLVal
suspend = App

getState :: App State
getState = App (\gvTop kAns -> kAns gvTop gvTop)

getGlobal :: App Global
getGlobal = do
  (g, _) <- getState
  return g

getLocal :: App Local
getLocal = do
  (_, l) <- getState
  return l

setState :: State -> App ()
setState ngv = App (\_ kAns -> kAns ngv ())

setGlobal :: Global -> App ()
setGlobal g = do
  l <- getLocal
  setState (g, l)

setLocal :: Local -> App ()
setLocal l = do
  g <- getGlobal
  setState (g, l)

runApp :: State -> App DLVal -> PartState
runApp st (App f) = f st PS_Done

initApp :: LLProg -> State -> PartState
initApp p st = runApp st $ interp p

initAppFromStep :: LLStep -> State -> PartState
initAppFromStep step st = runApp st $ interp step

runWithState :: (Interp a) => a -> State -> PartState
runWithState k st = runApp st $ interp k

ledgerNewTokenRefs :: Integer -> DLTokenNew -> App (Token)
ledgerNewTokenRefs n tk = do
  (e, _) <- getState
  let tokId = e_ntok e
  _ <- mapM (\x -> ledgerNewToken x tk tokId) [-1..n]
  (e', _) <- getState
  setGlobal $ e' {e_ntok = tokId + 1}
  return tokId

ledgerNewToken :: Account -> DLTokenNew -> Token -> App ()
ledgerNewToken acc tk tokId = do
  (e, _) <- getState
  let ledger = e_ledger e
  supply <- case (acc == simContract) of
    True -> vUInt <$> interp (dtn_supply tk)
    False -> return 0
  let m = saferMaybe "ledgerNewToken: account not found" $ M.lookup acc ledger
  let new_nw_ledger = M.insert acc (M.insert tokId supply m) ledger
  setGlobal $ e {e_ledger = new_nw_ledger}

data Action
  = A_TieBreak PhaseId [String]
  | A_None
  | A_AdvanceTime Integer
  | A_AdvanceSeconds Integer
  | A_Interact [SLCtxtFrame] String String DLType [DLVal]
  | A_Receive PhaseId
  | A_Remote [SLCtxtFrame] String [DLVal] [DLVal]
  deriving (Generic)

instance ToJSON Action

instance FromJSON Action

type Account = Integer

type LinearState = M.Map DLMVar (M.Map Account DLVal)

data DLVal
  = V_Null
  | V_Bool Bool
  | V_UInt Integer
  | V_Token Int
  | V_Bytes String
  | V_Digest DLVal
  | V_Address Account
  | V_Contract Account
  | V_Array [DLVal]
  | V_Tuple [DLVal]
  | V_Object (M.Map SLVar DLVal)
  | V_Data SLVar DLVal
  | V_Struct [(SLVar, DLVal)]
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DLVal
instance FromJSON DLVal

addToStore :: DLVar -> DLVal -> App ()
addToStore x v = do
  (_, l) <- getState
  let locals = l_locals l
  let aid = l_curr_actor_id l
  case M.lookup aid locals of
    Nothing -> possible "addToStore: no local store"
    Just lst -> do
      let st = l_store lst
      let lst' = lst {l_store = M.insert x v st}
      setLocal $ l {l_locals = M.insert aid lst' locals}

fixMessageInRecord :: PhaseId -> ActorId -> Store -> DLPayAmt -> Bool -> App ()
fixMessageInRecord phId actId m_store m_pay m_api = do
  (g, _) <- getState
  let msgs = e_messages g
  let msgs' = M.insert phId (Fixed (actId, Message {..})) msgs
  setGlobal $ g {e_messages = msgs'}

incrNWtime :: Integer -> App ()
incrNWtime n = do
  (e, _) <- getState
  let t = e_nwtime e
  setGlobal $ e {e_nwtime = t + n}

incrNWsecs :: Integer -> App ()
incrNWsecs n = do
  (e, _) <- getState
  let t = e_nwsecs e
  setGlobal $ e {e_nwsecs = t + n}

incrPhaseId :: App ()
incrPhaseId = do
  l <- getLocal
  let actorId = l_curr_actor_id l
  let locals = l_locals l
  let x = saferMaybe "incrPhaseId" $ M.lookup actorId locals
  let x' = x {l_phase = (l_phase x) + 1}
  let locals' = M.insert actorId x' locals
  setLocal $ l {l_locals = locals'}

updateLedgers :: Integer -> Token -> (Integer -> Integer) -> App ()
updateLedgers n tok f = do
  _ <- mapM (\x -> updateLedger x tok f) [-1..n]
  return ()

updateLedger :: Account -> Token -> (Integer -> Integer) -> App ()
updateLedger acc tok f = do
  (e, _) <- getState
  let map_ledger = e_ledger e
  let m = saferMaybe ("updateLedger: account not found" <> show acc <> " " <> show map_ledger) $ M.lookup acc map_ledger
  let prev_amt = saferMaybe ("updateLedger: token not found " <> show tok <> " " <> show map_ledger) $ M.lookup tok m
  let new_amt = f prev_amt
  let new_nw_ledger = M.insert acc (M.insert tok new_amt m) map_ledger
  setGlobal $ e {e_ledger = new_nw_ledger}

transferLedger :: Account -> Account -> Token -> Integer -> App ()
transferLedger fromAcc toAcc tok n = do
  updateLedger fromAcc tok (\x -> x - n)
  updateLedger toAcc tok (+ n)

consensusLookup :: DLVar -> App DLVal
consensusLookup dlvar = do
  (_, l) <- getState
  let locals = l_locals l
  case M.lookup consensusId locals of
    Nothing -> possible "consensusLookup: no local store"
    Just lst -> do
      let st = l_store lst
      case M.lookup dlvar st of
        Nothing ->
          possible $
            "consensusLookup"
              <> show dlvar
              <> " "
              <> show st
              <> " "
              <> show (l_who lst)
        Just a -> return a

-- ## INTERPRETER ## --

class Interp a where
  interp :: a -> App DLVal

interpAs :: (Interp a) => ActorId -> a -> App DLVal
interpAs aid p = do
  (g,l) <- getState
  let fAid = l_curr_actor_id l
  let l' = l {l_curr_actor_id = aid}
  setState (g,l')
  v <- interp p
  (g',l'') <- getState
  let l''' = l'' {l_curr_actor_id = fAid}
  setState (g',l''')
  return v

interpPrim :: (PrimOp, [DLVal]) -> App DLVal
interpPrim = \case
  (ADD _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (+) lhs rhs
  (SUB _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (-) lhs rhs
  (MUL _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (*) lhs rhs
  (DIV _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ div lhs rhs
  (MOD _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ mod lhs rhs
  (PLT _, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (<) lhs rhs
  (PLE _, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (<=) lhs rhs
  (PEQ _, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (==) lhs rhs
  (PGE _, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (>=) lhs rhs
  (PGT _, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (>) lhs rhs
  (IF_THEN_ELSE, [V_Bool cond, cond_val, alt]) -> do
    return $ if cond then cond_val else alt
  (DIGEST_EQ, [V_Digest lhs, V_Digest rhs]) -> return $ V_Bool $ (==) lhs rhs
  (ADDRESS_EQ, [V_Address lhs, V_Address rhs]) -> return $ V_Bool $ (==) lhs rhs
  (TOKEN_EQ, [V_Token lhs, V_Token rhs]) -> return $ V_Bool $ (==) lhs rhs
  (SELF_ADDRESS _slpart _bool _int, _) -> do
    (_, l) <- getState
    let actorid = l_curr_actor_id l
    case actorid == consensusId of
      False -> do
        let locals = l_locals l
        return $ V_Address $ l_acct $ saferMaybe "SELF_ADDRESS" $ M.lookup actorid locals
      True -> do
        return $ V_Address $ simContract
  (LSH, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ shiftL lhs (fromIntegral rhs)
  (RSH, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ shiftR lhs (fromIntegral rhs)
  (BAND _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (.&.) lhs rhs
  (BIOR _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (.|.) lhs rhs
  (BXOR _, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ xor lhs rhs
  (f, args) -> impossible $ "unhandled primop" <> show f <> " " <> show args

conCons' :: DLConstant -> DLVal
conCons' = \case
  DLC_UInt_max  -> V_UInt $ 2 ^ (64 :: Integer) - 1
  DLC_Token_zero -> V_Token 0

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      (_, l) <- getState
      let locals = l_locals l
      let aid = l_curr_actor_id l
      case M.lookup aid locals of
        Nothing -> possible "DLA_Var: no local store"
        Just lst -> do
          let st = l_store lst
          case M.lookup dlvar st of
            Nothing -> do
              possible $
                "DLA_Var "
                  <> show dlvar
                  <> " "
                  <> show st
                  <> " "
                  <> show (l_who lst)
            Just a -> return a
    DLA_Constant dlconst -> return $ conCons' dlconst
    DLA_Literal dllit -> interp dllit
    DLA_Interact _slpart str _dltype -> do
      livs <- l_livs <$> getMyLocalInfo
      let v = saferMaybe "DLA_Interact" $ M.lookup str livs
      return v

instance Interp DLLiteral where
  interp = \case
    DLL_Null -> return $ V_Null
    DLL_Bool bool -> return $ V_Bool bool
    DLL_Int _at _ int -> return $ V_UInt int
    DLL_TokenZero -> return $ V_Token 0

instance Interp DLLargeArg where
  interp = \case
    DLLA_Array _dltype dlargs -> V_Array <$> mapM interp dlargs
    DLLA_Tuple dlargs -> V_Tuple <$> mapM interp dlargs
    DLLA_Obj strs_to_dlargs -> V_Object <$> mapM interp strs_to_dlargs
    DLLA_Data _map_slvars_to_dltypes string dlarg -> do
      evd_arg <- interp $ dlarg
      return $ V_Data string evd_arg
    DLLA_Struct assoc_slvars_dlargs -> do
      evd_args <- mapM (\arg -> interp arg) $ M.fromList assoc_slvars_dlargs
      return $ V_Struct $ M.toAscList evd_args
    DLLA_Bytes bs -> return $ V_Bytes $ bunpack bs

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> interp dllargearg
    DLE_Impossible at _int err -> expect_thrown at err
    DLE_VerifyMuldiv at _ _ _ err -> expect_thrown at err
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- mapM interp dlargs
      interpPrim (primop, evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      arr <- vArray <$> interp dlarg1
      n <- vUInt <$> interp dlarg2
      return $ saferIndex (fromIntegral n) arr
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      arr <- vArray <$> interp dlarg1
      n <- vUInt <$> interp dlarg2
      ev3 <- interp dlarg3
      let n' = fromIntegral n
      return $ V_Array $ arraySet n' ev3 arr
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      arr1 <- vArray <$> interp dlarg1
      arr2 <- vArray <$> interp dlarg2
      return $ V_Array $ arr1 <> arr2
    DLE_TupleRef _at dlarg n -> do
      arr <- vTuple <$> interp dlarg
      return $ saferIndex (fromIntegral n) arr
    DLE_ObjectRef _at dlarg str -> do
      v <- interp dlarg
      case v of
        V_Struct assocs -> do
          return $ saferMaybe "DLE_ObjectRef: V_Struct" $ M.lookup str $ M.fromList assocs
        V_Object vmap -> do
          return $ saferMaybe "DLE_ObjectRef: V_Object" $ M.lookup str vmap
        _ -> possible "DLE_ObjectRef: expected Object or Struct"
    DLE_Interact at slcxtframes slpart str dltype dlargs -> do
      args <- mapM interp dlargs
      g <- getGlobal
      let apiObs = e_apis g
      let apis = M.fromList $ map (\(a,b) -> (a_name b, a)) $ M.toList apiObs
      let partName' = bunpack slpart
      case M.lookup partName' apis of
        Just apid -> do
          case a_val =<< M.lookup apid apiObs of
            Just v -> return v
            Nothing -> possible $ "DLE_Interact: late api call for " ++ partName'
        Nothing -> do
          case dltype of
            T_Null -> do
              return V_Null
            _ -> do
              suspend $ PS_Suspend (Just at) (A_Interact slcxtframes partName' str dltype args)
    DLE_Digest _at dlargs -> V_Digest <$> V_Tuple <$> mapM interp dlargs
    DLE_Claim _at _slcxtframes claimtype dlarg _maybe_bytestring -> case claimtype of
      CT_Assert -> interp dlarg
      CT_Assume _bool -> interp dlarg
      CT_Require -> interp dlarg
      CT_Possible -> interp dlarg
      CT_Unknowable _slpart dlargs -> do
        _ <- mapM interp dlargs
        interp dlarg
    DLE_Transfer _at dlarg1 dlarg2 maybe_dlarg -> do
      who <- whoAmI
      case who of
        Participant _ -> return V_Null
        Consensus -> do
          acc <- vAddress <$> interp dlarg1
          n <- vUInt <$> interp dlarg2
          case maybe_dlarg of
            Nothing -> do
              transferLedger simContract acc nwToken n
              return V_Null
            Just tok -> do
              ev <- vTok <$> interp tok
              transferLedger simContract acc ev n
              return V_Null
    DLE_TokenInit _at _dlarg -> return V_Null
    DLE_CheckPay _at _slcxtframes _dlarg _maybe_dlarg -> return $ V_Null
    DLE_Wait at dltimearg -> case dltimearg of
      Left dlarg -> do
        ev <- vUInt <$> interp dlarg
        suspend $ PS_Suspend (Just at) (A_AdvanceTime ev)
      Right dlarg -> do
        ev <- vUInt <$> interp dlarg
        suspend $ PS_Suspend (Just at) (A_AdvanceSeconds ev)
    DLE_PartSet _at _slpart dlarg -> interp dlarg
    DLE_MapRef _at dlmvar dlarg -> do
      (g, _) <- getState
      let linstate = e_linstate g
      acc <- vAddress <$> interp dlarg
      case M.lookup dlmvar linstate of
        Nothing -> return $ V_Data "None" V_Null
        Just m -> do
          case M.lookup acc m of
            Nothing -> return $ V_Data "None" V_Null
            Just m' -> return $ V_Data "Some" m'
    DLE_MapSet _at dlmvar dlarg maybe_dlarg -> do
      (e, _) <- getState
      let linst = e_linstate e
      acc <- vAddress <$> interp dlarg
      f <- case maybe_dlarg of
        Nothing -> return M.delete
        Just a -> do
          v <- interp a
          return $ flip M.insert v
      let m'' = case M.lookup dlmvar linst of
            Nothing -> M.empty
            Just m' -> m'
      let m = f acc m''
      setGlobal $ e {e_linstate = M.insert dlmvar m linst}
      return V_Null
    DLE_Remote at fs ra _rt f' pa as (DLWithBill {..}) ma -> do
      who <- whoAmI
      let f = fromMaybe f' ma
      case who of
        Participant _ -> return V_Null
        Consensus -> do
          acc <- fromIntegral <$> vContract <$> interp ra
          tok_billed <- mapM interp dwb_tok_billed
          vs <- mapM interp as
          v <- suspend $ PS_Suspend (Just at) (A_Remote fs f vs tok_billed)
          consensusPayout acc consensusId pa
          return v
    DLE_TokenNew _at dln -> do
      (g, _) <- getState
      let accIdMax = (e_naccid g) - 1
      tokId <- ledgerNewTokenRefs accIdMax dln
      return $ V_Token $ fromIntegral tokId
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      tok <- vTok <$> interp dlarg1
      burn_amt <- vUInt <$> interp dlarg2
      updateLedger simContract tok (burn_amt -)
      return V_Null
    DLE_TokenDestroy _at _dlarg -> return V_Null
    DLE_TimeOrder {} -> return V_Null
    DLE_GetContract _at -> V_Contract <$> l_acct <$> getMyLocalInfo
    DLE_GetAddress _at -> V_Address <$> l_acct <$> getMyLocalInfo
    DLE_EmitLog _at (L_Api _) [dlvar] -> consensusLookup dlvar
    DLE_EmitLog _at L_Internal [dlvar] -> consensusLookup dlvar
    -- events from Events are : [a] -> Null
    DLE_EmitLog _ (L_Event {}) _ -> return V_Null
    DLE_EmitLog {} -> impossible "DLE_EmitLog invariants not satisified"
    DLE_setApiDetails _ _ _ _ _ -> return V_Null
    DLE_GetUntrackedFunds _ mtokA _ -> do
      (e, _) <- getState
      tok <- maybe (return nwToken) (fmap vUInt . interp) mtokA
      let bal =
            saferMaybe "getActualBalance1" $
              M.lookup tok $
                saferMaybe "getActualBalance" $ M.lookup simContract $ e_ledger e
      return $ V_UInt bal
    DLE_FromSome _ mo da -> do
      (k, v) <- vData <$> interp mo
      case k of
        "Some" -> return v
        _ -> interp da

instance Interp DLStmt where
  interp = \case
    DL_Nop _at -> return V_Null
    DL_Let _at let_var expr -> case let_var of
      DLV_Eff -> do
        _ <- interp expr
        return V_Null
      DLV_Let _ var -> do
        ev <- interp expr
        addToStore var ev
        return V_Null
    DL_ArrayMap _at ans xs as i f -> do
      arrs' <- mapM vArray <$> mapM interp xs
      let f' avs iv = do
            zipWithM_ (\a v -> addToStore a v) as avs
            addToStore i $ V_UInt iv
            interp f
      res <- V_Array <$> zipWithM f' arrs' [0..]
      addToStore ans res
      return V_Null
    DL_ArrayReduce _at ans xs z b as i f -> do
      acc <- interp z
      arrs' <- mapM vArray <$> mapM interp xs
      let f' acc_v (elem_vs, iv) = do
             zipWithM_ (\a v -> addToStore a v) as elem_vs
             addToStore b acc_v
             addToStore i $ V_UInt iv
             interp f
      res <- foldM f' acc $ zip arrs' [0..]
      addToStore ans res
      return V_Null
    DL_Var _at _var -> return V_Null
    DL_Set _at var arg -> do
      ev <- interp arg
      addToStore var ev
      return V_Null
    DL_LocalDo _at dltail -> interp dltail
    DL_LocalIf _at arg tail1 tail2 -> do
      ev <- interp arg
      case ev of
        V_Bool True -> interp tail1
        V_Bool False -> interp tail2
        _ -> impossible "DL_LocalIf: statement interpreter"
    DL_LocalSwitch _at var cases -> do
      (k, v) <- vData <$> interp (DLA_Var var)
      let (switch_binding, _, dltail) = saferMaybe "DL_LocalSwitch" $ M.lookup k cases
      addToStore switch_binding v
      interp dltail
    DL_Only _at either_part dltail -> do
      case either_part of
        Left slpart -> do
          who <- whoAmI
          let slname = bunpack slpart
          case who == Participant slname of
            False -> do
              case who == Consensus of
                False -> return V_Null
                True -> do
                  g <- getGlobal
                  let apiObs = e_apis g
                  let apis = M.fromList $ map (\(a,b) -> (a_name b, a)) $ M.toList apiObs
                  case M.lookup slname apis of
                    Nothing -> return V_Null
                    Just i -> do
                      case a_val =<< (M.lookup i apiObs) of
                        Nothing -> return V_Null
                        Just _ -> interp dltail
            True -> interp dltail
        _ -> impossible "DL_Only: unexpected error (Right)"
    DL_MapReduce _at _int var1 dlmvar arg var2 var3 block -> do
      accu <- interp arg
      (g, _) <- getState
      let linst = e_linstate g
      let f =
            (\a x b y -> do
               addToStore a x
               addToStore b y
               interp block)
      let m = saferMaybe "DL_MapReduce" $ M.lookup dlmvar linst
      res <- foldM (\x y -> f var2 x var3 y) accu $ m
      addToStore var1 res
      return V_Null

instance Interp DLTail where
  interp = \case
    DT_Return _at -> return V_Null
    DT_Com stmt dltail -> do
      _ <- interp stmt
      interp dltail

instance Interp DLBlock where
  interp = \case
    DLBlock _at _frame dltail dlarg -> do
      _ <- interp dltail
      interp dlarg

instance Interp LLConsensus where
  interp = \case
    LLC_Com stmt cons -> do
      _ <- interp stmt
      interp cons
    LLC_If _at arg cons1 cons2 -> do
      ev <- interp arg
      case ev of
        V_Bool True -> interp cons1
        V_Bool False -> interp cons2
        _ -> impossible "consensus interpreter"
    LLC_Switch _at var switch_cases -> do
      (k, v) <- vData <$> interp (DLA_Var var)
      let (switch_binding, _, cons) = saferMaybe "LLC_Switch" $ M.lookup k switch_cases
      addToStore switch_binding v
      interp cons
    LLC_FromConsensus _at1 _at2 _fs step -> do
      incrNWtime 1
      incrNWsecs 1
      interp step
    LLC_While at asn _inv cond body k -> do
      case asn of
        DLAssignment asn' -> do
          _ <- mapM (\(var, val) -> interp $ DL_Set at var val) $ M.toAscList asn'
          _ <- while cond body
          interp k
    LLC_Continue at asn -> do
      case asn of
        DLAssignment asn' -> do
          _ <- mapM (\(k, v) -> interp $ DL_Set at k v) $ M.toAscList asn'
          return V_Null
    LLC_ViewIs _at part' var export cons -> do
      g <- getGlobal
      let views = e_views g
      let viewids = e_viewids g
      let part = fmap bunpack part'
      let pName = case part of
            Nothing -> ""
            Just p -> p
      case M.lookup (pName <> var) viewids of
        Nothing -> possible "LLC_ViewIs1 : view not found"
        Just i -> do
          case M.lookup i views of
            Nothing -> possible "LLC_ViewIs2 : view not found"
            Just view -> do
              let view' = view { v_bl = export }
              let views' = M.insert i view' views
              let g' = g { e_views = views' }
              setGlobal g'
      interp cons

instance Interp LLStep where
  interp = \case
    LLS_Com stmt step -> do
      _ <- interp stmt
      interp step
    LLS_Stop _at -> return V_Null
    LLS_ToConsensus at _lct tc_send dlr@(DLRecv {..}) tc_mtime -> do
      (g, l) <- getState
      let actId = l_curr_actor_id l
      phId <- getPhaseId actId
      let msgs' = fromMaybe (NotFixedYet mempty) $ M.lookup phId $ e_messages g
      let sends = M.mapKeys bunpack tc_send
      incrPhaseId
      isTheTimePast tc_mtime >>= \case
        Just sk -> do
          interp sk
        Nothing -> do
          whoAmI >>= \case
            Participant who -> do
              case M.lookup who sends of
                Nothing -> do
                  case msgs' of
                    NotFixedYet _msgs'' -> do
                      _ <- suspend $ PS_Suspend (Just at) (A_Receive phId)
                      runWithWinner dlr phId
                    Fixed _ -> do
                      runWithWinner dlr phId
                Just dls -> do
                  case msgs' of
                    NotFixedYet msgs'' -> do
                      _ <- placeMsg dls dlr phId (fromIntegral actId) msgs'' False
                      _ <- suspend $ PS_Suspend (Just at) (A_Receive phId)
                      runWithWinner dlr phId
                    Fixed _ -> do
                      runWithWinner dlr phId
            Consensus -> do
              v <- suspend $ PS_Suspend (Just at) (A_TieBreak phId $ M.keys sends)
              case v of
                V_Data s v' -> do
                  let actId' = vUInt v'
                  apiFlag <- case s == "api" of
                    True -> return True
                    False -> do
                      accId <- getAccId $ fromIntegral actId'
                      part <- partName <$> whoIs (fromIntegral actId')
                      let dls = saferMaybe "LLS_ToConsensus1" $ M.lookup part sends
                      consensusPayout accId (fromIntegral actId') (ds_pay dls)
                      return False
                  m' <- case apiFlag of
                    True -> do
                      let apiObs = e_apis g
                      case (\(who) -> M.lookup who sends) =<< a_name <$> (M.lookup actId' apiObs) of
                        Nothing -> possible "API DLSend not found"
                        Just dls -> do
                          placeMsg dls dlr phId (fromIntegral actId') mempty True
                    False -> saferMaybe ("Phase not yet seen") <$> M.lookup phId <$> e_messages <$> getGlobal
                  let msgs = unfixedMsgs $ m'
                  let winningMsg = saferMaybe ("Message not yet seen") $ M.lookup (fromIntegral actId') msgs
                  _ <- fixMessageInRecord phId (fromIntegral actId') (m_store winningMsg) (m_pay winningMsg) apiFlag
                  winner dlr phId
                  interp $ dr_k
                _ -> possible "expected V_Data value"

placeMsg :: DLSend -> DLRecv a -> PhaseId -> ActorId -> (M.Map ActorId Message) -> Bool -> App MessageInfo
placeMsg (DLSend {..}) (DLRecv {..}) phId actId priors m_api = do
  g <- getGlobal
  ds_msg' <- mapM interp ds_msg
  let m_store = M.fromList $ zip dr_msg ds_msg'
  let m_pay = ds_pay
  let m = Message {..}
  let m' = NotFixedYet $ M.insert actId m priors
  let m'' = M.insert phId m' (e_messages g)
  g' <- getGlobal
  setGlobal g' { e_messages = m'' }
  return m'

poll :: PhaseId -> App (ActorId, APIFlag, Store)
poll phId = do
  (g, l) <- getState
  who <- show <$> whoIs (l_curr_actor_id l)
  return $ fixedMsg $ saferMaybe ("early poll for " <> who) $ M.lookup phId $ e_messages g

runWithWinner :: DLRecv LLConsensus -> PhaseId -> App DLVal
runWithWinner dlr phId = do
  winner dlr phId
  interp $ (dr_k dlr)

winner :: DLRecv LLConsensus -> PhaseId -> App ()
winner dlr phId = do
  g <- getGlobal
  let (actId, apiFlag, winningMsg) = fixedMsg $ saferMaybe "winner" $ M.lookup phId $ e_messages g
  let (xs, vs) = unzip $ M.toAscList winningMsg
  _ <- zipWithM addToStore xs vs
  accId <- case apiFlag of
    False -> getAccId actId
    True -> do
      let apis = e_apis g
      return $ a_acc $ saferMaybe "api winner" $ M.lookup (fromIntegral actId) apis
  bindConsensusMeta dlr actId accId

getAccId :: ActorId -> App Account
getAccId actId = do
  l <- getLocal
  let locals = l_locals l
  let caid = l_curr_actor_id l
  let lclsv = saferMaybe ("getAccId: couldn't find actorId: " <> show actId <> ", caid: " <> show caid) $ M.lookup (fromIntegral actId) locals
  return $ fromIntegral $ l_acct lclsv

getPhaseId :: ActorId -> App PhaseId
getPhaseId actId = do
  l <- getLocal
  let locals = l_locals l
  let lclsv = saferMaybe "getPhaseId" $ M.lookup (fromIntegral actId) locals
  return $ l_phase lclsv

getMyLocalInfo :: App LocalInfo
getMyLocalInfo = do
  l <- getLocal
  let actId = l_curr_actor_id l
  let locals = l_locals l
  return $ saferMaybe "getMyLocalInfo" $ M.lookup (fromIntegral actId) locals

consensusPayout :: Account -> ActorId -> DLPayAmt -> App ()
consensusPayout accId actId DLPayAmt {..} = do
  _ <-
    mapM
      (\(a, b) -> do
         b' <- vTok <$> (interpAs actId b)
         a' <- vUInt <$> (interpAs actId a)
         transferLedger (fromIntegral accId) simContract b' a')
      pa_ks
  net <- vUInt <$> interpAs actId pa_net
  transferLedger (fromIntegral accId) simContract nwToken net

bindConsensusMeta :: DLRecv LLConsensus -> ActorId -> Account -> App ()
bindConsensusMeta (DLRecv {..}) actorId accId = do
  (g, l) <- getState
  addToStore dr_time $ V_UInt (e_nwtime g)
  addToStore dr_secs $ V_UInt (e_nwsecs g)
  addToStore dr_from $ V_Address $ fromIntegral accId
  let myActorId = l_curr_actor_id l
  case actorId == myActorId of
    True -> addToStore dr_didSend $ V_Bool True
    False -> addToStore dr_didSend $ V_Bool False


instance Interp LLProg where
  interp (LLProg _at _llo slparts _dli _dex dvs _apis _alias _evts step) = do
    let apiNames = sps_apis slparts
    let (apiParts,regParts) = partition (\(a,_b) -> member a apiNames) $ M.toAscList $ sps_ies slparts
    registerParts regParts
    registerAPIs apiParts
    registerViews $ M.toAscList $ M.map M.toAscList dvs
    interp step

isTheTimePast :: Maybe (DLTimeArg, LLStep) -> App (Maybe LLStep)
isTheTimePast tc_mtime = do
  (g, _) <- getState
  case tc_mtime of
    Just (dltimearg, step) -> case dltimearg of
      Left dlarg -> do
        n' <- vUInt <$> interp dlarg
        let t = e_nwtime g
        case (t < n') of
          True -> return Nothing
          False -> return $ Just step
      Right dlarg -> do
        n' <- vUInt <$> interp dlarg
        let t = e_nwsecs g
        case (t < n') of
          True -> return Nothing
          False -> return $ Just step
    Nothing -> return $ Nothing

registerViews :: [(Maybe SLPart, [(SLVar, IType)])] -> App ()
registerViews [] = return ()
registerViews ((_, []) : vs) = registerViews vs
registerViews ((sl, ((slv,ty) : vars)) : vs) = do
  s <- getState
  let (g,l) = registerView s (fmap bunpack sl) slv ty
  setGlobal g
  setLocal l
  registerViews [(sl,vars)]
  registerViews vs

registerView :: State -> Maybe String -> SLVar -> IType -> State
registerView (g, l) v_name v_var v_ty = do
  let views = e_views g
  let viewids = e_viewids g
  let vid = e_nvid g
  let v_bl = Nothing
  let v = ReachView {..}
  let pName = case v_name of
        Nothing -> ""
        Just p -> p
  let g' =
        g
          { e_views = M.insert vid v views
          , e_viewids = M.insert (pName <> v_var) vid viewids
          , e_nvid = vid + 1
          }
  (g', l)

registerParts :: [(SLPart, InteractEnv)] -> App ()
registerParts [] = return ()
registerParts ((p, iv) : ps) = do
  s <- getState
  let (g,l) = registerPart s (bunpack p) iv
  setGlobal g
  setLocal l
  registerParts ps

registerPart :: State -> String -> InteractEnv -> State
registerPart (g, l) s iv = do
  let actorId = e_nactorid g
  let pacts = e_partacts g
  let aid = e_naccid g
  let locals = l_locals l
  let lcl =
        LocalInfo
          { l_acct = fromIntegral aid
          , l_who = Just $ s
          , l_store = mempty
          , l_phase = 0
          , l_ks = Nothing
          , l_livs = mempty
          , l_ivd = iv
          }
  let locals' = M.insert actorId lcl locals
  let ledger = e_ledger g
  let ledger' = M.insert (fromIntegral aid) (M.singleton nwToken simContractAmt) ledger
  let g' =
        g
          { e_nactorid = actorId + 1
          , e_naccid = aid + 1
          , e_partacts = M.insert s actorId pacts
          , e_ledger = ledger'
          }
  let l' = l {l_locals = locals'}
  (g', l')

registerAPIs :: [(SLPart, InteractEnv)] -> App ()
registerAPIs [] = return ()
registerAPIs ((p, iv) : ps) = do
  s <- getState
  let (g,l) = registerAPI s (bunpack p) iv
  setGlobal g
  setLocal l
  registerAPIs ps

registerAPI :: State -> String -> InteractEnv -> State
registerAPI (g, l) a_name a_liv = do
  let apid = e_napid g
  let apis = e_apis g
  let a_acc = e_naccid g
  let a_val = Nothing
  let ledger = e_ledger g
  let ledger' = M.insert (fromIntegral a_acc) (M.singleton nwToken simContractAmt) ledger
  let a = ReachAPI {..}
  let g' =
        g
          { e_apis = M.insert apid a apis
          , e_napid = apid + 1
          , e_naccid = a_acc + 1
          , e_ledger = ledger'
          }
  (g', l)

while :: DLBlock -> LLConsensus -> App DLVal
while bl cons = do
  bool <- interp bl
  case bool of
    V_Bool False -> return V_Null
    V_Bool True -> do
      _ <- interp cons
      while bl cons
    _ -> possible $ ("in (while) expected boolean, received " <> show bool)

saferIndex :: Int -> [a] -> a
saferIndex 0 (x : _) = x
saferIndex _ [] = possible "saferIndex failed"
saferIndex n (_ : xs) = saferIndex (n -1) xs

vUInt :: G.HasCallStack => DLVal -> Integer
vUInt (V_UInt n) = n
vUInt b = impossible ("unexpected error: expected integer value: received " <> show b)

vContract :: G.HasCallStack => DLVal -> Account
vContract (V_Contract n) = n
vContract b = impossible ("unexpected error: expected account value: received " <> show b)

vTok :: G.HasCallStack => DLVal -> Integer
vTok (V_Token n) = fromIntegral n
vTok b = impossible ("unexpected error: expected token integer value: received " <> show b)

vArray :: G.HasCallStack => DLVal -> [DLVal]
vArray (V_Array a) = a
vArray b = impossible ("unexpected error: expected array value: received " <> show b)

vTuple :: G.HasCallStack => DLVal -> [DLVal]
vTuple (V_Tuple a) = a
vTuple b = impossible ("unexpected error: expected tuple value: received " <> show b)

vObject :: G.HasCallStack => DLVal -> (M.Map SLVar DLVal)
vObject (V_Object a) = a
vObject b = impossible ("unexpected error: expected object value: received " <> show b)

vData :: G.HasCallStack => DLVal -> (SLVar, DLVal)
vData (V_Data a b) = (a, b)
vData b = impossible ("unexpected error: expected data value: received " <> show b)

vAddress :: G.HasCallStack => DLVal -> Account
vAddress (V_Address a) = a
vAddress b = impossible ("unexpected error: expected address value: received " <> show b)

unfixedMsgs :: G.HasCallStack => MessageInfo -> M.Map ActorId Message
unfixedMsgs (NotFixedYet m) = m
unfixedMsgs _ = possible "unexpected error: expected unfixed message"

fixedMsg :: G.HasCallStack => MessageInfo -> (ActorId, APIFlag, Store)
fixedMsg (Fixed (a, m)) = (a, m_api m, m_store m)
fixedMsg _ = possible "unexpected error: expected fixed message"

whoAmI :: App SimIdentity
whoAmI = do
  l <- getLocal
  let actId = l_curr_actor_id l
  let locals = l_locals l
  let local' = saferMaybe "whoAmI" $ M.lookup actId locals
  let who = l_who local'
  case who of
    Nothing -> return Consensus
    Just p -> return $ Participant p

whoIs :: ActorId -> App SimIdentity
whoIs actId = do
  l <- getLocal
  let locals = l_locals l
  let local' = saferMaybe "whoIs" $ M.lookup actId locals
  let who = l_who local'
  case who of
    Nothing -> return Consensus
    Just p -> return $ Participant p

partName :: SimIdentity -> Participant
partName (Participant p) = p
partName _ = possible "expected participant"
