{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Core where

import Control.Monad.Reader
import Data.Aeson
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
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

type AccountId = Int

type Ledger = M.Map Account (M.Map Token Balance)

data Message = Message
  { m_store :: Store
  , m_pay :: DLPayAmt
  }
  deriving (Show, Generic)

instance ToJSON Message where
  toJSON (Message _ _) = "Message"

data MessageInfo = NotFixedYet (M.Map ActorId Message) | Fixed (ActorId, Message)
  deriving (Show, Generic)

instance ToJSON MessageInfo

data Global = Global
  { e_ledger :: Ledger
  , e_ntok :: Token
  , e_linstate :: LinearState
  , e_nwtime :: Integer
  , e_nwsecs :: Integer
  , e_nactorid :: ActorId
  , e_naccid :: AccountId
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

type ConsensusEnv = M.Map DLVar DLVal

type LocalInteractEnv = M.Map String DLVal

type Store = ConsensusEnv

type Balance = Integer

type Token = Integer

consensusId :: ActorId
consensusId = -1

nwToken :: Token
nwToken = -1

simContract :: Account
simContract = fromIntegral consensusId

simContractAmt :: Balance
simContractAmt = 0

ledgerNewToken :: Account -> DLTokenNew -> App ()
ledgerNewToken acc tk = do
  (e, _) <- getState
  let ledger = e_ledger e
  let tokId = e_ntok e
  supply <- vUInt <$> interp (dtn_supply tk)
  let new_nw_ledger = M.insert acc (M.singleton tokId supply) ledger
  setGlobal $ e {e_ledger = new_nw_ledger, e_ntok = tokId + 1}

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
  case (M.lookup aid locals, M.lookup consensusId locals) of
    (Nothing, _) -> possible "addToStore: no local store"
    (Just lst, Just cslst) -> do
      let st = l_store lst
      let lst' = lst {l_store = M.insert x v st}
      let csst = l_store cslst
      let cslst' = cslst {l_store = M.insert x v csst}
      setLocal $ l {l_locals = M.insert consensusId cslst' $ M.insert aid lst' locals}
    _ -> impossible "consensus store must exist"

fixMessageInRecord :: PhaseId -> ActorId -> Store -> DLPayAmt -> App ()
fixMessageInRecord phId actId sto pay = do
  (g, _) <- getState
  let msgs = e_messages g
  let msgs' = M.insert phId (Fixed (actId, Message {m_store = sto, m_pay = pay})) msgs
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

updateLedger :: Account -> Token -> (Integer -> Integer) -> App ()
updateLedger acc tok f = do
  (e, _) <- getState
  let map_ledger = e_ledger e
  let m = saferMaybe "updateLedger" $ M.lookup acc map_ledger
  let prev_amt = saferMaybe "updateLedger1" $ M.lookup tok m
  let new_amt = f prev_amt
  let new_nw_ledger = M.insert acc (M.insert tok new_amt m) map_ledger
  setGlobal $ e {e_ledger = new_nw_ledger}

transferLedger :: Account -> Account -> Token -> Integer -> App ()
transferLedger fromAcc toAcc tok n = do
  updateLedger fromAcc tok (\x -> x - n)
  updateLedger toAcc tok (+ n)

-- ## INTERPRETER ## --

class Interp a where
  interp :: a -> App DLVal

interpPrim :: (PrimOp, [DLVal]) -> App DLVal
interpPrim = \case
  (ADD, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (+) lhs rhs
  (SUB, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (-) lhs rhs
  (MUL, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (*) lhs rhs
  (DIV, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ div lhs rhs
  (MOD, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ mod lhs rhs
  (PLT, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (<) lhs rhs
  (PLE, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (<=) lhs rhs
  (PEQ, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (==) lhs rhs
  (PGE, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (>=) lhs rhs
  (PGT, [V_UInt lhs, V_UInt rhs]) -> return $ V_Bool $ (>) lhs rhs
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
  (BAND, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (.&.) lhs rhs
  (BIOR, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ (.|.) lhs rhs
  (BXOR, [V_UInt lhs, V_UInt rhs]) -> return $ V_UInt $ xor lhs rhs
  (f, args) -> impossible $ "unhandled primop" <> show f <> " " <> show args

conCons' :: DLConstant -> DLVal
conCons' DLC_UInt_max = V_UInt $ 2 ^ (64 :: Integer) - 1

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
            Nothing ->
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
    DLL_Int _at int -> return $ V_UInt int

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
    DLLA_Bytes _ -> impossible "undefined"

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
    DLE_ArrayZip _at dlarg1 dlarg2 -> do
      arr1 <- vArray <$> interp dlarg1
      arr2 <- vArray <$> interp dlarg2
      return $ V_Array $ zipWith (\l r -> V_Tuple $ [l, r]) arr1 arr2
    DLE_TupleRef _at dlarg n -> do
      arr <- vTuple <$> interp dlarg
      return $ saferIndex (fromIntegral n) arr
    DLE_ObjectRef _at dlarg str -> do
      obj <- vObject <$> interp dlarg
      return $ saferMaybe "DLE_ObjectRef" $ M.lookup str obj
    DLE_Interact at slcxtframes slpart str dltype dlargs -> do
      args <- mapM interp dlargs
      suspend $ PS_Suspend (Just at) (A_Interact slcxtframes (bunpack slpart) str dltype args)
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
              ev <- vUInt <$> interp tok
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
    DLE_Remote at slcxtframes dlarg str dlPayAmnt dlargs _dlWithBill@DLWithBill {..} -> do
      acc <- fromIntegral <$> vUInt <$> interp dlarg
      tok_billed <- mapM interp dwb_tok_billed
      args <- mapM interp dlargs
      v <- suspend $ PS_Suspend (Just at) (A_Remote slcxtframes str args tok_billed)
      consensusPayout acc dlPayAmnt
      return v
    DLE_TokenNew _at dltokennew -> do
      ledgerNewToken simContract dltokennew
      return V_Null
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      tok <- vUInt <$> interp dlarg1
      burn_amt <- vUInt <$> interp dlarg2
      updateLedger 0 tok (burn_amt -)
      return V_Null
    DLE_TokenDestroy _at dlarg -> do
      ev <- vUInt <$> interp dlarg
      (e, _) <- getState
      let map_ledger = e_ledger e
      let m = saferMaybe "DLE_TokenDestroy" $ M.lookup 0 map_ledger
      let new_nw_ledger = M.insert 0 (M.delete ev m) map_ledger
      setGlobal $ e {e_ledger = new_nw_ledger}
      return V_Null
    DLE_TimeOrder _at _assoc_maybe_arg_vars -> return V_Null
    DLE_GetContract _at -> V_Contract <$> l_acct <$> getMyLocalInfo
    DLE_GetAddress _at -> V_Address <$> l_acct <$> getMyLocalInfo
    DLE_EmitLog at (L_Api _) [dlvar] -> interp $ DL_Var at dlvar
    DLE_EmitLog at L_Internal [dlvar] -> interp $ DL_Var at dlvar
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
    DL_ArrayMap _at ans x a i f -> do
      arr' <- vArray <$> interp x
      let f' av iv = do
            addToStore a av
            addToStore i $ V_UInt iv
            interp f
      res <- V_Array <$> zipWithM f' arr' [0..]
      addToStore ans res
      return V_Null
    DL_ArrayReduce _at ans x z b a i f -> do
      acc <- interp z
      arr' <- vArray <$> interp x
      let f' acc_v (elem_v, iv) = do
             addToStore a elem_v
             addToStore b acc_v
             addToStore i $ V_UInt iv
             interp f
      res <- foldM f' acc $ zip arr' [0..]
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
          case who == Participant (bunpack slpart) of
            False -> return V_Null
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
    LLC_FromConsensus _at1 _at2 step -> do
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
    LLC_ViewIs _at _part _var _export cons -> interp cons

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
                      (actId',_) <- poll phId
                      runWithWinner dlr actId' phId
                    Fixed (actId', _msg) -> do
                      runWithWinner dlr actId' phId
                Just (DLSend {..}) -> do
                  case msgs' of
                    NotFixedYet msgs'' -> do
                      ds_msg' <- mapM interp ds_msg
                      let sto = M.fromList $ zip dr_msg ds_msg'
                      let m = Message {m_store = sto, m_pay = ds_pay}
                      let m' = M.insert phId (NotFixedYet $ M.insert actId m msgs'') (e_messages g)
                      setGlobal g { e_messages = m' }
                      _ <- suspend $ PS_Suspend (Just at) (A_Receive phId)
                      (actId',_) <- poll phId
                      runWithWinner dlr actId' phId
                    Fixed (actId', _msg) -> do
                      runWithWinner dlr actId' phId
            Consensus -> do
              v <- suspend $ PS_Suspend (Just at) (A_TieBreak phId $ M.keys sends)
              let actId' = fromIntegral $ vUInt v
              part <- partName <$> whoIs actId'
              let dls = saferMaybe "LLS_ToConsensus1" $ M.lookup part sends
              accId <- getAccId actId'
              m' <- saferMaybe ("Phase not yet seen") <$> M.lookup phId <$> e_messages <$> getGlobal
              let msgs = unfixedMsgs $ m'
              let winningMsg = saferMaybe ("Message not yet seen") $ M.lookup actId' msgs
              _ <- fixMessageInRecord phId (fromIntegral actId') (m_store winningMsg) (m_pay winningMsg)
              winner dlr actId' phId
              consensusPayout accId (ds_pay dls)
              interp $ dr_k

poll :: PhaseId -> App (ActorId, Store)
poll phId = do
  (g, l) <- getState
  who <- show <$> whoIs (l_curr_actor_id l)
  return $ fixedMsg $ saferMaybe ("early poll for " ++ who) $ M.lookup phId $ e_messages g

runWithWinner :: DLRecv LLConsensus -> ActorId -> PhaseId -> App DLVal
runWithWinner dlr actId phId = do
  winner dlr actId phId
  interp $ (dr_k dlr)

winner :: DLRecv LLConsensus -> ActorId -> PhaseId -> App ()
winner dlr actId phId = do
  g <- getGlobal
  let (_, winningMsg) = fixedMsg $ saferMaybe "winner" $ M.lookup phId $ e_messages g
  accId <- getAccId actId
  bindConsensusMeta dlr actId accId
  let (xs, vs) = unzip $ M.toAscList winningMsg
  _ <- zipWithM addToStore xs vs
  return ()

getAccId :: ActorId -> App AccountId
getAccId actId = do
  l <- getLocal
  let locals = l_locals l
  let lclsv = saferMaybe "getAccId" $ M.lookup (fromIntegral actId) locals
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

consensusPayout :: AccountId -> DLPayAmt -> App ()
consensusPayout accId DLPayAmt {..} = do
  _ <-
    mapM
      (\(a, b) -> do
         b' <- vUInt <$> (interp b)
         a' <- vUInt <$> (interp a)
         transferLedger (fromIntegral accId) simContract b' a')
      pa_ks
  net <- vUInt <$> interp pa_net
  transferLedger (fromIntegral accId) simContract nwToken net

bindConsensusMeta :: DLRecv LLConsensus -> ActorId -> AccountId -> App ()
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
  interp (LLProg _at _llo slparts _dli _dex _dvs _apis _evts step) = do
    registerParts $ M.toAscList $ sps_ies slparts
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

registerParts :: [(SLPart, InteractEnv)] -> App ()
registerParts [] = return ()
registerParts ((p, iv) : ps) = do
  s <- getState
  let (g, l) = registerPart s (bunpack p) iv
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

while :: DLBlock -> LLConsensus -> App DLVal
while bl cons = do
  bool <- interp bl
  case bool of
    V_Bool False -> return V_Null
    V_Bool True -> do
      _ <- interp cons
      while bl cons
    _ -> impossible "unexpected error"

saferIndex :: Int -> [a] -> a
saferIndex 0 (x : _) = x
saferIndex _ [] = possible "saferIndex failed"
saferIndex n (_ : xs) = saferIndex (n -1) xs

vUInt :: G.HasCallStack => DLVal -> Integer
vUInt (V_UInt n) = n
vUInt _ = impossible "unexpected error: expected integer"

vArray :: G.HasCallStack => DLVal -> [DLVal]
vArray (V_Array a) = a
vArray _ = impossible "unexpected error: expected array"

vTuple :: G.HasCallStack => DLVal -> [DLVal]
vTuple (V_Tuple a) = a
vTuple _ = impossible "unexpected error: expected tuple"

vObject :: G.HasCallStack => DLVal -> (M.Map SLVar DLVal)
vObject (V_Object a) = a
vObject _ = impossible "unexpected error: expected object"

vData :: G.HasCallStack => DLVal -> (SLVar, DLVal)
vData (V_Data a b) = (a, b)
vData _ = impossible "unexpected error: expected data"

vAddress :: G.HasCallStack => DLVal -> Account
vAddress (V_Address a) = a
vAddress _ = impossible "unexpected error: expected address"

unfixedMsgs :: G.HasCallStack => MessageInfo -> M.Map ActorId Message
unfixedMsgs (NotFixedYet m) = m
unfixedMsgs _ = possible "unexpected error: expected unfixed message"

fixedMsg :: G.HasCallStack => MessageInfo -> (ActorId, Store)
fixedMsg (Fixed (a, m)) = (a, m_store m)
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
