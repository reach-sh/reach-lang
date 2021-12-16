{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Core where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util
import Data.Bits
import Data.Aeson
import GHC.Generics

type Participant = String

data Transmission = Transmission
  { t_sender :: ActorId
  , t_store :: Store
  } deriving (Generic)

instance ToJSON Transmission where
  toEncoding = genericToEncoding defaultOptions
-- instance FromJSON Transmission

data LocalInfo = LocalInfo
  { l_acct :: Account
  , l_who :: Maybe Participant
  , l_store :: Store
  } deriving (Generic)

instance ToJSON LocalInfo

type ActorId = Int
type PoolId = Integer
type AccountId = Int

data Global = Global
  { e_record :: [Transmission]
  , e_ledger :: M.Map Account (M.Map Token Balance)
  , e_next_token :: Token
  , e_linstate :: LinearState
  , e_nwtime :: Integer
  , e_nwsecs :: Integer
  , e_nactorid :: ActorId
  , e_naccid :: AccountId
  , e_nmsgid :: PoolId
  , e_partacts :: M.Map Participant ActorId
  } deriving (Generic)

instance ToJSON Global
-- instance FromJSON Global

type Locals = M.Map ActorId LocalInfo

data Local = Local
  { e_locals :: Locals
  , e_curr_actorid :: ActorId
  } deriving (Generic)

instance ToJSON Local

type State = (Global, Local)

initState :: State
initState = (initGlobal, initLocal)

initGlobal :: Global
initGlobal = Global
  { e_record = mempty
  , e_ledger = M.singleton simContract (M.singleton nwToken simContractAmt)
  , e_next_token = 0
  , e_linstate = mempty
  , e_nwtime = 0
  , e_nwsecs = 0
  , e_nactorid = 0
  , e_naccid = 0
  , e_nmsgid = 0
  , e_partacts = mempty
  }

initLocal :: Local
initLocal = Local
  { e_locals = M.singleton (fromIntegral consensusId) LocalInfo
      {l_acct = consensusId, l_who = Nothing, l_store = mempty}
  , e_curr_actorid = fromIntegral consensusId
  }

type PartCont = State -> DLVal -> PartState

data PartState
  = PS_Done State DLVal
  | PS_Suspend Action State PartCont

initPartState :: PartState
initPartState = PS_Done initState V_Null

newtype App a =
  App (State -> (State -> a -> PartState) -> PartState)

instance Functor App where
  fmap t (App f) = App (\gv k -> f gv (\gv' v -> k gv' (t v)))

instance Applicative App where
  pure v = App (\gv k -> k gv v)
  (App fun) <*> (App val) =
    App (\gvTop kAns ->
      fun gvTop (\gvFun vFun ->
        val gvFun (\gvVal vVal ->
          kAns gvVal (vFun vVal))))

instance Monad App where
  (App val) >>= conF =
    App (\gvTop kAns ->
      val gvTop (\gvVal vVal ->
        let App con = conF vVal
         in con gvVal kAns))


suspend :: (State -> PartCont -> PartState) -> App DLVal
suspend = App

getState :: App State
getState = App (\gvTop kAns -> kAns gvTop gvTop)

getGlobal :: App Global
getGlobal = do
  (g,_) <- getState
  return g

getLocal :: App Local
getLocal = do
  (_,l) <- getState
  return l

setState :: State -> App ()
setState ngv = App (\_ kAns -> kAns ngv ())

setGlobal :: Global -> App ()
setGlobal g = do
  l <- getLocal
  setState (g,l)

setLocal :: Local -> App ()
setLocal l = do
  g <- getGlobal
  setState (g,l)

runApp :: State -> App DLVal -> PartState
runApp st (App f) = f st PS_Done

initApp :: LLProg -> State -> PartState
initApp p st = runApp st $ interp p

type ConsensusEnv = M.Map DLVar DLVal
type Store = ConsensusEnv
type Balance = Integer
type Token = Integer

consensusId :: Account
consensusId = -1

nwToken :: Token
nwToken = -1

simContract :: Account
simContract = consensusId

simContractAmt :: Balance
simContractAmt = 0

ledgerNewToken :: Account -> DLTokenNew -> App ()
ledgerNewToken acc tk = do
  (e, _) <- getState
  let ledger = e_ledger e
  let token_id = e_next_token e
  supply' <- interp (dtn_supply tk)
  case supply' of
    V_UInt supply -> do
      let new_nw_ledger = M.insert acc (M.singleton token_id supply) ledger
      setGlobal $ e { e_ledger = new_nw_ledger, e_next_token = token_id + 1 }
    _ -> impossible "expression interpreter"

data Action
  = A_TieBreak PoolId [String]
  | A_NewActor
  | A_None
  | A_ChangeActor Int
  | A_AdvanceTime Integer
  | A_AdvanceSeconds Integer
  | A_InteractV String String DLType --TODO either remove or add protection
  | A_Interact SrcLoc [SLCtxtFrame] String String DLType [DLVal]
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
  | V_Array [DLVal]
  | V_Tuple [DLVal]
  | V_Object (M.Map SLVar DLVal)
  | V_Data SLVar DLVal
  | V_Struct [(SLVar, DLVal)]
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DLVal
instance FromJSON DLVal

addToRecord :: ActorId -> DLVar -> DLVal -> App ()
addToRecord aid x v = do
  (e, _) <- getState
  let trs = e_record e
  setGlobal $ e {e_record = trs <> [Transmission {t_sender = aid, t_store = M.singleton x v}]}

addToStore :: DLVar -> DLVal -> App ()
addToStore x v = do
  (_, l) <- getState
  let locals = e_locals l
  let aid = e_curr_actorid l
  case M.lookup aid locals of
    Nothing -> possible "addToStore: no local store"
    Just lst -> do
      let st = l_store lst
      let lst' = lst {l_store = M.insert x v st}
      setLocal $ l {e_locals = M.insert aid lst' locals}

--TODO: replace with correct queue based impl
checkRecord :: DLVar -> App (Maybe DLVal)
checkRecord x = do
  (g, _) <- getState
  let record = reverse $ e_record g
  return $ checkRecord' record
  where
    checkRecord' [] = Nothing
    checkRecord' (a:as) = do
      let store = t_store a
      case M.lookup x store of
        Nothing -> checkRecord' as
        Just v -> Just v

incrNWtime :: Integer -> App ()
incrNWtime n = do
  (e, _) <- getState
  let t = e_nwtime e
  setGlobal $ e {e_nwtime = t + n }

incrNWsecs :: Integer -> App ()
incrNWsecs n = do
  (e, _) <- getState
  let t = e_nwsecs e
  setGlobal $ e {e_nwsecs = t + n }

incrMessageId :: App ()
incrMessageId = do
  (e, _) <- getState
  let t = e_nmsgid e
  setGlobal $ e {e_nwsecs = t + 1 }

updateLedger :: Account -> Token -> (Integer -> Integer) -> App ()
updateLedger acc tok f = do
  (e, _) <- getState
  let map_ledger = e_ledger e
  let m = saferMapRef "updateLedger" $ M.lookup acc map_ledger
  let prev_amt = saferMapRef "updateLedger1" $ M.lookup tok m
  let new_amt = f prev_amt
  let q = saferMapRef "updateLedger2" $ M.lookup acc map_ledger
  let new_nw_ledger = M.insert acc (M.insert tok new_amt q) map_ledger
  setGlobal $ e { e_ledger = new_nw_ledger }

transferLedger :: Account -> Account -> Token -> Integer -> App ()
transferLedger fromAcc toAcc tok n = do
  updateLedger fromAcc tok (n-)
  updateLedger toAcc tok (+n)

-- ## INTERPRETER ## --

class Interp a where
 interp :: a -> App DLVal

interpPrim :: (PrimOp, [DLVal]) -> App DLVal
interpPrim = \case
  (ADD, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (+) lhs rhs
  (SUB, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (-) lhs rhs
  (MUL, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (*) lhs rhs
  (DIV, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ div lhs rhs
  (MOD, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ mod lhs rhs
  (PLT, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (<) lhs rhs
  (PLE, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (<=) lhs rhs
  (PEQ, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (==) lhs rhs
  (PGE, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (>=) lhs rhs
  (PGT, [V_UInt lhs,V_UInt rhs]) -> return $ V_Bool $ (>) lhs rhs
  (IF_THEN_ELSE, [V_Bool cond, cond_val, alt]) -> do
    return $ if cond then cond_val else alt
  (DIGEST_EQ,  [V_Digest lhs,V_Digest rhs]) -> return $ V_Bool $ (==) lhs rhs
  (ADDRESS_EQ,  [V_Address lhs,V_Address rhs]) -> return $ V_Bool $ (==) lhs rhs
  (TOKEN_EQ, [V_Token lhs,V_Token rhs]) -> return $ V_Bool $ (==) lhs rhs
  (SELF_ADDRESS _slpart _bool _int, _) -> do
    (_, l) <- getState
    let actorid = e_curr_actorid l
    case actorid == (fromIntegral consensusId) of
      False -> do
        let locals = e_locals l
        return $ V_Address $ l_acct $ saferMapRef "SELF_ADDRESS" $ M.lookup actorid locals
      True -> do
        return $ V_Address $ consensusId
  (LSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftL lhs (fromIntegral rhs)
  (RSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftR lhs (fromIntegral rhs)
  (BAND, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.&.) lhs rhs
  (BIOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.|.) lhs rhs
  (BXOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ xor lhs rhs
  (f, args) -> impossible $ "unhandled primop" <> show f <> " " <> show args

conCons' :: DLConstant -> DLVal
conCons' DLC_UInt_max = V_UInt $ 2 ^ (64 :: Integer) - 1

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      (_, l) <- getState
      let locals = e_locals l
      let aid = e_curr_actorid l
      case (M.lookup aid locals, M.lookup (fromIntegral consensusId) locals) of
        (Nothing,_) -> possible "DLA_Var: no local store"
        (Just lst,Just conslst) -> do
          let st = l_store lst
          let cst = l_store conslst
          --TODO: replace with correct queue based impl
          case M.lookup dlvar (M.union st cst) of
            Nothing -> do
              r <- checkRecord dlvar
              case r of
                Just a -> return a
                Nothing -> possible $ "DLA_Var "
                  <> show dlvar
                  <> " "
                  <> show st
                  <> " "
                  <> show (l_who lst)
                  <> " "
                  <> show (M.map l_store locals)
            Just a -> return a
        _ -> impossible "a consensus store must exist"
    DLA_Constant dlconst -> return $ conCons' dlconst
    DLA_Literal dllit -> interp dllit
    DLA_Interact slpart str dltype -> do
      suspend $ PS_Suspend (A_InteractV (bunpack slpart) str dltype)

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
      return $ V_Struct $ M.toList evd_args
    DLLA_Bytes _ -> impossible "undefined"

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> interp dllargearg
    DLE_Impossible at _int err -> expect_thrown at err
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- mapM interp dlargs
      interpPrim (primop,evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> return $ saferIndex (fromIntegral n) arr
        _ -> impossible "expression interpreter"
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      ev3 <- interp dlarg3
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> do
          let n' = fromIntegral n
          return $ V_Array $ arraySet n' ev3 arr
        _ -> impossible "expression interpreter"
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> return $ V_Array $ arr1 <> arr2
        _ -> impossible "expression interpreter"
    DLE_ArrayZip _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> do
          return $ V_Array $ map (\(l,r)-> V_Tuple $ [l,r]) $ zip arr1 arr2
        _ -> impossible "expression interpreter"
    DLE_TupleRef _at dlarg n -> do
      ev <- interp dlarg
      case ev of
        V_Tuple arr -> return $ saferIndex (fromIntegral n) arr
        _ -> impossible "expression interpreter"
    DLE_ObjectRef _at dlarg str -> do
      ev <- interp dlarg
      case ev of
        V_Object obj -> return $ saferMapRef "DLE_ObjectRef" $ M.lookup str obj
        _ -> impossible $ "expression interpreter"
    DLE_Interact at slcxtframes slpart str dltype dlargs -> do
      args <- mapM interp dlargs
      suspend $ PS_Suspend (A_Interact at slcxtframes (bunpack slpart) str dltype args)
    DLE_Digest _at dlargs -> V_Digest <$> V_Tuple <$> mapM interp dlargs
    DLE_Claim _at _slcxtframes claimtype dlarg _maybe_bytestring -> case claimtype of
      CT_Assert -> interp dlarg
      CT_Assume bool -> case bool of
        True -> interp dlarg
        False -> error "CT_Assume error"
      CT_Require -> interp dlarg
      CT_Possible -> interp dlarg
      CT_Unknowable _slpart dlargs -> do
        _ <- mapM interp dlargs
        interp dlarg
    DLE_Transfer _at dlarg1 dlarg2 maybe_dlarg -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Address acc, V_UInt n) -> do
          case maybe_dlarg of
            Nothing -> do
              transferLedger simContract acc nwToken n
              return V_Null
            Just tok -> do
              ev <- interp tok
              case ev of
                V_UInt tok' -> do
                  transferLedger simContract acc tok' n
                  return V_Null
                _ -> impossible "unexpected error"
        _ -> impossible "expression interpreter"
    DLE_TokenInit _at _dlarg -> return V_Null
    DLE_CheckPay _at _slcxtframes _dlarg _maybe_dlarg -> return $ V_Null
    DLE_Wait _at dltimearg -> case dltimearg of
      Left dlarg -> do
        ev <- interp dlarg
        case ev of
          V_UInt n -> do
            suspend $ PS_Suspend (A_AdvanceTime n)
          _ -> impossible "unexpected error"
      Right dlarg -> do
        ev <- interp dlarg
        case ev of
          V_UInt n -> do
            suspend $ PS_Suspend (A_AdvanceSeconds n)
          _ -> impossible "unexpected error"
    DLE_PartSet _at _slpart dlarg -> interp dlarg
    DLE_MapRef _at dlmvar dlarg -> do
      (g, _) <- getState
      let linstate = e_linstate g
      ev <- interp dlarg
      case ev of
        V_Address acc -> do
          let m = saferMapRef "DLE_MapRef1" $ M.lookup dlmvar linstate
          return $ saferMapRef "DLE_MapRef2" $ M.lookup acc m
        _ -> impossible "unexpected error"
    DLE_MapSet _at dlmvar dlarg maybe_dlarg -> do
      (e, _) <- getState
      let linst = e_linstate e
      ev <- interp dlarg
      case ev of
        V_Address acc -> case maybe_dlarg of
          Nothing -> do
            let m = M.delete acc $ saferMapRef "DLE_MapSet1" $ M.lookup dlmvar linst
            setGlobal $ e {e_linstate = M.insert dlmvar m linst}
            return V_Null
          Just dlarg' -> do
            ev' <- interp dlarg'
            let m = M.insert acc ev' $ saferMapRef "DLE_MapSet2" $ M.lookup dlmvar linst
            setGlobal $ e {e_linstate = M.insert dlmvar m linst}
            return V_Null
        _ -> impossible "unexpected error"
    DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill -> impossible "undefined"
    DLE_TokenNew _at dltokennew -> do
      ledgerNewToken simContract dltokennew
      return V_Null
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_UInt tok, V_UInt burn_amt) -> do
          updateLedger 0 tok (burn_amt-)
          return V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenDestroy _at dlarg -> do
      ev <- interp dlarg
      case ev of
        V_UInt tok -> do
          (e, _) <- getState
          let map_ledger = e_ledger e
          let m = saferMapRef "DLE_TokenDestroy" $ M.lookup 0 map_ledger
          let new_nw_ledger = M.insert 0 (M.delete tok m) map_ledger
          setGlobal $ e {e_ledger = new_nw_ledger }
          return V_Null
        _ -> impossible "expression interpreter"
    -- TODO: new stuff
    DLE_TimeOrder _at _assoc_maybe_arg_vars -> return V_Null
    DLE_GetContract _at -> impossible "undefined"
    DLE_GetAddress _at -> impossible "undefined"
    DLE_EmitLog at (L_Api _) [dlvar] -> interp $ DL_Var at dlvar
    DLE_EmitLog at L_Internal [dlvar] -> interp $ DL_Var at dlvar
    -- events from Events are : [a] -> Null
    DLE_EmitLog _ (L_Event {}) _ -> return V_Null
    DLE_EmitLog {} -> impossible "DLE_EmitLog invariants not satisified"
    DLE_setApiDetails _ _ _ _ _ -> return V_Null

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
    DL_ArrayMap _at var1 arg var2 block -> do
      arr <- interp arg
      let f = (\x val -> do
            addToStore x val
            interp block)
      case arr of
        V_Array arr' -> do
          res <- V_Array <$> mapM (\val -> f var2 val) arr'
          addToStore var1 res
          return V_Null
        _ -> impossible "statement interpreter"
    DL_ArrayReduce _at var1 arg1 arg2 var2 var3 block -> do
      acc <- interp arg1
      arr <- interp arg2
      let f = (\a x b y -> do
            addToStore a x
            addToStore b y
            interp block)
      case arr of
        V_Array arr' -> do
          res <- foldM (\x y -> f var2 x var3 y) acc arr'
          addToStore var1 res
          return V_Null
        _ -> impossible "statement interpreter"
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
        _ -> impossible "statement interpreter"
    DL_LocalSwitch _at var cases -> do
      ev <- interp (DLA_Var var)
      case ev of
        V_Data k v -> do
          let (switch_binding,_,dltail) = saferMapRef "DL_LocalSwitch" $ M.lookup k cases
          addToStore switch_binding v
          interp dltail
        _ -> impossible "unexpected error"
    DL_Only _at either_part dltail -> do
      (g,l) <- getState
      --TODO: You should only run this if you actually are this part
      case either_part of
        Left slpart -> do
          let pacts = e_partacts g
          let actid = saferMapRef "DL_Only" $ M.lookup (bunpack slpart) pacts
          let restore_id = e_curr_actorid l
          setLocal $ l {e_curr_actorid = actid}
          r <- interp dltail
          l' <- getLocal
          setLocal $ l' {e_curr_actorid = restore_id}
          return r
        _ -> impossible "unexpected error"
    DL_MapReduce _at _int var1 dlmvar arg var2 var3 block -> do
      accu <- interp arg
      (g, _) <- getState
      let linst = e_linstate g
      let f = (\a x b y -> do
            addToStore a x
            addToStore b y
            interp block)
      let m = saferMapRef "DL_MapReduce" $ M.lookup dlmvar linst
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
      ev <- interp (DLA_Var var)
      case ev of
        V_Data k v -> do
          let (switch_binding,_, cons) = saferMapRef "LLC_Switch" $ M.lookup k switch_cases
          addToStore switch_binding v
          interp cons
        _ -> impossible "unexpected error"
    LLC_FromConsensus _at1 _at2 step -> do
      incrNWtime 1
      incrNWsecs 1
      interp step
    LLC_While at asn _inv cond body k -> do
      case asn of
        DLAssignment asn' -> do
          _ <- mapM (\(var,val)-> interp $ DL_Set at var val) $ M.toList asn'
          _ <- while cond body
          interp k
    LLC_Continue at asn -> do
      case asn of
        DLAssignment asn' -> do
          _ <- mapM (\(k,v)-> interp $ DL_Set at k v) $ M.toList asn'
          return V_Null
    LLC_ViewIs _at _part _var _export cons -> interp cons

instance Interp LLStep where
  interp = \case
    LLS_Com stmt step -> do
      _ <- interp stmt
      interp step
    LLS_Stop _at -> return V_Null
    LLS_ToConsensus _at _lct tc_send recv tc_mtime -> do
      (g, l) <- getState
      let mid = e_nmsgid g
      incrMessageId
      let sends = M.mapKeys bunpack tc_send
      v <- suspend $ PS_Suspend (A_TieBreak mid $ M.keys sends)
      case v of
        V_UInt n -> do
          let locals = e_locals l
          let lclsv = saferMapRef "LLS_ToConsensus" $ M.lookup (fromIntegral n) locals
          let lclst = l_who $ lclsv
          case lclst of
            Nothing -> impossible "expected participant id, received consensus id"
            Just part -> do
              let dls = saferMapRef "LLS_ToConsensus1" $ M.lookup part sends
              --TODO: there should be another action called "Run Timeout"
              let f t n' step = case (t < n') of
                    True -> consensusBody n g l dls recv
                    False -> interp step
              case tc_mtime of
                Just (dltimearg, step) -> case dltimearg of
                  Left dlarg -> do
                    n' <- vUInt <$> interp dlarg
                    let t = e_nwtime g
                    f t n' step
                  Right dlarg -> do
                    n' <- vUInt <$> interp dlarg
                    let t = e_nwtime g
                    f t n' step
                Nothing -> consensusBody n g l dls recv
        _ -> impossible "unexpected client answer"

consensusBody :: Integer -> Global -> Local -> DLSend -> DLRecv LLConsensus -> App DLVal
consensusBody n g l (DLSend {..}) (DLRecv {..}) = do
  let locals = e_locals l
  let lclsv = saferMapRef "LLS_ToConsensus" $ M.lookup (fromIntegral n) locals
  --TODO: replace with correct queue based impl
  addToRecord (fromIntegral n) dr_time $ V_UInt (e_nwtime g)
  addToRecord (fromIntegral n) dr_secs $ V_UInt (e_nwsecs g)
  --TODO: This is only true for the actual sender. It is false for everyone else
  addToRecord (fromIntegral n) dr_didSend $ V_Bool True
  addToRecord (fromIntegral n) dr_from $ V_Address $ l_acct lclsv
  let (DLPayAmt {..}) = ds_pay
  let restore_id = e_curr_actorid l
  setLocal $ l {e_curr_actorid = fromIntegral n}
  --TODO: currently ignoring the token amounts
  net <- interp pa_net
  case net of
    V_UInt net' -> do
      transferLedger (l_acct lclsv) simContract nwToken net'
      ds_msg' <- mapM interp ds_msg
      l' <- getLocal
      setLocal $ l' {e_curr_actorid = restore_id}
      _ <- zipWithM addToStore dr_msg ds_msg'
      _ <- zipWithM (addToRecord $ fromIntegral n) dr_msg ds_msg'
      interp $ dr_k
    _ -> impossible "LLS_ToConsensus2: expected integer"

-- evaluate a linear Reach program
instance Interp LLProg where
  interp (LLProg _at _llo slparts _dli _dex _dvs _apis _evts step) = do
    registerParts $ M.keys $ sps_ies slparts
    interp step

registerParts :: [SLPart] -> App ()
registerParts [] = return ()
registerParts (p:ps) = do
  s <- getState
  let (g,l) = registerPart s (bunpack p)
  setGlobal g
  setLocal l
  registerParts ps

registerPart :: State -> String -> State
registerPart (g,l) s = do
  let actorid = e_nactorid g
  let pacts = e_partacts g
  let aid = e_naccid g
  let locals = e_locals l
  let lcl = LocalInfo
        { l_acct = fromIntegral aid
        , l_who = Just $ s
        , l_store = mempty
        }
  let locals' = M.insert actorid lcl locals
  let ledger = e_ledger g
  let ledger' = M.insert (fromIntegral aid) (M.singleton nwToken simContractAmt) ledger
  let g' = g { e_nactorid = actorid + 1,
    e_naccid = aid + 1,
    e_partacts = M.insert s aid pacts,
    e_ledger = ledger' }
  let l' = l {e_locals = locals'}
  (g',l')

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
saferIndex 0 (x:_) = x
saferIndex _ [] = possible "saferIndex failed"
saferIndex n (_:xs) = saferIndex (n-1) xs

vUInt :: DLVal -> Integer
vUInt (V_UInt n) = n
vUInt _ =  impossible "unexpected error: expected integer"
