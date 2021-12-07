{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator.Core where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util
import Data.Bits
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

type Participant = String

data Transmission = Transmission
  { t_sender :: ActorId
  , t_store :: Store
  }

data LocalInfo = LocalInfo
  { l_acct :: Account
  , l_who :: Maybe Participant
  , l_store :: Store
  }

type ActorId = Int
type PoolId = Integer
type AccountId = Int

data Global = Global
  { e_record :: [Transmission]
  , e_ledger :: Ledger
  , e_linstate :: LinearState
  , e_nwtime :: Integer
  , e_nwsecs :: Integer
  , e_nactorid :: ActorId
  , e_naccid :: AccountId
  , e_nmsgid :: PoolId
  , e_partacts :: M.Map Participant ActorId
  }

type Locals = M.Map ActorId LocalInfo

data Local = Local
  { e_locals :: Locals
  , e_curr_actorid :: ActorId
  }

type State = (Global, Local)

initState :: State
initState = (initGlobal, initLocal)

initGlobal :: Global
initGlobal = Global
  { e_record = mempty
  , e_ledger = initLedger
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

-- unsuspend :: PartState -> State -> DLVal -> PartState
-- unsuspend ps st v =
--  case ps of
--    PS_Done _ _ -> do
--      -- error?
--      ps
--    PS_Suspend _ _ k ->
--      k st v

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

data Ledger = Ledger
  { nw_ledger :: M.Map Account (M.Map Token Balance)
  , nw_next_acc :: Integer
  , nw_next_token :: Integer
  }
  deriving (Eq)

consensusId :: Account
consensusId = -1

nwToken :: Token
nwToken = -1

simContract :: Account
simContract = consensusId

simContractAmt :: Balance
simContractAmt = 0

initLedger :: Ledger
initLedger = Ledger
  { nw_ledger = M.singleton simContract (M.singleton nwToken simContractAmt)
  , nw_next_acc = 0
  , nw_next_token = 0
  }

ledgerNewToken :: Account -> DLTokenNew -> App ()
ledgerNewToken acc tk = do
  (e, _) <- getState
  let ledger = e_ledger e
  let token_id = nw_next_token ledger
  supply' <- interp (dtn_supply tk)
  case supply' of
    V_UInt supply -> do
      let new_nw_ledger = M.insert acc (M.singleton token_id supply) (nw_ledger ledger)
      let new_ledger = ledger { nw_ledger = new_nw_ledger, nw_next_token = token_id + 1 }
      setGlobal $ e {e_ledger = new_ledger}
    _ -> impossible "expression interpreter"

data Action
  = A_TieBreak PoolId [String]
  | A_NewActor
  | A_None
  | A_ChangeActor Int
  | A_AdvanceTime Integer
  | A_AdvanceSeconds Integer
  | A_InteractV String String DLType
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
  setGlobal $ e {e_record = trs ++ [Transmission {t_sender = aid, t_store = M.singleton x v}]}

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
  let ledger = e_ledger e
  let map_ledger = nw_ledger ledger
  let m = saferMapRef "updateLedger" $ M.lookup acc map_ledger
  let prev_amt = saferMapRef "updateLedger1" $ M.lookup tok m
  let new_amt = f prev_amt
  let q = saferMapRef "updateLedger2" $ M.lookup acc map_ledger
  let new_nw_ledger = M.insert acc (M.insert tok new_amt q) map_ledger
  setGlobal $ e {e_ledger = ledger { nw_ledger = new_nw_ledger }}

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
  _ -> impossible "unexpected error"

conCons' :: DLConstant -> DLVal
conCons' DLC_UInt_max = V_UInt $ 2 ^ (64 :: Integer) - 1

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      (_, l) <- getState
      let locals = e_locals l
      let aid = e_curr_actorid l
      case M.lookup aid locals of
        Nothing -> possible "addToStore: no local store"
        Just lst -> do
          let st = l_store lst
          return $ saferMapRef "DLA_Var" $ M.lookup dlvar st
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
        (V_UInt n, V_Address acc) -> do
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
          let ledger = e_ledger e
          let map_ledger = nw_ledger ledger
          let m = saferMapRef "DLE_TokenDestroy" $ M.lookup 0 map_ledger
          let new_nw_ledger = M.insert 0 (M.delete tok m) map_ledger
          setGlobal $ e {e_ledger = ledger { nw_ledger = new_nw_ledger }}
          return V_Null
        _ -> impossible "expression interpreter"
    -- TODO: new stuff
    DLE_TimeOrder _at _assoc_maybe_arg_vars -> return V_Null
    DLE_GetContract _at -> impossible "undefined"
    DLE_GetAddress _at -> impossible "undefined"
    DLE_EmitLog at _str _maybe_str dlvar -> interp $ DL_Var at dlvar
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
    DL_Only _at _either_part dltail -> interp dltail
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
    LLS_ToConsensus _at _lct tc_send (DLRecv {..}) _tc_mtime -> do
      (g, l) <- getState
      let mid = e_nmsgid g
      incrMessageId
      v <- suspend $ PS_Suspend (A_TieBreak mid $ M.keys $ M.mapKeys bunpack tc_send)
      case v of
        V_UInt n -> do
          let locals = e_locals l
          let lclst = l_who $ saferMapRef "LLS_ToConsensus" $ M.lookup (fromIntegral n) locals
          case lclst of
            Nothing -> impossible "expected participant id, received consensus id"
            Just part -> do
              let m = saferMapRef "LLS_ToConsensus1" $ M.lookup (bpack part) tc_send
              case m of
                DLSend {..} -> do
                  ds_msg' <- mapM interp ds_msg
                  _ <- zipWithM addToStore dr_msg ds_msg'
                  _ <- zipWithM (addToRecord $ fromIntegral n) dr_msg ds_msg'
                  interp $ dr_k
        _ -> impossible "unexpected client answer"

-- evaluate a linear Reach program
instance Interp LLProg where
  interp (LLProg _at _llo slparts _dli _dex _dvs _apis step) = do
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
  let g' = g {e_nactorid = actorid + 1, e_naccid = aid + 1, e_partacts = M.insert s aid pacts}
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
