{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Reach.Debugger where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util
import Data.Bits
import Control.Monad.Cont
import Control.Monad.Trans.Cont
-- import Data.IORef
-- import Data.Either
-- import Control.Concurrent

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

ledgerNewToken :: Integer -> ResCont -> ResCont
ledgerNewToken supply f  = do
  -- TODO QUESTION: which account? 0 is placeholder
  ledger <- asks e_ledger
  let token_id = nw_next_token ledger
  let new_nw_ledger = M.insert 0 (M.singleton token_id supply) (nw_ledger ledger)
  let new_ledger = ledger { nw_ledger = new_nw_ledger, nw_next_token = token_id + 1 }
  local (\e -> e {e_ledger = new_ledger}) f

type Frontend = M.Map String ([DLVal] -> DLVal)

-- type Frontend = ( Account, SLPart, M.Map DLVar DLVal, [ DLTail ] )
--
-- type Frontends = [ Frontend ]
--
-- type NewPartActions = M.Map SLPart [ DLTail ]

-- type State = (ConsensusNetworkState, ConsensusEnv, Frontends, NewPartActions, DAppCode)

-- data DAppCode
--   = DAppLLCons LLConsensus
--   | DAppLLStep LLStep

-- data UIAction
--   = ContinueAction -- run until breakpoint or error
--   | NextAction Integer -- proceeds through the next N computation steps
--   | BTAction -- print the backtrace
--   | ShowAction String -- print the variable described by this string

data Action
  = TieBreakAction
  | NewAccAction
  | NewPartAction
  | ImitateFrontendAction

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
  deriving (Eq, Ord, Show)

type App = ReaderT Env IO

-- data Env = Env
--   { e_state :: IORef Session
--   }

-- state
data Env = Env
  {  e_store :: Store
   , e_ledger :: Ledger
   , e_linstate :: LinearState
   , e_frontend :: Frontend
   , e_nwtime :: Integer
   , e_nwsecs :: Integer
  }

-- Identify program states
type Id = Integer

-- All of the states visited
-- type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value

addToStore :: DLVar -> DLVal -> ResCont -> ResCont
addToStore x v app = do
  st <- asks e_store
  local (\e -> e {e_store = M.insert x v st}) $ app

incrNWtime :: Integer -> ResCont -> ResCont
incrNWtime n app = do
  t <- asks e_nwtime
  local (\e -> e {e_nwtime = t + n }) $ app

incrNWsecs :: Integer -> ResCont -> ResCont
incrNWsecs n app = do
  t <- asks e_nwsecs
  local (\e -> e {e_nwsecs = t + n }) $ app

updateLedger :: Account -> Token -> (Integer -> Integer) -> ResCont -> ResCont
updateLedger acc tok f app = do
  ledger <- asks e_ledger
  let map_ledger = nw_ledger ledger
  let prev_amt = flip (M.!) tok $ (M.!) map_ledger acc
  let new_amt = f prev_amt
  let new_nw_ledger = M.insert acc (M.insert tok new_amt ((M.!) map_ledger acc)) map_ledger
  local (\e -> e {e_ledger = ledger { nw_ledger = new_nw_ledger }}) $ app

-- ## INTERPRETER ## --

data Result
 = Succ DLVal
 | Blocked Action (DLVal -> ResCont)

type ResCont = ContT DLVal App Result

class Interp a where
 interp :: a -> ResCont

-- TODO: needs abort-like
block :: Action -> ResCont -> ResCont
block a k = callCC $ \_ -> return $ Blocked a $ \_ -> k

forceSucc :: Result -> DLVal
forceSucc = \case
  Succ v -> v
  _ -> impossible "unexpected block"

chainBlocks ::  DLVal -> (DLVal -> ResCont) -> (DLVal -> ResCont) -> ResCont
chainBlocks v ki kl = do
  r <- ki v
  case r of
    Succ k' -> kl k'
    Blocked a k' ->
      return $ Blocked a $ \v -> chainBlocks v k' kl

abort :: Result -> ResCont
abort = undefined

mapMToVal ::  (DLVal -> ResCont) -> [DLVal] -> ContT DLVal App [DLVal]
mapMToVal f [] = return []
mapMToVal f (x : xs) = do
  r <- f x
  case r of
    Succ r' -> (:) r' <$> mapMToVal f xs
    Blocked a k -> do
      callCC $ \k -> do
        shiftT $ Blocked a $ \v -> chainBlocks v k ((:) <$> f x <*> mapMToVal f xs)

interpPrim :: (PrimOp, [DLVal]) -> ResCont
interpPrim = \case
  (ADD, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ (+) lhs rhs
  (SUB, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ (-) lhs rhs
  (MUL, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ (*) lhs rhs
  (DIV, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ div lhs rhs
  (MOD, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ mod lhs rhs
  (PLT, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_Bool $ (<) lhs rhs
  (PLE, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_Bool $ (<=) lhs rhs
  (PEQ, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_Bool $ (==) lhs rhs
  (PGE, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_Bool $ (>=) lhs rhs
  (PGT, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_Bool $ (>) lhs rhs
  (IF_THEN_ELSE, [V_Bool cond, cond_val, alt]) -> do
    return $ Succ $ if cond then cond_val else alt
  (DIGEST_EQ,  [V_Digest lhs,V_Digest rhs]) -> return $ Succ $ V_Bool $ (==) lhs rhs
  (ADDRESS_EQ,  [V_Address lhs,V_Address rhs]) -> return $ Succ $ V_Bool $ (==) lhs rhs
  (TOKEN_EQ, [V_Token lhs,V_Token rhs]) -> return $ Succ $ V_Bool $ (==) lhs rhs
  -- TODO QUESTION
  (SELF_ADDRESS, _) -> undefined
  (LSH, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ shiftL lhs (fromIntegral rhs)
  (RSH, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ shiftR lhs (fromIntegral rhs)
  (BAND, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ (.&.) lhs rhs
  (BIOR, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ (.|.) lhs rhs
  (BXOR, [V_UInt lhs,V_UInt rhs]) -> return $ Succ $ V_UInt $ xor lhs rhs
  (BYTES_CONCAT, [V_Bytes lhs,V_Bytes rhs]) -> return $ Succ $ V_Bytes $ (++) lhs rhs

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      st <- asks e_store
      return $ Succ $ st M.! dlvar
    -- TODO: fake constants per connector
    DLA_Constant _dlconst -> return $ Succ $ V_UInt $ toInteger (maxBound :: Int)
    DLA_Literal dllit -> interp dllit
    -- TODO: handle interact case: needs participant frontend state
    DLA_Interact _slpart _string _dltype -> undefined

instance Interp DLLiteral where
  interp = \case
    DLL_Null -> return $ Succ $ V_Null
    DLL_Bool bool -> return $ Succ $ V_Bool bool
    DLL_Int _at int -> return $ Succ $ V_UInt int
    DLL_Bytes bytes -> return $ Succ $ V_Bytes $ show bytes

instance Interp DLLargeArg where
  interp = \case
    DLLA_Array _dltype dlargs -> Succ <$> V_Array <$> map forceSucc <$> mapM interp dlargs
    DLLA_Tuple dlargs -> Succ <$> V_Tuple <$> map forceSucc <$> mapM interp dlargs
    DLLA_Obj strs_to_dlargs -> Succ <$> V_Object <$> M.map forceSucc <$> mapM interp strs_to_dlargs
    DLLA_Data _map_slvars_to_dltypes string dlarg -> do
      evd_arg <- interp $ dlarg
      return $ Succ $ V_Data string $ forceSucc evd_arg
    DLLA_Struct assoc_slvars_dlargs -> do
      evd_args <- mapM (\arg -> interp arg) $ M.fromList assoc_slvars_dlargs
      return $ Succ $ V_Struct $ M.toList $ M.map forceSucc evd_args

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> interp dllargearg
    DLE_Impossible at err -> expect_thrown at err
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- map forceSucc <$> mapM interp dlargs
      interpPrim (primop,evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> return $ Succ $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      ev3 <- forceSucc <$> interp dlarg3
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> do
          let n' = fromIntegral n
          return $ Succ $ V_Array $ take n' arr ++ [ev3] ++ drop (n' + 1) arr
        _ -> impossible "expression interpreter"
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> return $ Succ $ V_Array $ arr1 ++ arr2
        _ -> impossible "expression interpreter"
    DLE_ArrayZip _at dlarg1 dlarg2 -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> do
          return $ Succ $ V_Array $ map (\(l,r)-> V_Tuple $ [l,r]) $ zip arr1 arr2
        _ -> impossible "expression interpreter"
    DLE_TupleRef _at dlarg n -> do
      ev <- forceSucc <$> interp dlarg
      case ev of
        V_Tuple arr -> return $ Succ $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ObjectRef _at dlarg str -> do
      ev <- forceSucc <$> interp dlarg
      case ev of
        V_Object obj -> return $ Succ $ obj M.! str
        _ -> impossible "expression interpreter"
    DLE_Interact _at _slcxtframes _slpart str _dltype dlargs -> do
      args <- map forceSucc <$> mapM interp dlargs
      frontend <- asks e_frontend
      let f = (M.!) frontend str
      return $ Succ $ f args
    DLE_Digest _at dlargs -> Succ <$> V_Digest <$> V_Tuple <$> map forceSucc <$> mapM interp dlargs
    DLE_Claim _at _slcxtframes claimtype dlarg _maybe_bytestring -> case claimtype of
      CT_Assert -> undefined
      CT_Assume _bool -> undefined
      CT_Require -> interp dlarg
      CT_Possible -> return $ Succ $ V_Null
      CT_Unknowable _slpart _dlargs -> return $ Succ $ V_Null
    DLE_Transfer _at dlarg1 dlarg2 maybe_dlarg -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      case (ev1,ev2) of
        -- TODO: "Remove funds from contract..." which contract?
        (V_UInt n, V_Address acc) -> do
          case maybe_dlarg of
            Nothing -> updateLedger acc 0 (+n) $ return $ Succ V_Null
            Just tok -> do
              ev <- forceSucc <$> interp tok
              case ev of
                V_UInt tok' -> updateLedger acc tok' (+n) $ return $ Succ V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenInit _at _dlarg -> return $ Succ V_Null
    DLE_CheckPay _at _slcxtframes dlarg _maybe_dlarg -> interp dlarg
    DLE_Wait _at dltimearg -> case dltimearg of
      Left dlarg -> do
        ev <- forceSucc <$> interp dlarg
        case ev of
          V_UInt n -> incrNWtime n $ return $ Succ V_Null
      Right dlarg -> do
        ev <- forceSucc <$> interp dlarg
        case ev of
          V_UInt n -> incrNWsecs n $ return $ Succ V_Null
    DLE_PartSet _at _slpart dlarg -> interp dlarg
    DLE_MapRef _at dlmvar dlarg -> do
      linstate <- asks e_linstate
      ev <- forceSucc <$> interp dlarg
      case ev of
        V_Address acc -> do
          return $ Succ $ flip (M.!) acc $ (M.!) linstate dlmvar
    DLE_MapSet _at dlmvar dlarg maybe_dlarg -> do
      linst <- asks e_linstate
      ev <- forceSucc <$> interp dlarg
      case ev of
        V_Address acc -> case maybe_dlarg of
          Nothing -> do
            let m = M.delete acc ((M.!) linst dlmvar)
            local (\e -> e {e_linstate = M.insert dlmvar m linst}) $ return $ Succ V_Null
          Just dlarg' -> do
            ev' <- forceSucc <$> interp dlarg'
            let m = M.insert acc ev' ((M.!) linst dlmvar)
            local (\e -> e {e_linstate = M.insert dlmvar m linst}) $ return $ Succ V_Null
    DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill -> undefined
    DLE_TokenNew _at dltokennew -> do
      supply <- forceSucc <$> interp (dtn_supply dltokennew)
      case supply of
        V_UInt supply' -> do
          ledgerNewToken supply' $ return $ Succ V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      ev1 <- forceSucc <$> interp dlarg1
      ev2 <- forceSucc <$> interp dlarg2
      case (ev1,ev2) of
        (V_UInt tok, V_UInt burn_amt) -> do
          updateLedger 0 tok (burn_amt-) $ return $ Succ V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenDestroy _at dlarg -> do
      ev <- forceSucc <$> interp dlarg
      case ev of
        V_UInt tok -> do
          ledger <- asks e_ledger
          let map_ledger = nw_ledger ledger
          let new_nw_ledger = M.insert 0 (M.delete tok ((M.!) map_ledger 0)) map_ledger
          local (\e -> e {e_ledger = ledger { nw_ledger = new_nw_ledger }}) $ return $ Succ V_Null
        _ -> impossible "expression interpreter"

instance Interp DLStmt where
  interp = \case
    DL_Nop _at -> return $ Succ V_Null
    DL_Let _at let_var expr -> case let_var of
      DLV_Eff -> do
        _ <- interp expr
        return $ Succ V_Null
      DLV_Let _ var -> do
        ev <- interp expr
        case ev of
          Succ ev' -> addToStore var ev' $ return $ Succ V_Null
          Blocked a k -> do
            return $ Blocked a $ \v -> do
              chainBlocks v k (\ev' -> do
                addToStore var ev' $ return $ Succ V_Null)
    DL_ArrayMap _at var1 arg var2 block -> do
      arr <- forceSucc <$> interp arg
      let f x val = addToStore x val $ interp block
      case arr of
        V_Array arr' -> do
          res <- V_Array <$> mapM (\val -> f var2 val) arr'
          addToStore var1 res $ return $ Succ V_Null
        _ -> impossible "statement interpreter"
    DL_ArrayReduce _at var1 arg1 arg2 var2 var3 block -> do
      acc <- forceSucc <$> interp arg1
      arr <- forceSucc <$> interp arg2
      let f a x b y = addToStore a x $ addToStore b y $ interp block
      case arr of
        V_Array arr' -> do
          res <- foldM (\x y -> f var2 x var3 y) acc arr'
          addToStore var1 res $ return $ Succ V_Null
        _ -> impossible "statement interpreter"
    DL_Var _at _var -> return $ V_Null
    DL_Set _at var arg -> do
      ev <- forceSucc <$> interp arg
      addToStore var ev $ return $ Succ V_Null
    DL_LocalDo _at dltail -> interp dltail
    DL_LocalIf _at arg tail1 tail2 -> do
      ev <- forceSucc <$> interp arg
      case ev of
        V_Bool True -> interp tail1
        V_Bool False -> interp tail2
        _ -> impossible "statement interpreter"
    DL_LocalSwitch _at var switch_cases -> do
      ev <- interp $ DLA_Var var
      case ev of
        V_Data k v -> do
          let (switch_binding, dltail) = (M.!) switch_cases k
          case switch_binding of
            Nothing -> interp dltail
            Just ident -> addToStore ident v $ interp dltail
    DL_Only _at _either_part dltail -> interp dltail
    DL_MapReduce _at _int var1 dlmvar arg var2 var3 block -> do
      accu <- interp arg
      linst <- asks e_linstate
      let f a x b y = addToStore a x $ addToStore b y $ interp block
      res <- foldM (\x y -> f var2 x var3 y) accu $ (M.!) linst dlmvar
      addToStore var1 res $ return $ Succ V_Null

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
      ev <- interp $ DLA_Var var
      case ev of
        V_Data k v -> do
          let (switch_binding, cons) = (M.!) switch_cases k
          case switch_binding of
            Nothing -> interp cons
            Just ident -> addToStore ident v $ interp cons
    LLC_FromConsensus _at1 _at2 step -> interp step
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
    LLS_ToConsensus _at _tc_send _tc_recv _tc_mtime -> undefined

-- evaluate a linear Reach program
instance Interp LLProg where
  interp (LLProg _at _llo _ps _dli _dex _dvs step) = interp step

while :: DLBlock -> LLConsensus -> ResCont
while bl cons = do
  bool <- interp bl
  case bool of
    V_Bool False -> return V_Null
    V_Bool True -> do
      _ <- interp cons
      while bl cons

-- interpM :: Store -> LLProg -> App (Store)
-- interpM st mprog = do
--   prog <- mprog
--   interp st prog

-- evaluate the next N steps
-- interpN :: Store -> Integer -> LLProg -> App (Store)
-- interpN st 0 prog = return st
-- interpN st n prog = do
--   st' <- interp st prog
--   interpN st' (n-1) ???

-- creates the first state and returns its id
init :: LLProg -> App Id
init (LLProg _at _llo _ps _dli _dex _dvs _s) = return 0

-- returns the set of possible reductions
-- actions  :: Id -> App [ Action ]
-- actions = undefined

-- applies an action (with its parameters) and returns a new state. Memoizes
-- apply :: Id -> Action -> Params -> App Id
-- apply = undefined

-- returns the children and how they can be derived
-- children :: Id -> App (M.Map Id [ (Action, Params) ])
-- children = undefined

-- returns the parent
parent :: Id -> App Id
parent = undefined

-- returns the state
-- inspect :: Id -> App State
-- inspect = undefined
