{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Simulator where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util
import Data.Bits

-- state
data Env = Env
  {  e_store :: Store
   , e_ledger :: Ledger
   , e_linstate :: LinearState
   , e_nwtime :: Integer
   , e_nwsecs :: Integer
  }

type PartCont = Env -> DLVal -> PartState
-- type Part = Expr

data PartState
  = PS_Done Env DLVal
  | PS_Suspend Action Env PartCont

-- Manual Monad
newtype App a =
  App (Env -> (Env -> a -> PartState) -> PartState)

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

suspend :: (Env -> PartCont -> PartState) -> App DLVal
suspend = App

globalGet :: App Env
globalGet = App (\gvTop kAns -> kAns gvTop gvTop)

globalSet :: Env -> App ()
globalSet ngv = App (\_ kAns -> kAns ngv ())

runApp :: Env -> App DLVal -> PartState
runApp gv (App f) = f gv PS_Done

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

ledgerNewToken :: Integer -> App DLVal -> App DLVal
ledgerNewToken supply f  = do
  -- TODO QUESTION: which account? 0 is placeholder
  e <- globalGet
  let ledger = e_ledger e
  let token_id = nw_next_token ledger
  let new_nw_ledger = M.insert 0 (M.singleton token_id supply) (nw_ledger ledger)
  let new_ledger = ledger { nw_ledger = new_nw_ledger, nw_next_token = token_id + 1 }
  globalSet $ e {e_ledger = new_ledger}
  f

data Action
  = Action_TieBreak
  | Action_NewAcc
  | Action_NewPart
  | Action_ImitateFrontend
  | Action_Interact [DLVal]

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

-- Identify program states
-- type Id = Integer

-- All of the states visited
-- type Session = M.Map Id State

-- free-form & untyped parameters for the action
type Params = String -- JSON.Value

addToStore :: DLVar -> DLVal -> App DLVal -> App DLVal
addToStore x v app = do
  e <- globalGet
  let st = e_store e
  globalSet $ e {e_store = M.insert x v st}
  app

incrNWtime :: Integer -> App DLVal -> App DLVal
incrNWtime n app = do
  e <- globalGet
  let t = e_nwtime e
  globalSet $ e {e_nwtime = t + n }
  app

incrNWsecs :: Integer -> App DLVal -> App DLVal
incrNWsecs n app = do
  e <- globalGet
  let t = e_nwsecs e
  globalSet $ e {e_nwsecs = t + n }
  app

updateLedger :: Account -> Token -> (Integer -> Integer) -> App DLVal -> App DLVal
updateLedger acc tok f app = do
  e <- globalGet
  let ledger = e_ledger e
  let map_ledger = nw_ledger ledger
  let prev_amt = flip (M.!) tok $ (M.!) map_ledger acc
  let new_amt = f prev_amt
  let new_nw_ledger = M.insert acc (M.insert tok new_amt ((M.!) map_ledger acc)) map_ledger
  globalSet $ e {e_ledger = ledger { nw_ledger = new_nw_ledger }}
  app

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
  -- TODO
  -- (SELF_ADDRESS, _) -> undefined
  (LSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftL lhs (fromIntegral rhs)
  (RSH, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ shiftR lhs (fromIntegral rhs)
  (BAND, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.&.) lhs rhs
  (BIOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ (.|.) lhs rhs
  (BXOR, [V_UInt lhs,V_UInt rhs]) -> return $ V_UInt $ xor lhs rhs
  _ -> impossible "unexpected error"

instance Interp DLArg where
  interp = \case
    DLA_Var dlvar -> do
      g <- globalGet
      let st = e_store g
      return $ st M.! dlvar
    -- TODO: fake constants per connector
    DLA_Constant _dlconst -> return $ V_UInt $ toInteger (maxBound :: Int)
    DLA_Literal dllit -> interp dllit
    -- TODO: handle interact case: needs participant frontend state
    DLA_Interact _slpart _string _dltype -> undefined

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
    DLLA_Bytes _ -> undefined

instance Interp DLExpr where
  interp = \case
    DLE_Arg _at dlarg -> interp dlarg
    DLE_LArg _at dllargearg -> interp dllargearg
    -- DLE_Impossible at err -> expect_thrown at err
    DLE_PrimOp _at primop dlargs -> do
      evd_args <- mapM interp dlargs
      interpPrim (primop,evd_args)
    DLE_ArrayRef _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> return $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ArraySet _at dlarg1 dlarg2 dlarg3 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      ev3 <- interp dlarg3
      case (ev1,ev2) of
        (V_Array arr, V_UInt n) -> do
          let n' = fromIntegral n
          return $ V_Array $ take n' arr ++ [ev3] ++ drop (n' + 1) arr
        _ -> impossible "expression interpreter"
    DLE_ArrayConcat _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_Array arr1, V_Array arr2) -> return $ V_Array $ arr1 ++ arr2
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
        V_Tuple arr -> return $ arr !! (fromIntegral n)
        _ -> impossible "expression interpreter"
    DLE_ObjectRef _at dlarg str -> do
      ev <- interp dlarg
      case ev of
        V_Object obj -> return $ obj M.! str
        _ -> impossible "expression interpreter"
    DLE_Interact _at _slcxtframes _slpart _str _dltype dlargs -> do
      args <- mapM interp dlargs
      suspend $ PS_Suspend (Action_Interact args)
    DLE_Digest _at dlargs -> V_Digest <$> V_Tuple <$> mapM interp dlargs
    -- TODO
    DLE_Claim _at _slcxtframes claimtype dlarg _maybe_bytestring -> case claimtype of
      CT_Assert -> interp dlarg
      CT_Assume _bool -> interp dlarg
      CT_Require -> interp dlarg
      CT_Possible -> return V_Null
      CT_Unknowable _slpart _dlargs -> return $ V_Null
    DLE_Transfer _at dlarg1 dlarg2 maybe_dlarg -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        -- TODO: "Remove funds from contract..." which contract?
        (V_UInt n, V_Address acc) -> do
          case maybe_dlarg of
            Nothing -> updateLedger acc 0 (+n) $ return V_Null
            Just tok -> do
              ev <- interp tok
              case ev of
                V_UInt tok' -> updateLedger acc tok' (+n) $ return V_Null
                _ -> impossible "unexpected error"
        _ -> impossible "expression interpreter"
    DLE_TokenInit _at _dlarg -> return V_Null
    DLE_CheckPay _at _slcxtframes dlarg _maybe_dlarg -> interp dlarg
    DLE_Wait _at dltimearg -> case dltimearg of
      Left dlarg -> do
        ev <- interp dlarg
        case ev of
          V_UInt n -> incrNWtime n $ return V_Null
          _ -> impossible "unexpected error"
      Right dlarg -> do
        ev <- interp dlarg
        case ev of
          V_UInt n -> incrNWsecs n $ return V_Null
          _ -> impossible "unexpected error"
    DLE_PartSet _at _slpart dlarg -> interp dlarg
    DLE_MapRef _at dlmvar dlarg -> do
      g <- globalGet
      let linstate = e_linstate g
      ev <- interp dlarg
      case ev of
        V_Address acc -> do
          return $ flip (M.!) acc $ (M.!) linstate dlmvar
        _ -> impossible "unexpected error"
    DLE_MapSet _at dlmvar dlarg maybe_dlarg -> do
      e <- globalGet
      let linst = e_linstate e
      ev <- interp dlarg
      case ev of
        V_Address acc -> case maybe_dlarg of
          Nothing -> do
            let m = M.delete acc ((M.!) linst dlmvar)
            globalSet $ e {e_linstate = M.insert dlmvar m linst}
            return V_Null
          Just dlarg' -> do
            ev' <- interp dlarg'
            let m = M.insert acc ev' ((M.!) linst dlmvar)
            globalSet $ e {e_linstate = M.insert dlmvar m linst}
            return V_Null
        _ -> impossible "unexpected error"
    DLE_Remote _at _slcxtframes _dlarg _string _dlpayamnt _dlargs _dlwithbill -> undefined
    DLE_TokenNew _at dltokennew -> do
      supply <- interp (dtn_supply dltokennew)
      case supply of
        V_UInt supply' -> do
          ledgerNewToken supply' $ return V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenBurn _at dlarg1 dlarg2 -> do
      ev1 <- interp dlarg1
      ev2 <- interp dlarg2
      case (ev1,ev2) of
        (V_UInt tok, V_UInt burn_amt) -> do
          updateLedger 0 tok (burn_amt-) $ return V_Null
        _ -> impossible "expression interpreter"
    DLE_TokenDestroy _at dlarg -> do
      ev <- interp dlarg
      case ev of
        V_UInt tok -> do
          e <- globalGet
          let ledger = e_ledger e
          let map_ledger = nw_ledger ledger
          let new_nw_ledger = M.insert 0 (M.delete tok ((M.!) map_ledger 0)) map_ledger
          globalSet $ e {e_ledger = ledger { nw_ledger = new_nw_ledger }}
          return V_Null
        _ -> impossible "expression interpreter"

instance Interp DLStmt where
  interp = \case
    DL_Nop _at -> return V_Null
    DL_Let _at let_var expr -> case let_var of
      DLV_Eff -> do
        _ <- interp expr
        return V_Null
      DLV_Let _ var -> do
        ev <- interp expr
        addToStore var ev $ return V_Null
    DL_ArrayMap _at var1 arg var2 block -> do
      arr <- interp arg
      let f x val = addToStore x val $ interp block
      case arr of
        V_Array arr' -> do
          res <- V_Array <$> mapM (\val -> f var2 val) arr'
          addToStore var1 res $ return V_Null
        _ -> impossible "statement interpreter"
    DL_ArrayReduce _at var1 arg1 arg2 var2 var3 block -> do
      acc <- interp arg1
      arr <- interp arg2
      let f a x b y = addToStore a x $ addToStore b y $ interp block
      case arr of
        V_Array arr' -> do
          res <- foldM (\x y -> f var2 x var3 y) acc arr'
          addToStore var1 res $ return V_Null
        _ -> impossible "statement interpreter"
    DL_Var _at _var -> return V_Null
    DL_Set _at var arg -> do
      ev <- interp arg
      addToStore var ev $ return V_Null
    DL_LocalDo _at dltail -> interp dltail
    DL_LocalIf _at arg tail1 tail2 -> do
      ev <- interp arg
      case ev of
        V_Bool True -> interp tail1
        V_Bool False -> interp tail2
        _ -> impossible "statement interpreter"
    DL_LocalSwitch _at var switch_cases -> do
      ev <- interp (DLA_Var var)
      case ev of
        V_Data k v -> do
          let (switch_binding,_,dltail) = (M.!) switch_cases k
          addToStore switch_binding v $ interp dltail
        _ -> impossible "unexpected error"
    DL_Only _at _either_part dltail -> interp dltail
    DL_MapReduce _at _int var1 dlmvar arg var2 var3 block -> do
      accu <- interp arg
      g <- globalGet
      let linst = e_linstate g
      let f a x b y = addToStore a x $ addToStore b y $ interp block
      res <- foldM (\x y -> f var2 x var3 y) accu $ (M.!) linst dlmvar
      addToStore var1 res $ return V_Null

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
          let (switch_binding,_, cons) = (M.!) switch_cases k
          addToStore switch_binding v $ interp cons
        _ -> impossible "unexpected error"
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
    LLS_ToConsensus _at _lct _tc_send tc_recv _tc_mtime -> do
      -- TODO: block thread for race winner
      interp $ dr_k tc_recv

-- evaluate a linear Reach program
instance Interp LLProg where
  interp (LLProg _at _llo _ps _dli _dex _dvs _apis step) = interp step

while :: DLBlock -> LLConsensus -> App DLVal
while bl cons = do
  bool <- interp bl
  case bool of
    V_Bool False -> return V_Null
    V_Bool True -> do
      _ <- interp cons
      while bl cons
    _ -> impossible "unexpected error"

-- creates the first state and returns its id
-- init :: LLProg -> App Id
-- init (LLProg _at _llo _ps _dli _dex _dvs _apis _s) = return 0

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
-- parent :: Id -> App Id
-- parent = undefined

-- returns the state
-- inspect :: Id -> App State
-- inspect = undefined
