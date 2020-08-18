module Reach.Verify.SMT (verify_smt) where

import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
import Data.Digest.CRC32
import Data.IORef
import Data.List.Extra (foldl', mconcatMap)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Reach.AST
import Reach.CollectTypes
import Reach.EmbeddedFiles
import Reach.Pretty ()
import Reach.Type
import Reach.Util
import Reach.Verify.SMTParser (parseModel)
import SimpleSMT (Logger (Logger), Result (..), SExpr (..), Solver)
import qualified SimpleSMT as SMT
import System.Exit
import System.IO

--- SMT Helpers

--- FIXME decide on fixed bitvectors
use_bitvectors :: Bool
use_bitvectors = False

smtStdLib :: String
smtStdLib = B.unpack $ case use_bitvectors of
  True -> runtime_bt_smt2
  False -> runtime_smt2

uint256_sort :: SExpr
uint256_sort = case use_bitvectors of
  True -> List [Atom "_", Atom "BitVec", Atom "256"]
  False -> Atom "Int"

uint256_zero :: SExpr
uint256_zero = case use_bitvectors of
  True -> List [Atom "_", Atom "bv0", Atom "256"]
  False -> Atom "0"

uint256_le :: SExpr -> SExpr -> SExpr
uint256_le lhs rhs = smtPrimOp (impossible "raw") PLE [lhs, rhs]

uint256_inv :: Solver -> SExpr -> IO ()
uint256_inv smt v = SMT.assert smt (uint256_le uint256_zero v)

uint256_sub :: SExpr -> SExpr -> SExpr
uint256_sub lhs rhs = smtPrimOp (impossible "raw") SUB [lhs, rhs]

uint256_add :: SExpr -> SExpr -> SExpr
uint256_add lhs rhs = smtPrimOp (impossible "raw") ADD [lhs, rhs]

smtApply :: String -> [SExpr] -> SExpr
smtApply f args = List (Atom f : args)

smtEq :: SExpr -> SExpr -> SExpr
smtEq x y = smtApply "=" [x, y]

smtNot :: SExpr -> SExpr
smtNot se = smtApply "not" [se]

--- SMT conversion code

data Role
  = RoleContract
  | RolePart SLPart
  deriving (Eq, Show)

data VerifyMode
  = VM_Honest
  | VM_Dishonest Role
  deriving (Eq, Show)

data BindingOrigin
  = O_DishonestMsg SLPart
  | O_DishonestPay SLPart
  | O_HonestMsg SLPart DLArg
  | O_HonestPay SLPart DLArg
  | O_Transfer DLArg DLArg
  | O_ToConsensus
  | O_Var
  | O_Initialize
  | O_Interact
  | O_Expr DLExpr
  | O_Join
  | O_Assignment
  deriving (Eq)

instance Show BindingOrigin where
  show bo =
    case bo of
      O_DishonestMsg who -> "a dishonest message from " ++ sp who
      O_DishonestPay who -> "a dishonest payment from " ++ sp who
      O_HonestMsg who what -> "an honest message from " ++ sp who ++ " of " ++ sp what
      O_HonestPay who amt -> "an honest payment from " ++ sp who ++ " of " ++ sp amt
      O_Transfer to amt -> "a transfer to " ++ sp to ++ " of " ++ sp amt
      O_ToConsensus -> "publication"
      O_Var -> "function return"
      O_Initialize -> "initialization"
      O_Interact -> "interaction"
      O_Expr e -> "evaluating " ++ sp e
      O_Join -> "participant join"
      O_Assignment -> "loop variable"
    where
      sp :: Pretty a => a -> String
      sp = show . pretty

type SMTTypeInv =
  SExpr -> IO ()

type SMTTypeMap =
  M.Map SLType (String, SMTTypeInv)

data SMTCtxt = SMTCtxt
  { ctxt_smt :: Solver
  , ctxt_typem :: SMTTypeMap
  , ctxt_res_succ :: IORef Int
  , ctxt_res_fail :: IORef Int
  , ctxt_modem :: Maybe VerifyMode
  , ctxt_balance :: Int
  , ctxt_mtxn_value :: Maybe Int
  , ctxt_path_constraint :: [SExpr]
  , ctxt_bindingsrr :: IORef (IORef (M.Map String (Maybe DLVar, SrcLoc, BindingOrigin, Maybe SExpr)))
  , ctxt_while_invariant :: Maybe (LLBlock LLLocal)
  , ctxt_loop_var_subst :: M.Map DLVar DLArg
  , ctxt_primed_vars :: S.Set DLVar
  }

ctxt_mode :: SMTCtxt -> VerifyMode
ctxt_mode ctxt =
  case ctxt_modem ctxt of
    Nothing -> impossible "uninitialized"
    Just x -> x

newIORefRef :: a -> IO (IORef (IORef a))
newIORefRef v = do
  r <- newIORef v
  newIORef r

modifyIORefRef :: IORef (IORef a) -> (a -> a) -> IO ()
modifyIORefRef rr f = do
  r <- readIORef rr
  modifyIORef r f

paramIORef :: IORef (IORef a) -> IO b -> IO b
paramIORef rr m = do
  old_r <- readIORef rr
  old_v <- readIORef old_r
  new_r <- newIORef old_v
  writeIORef rr new_r
  ans <- m
  writeIORef rr old_r
  return $ ans

ctxtNewScope :: SMTCtxt -> SMTComp -> SMTComp
ctxtNewScope ctxt m = do
  paramIORef (ctxt_bindingsrr ctxt) $
    SMT.inNewScope (ctxt_smt ctxt) $ m

ctxt_txn_value :: SMTCtxt -> Int
ctxt_txn_value ctxt = fromMaybe (impossible "no txn value") $ ctxt_mtxn_value ctxt

shouldSimulate :: SMTCtxt -> SLPart -> Bool
shouldSimulate ctxt p =
  case ctxt_mode ctxt of
    VM_Honest -> True
    VM_Dishonest which ->
      case which of
        RoleContract -> False
        RolePart me -> me == p

smtBalance :: Int -> String
smtBalance i = "ctc_balance" ++ show i

smtBalanceRef :: Int -> SExpr
smtBalanceRef = Atom . smtBalance

smtTxnValue :: Int -> String
smtTxnValue i = "txn_value" ++ show i

smtTxnValueRef :: Int -> SExpr
smtTxnValueRef = Atom . smtTxnValue

smtInteract :: SMTCtxt -> SLPart -> String -> String
smtInteract _ctxt who m = "interact_" ++ (B.unpack who) ++ "_" ++ m

smtVar :: SMTCtxt -> DLVar -> String
smtVar ctxt dv@(DLVar _ _ _ i) = "v" ++ show i ++ mp
  where
    mp =
      case elem dv $ ctxt_primed_vars ctxt of
        True -> "p"
        False -> ""

smtTypeSort :: SMTCtxt -> SLType -> String
smtTypeSort ctxt t = fst $ (ctxt_typem ctxt) M.! t

smtTypeInv :: SMTCtxt -> SLType -> SMTTypeInv
smtTypeInv ctxt t = snd $ (ctxt_typem ctxt) M.! t

smtDeclare_v :: SMTCtxt -> String -> SLType -> IO ()
smtDeclare_v ctxt v t = do
  let smt = ctxt_smt ctxt
  let s = smtTypeSort ctxt t
  void $ SMT.declare smt v $ Atom s
  smtTypeInv ctxt t $ Atom v

smtPrimOp :: SMTCtxt -> PrimOp -> [SExpr] -> SExpr
smtPrimOp ctxt p =
  case p of
    ADD -> bvapp "bvadd" "+"
    SUB -> bvapp "bvsub" "-"
    MUL -> bvapp "bvmul" "*"
    DIV -> bvapp "bvudiv" "div"
    MOD -> bvapp "bvumod" "mod"
    PLT -> bvapp "bvult" "<"
    PLE -> bvapp "bvule" "<="
    PEQ -> app "="
    PGE -> bvapp "bvuge" ">="
    PGT -> bvapp "bvugt" ">"
    LSH -> bvapp "bvshl" cant
    RSH -> bvapp "bvlshr" cant
    BAND -> bvapp "bvand" cant
    BIOR -> bvapp "bvor" cant
    BXOR -> bvapp "bvxor" cant
    IF_THEN_ELSE -> app "ite"
    BYTES_EQ -> app "="
    BALANCE ->
      const (smtBalanceRef $ ctxt_balance ctxt)
    TXN_VALUE ->
      const (smtTxnValueRef $ ctxt_txn_value ctxt)
  where
    cant = impossible $ "Int doesn't support " ++ show p
    app n = smtApply n
    bvapp n_bv n_i = app $ if use_bitvectors then n_bv else n_i

smtTypeByteConverter :: SMTCtxt -> SLType -> String
smtTypeByteConverter ctxt t = (smtTypeSort ctxt t) ++ "_toBytes"

smtArgByteConverter :: SMTCtxt -> DLArg -> String
smtArgByteConverter ctxt arg =
  smtTypeByteConverter ctxt (argTypeOf arg)

smtArgBytes :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smtArgBytes ctxt at arg = smtApply (smtArgByteConverter ctxt arg) [smt_a ctxt at arg]

smtDigestCombine :: SMTCtxt -> SrcLoc -> [DLArg] -> SExpr
smtDigestCombine ctxt at args =
  case args of
    [] -> smtApply "bytes0" []
    [x] -> convert1 x
    (x : xs) -> smtApply "bytesAppend" [convert1 x, smtDigestCombine ctxt at xs]
  where
    convert1 = smtArgBytes ctxt at

--- Verifier

data TheoremKind
  = TAssert
  | TRequire
  | TPossible
  | TBalanceZero
  | TBalanceSufficient
  | TInvariant
  | TBounds
  deriving (Show)

data ResultDesc
  = RD_UnsatCore [String]
  | RD_Model SExpr

type SMTComp = IO ()

seVars :: SExpr -> S.Set String
seVars se =
  case se of
    Atom a ->
      --- FIXME try harder to figure out what is a variable, like v7,
      --- and what is a function symbol, like <
      S.singleton a
    List l -> mconcatMap seVars l

set_to_seq :: S.Set a -> Seq.Seq a
set_to_seq = Seq.fromList . S.toList

--- FYI, the last version that had Dan's display code was
--- https://github.com/reach-sh/reach-lang/blob/ab15ea9bdb0ef1603d97212c51bb7dcbbde879a6/hs/src/Reach/Verify/SMT.hs

display_fail :: SMTCtxt -> SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe ResultDesc -> IO ()
display_fail ctxt tat f tk tse mrd = do
  putStrLn $ "Verification failed:"
  putStrLn $ "  in " ++ (show $ ctxt_mode ctxt) ++ " mode"
  putStrLn $ "  of theorem " ++ show tk
  putStrLn $ "  at " ++ show tat
  mapM_ (putStrLn . ("  " ++) . show) f
  putStrLn $ ""
  --- FIXME Another way to think about this is to take `tse` and fully
  --- substitute everything that came from the program (the "context"
  --- below) and then just show the remaining variables found by the
  --- model.
  putStrLn $ "  Theorem formalization:"
  putStrLn $ "  " ++ (SMT.showsSExpr tse "")
  putStrLn $ ""
  putStrLn $ "  This could be violated if..."
  let pm =
        case mrd of
          Nothing ->
            mempty
          Just (RD_UnsatCore _uc) -> do
            --- FIXME Do something useful here
            mempty
          Just (RD_Model m) -> do
            parseModel m
  bindingsm <- readIORef =<< (readIORef $ ctxt_bindingsrr ctxt)
  let show_vars :: (S.Set String) -> (Seq.Seq String) -> IO [String]
      show_vars shown q =
        case q of
          Seq.Empty -> return $ []
          (v0 Seq.:<| q') -> do
            (vc, v0vars) <-
              case M.lookup v0 bindingsm of
                Nothing ->
                  return $ (mempty, mempty)
                Just (mdv, at, bo, mvse) -> do
                  let this se =
                        [("    " ++ show v0 ++ " = " ++ (SMT.showsSExpr se ""))]
                          ++ (case mdv of
                                Nothing -> mempty
                                Just dv -> ["      (from: " ++ show (pretty dv) ++ ")"])
                          ++ [ ("      (bound at: " ++ show at ++ ")")
                             , ("      (because: " ++ show bo ++ ")")
                             ]
                  case mvse of
                    Nothing ->
                      --- FIXME It might be useful to do `get-value` rather than parse
                      case M.lookup v0 pm of
                        Nothing ->
                          return $ mempty
                        Just (_ty, se) -> do
                          mapM_ putStrLn (this se)
                          return $ ([], seVars se)
                    Just se ->
                      return $ ((this se), seVars se)
            let nvars = S.difference v0vars shown
            let shown' = S.union shown nvars
            let new_q = set_to_seq nvars
            let q'' = q' <> new_q
            liftM (vc ++) $ show_vars shown' q''
  let tse_vars = seVars tse
  vctxt <- show_vars tse_vars $ set_to_seq $ tse_vars
  putStrLn $ ""
  putStrLn $ "  In context..."
  mapM_ putStrLn vctxt

smtAssert :: SMTCtxt -> SExpr -> SMTComp
smtAssert ctxt se = SMT.assert smt se'
  where
    smt = ctxt_smt ctxt
    se' =
      case ctxt_path_constraint ctxt of
        [] -> se
        pcs ->
          smtApply "=>" [(smtApply "and" pcs), se]

verify1 :: SMTCtxt -> SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> SMTComp
verify1 ctxt at mf tk se = SMT.inNewScope smt $ do
  smtAssert ctxt $ if isPossible then se else smtNot se
  r <- SMT.check smt
  case isPossible of
    True ->
      case r of
        Unknown -> bad $ return Nothing
        Unsat -> bad $ liftM (Just . RD_UnsatCore) $ SMT.getUnsatCore smt
        Sat -> good
    False ->
      case r of
        Unknown -> bad $ return Nothing
        Unsat -> good
        Sat -> bad $ liftM (Just . RD_Model) $ SMT.command smt $ List [Atom "get-model"]
  where
    smt = ctxt_smt ctxt
    good =
      modifyIORef (ctxt_res_succ ctxt) $ (1 +)
    bad mgetm = do
      mm <- mgetm
      display_fail ctxt at mf tk se mm
      modifyIORef (ctxt_res_fail ctxt) $ (1 +)
    isPossible =
      case tk of
        TPossible -> True
        _ -> False

pathAddUnbound_v :: SMTCtxt -> Maybe DLVar -> SrcLoc -> String -> SLType -> BindingOrigin -> SMTComp
pathAddUnbound_v ctxt mdv at_dv v t bo = do
  smtDeclare_v ctxt v t
  modifyIORefRef (ctxt_bindingsrr ctxt) $ M.insert v (mdv, at_dv, bo, Nothing)

pathAddBound_v :: SMTCtxt -> Maybe DLVar -> SrcLoc -> String -> SLType -> BindingOrigin -> SExpr -> SMTComp
pathAddBound_v ctxt mdv at_dv v t bo se = do
  smtDeclare_v ctxt v t
  let smt = ctxt_smt ctxt
  --- Note: We don't use smtAssert because variables are global, so
  --- this variable isn't affected by the path.
  SMT.assert smt (smtEq (Atom $ v) se)
  modifyIORefRef (ctxt_bindingsrr ctxt) $ M.insert v (mdv, at_dv, bo, Just se)

pathAddUnbound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SMTComp
pathAddUnbound ctxt at_dv dv bo = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddUnbound_v ctxt (Just dv) at_dv v t bo

pathAddBound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SExpr -> SMTComp
pathAddBound ctxt at_dv dv bo se = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddBound_v ctxt (Just dv) at_dv v t bo se

smt_c :: SMTCtxt -> SrcLoc -> DLConstant -> SExpr
smt_c _ctxt _at_de dc =
  case dc of
    DLC_Null -> Atom "null"
    DLC_Bool b ->
      case b of
        True -> Atom "true"
        False -> Atom "false"
    DLC_Int i ->
      case use_bitvectors of
        True ->
          List
            [ List [Atom "_", Atom "int2bv", Atom "256"]
            , Atom (show i)
            ]
        False -> Atom $ show i
    DLC_Bytes bs ->
      smtApply "bytes" [Atom (show $ crc32 bs)]

smt_a :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smt_a ctxt at_de da =
  case da of
    DLA_Var dv ->
      case M.lookup dv lvars of
        Nothing ->
          Atom $ smtVar ctxt dv
        Just da' ->
          smt_a ctxt' at_de da'
      where
        lvars = ctxt_loop_var_subst ctxt
        ctxt' =
          ctxt
            { ctxt_loop_var_subst = mempty
            , ctxt_primed_vars = mempty
            }
    DLA_Con c -> smt_c ctxt at_de c
    DLA_Array _ as -> cons as
    DLA_Tuple as -> cons as
    DLA_Obj m -> cons $ M.elems m
    DLA_Interact who i _ -> Atom $ smtInteract ctxt who i
  where
    s = smtTypeSort ctxt t
    t = argTypeOf da
    cons as = smtApply (s ++ "_cons") (map (smt_a ctxt at_de) as)

smt_e :: SMTCtxt -> SrcLoc -> DLVar -> DLExpr -> SMTComp
smt_e ctxt at_dv dv de =
  case de of
    DLE_PrimOp at cp args ->
      pathAddBound ctxt at_dv dv bo se
      where
        args' = map (smt_a ctxt at) args
        se = smtPrimOp ctxt cp args'
    DLE_ArrayRef at f arr_da sz idx_da -> do
      verify1 ctxt at f TBounds check_se
      pathAddBound ctxt at_dv dv bo se
      where
        se = smtApply "select" [arr_da', idx_da']
        check_se = uint256_le idx_da' (smt_c ctxt at $ DLC_Int sz)
        arr_da' = smt_a ctxt at arr_da
        idx_da' = smt_a ctxt at idx_da
    DLE_ArraySet at f arr_da sz idx_da val_da -> do
      verify1 ctxt at f TBounds check_se
      pathAddBound ctxt at_dv dv bo se
      where
        se = smtApply "store" [arr_da', idx_da', val_da']
        check_se = uint256_le idx_da' (smt_c ctxt at $ DLC_Int sz)
        arr_da' = smt_a ctxt at arr_da
        idx_da' = smt_a ctxt at idx_da
        val_da' = smt_a ctxt at val_da
    DLE_TupleRef at arr_da i ->
      pathAddBound ctxt at_dv dv bo se
      where
        se = smtApply (s ++ "_elem" ++ show i) [arr_da']
        s = smtTypeSort ctxt t
        t = argTypeOf arr_da
        arr_da' = smt_a ctxt at arr_da
    DLE_ObjectRef at obj_da f ->
      pathAddBound ctxt at_dv dv bo se
      where
        se = smtApply (s ++ "_" ++ f) [obj_da']
        s = smtTypeSort ctxt t
        t = argTypeOf obj_da
        obj_da' = smt_a ctxt at obj_da
    DLE_Interact {} ->
      pathAddUnbound ctxt at_dv dv bo
    DLE_Digest at args ->
      pathAddBound ctxt at dv bo se
      where
        se = smtApply "digest" [smtDigestCombine ctxt at args]
  where
    bo = O_Expr de

smt_m :: (SMTCtxt -> a -> SMTComp) -> SMTCtxt -> LLCommon a -> SMTComp
smt_m iter ctxt m =
  case m of
    LL_Return {} -> mempty
    LL_Let at dv de k -> smt_e ctxt at dv de <> iter ctxt k
    LL_Var at dv k -> var_m <> iter ctxt k
      where
        var_m =
          pathAddUnbound ctxt at dv O_Var
    LL_Set at dv va k -> set_m <> iter ctxt k
      where
        set_m =
          smtAssert ctxt (smtEq (smt_a ctxt at (DLA_Var dv)) (smt_a ctxt at va))
    LL_Claim at f ct ca k -> this_m <> iter ctxt k
      where
        this_m =
          case ct of
            CT_Possible -> possible_m
            CT_Assert -> check_m TAssert <> assert_m
            CT_Assume -> assert_m
            CT_Require ->
              case ctxt_mode ctxt of
                VM_Honest -> check_m TRequire <> assert_m
                VM_Dishonest {} -> assert_m
        ca' = smt_a ctxt at ca
        possible_m = check_m TPossible
        check_m tk =
          verify1 ctxt at f tk ca'
        assert_m =
          smtAssert ctxt ca'
    LL_LocalIf at ca t f k ->
      smt_l ctxt_t t <> smt_l ctxt_f f <> iter ctxt k
      where
        ctxt_f = ctxt {ctxt_path_constraint = (smtNot ca_se) : pc}
        ctxt_t = ctxt {ctxt_path_constraint = ca_se : pc}
        pc = ctxt_path_constraint ctxt
        ca_se = smt_a ctxt at ca

smt_l :: SMTCtxt -> LLLocal -> SMTComp
smt_l ctxt (LLL_Com m) = smt_m smt_l ctxt m

data BlockMode
  = B_Assume Bool
  | B_Prove

smt_block :: SMTCtxt -> BlockMode -> LLBlock LLLocal -> SMTComp
smt_block ctxt bm b = before_m <> after_m
  where
    LLBlock at f l da = b
    before_m = smt_l ctxt l
    da' = smt_a ctxt at da
    after_m =
      case bm of
        B_Assume True ->
          smtAssert ctxt da'
        B_Assume False ->
          smtAssert ctxt (smtNot da')
        B_Prove ->
          verify1 ctxt at f TInvariant da'

gatherDefinedVars_m :: (LLCommon LLLocal) -> S.Set DLVar
gatherDefinedVars_m m =
  case m of
    LL_Return {} -> mempty
    LL_Let _ dv _ k -> S.singleton dv <> gatherDefinedVars_l k
    LL_Var _ dv k -> S.singleton dv <> gatherDefinedVars_l k
    LL_Set _ _ _ k -> gatherDefinedVars_l k
    LL_Claim _ _ _ _ k -> gatherDefinedVars_l k
    LL_LocalIf _ _ t f k -> gatherDefinedVars_l t <> gatherDefinedVars_l f <> gatherDefinedVars_l k

gatherDefinedVars_l :: LLLocal -> S.Set DLVar
gatherDefinedVars_l (LLL_Com m) = gatherDefinedVars_m m

gatherDefinedVars :: LLBlock LLLocal -> S.Set DLVar
gatherDefinedVars (LLBlock _ _ l _) = gatherDefinedVars_l l

smt_asn :: SMTCtxt -> Bool -> DLAssignment -> SMTComp
smt_asn ctxt vars_are_primed asn = smt_block ctxt' B_Prove inv
  where
    ctxt' =
      ctxt
        { ctxt_loop_var_subst = asnm
        , ctxt_primed_vars = pvars
        }
    DLAssignment asnm = asn
    pvars = case vars_are_primed of
      True -> gatherDefinedVars inv
      False -> mempty
    inv = case ctxt_while_invariant ctxt of
      Just x -> x
      Nothing -> impossible "asn outside loop"

smt_asn_def :: SMTCtxt -> SrcLoc -> DLAssignment -> SMTComp
smt_asn_def ctxt at asn = mapM_ def1 $ M.keys asnm
  where
    DLAssignment asnm = asn
    def1 dv =
      pathAddUnbound ctxt at dv O_Assignment

smt_n :: SMTCtxt -> LLConsensus -> SMTComp
smt_n ctxt n =
  case n of
    LLC_Com m -> smt_m smt_n ctxt m
    LLC_If at ca t f ->
      mapM_ ((ctxtNewScope ctxt) . go) [(True, t), (False, f)]
      where
        ca' = smt_a ctxt at ca
        go (v, k) =
          --- FIXME Can we use path constraints to avoid this forking?
          --- Probably need to do `malloc` on balance variables to
          --- deal with this.
          smtAssert ctxt (smtEq ca' v') <> smt_n ctxt k
          where
            v' = smt_a ctxt at (DLA_Con (DLC_Bool v))
    LLC_Transfer at f to amt k -> transfer_m <> smt_n ctxt' k
      where
        transfer_m = do
          verify1 ctxt at f TBalanceSufficient amt_le_se
          pathAddBound_v ctxt Nothing at (smtBalance cbi') T_UInt256 bo cbi'_se
        bo = O_Transfer to amt
        cbi = ctxt_balance ctxt
        cbi' = cbi + 1
        amt_se = smt_a ctxt at amt
        cbi_se = smtBalanceRef cbi
        cbi'_se = uint256_sub cbi_se amt_se
        amt_le_se = uint256_le amt_se cbi_se
        ctxt' = ctxt {ctxt_balance = cbi'}
    LLC_FromConsensus _ _ s -> smt_s ctxt s
    LLC_While at asn inv cond body k ->
      mapM_ (ctxtNewScope ctxt) [before_m, loop_m, after_m]
      where
        ctxt_inv = ctxt {ctxt_while_invariant = Just inv}
        before_m = smt_asn ctxt_inv False asn
        loop_m =
          smt_asn_def ctxt at asn
            <> smt_block ctxt (B_Assume True) inv
            <> smt_block ctxt (B_Assume True) cond
            <> smt_n ctxt_inv body
        after_m =
          smt_asn_def ctxt at asn
            <> smt_block ctxt (B_Assume True) inv
            <> smt_block ctxt (B_Assume False) cond
            <> smt_n ctxt k
    LLC_Continue _at asn ->
      smt_asn ctxt True asn

smt_fs :: SMTCtxt -> SrcLoc -> FromSpec -> SMTComp
smt_fs ctxt at fs =
  case fs of
    FS_Again _ -> mempty
    FS_Join dv ->
      pathAddUnbound ctxt at dv O_Join

smt_s :: SMTCtxt -> LLStep -> SMTComp
smt_s ctxt s =
  case s of
    LLS_Com m -> smt_m smt_s ctxt m
    LLS_Stop at f ->
      verify1 ctxt at f TBalanceZero se
      where
        se = smtEq (smtBalanceRef $ ctxt_balance ctxt) uint256_zero
    LLS_Only _at who loc k ->
      loc_m <> smt_s ctxt k
      where
        loc_m =
          case shouldSimulate ctxt who of
            True -> smt_l ctxt loc
            False -> mempty
    LLS_ToConsensus at from fs from_as from_msg from_amt mtime next_n ->
      mapM_ (ctxtNewScope ctxt) [timeout, notimeout]
      where
        timeout =
          case mtime of
            Nothing -> mempty
            Just (_, time_s) ->
              smt_s ctxt time_s
        notimeout = fs_m <> from_m <> smt_n ctxt' next_n
        cbi = ctxt_balance ctxt
        tv' = case ctxt_mtxn_value ctxt of
          Just x -> x + 1
          Nothing -> 0
        cbi' = cbi + 1
        ctxt' =
          ctxt
            { ctxt_balance = cbi'
            , ctxt_mtxn_value = Just tv'
            }
        fs_m = smt_fs ctxt at fs
        from_m = do
          case shouldSimulate ctxt from of
            False -> do
              pathAddUnbound_v ctxt Nothing at (smtTxnValue tv') T_UInt256 (O_DishonestPay from)
              mapM_ (\msg_dv -> pathAddUnbound ctxt at msg_dv (O_DishonestMsg from)) from_msg
            True -> do
              pathAddBound_v ctxt Nothing at (smtTxnValue tv') T_UInt256 (O_HonestPay from from_amt) (smt_a ctxt at from_amt)
              zipWithM_ (\msg_dv msg_da -> pathAddBound ctxt at msg_dv (O_HonestMsg from msg_da) (smt_a ctxt at msg_da)) from_msg from_as
          pathAddBound_v ctxt Nothing at (smtBalance cbi') T_UInt256 O_ToConsensus (uint256_add (smtBalanceRef cbi) (smtTxnValueRef tv'))

_smtDefineTypes :: Solver -> S.Set SLType -> IO SMTTypeMap
_smtDefineTypes smt ts = do
  tnr <- newIORef (0 :: Int)
  let none _ = mempty
  tmr <-
    newIORef
      (M.fromList
         [ (T_Null, ("Null", none))
         , (T_Bool, ("Bool", none))
         , (T_UInt256, ("UInt256", uint256_inv smt))
         , (T_Bytes, ("Bytes", none))
         , (T_Address, ("Address", none))
         ])
  let base = impossible "default"
  let bind_type :: SLType -> String -> IO SMTTypeInv
      bind_type t n =
        case t of
          T_Null -> base
          T_Bool -> base
          T_UInt256 -> base
          T_Bytes -> base
          T_Address -> base
          T_Fun {} -> mempty
          T_Forall {} -> impossible "forall in ll"
          T_Var {} -> impossible "var in ll"
          T_Array et sz -> do
            tni <- type_name et
            let tn = fst tni
            let tinv = snd tni
            void $ SMT.command smt $ smtApply "define-sort" [Atom n, List [], smtApply "Array" [uint256_sort, Atom tn]]
            let z = "z_" ++ n
            void $ SMT.declare smt z $ Atom n
            let idxs = [0 .. (sz -1)]
            let idxses = map (smt_c (error "no context") (error "no at") . DLC_Int) idxs
            let cons_vars = map (("e" ++) . show) idxs
            let cons_params = map (\x -> (x, Atom tn)) cons_vars
            let defn1 arrse (idxse, var) = smtApply "store" [arrse, idxse, Atom var]
            let cons_defn = foldl' defn1 (Atom z) $ zip idxses cons_vars
            void $ SMT.defineFun smt (n ++ "_cons") cons_params (Atom n) cons_defn
            void $ SMT.declareFun smt (n ++ "_toBytes") [Atom n] (Atom "Bytes")
            let inv se = do
                  let invarg ise = tinv $ smtApply "select" [se, ise]
                  mapM_ invarg idxses
            return inv
          T_Tuple ats -> do
            ts_nis <- mapM type_name ats
            let mkargn _ (i :: Int) = n ++ "_elem" ++ show i
            let argns = zipWith mkargn ts_nis [0 ..]
            let mkarg (arg_tn, _) argn = (argn, Atom arg_tn)
            let args = zipWith mkarg ts_nis argns
            SMT.declareDatatype smt n [] [(n ++ "_cons", args)]
            void $ SMT.declareFun smt (n ++ "_toBytes") [Atom n] (Atom "Bytes")
            let inv se = do
                  let invarg (_, arg_inv) argn = arg_inv $ smtApply argn [se]
                  zipWithM_ invarg ts_nis argns
            return inv
          T_Obj tm -> do
            let tml = M.toAscList tm
            ts_nis <-
              mapM
                (\(f, at) -> do
                   let argn = (n ++ "_" ++ f)
                   r <- type_name at
                   return $ (argn, r))
                tml
            let mkarg (argn, (at, inv)) = ((argn, Atom at), inv)
            let args = map mkarg ts_nis
            SMT.declareDatatype smt n [] [(n ++ "_cons", map fst args)]
            void $ SMT.declareFun smt (n ++ "_toBytes") [Atom n] (Atom "Bytes")
            let inv se = do
                  let invarg ((argn, _), arg_inv) = arg_inv $ smtApply argn [se]
                  mapM_ invarg args
            return inv
      type_name :: SLType -> IO (String, SMTTypeInv)
      type_name t = do
        tm <- readIORef tmr
        case M.lookup t tm of
          Just x -> return x
          Nothing -> do
            tn <- readIORef tnr
            modifyIORef tnr $ (1 +)
            let n = "T" ++ show tn
            let bad _ = impossible "recursive type"
            modifyIORef tmr $ M.insert t (n, bad)
            inv <- bind_type t n
            let b = (n, inv)
            modifyIORef tmr $ M.insert t b
            return b
  mapM_ type_name ts
  readIORef tmr

_verify_smt :: Solver -> LLProg -> IO ExitCode
_verify_smt smt lp = do
  SMT.loadString smt smtStdLib
  succ_ref <- newIORef 0
  fail_ref <- newIORef 0
  bindingsrr <- newIORefRef mempty
  typem <- _smtDefineTypes smt (cts lp)
  let LLProg at (SLParts pies_m) s = lp
  let ctxt =
        SMTCtxt
          { ctxt_smt = smt
          , ctxt_typem = typem
          , ctxt_res_succ = succ_ref
          , ctxt_res_fail = fail_ref
          , ctxt_modem = Nothing
          , ctxt_path_constraint = []
          , ctxt_bindingsrr = bindingsrr
          , ctxt_balance = 0
          , ctxt_mtxn_value = Nothing
          , ctxt_while_invariant = Nothing
          , ctxt_loop_var_subst = mempty
          , ctxt_primed_vars = mempty
          }
  let defineIE who (v, it) =
        case it of
          T_Fun {} -> mempty
          _ ->
            pathAddUnbound_v ctxt Nothing at (smtInteract ctxt who v) it O_Interact
  let definePIE (who, InteractEnv iem) = mapM_ (defineIE who) $ M.toList iem
  mapM_ definePIE $ M.toList pies_m
  pathAddBound_v ctxt Nothing at (smtBalance 0) T_UInt256 O_Initialize uint256_zero
  let smt_s_top mode = do
        putStrLn $ "Verifying with mode = " ++ show mode
        let ctxt' = ctxt {ctxt_modem = Just mode}
        ctxtNewScope ctxt' $ smt_s ctxt' s
  let ms = VM_Honest : (map VM_Dishonest (RoleContract : (map RolePart $ M.keys pies_m)))
  mapM_ smt_s_top ms
  ss <- readIORef succ_ref
  fs <- readIORef fail_ref
  putStr $ "Checked " ++ (show $ ss + fs) ++ " theorems;"
  (if (fs == 0)
     then do
       putStrLn $ " No failures!"
       return ExitSuccess
     else do
       putStrLn $ " " ++ show fs ++ " failures. :'("
       return $ ExitFailure 1)

newFileLogger :: FilePath -> IO (IO (), Logger)
newFileLogger p = do
  logh <- openFile p WriteMode
  tabr <- newIORef 0
  let logLevel = return 0
      logSetLevel _ = return ()
      logTab = modifyIORef tabr $ \x -> x + 1
      logUntab = modifyIORef tabr $ \x -> x - 1
      printTab = do
        tab <- readIORef tabr
        mapM_ (\_ -> hPutStr logh " ") $ take tab $ repeat ()
      send_tag = "[send->]"
      recv_tag = "[<-recv]"
      logMessage m' = do
        let (which, m) = splitAt (length send_tag) m'
        let short_which = if which == send_tag then "+" else "-"
        if (which == recv_tag && m == " success")
          then return ()
          else
            if (m == " (push 1 )")
              then do
                printTab
                hPutStrLn logh $ "(push"
                hFlush logh
                logTab
              else
                if (m == " (pop 1 )")
                  then do
                    logUntab
                    printTab
                    hPutStrLn logh $ ")"
                    hFlush logh
                  else do
                    printTab
                    hPutStrLn logh $ "(" ++ short_which ++ m ++ ")"
                    hFlush logh
      close = hClose logh
  return (close, Logger {..})

verify_smt :: Maybe FilePath -> LLProg -> String -> [String] -> IO ExitCode
verify_smt logpMay lp prog args = do
  (close, logplMay) <- mkLogger
  smt <- SMT.newSolver prog args logplMay
  unlessM (SMT.produceUnsatCores smt) $ impossible "Prover doesn't support possible?"
  vec <- _verify_smt smt lp
  zec <- SMT.stop smt
  close
  return $ case (zec, vec) of
    (ExitSuccess, ExitSuccess) -> ExitSuccess
    (e@ExitFailure {}, _) -> e
    (_, e@ExitFailure {}) -> e
  where
    mkLogger = case logpMay of
      Just logp -> do
        (close, logpl) <- newFileLogger logp
        return (close, Just logpl)
      Nothing -> return (return (), Nothing)
