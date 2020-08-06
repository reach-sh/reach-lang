module Reach.Verify.SMT where

---import Control.Loop
import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Char8 as BS
---import Data.List
---import Data.Monoid
---import Data.Char (isDigit)
import Data.Digest.CRC32
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
---import qualified Data.Set as S
---import Data.Text (Text)
---import qualified Data.Text as T
---import qualified Data.Text.IO as TIO
---import Data.Text.Prettyprint.Doc
import Reach.NL_Type
import Reach.NL_AST
import Reach.EmbeddedFiles
import Reach.Pretty ()
import Reach.Util
import Reach.Verify.Verifier
import qualified SimpleSMT as SMT
import SimpleSMT (Solver, Logger (Logger), SExpr (..), Result (..))
import System.Exit
import System.IO
---import Text.Read (readMaybe)

--- SMT Helpers

--- FIXME decide on fixed bitvectors
-- | bv == True means "use BitVector 256", False means "use Int"
use_bitvectors :: Bool
use_bitvectors = False

smtStdLib :: String
smtStdLib = BS.unpack $ case use_bitvectors of
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
uint256_le lhs rhs = smtConsensusPrimOp (impossible "raw") PLE [lhs, rhs]

uint256_sub :: SExpr -> SExpr -> SExpr
uint256_sub lhs rhs = smtConsensusPrimOp (impossible "raw") SUB [lhs, rhs]

uint256_add :: SExpr -> SExpr -> SExpr
uint256_add lhs rhs = smtConsensusPrimOp (impossible "raw") ADD [lhs, rhs]

smtApply :: String -> [SExpr] -> SExpr
smtApply f args = List (Atom f : args)

smtEq :: SExpr -> SExpr -> SExpr
smtEq x y = smtApply "=" [x, y]

smtNot :: SExpr -> SExpr
smtNot se = smtApply "not" [ se ]

--- SMT conversion code

type Role = Maybe SLPart

data BindingOrigin
  = O_DishonestMsg SLPart
  | O_DishonestPay SLPart
  | O_HonestMsg SLPart DLArg
  | O_HonestPay SLPart DLArg
  | O_Transfer DLArg DLArg
  | O_ToConsensus
  | O_Var
  | O_Initialize
  | O_Expr DLExpr
  | O_Join
  deriving (Eq, Show)

data SMTCtxt = SMTCtxt
  { ctxt_smt :: Solver
  , ctxt_res_succ :: IORef Int
  , ctxt_res_fail :: IORef Int
  , ctxt_honest :: Bool
  , ctxt_me :: Role
  , ctxt_balance :: Int
  , ctxt_mtxn_value :: Maybe Int
  , ctxt_path_constraint :: [SExpr]
  , ctxt_boundrr :: IORef (IORef (M.Map String (SrcLoc, BindingOrigin, SExpr)))
  , ctxt_unboundrr :: IORef (IORef (M.Map String (SrcLoc, BindingOrigin))) }

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
  paramIORef (ctxt_boundrr ctxt) $
    paramIORef (ctxt_unboundrr ctxt) $
    SMT.inNewScope (ctxt_smt ctxt) $ m

ctxt_txn_value :: SMTCtxt -> Int
ctxt_txn_value ctxt = fromMaybe (impossible "no txn value") $ ctxt_mtxn_value ctxt

shouldSimulate :: SMTCtxt -> SLPart -> Bool
shouldSimulate ctxt p = (ctxt_honest ctxt) || p_is_me
  where p_is_me =
          case ctxt_me ctxt of
            Nothing -> True
            Just me -> me == p
  

smtBalance :: Int -> String
smtBalance i = "ctc_balance" ++ show i
smtBalanceRef :: Int -> SExpr
smtBalanceRef = Atom . smtBalance

smtTxnValue :: Int -> String
smtTxnValue i = "txn_value" ++ show i
smtTxnValueRef :: Int -> SExpr
smtTxnValueRef = Atom . smtTxnValue

smtVar :: SMTCtxt -> DLVar -> String
smtVar _XXX_ctxt _XXX_dv@(DLVar _ _ _ i) = "v" ++ show i

smtDeclare_v :: SMTCtxt -> String -> SLType -> IO ()
smtDeclare_v ctxt v t = do
  let smt = ctxt_smt ctxt
  let simple s = do
        void $ SMT.declare smt v s
  case t of
    T_Bool -> simple $ Atom "Bool"
    T_UInt256 -> do
      simple uint256_sort
      SMT.assert smt (uint256_le uint256_zero $ Atom v)
    T_Bytes -> simple $ Atom "Bytes"
    T_Address -> simple $ Atom "Address"
    T_Array ts ->
      --- XXX This could be really bad with big arrays
      zipWithM_ add_elem [0..] ts
      where add_elem (i::Int) et =
              smtDeclare_v ctxt (v ++ "_elem" ++ show i) et
    T_Null ->
      --- Note: This might cause some problems because we assume v is bound
      mempty
    _ ->
      error $ "XXX smtDeclare_v " ++ show t

smtConsensusPrimOp :: SMTCtxt -> ConsensusPrimOp -> [SExpr] -> SExpr
smtConsensusPrimOp ctxt p =
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
      const (smtTxnValueRef $ fromMaybe (impossible "txn value") $ ctxt_mtxn_value ctxt)
  where
    cant = impossible $ "Int doesn't support " ++ show p
    app n = smtApply n
    bvapp n_bv n_i = app $ if use_bitvectors then n_bv else n_i

smtTypeByteConverter :: SMTCtxt -> SLType -> String
smtTypeByteConverter _ctxt t =
  case t of
    T_Null -> "toBytes_Null"
    T_Bool -> "toBytes_Bool"
    T_UInt256 -> "toBytes_Int"
    T_Bytes -> "toBytes_Bytes"
    _ -> error "XXX"

smtArgByteConverter :: SMTCtxt -> DLArg -> String
smtArgByteConverter ctxt arg =
  smtTypeByteConverter ctxt (argTypeOf arg)
          
smtArgBytes :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smtArgBytes ctxt at arg = smtApply (smtArgByteConverter ctxt arg) [ smt_a ctxt at arg ]

smtDigestCombine :: SMTCtxt -> SrcLoc -> [DLArg] -> SExpr
smtDigestCombine ctxt at args =
  case args of
    [] -> smtApply "bytes0" []
    [x] -> convert1 x
    (x : xs) -> smtApply "msg-cat" [convert1 x, smtDigestCombine ctxt at xs]
  where convert1 = smtArgBytes ctxt at

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

display_fail :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> Maybe ResultDesc -> IO ()
display_fail _XXX_ctxt _XXX_at _XXX_tk _XXX_se _XXX_mm =
  error "XXX"

smtAssert :: SMTCtxt -> SExpr -> SMTComp
smtAssert ctxt se = SMT.assert smt se'
  where smt = ctxt_smt ctxt
        se' =
          case ctxt_path_constraint ctxt of
            [] -> se
            pcs ->
              smtApply "=>" [ (smtApply "and" pcs), se ]

verify1 :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> SMTComp
verify1 ctxt at tk se = SMT.inNewScope smt $ do
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
  where smt = ctxt_smt ctxt
        good =
          modifyIORef (ctxt_res_succ ctxt) $ (1 +)
        bad mgetm = do
          mm <- mgetm
          display_fail ctxt at tk se mm
          modifyIORef (ctxt_res_fail ctxt) $ (1 +)
        isPossible =
          case tk of
            TPossible -> True
            _ -> False

pathAddUnbound_v :: SMTCtxt -> SrcLoc -> String -> SLType -> BindingOrigin -> SMTComp
pathAddUnbound_v ctxt at_dv v t bo = do
  smtDeclare_v ctxt v t
  modifyIORefRef (ctxt_unboundrr ctxt) $ M.insert v (at_dv, bo)

pathAddBound_v :: SMTCtxt -> SrcLoc -> String -> SLType -> BindingOrigin -> SExpr -> SMTComp
pathAddBound_v ctxt at_dv v t bo se = do
  smtDeclare_v ctxt v t
  let smt = ctxt_smt ctxt
  --- Note: We don't use smtAssert because variables are global, so
  --- this variable isn't affected by the path.
  SMT.assert smt (smtEq (Atom $ v) se)
  modifyIORefRef (ctxt_boundrr ctxt) $ M.insert v (at_dv, bo, se)

pathAddUnbound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SMTComp
pathAddUnbound ctxt at_dv dv bo = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddUnbound_v ctxt at_dv v t bo

pathAddBound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SExpr -> SMTComp
pathAddBound ctxt at_dv dv bo se = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddBound_v ctxt at_dv v t bo se

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
        True -> List [ List [Atom "_", Atom "int2bv", Atom "256"]
                     , Atom (show i) ]
        False -> Atom $ show i
    DLC_Bytes bs ->
      smtApply "bytes-literal" [Atom (show $ crc32 bs)]

smt_a :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smt_a ctxt at_de da =
  case da of
    DLA_Var dv -> Atom $ smtVar ctxt dv
    DLA_Con c -> smt_c ctxt at_de c
    DLA_Array {} -> error "XXX"
    DLA_Obj {} -> error "XXX"
    DLA_Interact {} -> error "XXX"

smt_e :: SMTCtxt -> SrcLoc -> DLVar -> DLExpr -> SMTComp
smt_e ctxt at_dv dv de =
  case de of
    DLE_PrimOp at p args ->
      case p of
        RANDOM -> pathAddUnbound ctxt at_dv dv bo
        CP cp -> pathAddBound ctxt at_dv dv bo se
          where args' = map (smt_a ctxt at) args
                se = smtConsensusPrimOp ctxt cp args'
          
    DLE_ArrayRef _at arr_da idx_da ->
      case (arr_da, idx_da) of
        (DLA_Var arr_dv, DLA_Con (DLC_Int i)) ->
          pathAddBound ctxt at_dv dv bo se
          where v = smtVar ctxt arr_dv
                se = Atom $ v ++ "_elem" ++ show i
        _ ->
          error "XXX"
    DLE_Interact {} ->
      pathAddUnbound ctxt at_dv dv bo
    DLE_Digest at args ->
      pathAddBound ctxt at dv bo se
      where se = smtApply "digest" [smtDigestCombine ctxt at args]
  where bo = O_Expr de

smt_m :: (SMTCtxt -> a -> SMTComp) -> SMTCtxt -> LLCommon a -> SMTComp
smt_m iter ctxt m =
  case m of
    LL_Return {} -> mempty
    LL_Let at dv de k -> smt_e ctxt at dv de <> iter ctxt k
    LL_Var at dv k -> var_m <> iter ctxt k
      where var_m =
              pathAddUnbound ctxt at dv O_Var
    LL_Set at dv va k -> set_m <> iter ctxt k
      where set_m =
              smtAssert ctxt (smtEq (smt_a ctxt at (DLA_Var dv)) (smt_a ctxt at va))
    LL_Claim at _XXX_f ct ca k -> this_m <> iter ctxt k
      where this_m =
              case ct of
                CT_Possible -> possible_m
                CT_Assert -> check_m TAssert <> assert_m
                CT_Assume -> assert_m
                CT_Require ->
                  case ctxt_honest ctxt of
                    True -> check_m TRequire <> assert_m
                    False -> assert_m
            ca' = smt_a ctxt at ca
            possible_m = check_m TPossible
            check_m tk =
              verify1 ctxt at tk ca'
            assert_m =
              smtAssert ctxt ca'
    LL_LocalIf at ca t f k ->
      smt_l ctxt_t t <> smt_l ctxt_f f <> iter ctxt k
      where ctxt_f = ctxt { ctxt_path_constraint = (smtNot ca_se) : pc }
            ctxt_t = ctxt { ctxt_path_constraint = ca_se : pc }
            pc = ctxt_path_constraint ctxt
            ca_se = smt_a ctxt at ca

smt_l :: SMTCtxt -> LLLocal -> SMTComp
smt_l ctxt (LLL_Com m) = smt_m smt_l ctxt m

smt_n :: SMTCtxt -> LLConsensus -> SMTComp
smt_n ctxt n =
  case n of
    LLC_Com m -> smt_m smt_n ctxt m
    LLC_If {} -> error "XXX"
    LLC_Transfer at to amt k -> transfer_m <> smt_n ctxt' k
      where transfer_m = do
              verify1 ctxt at TBalanceSufficient amt_le_se
              pathAddBound_v ctxt at (smtBalance cbi') T_UInt256 bo cbi'_se
            bo = O_Transfer to amt
            cbi = ctxt_balance ctxt
            cbi' = cbi + 1
            amt_se = smt_a ctxt at amt
            cbi_se = smtBalanceRef cbi
            cbi'_se = uint256_sub cbi_se amt_se
            amt_le_se = uint256_le amt_se cbi_se
            ctxt' = ctxt { ctxt_balance = cbi' }
    LLC_FromConsensus _ _ s -> smt_s ctxt s
    LLC_While {} -> error "XXX"
    LLC_Continue {} -> error "XXX"

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
    LLS_Stop at _ ->
      verify1 ctxt at TBalanceZero (smtEq (smtBalanceRef $ ctxt_balance ctxt) uint256_zero)
    LLS_Only _at who loc k ->
      loc_m <> smt_s ctxt k
      where loc_m =
              case shouldSimulate ctxt who of
                True -> smt_l ctxt loc
                False -> mempty
    LLS_ToConsensus at from fs from_as from_msg from_amt mtime next_n ->
      mapM_ (ctxtNewScope ctxt) [timeout, notimeout]
      where timeout =
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
            ctxt' = ctxt
                    { ctxt_balance = cbi'
                    , ctxt_mtxn_value = Just tv' }
            fs_m = smt_fs ctxt at fs
            from_m = do
              case shouldSimulate ctxt from of
                False -> do
                  pathAddUnbound_v ctxt at (smtTxnValue tv') T_UInt256 (O_DishonestPay from)
                  mapM_ (\msg_dv -> pathAddUnbound ctxt at msg_dv (O_DishonestMsg from)) from_msg
                True -> do
                  pathAddBound_v ctxt at (smtTxnValue tv') T_UInt256 (O_HonestPay from from_amt) (smt_a ctxt at from_amt)
                  zipWithM_ (\msg_dv msg_da -> pathAddBound ctxt at msg_dv (O_HonestMsg from msg_da) (smt_a ctxt at msg_da)) from_msg from_as
              pathAddBound_v ctxt at (smtBalance cbi') T_UInt256 O_ToConsensus (uint256_add (smtBalanceRef cbi) (smtTxnValueRef tv'))

_verify_smt :: Solver -> LLProg -> IO ExitCode
_verify_smt smt lp = do
  SMT.loadString smt smtStdLib
  succ_ref <- newIORef 0
  fail_ref <- newIORef 0
  bound_ref_ref <- newIORefRef mempty
  unbound_ref_ref <- newIORefRef mempty
  let LLProg at (SLParts pies_m) s = lp
  let ps = Nothing : (map Just $ M.keys pies_m)
  let smt_s_top (honest, me) = do
        putStrLn $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
        let ctxt = SMTCtxt
              { ctxt_smt = smt
              , ctxt_res_succ = succ_ref
              , ctxt_res_fail = fail_ref
              , ctxt_honest = honest
              , ctxt_me = me
              , ctxt_path_constraint = []
              , ctxt_boundrr = bound_ref_ref
              , ctxt_unboundrr = unbound_ref_ref
              , ctxt_balance = 0
              , ctxt_mtxn_value = Nothing }
        ctxtNewScope ctxt $ do
          pathAddBound_v ctxt at (smtBalance 0) T_UInt256 O_Initialize uint256_zero
          smt_s ctxt s
  mapM_ smt_s_top (liftM2 (,) [True, False] ps)
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

verify_smt :: (Maybe Logger -> IO Solver) -> FilePath -> Verifier
verify_smt mkSolver logp lp = do
  (close, logpl) <- newFileLogger logp
  smt <- mkSolver (Just logpl)
  unlessM (SMT.produceUnsatCores smt) $ impossible "Prover doesn't support possible?"
  vec <- _verify_smt smt lp
  zec <- SMT.stop smt
  close
  maybeDie $ return zec
  maybeDie $ return vec
