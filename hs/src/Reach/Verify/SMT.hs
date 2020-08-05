module Reach.Verify.SMT where

---import Control.Loop
import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Char8 as BS
---import Data.List
---import Data.Monoid
---import Data.Char (isDigit)
---import Data.Digest.CRC32
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
---import qualified Data.Set as S
---import Data.Text (Text)
---import qualified Data.Text as T
---import qualified Data.Text.IO as TIO
---import Data.Text.Prettyprint.Doc
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

--- SMT conversion code

type Role = Maybe SLPart

data BindingOrigin
  = O_DishonestMsg SLPart
  | O_HonestMsg SLPart DLArg
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
  , ctxt_boundrr :: IORef (IORef (M.Map DLVar (SrcLoc, BindingOrigin, SExpr)))
  , ctxt_unboundrr :: IORef (IORef (M.Map DLVar (SrcLoc, BindingOrigin))) }

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
smtBalanceRef :: SMTCtxt -> SExpr
smtBalanceRef ctxt = Atom $ smtBalance $ ctxt_balance ctxt

smtTxnValue :: Int -> String
smtTxnValue i = "txn_value" ++ show i
smtTxnValueRef :: SMTCtxt -> SExpr
smtTxnValueRef ctxt = Atom $ smtTxnValue $ ctxt_txn_value ctxt

smtVar :: SMTCtxt -> DLVar -> String
smtVar _XXX_ctxt _XXX_dv@(DLVar _ _ _ i) = "v" ++ show i

smtDeclare_ :: SMTCtxt -> String -> SLType -> IO ()
smtDeclare_ ctxt v t = do
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
              smtDeclare_ ctxt (v ++ "_elem" ++ show i) et
    _ ->
      error $ "XXX smtDeclare"

smtDeclare :: SMTCtxt -> DLVar -> IO ()
smtDeclare ctxt dv = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  smtDeclare_ ctxt v t

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
    BALANCE -> const (smtBalanceRef ctxt)
    TXN_VALUE -> const (smtTxnValueRef ctxt)
  where
    cant = impossible $ "Int doesn't support " ++ show p
    app n = smtApply n
    bvapp n_bv n_i = app $ if use_bitvectors then n_bv else n_i

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

type SMTComp = IO ()

display_fail :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> Maybe SExpr -> IO ()
display_fail _XXX_ctxt _XXX_at _XXX_tk _XXX_se _XXX_mm =
  error "XXX"

verify1 :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> SMTComp
verify1 ctxt at tk se = SMT.inNewScope smt $ do
  SMT.assert smt (smtApply "not" [ se ])
  r <- SMT.check smt
  case r of
    Unknown -> bad Nothing
    Unsat -> good
    Sat -> bad $ Just $ SMT.command smt $ List [Atom "get-model"]
  where smt = ctxt_smt ctxt
        good =
          modifyIORef (ctxt_res_succ ctxt) $ (1 +)
        bad mgetm = do
          mm <- case mgetm of
                  Nothing -> return $ Nothing
                  Just getm -> liftM Just getm
          display_fail ctxt at tk se mm
          modifyIORef (ctxt_res_fail ctxt) $ (1 +)

pathAddUnbound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SMTComp
pathAddUnbound ctxt at_dv dv bo = do
  smtDeclare ctxt dv
  modifyIORefRef (ctxt_unboundrr ctxt) $ M.insert dv (at_dv, bo)

pathAddBound :: SMTCtxt -> SrcLoc -> DLVar -> BindingOrigin -> SExpr -> SMTComp
pathAddBound ctxt at_dv dv bo se = do
  smtDeclare ctxt dv
  let smt = ctxt_smt ctxt
  SMT.assert smt (smtEq (Atom $ smtVar ctxt dv) se)
  modifyIORefRef (ctxt_boundrr ctxt) $ M.insert dv (at_dv, bo, se)

smt_a :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smt_a ctxt _at_de da =
  case da of
    DLA_Var dv -> Atom $ smtVar ctxt dv
    _ ->
      error "XXX"

smt_e :: SMTCtxt -> SrcLoc -> DLVar -> DLExpr -> SMTComp
smt_e ctxt at_dv dv de =
  case de of
    DLE_PrimOp at p args ->
      case p of
        RANDOM -> pathAddUnbound ctxt at_dv dv bo
        CP cp -> do
          let args' = map (smt_a ctxt at) args
          let se = smtConsensusPrimOp ctxt cp args'
          pathAddBound ctxt at_dv dv bo se
    DLE_ArrayRef _at arr_da idx_da ->
      case (arr_da, idx_da) of
        (DLA_Var arr_dv, DLA_Con (DLC_Int i)) -> do
          let v = smtVar ctxt arr_dv
          let se = Atom $ v ++ "_elem" ++ show i
          pathAddBound ctxt at_dv dv bo se
        _ ->
          error "XXX"
    DLE_Interact {} ->
      pathAddUnbound ctxt at_dv dv bo
    DLE_Digest {} ->
      error "XXX"
  where bo = O_Expr de

smt_m :: (SMTCtxt -> a -> SMTComp) -> SMTCtxt -> LLCommon a -> SMTComp
smt_m iter ctxt m =
  case m of
    LL_Return {} -> mempty
    LL_Let at dv de k -> smt_e ctxt at dv de <> iter ctxt k
    LL_Var {} -> error "XXX"
    LL_Set {} -> error "XXX"
    LL_Claim {} -> error "XXX"
    LL_LocalIf {} -> error "XXX"

smt_l :: SMTCtxt -> LLLocal -> SMTComp
smt_l ctxt (LLL_Com m) = smt_m smt_l ctxt m

smt_n :: SMTCtxt -> LLConsensus -> SMTComp
smt_n ctxt n =
  case n of
    LLC_Com m -> smt_m smt_n ctxt m
    _ -> error "XXX"

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
      verify1 ctxt at TBalanceZero (smtEq (smtBalanceRef ctxt) uint256_zero)
    LLS_Only _at who loc k ->
      loc_m <> smt_s ctxt k
      where loc_m =
              case shouldSimulate ctxt who of
                True -> smt_l ctxt loc
                False -> mempty
    LLS_ToConsensus at from fs from_as from_msg _XXX_from_amt mtime next_n ->
      mapM_ (ctxtNewScope ctxt) [timeout, notimeout]
      where timeout =
              case mtime of
                Nothing -> mempty
                Just (_, time_s) ->
                  smt_s ctxt time_s
            notimeout = fs_m <> msg_m <> smt_n ctxt next_n
            --- XXX update balance
            fs_m = smt_fs ctxt at fs
            msg_m =
              case shouldSimulate ctxt from of
                False ->
                  mapM_ (\msg_dv -> pathAddUnbound ctxt at msg_dv (O_DishonestMsg from)) from_msg
                True ->
                  zipWithM_ (\msg_dv msg_da -> pathAddBound ctxt at msg_dv (O_HonestMsg from msg_da) (smt_a ctxt at msg_da)) from_msg from_as

_verify_smt :: Solver -> LLProg -> IO ExitCode
_verify_smt smt lp = do
  SMT.loadString smt smtStdLib
  succ_ref <- newIORef 0
  fail_ref <- newIORef 0
  bound_ref_ref <- newIORefRef mempty
  unbound_ref_ref <- newIORefRef mempty
  let LLProg _ (SLParts pies_m) s = lp
  let ps = Nothing : (map Just $ M.keys pies_m)
  let smt_s_top (honest, me) = do
        putStrLn $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
        let ctxt = SMTCtxt
              { ctxt_smt = smt
              , ctxt_res_succ = succ_ref
              , ctxt_res_fail = fail_ref
              , ctxt_honest = honest
              , ctxt_me = me
              , ctxt_boundrr = bound_ref_ref
              , ctxt_unboundrr = unbound_ref_ref
              , ctxt_balance = 0
              , ctxt_mtxn_value = Nothing }
        ctxtNewScope ctxt $ smt_s ctxt s
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
