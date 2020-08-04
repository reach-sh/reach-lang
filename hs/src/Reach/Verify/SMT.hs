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
import SimpleSMT hiding (not, const) --- Maybe use Language.SMTLib2 in future
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

data SMTPath = SMTPath
  { path_bound :: M.Map DLVar (SrcLoc, DLExpr, SExpr)
  , path_unbound :: M.Map DLVar (SrcLoc, DLExpr) }

default_path :: SMTPath
default_path = SMTPath
  { path_bound = mempty
  , path_unbound = mempty }

data SMTCtxt = SMTCtxt
  { sc_smt :: Solver
  , sc_honest :: Bool
  , sc_me :: Role
  , sc_balance :: Int
  , sc_mtxn_value :: Maybe Int
  , sc_path :: SMTPath }

sc_txn_value :: SMTCtxt -> Int
sc_txn_value ctxt = fromMaybe (impossible "no txn value") $ sc_mtxn_value ctxt

shouldSimulate :: SMTCtxt -> SLPart -> Bool
shouldSimulate ctxt p = (sc_honest ctxt) || p_is_me
  where p_is_me =
          case sc_me ctxt of
            Nothing -> True
            Just me -> me == p
  

smtBalance :: Int -> String
smtBalance i = "ctc_balance" ++ show i
smtBalanceRef :: SMTCtxt -> SExpr
smtBalanceRef ctxt = Atom $ smtBalance $ sc_balance ctxt

smtTxnValue :: Int -> String
smtTxnValue i = "txn_value" ++ show i
smtTxnValueRef :: SMTCtxt -> SExpr
smtTxnValueRef ctxt = Atom $ smtTxnValue $ sc_txn_value ctxt

smtVar :: SMTCtxt -> DLVar -> String
smtVar _XXX_ctxt _XXX_dv@(DLVar _ _ _ i) = "v" ++ show i

smtDeclare_ :: SMTCtxt -> String -> SLType -> IO ()
smtDeclare_ ctxt v t = do
  let smt = sc_smt ctxt
  let simple s = do
        void $ declare smt v s
  case t of
    T_Bool -> simple $ Atom "Bool"
    T_UInt256 -> do
      simple uint256_sort
      assert smt (uint256_le uint256_zero $ Atom v)
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

data VerifyResult
  = -- | # succeeded, # failed
    VR Int Int

instance Semigroup VerifyResult where
  (VR s1 f1) <> (VR s2 f2) = VR (s1 + s2) (f1 + f2)

instance Monoid VerifyResult where
  mempty = VR 0 0

data TheoremKind
  = TAssert
  | TRequire
  | TPossible
  | TBalanceZero
  | TBalanceSufficient
  | TInvariant
  | TBounds
  deriving (Show)

data SMTRes = SMTRes VerifyResult SMTPath

instance Semigroup SMTRes where
  (SMTRes vr0 _) <> (SMTRes vr1 y) = SMTRes (vr0 <> vr1) y

instance Monoid SMTRes where
  mempty = SMTRes mempty (impossible "no mempty of SMTPath")

type SMTComp = IO SMTRes

keepVRs :: VerifyResult -> SMTComp -> SMTComp
keepVRs vr0 m = do
  SMTRes vr1 path' <- m
  return $ SMTRes (vr0 <> vr1) path'

display_fail :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> Maybe SExpr -> IO ()
display_fail _XXX_ctxt _XXX_at _XXX_tk _XXX_se _XXX_mm =
  error "XXX"

verify1 :: SMTCtxt -> SrcLoc -> TheoremKind -> SExpr -> SMTComp
verify1 ctxt at tk se = inNewScope smt $ do
  assert smt (smtApply "not" [ se ])
  r <- check smt
  case r of
    Unknown -> bad Nothing
    Unsat -> good
    Sat -> bad $ Just $ command smt $ List [Atom "get-model"]
  where smt = sc_smt ctxt
        good = return $ SMTRes (VR 1 0) $ sc_path ctxt
        bad mgetm = do
          mm <- case mgetm of
                  Nothing -> return $ Nothing
                  Just getm -> liftM Just getm
          display_fail ctxt at tk se mm
          return $ SMTRes (VR 0 1) $ sc_path ctxt

pathAddUnbound :: SMTCtxt -> SrcLoc -> DLVar -> DLExpr -> SMTComp
pathAddUnbound ctxt at_dv dv de = do
  smtDeclare ctxt dv
  let path = sc_path ctxt
  let path' = (path { path_unbound = M.insert dv (at_dv, de) $ path_unbound path })
  return $ SMTRes mempty path'

pathAddBound :: SMTCtxt -> SrcLoc -> DLVar -> DLExpr -> SExpr -> SMTComp
pathAddBound ctxt at_dv dv de se = do
  smtDeclare ctxt dv
  let smt = sc_smt ctxt
  assert smt (smtEq (Atom $ smtVar ctxt dv) se)
  let path = sc_path ctxt
  let path' = (path { path_bound = M.insert dv (at_dv, de, se) $ path_bound path })
  return $ SMTRes mempty path'

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
        RANDOM -> pathAddUnbound ctxt at_dv dv de
        CP cp -> do
          let args' = map (smt_a ctxt at) args
          let se = smtConsensusPrimOp ctxt cp args'
          pathAddBound ctxt at_dv dv de se
    DLE_ArrayRef _at arr_da idx_da ->
      case (arr_da, idx_da) of
        (DLA_Var arr_dv, DLA_Con (DLC_Int i)) -> do
          let v = smtVar ctxt arr_dv
          let se = Atom $ v ++ "_elem" ++ show i
          pathAddBound ctxt at_dv dv de se
        _ ->
          error "XXX"
    DLE_Interact {} ->
      pathAddUnbound ctxt at_dv dv de
    DLE_Digest {} ->
      error "XXX"

smt_m :: (SMTCtxt -> a -> SMTComp) -> SMTCtxt -> LLCommon a -> SMTComp
smt_m iter ctxt m =
  case m of
    LL_Return {} ->
      return $ SMTRes mempty (sc_path ctxt)
    LL_Let at dv de k -> do
      SMTRes vr0 path' <- smt_e ctxt at dv de
      let ctxt' = ctxt { sc_path = path' }
      keepVRs vr0 $ iter ctxt' k
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

smt_s :: SMTCtxt -> LLStep -> SMTComp
smt_s ctxt s =
  case s of
    LLS_Com m -> smt_m smt_s ctxt m
    LLS_Stop at _ ->
      verify1 ctxt at TBalanceZero (smtEq (smtBalanceRef ctxt) uint256_zero)
    LLS_Only _at who loc k -> do
      SMTRes vr path' <-
        case shouldSimulate ctxt who of
          True -> smt_l ctxt loc
          False -> return $ SMTRes mempty $ sc_path ctxt
      let ctxt' = ctxt { sc_path = path' }
      keepVRs vr $ smt_s ctxt' k
    LLS_ToConsensus _XXX_at _XXX_from _XXX_fs _XXX_from_as _XXX_from_msg _XXX_from_amt mtime next_n -> do
      let timeout =
            case mtime of
              Nothing -> mempty
              Just (_, time_s) ->
                smt_s ctxt time_s
      let notimeout = do
            --- XXX define fs
            --- XXX connect as to msg
            --- XXX update balance
            smt_n ctxt next_n
      mconcatMapM (inNewScope $ sc_smt ctxt) [timeout, notimeout]

smt_s_top :: Solver -> LLStep -> (Bool, Role) -> SMTComp
smt_s_top smt s (honest, me) = do
  putStrLn $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
  let ctxt = SMTCtxt
        { sc_smt = smt
        , sc_honest = honest
        , sc_me = me
        , sc_path = default_path
        , sc_balance = 0
        , sc_mtxn_value = Nothing }
  inNewScope smt $ smt_s ctxt s

_verify_smt :: Solver -> LLProg -> IO ExitCode
_verify_smt smt lp = do
  loadString smt smtStdLib
  let LLProg _ (SLParts pies_m) s = lp
  let ps = Nothing : (map Just $ M.keys pies_m)
  SMTRes (VR ss fs) _ <- mconcatMapM (smt_s_top smt s) (liftM2 (,) [True, False] ps)
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
  unlessM (produceUnsatCores smt) $ impossible "Prover doesn't support possible?"
  vec <- _verify_smt smt lp
  zec <- stop smt
  close
  maybeDie $ return zec
  maybeDie $ return vec
