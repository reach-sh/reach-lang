module Reach.Verify.SMT (verify_smt) where

import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
import Data.Digest.CRC32
import Data.IORef
import Data.List.Extra (foldl', mconcatMap)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.CollectTypes
import Reach.Connector
import Reach.EmbeddedFiles
import Reach.IORefRef
import Reach.Pretty ()
import Reach.Texty
import Reach.Type
import Reach.UnrollLoops
import Reach.Util
import Reach.Verify.SMTParser (parseModel)
import Reach.Verify.Shared
import SimpleSMT (Logger (Logger), Result (..), SExpr (..), Solver)
import qualified SimpleSMT as SMT
import System.Directory
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
uint256_le lhs rhs = smtPrimOp (impossible "raw") PLE [] [lhs, rhs]

uint256_inv :: SMTTypeInv
uint256_inv v = uint256_le uint256_zero v

smtApply :: String -> [SExpr] -> SExpr
smtApply f args = List (Atom f : args)

smtAndAll :: [SExpr] -> SExpr
smtAndAll = \case
  [] -> Atom "true"
  [x] -> x
  xs -> smtApply "and" xs

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
  = -- XXX simplify into honest/dishonst
    O_DishonestJoin SLPart
  | O_DishonestMsg SLPart
  | O_DishonestPay SLPart
  | O_HonestJoin SLPart
  | O_HonestMsg SLPart DLArg
  | O_HonestPay SLPart DLArg
  | O_BuiltIn
  | O_Var
  | O_Interact
  | O_Expr DLExpr
  | O_Join
  | O_Assignment
  | O_SwitchCase SLVar
  deriving (Eq)

instance Show BindingOrigin where
  show bo =
    case bo of
      O_DishonestJoin who -> "a dishonest join from " ++ sp who
      O_DishonestMsg who -> "a dishonest message from " ++ sp who
      O_DishonestPay who -> "a dishonest payment from " ++ sp who
      O_HonestJoin who -> "an honest join from " ++ sp who
      O_HonestMsg who what -> "an honest message from " ++ sp who ++ " of " ++ sp what
      O_HonestPay who amt -> "an honest payment from " ++ sp who ++ " of " ++ sp amt
      O_BuiltIn -> "builtin"
      O_Var -> "function return"
      O_Interact -> "interaction"
      O_Expr e -> "evaluating " ++ sp e
      O_Join -> "participant join"
      O_Assignment -> "loop variable"
      O_SwitchCase vn -> "switch case " <> vn
    where
      sp :: Pretty a => a -> String
      sp = show . pretty

type SMTTypeInv = SExpr -> SExpr

type SMTTypeMap =
  M.Map SLType (String, SMTTypeInv)

data SMTCtxt = SMTCtxt
  { ctxt_smt :: Solver
  , ctxt_smt_con :: SMTCtxt -> SrcLoc -> DLConstant -> SExpr
  , ctxt_typem :: SMTTypeMap
  , ctxt_res_succ :: IORef Int
  , ctxt_res_fail :: IORef Int
  , ctxt_modem :: Maybe VerifyMode
  , ctxt_path_constraint :: [SExpr]
  , ctxt_bindingsrr :: (IORefRef (M.Map String (Maybe DLVar, SrcLoc, BindingOrigin, Maybe SExpr)))
  , ctxt_while_invariant :: Maybe LLBlock
  , ctxt_loop_var_subst :: M.Map DLVar DLArg
  , ctxt_primed_vars :: S.Set DLVar
  , ctxt_displayed :: IORef (S.Set SExpr)
  }

ctxt_mode :: SMTCtxt -> VerifyMode
ctxt_mode ctxt =
  case ctxt_modem ctxt of
    Nothing -> impossible "uninitialized"
    Just x -> x

ctxtNewScope :: SMTCtxt -> SMTComp -> SMTComp
ctxtNewScope ctxt m = do
  paramIORefRef (ctxt_bindingsrr ctxt) $
    SMT.inNewScope (ctxt_smt ctxt) $ m

shouldSimulate :: SMTCtxt -> SLPart -> Bool
shouldSimulate ctxt p =
  case ctxt_mode ctxt of
    VM_Honest -> True
    VM_Dishonest which ->
      case which of
        RoleContract -> False
        RolePart me -> me == p

smtInteract :: SMTCtxt -> SLPart -> String -> String
smtInteract _ctxt who m = "interact_" ++ (bunpack who) ++ "_" ++ m

smtAddress :: SLPart -> String
smtAddress who = "address_" <> bunpack who

smtConstant :: DLConstant -> String
smtConstant = \case
  DLC_UInt_max -> "dlc_UInt_max"

smtVar :: SMTCtxt -> DLVar -> String
smtVar ctxt dv@(DLVar _ _ _ i) = "v" ++ show i ++ mp
  where
    mp =
      case elem dv $ ctxt_primed_vars ctxt of
        True -> "p"
        False -> ""

smtTypeSort :: SMTCtxt -> SLType -> String
smtTypeSort ctxt t =
  case M.lookup t (ctxt_typem ctxt) of
    Just (s, _) -> s
    Nothing -> impossible $ "smtTypeSort " <> show t

smtTypeInv :: SMTCtxt -> SLType -> SExpr -> IO ()
smtTypeInv ctxt t se =
  case M.lookup t (ctxt_typem ctxt) of
    Just (_, i) -> smtAssert ctxt $ i se
    Nothing -> impossible $ "smtTypeInv " <> show t

smtDeclare_v :: SMTCtxt -> String -> SLType -> IO ()
smtDeclare_v ctxt v t = do
  let smt = ctxt_smt ctxt
  let s = smtTypeSort ctxt t
  void $ SMT.declare smt v $ Atom s
  smtTypeInv ctxt t $ Atom v

smtPrimOp :: SMTCtxt -> PrimOp -> [DLArg] -> [SExpr] -> SExpr
smtPrimOp _ctxt p dargs =
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
    DIGEST_EQ -> app "="
    ADDRESS_EQ -> app "="
    SELF_ADDRESS ->
      case dargs of
        [DLA_Literal (DLL_Bytes pn)] -> const $ Atom $ smtAddress pn
        se -> impossible $ "self address " <> show se
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
  = TClaim ClaimType
  | TInvariant
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

display_fail :: SMTCtxt -> SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> Bool -> Maybe ResultDesc -> IO ()
display_fail ctxt tat f tk tse mmsg repeated mrd = do
  cwd <- getCurrentDirectory
  putStrLn $ "Verification failed:"
  putStrLn $ "  in " ++ (show $ ctxt_mode ctxt) ++ " mode"
  putStrLn $ "  of theorem " ++ show tk
  case mmsg of
    Nothing -> mempty
    Just msg -> do
      putStrLn $ "  msg: " <> show msg
  putStrLn $ redactAbsStr cwd $ "  at " ++ show tat
  mapM_ (putStrLn . ("  " ++) . show) f
  putStrLn $ ""
  case repeated of
    True -> do
      --- FIXME have an option to force these to display
      putStrLn $ "  (details omitted on repeat)"
    False -> do
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
      bindingsm <- readIORefRef $ ctxt_bindingsrr ctxt
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
                              ++ (map
                                    (redactAbsStr cwd)
                                    [ ("      (bound at: " ++ show at ++ ")")
                                    , ("      (because: " ++ show bo ++ ")")
                                    ])
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

smtAddPathConstraints :: SMTCtxt -> SExpr -> SExpr
smtAddPathConstraints ctxt se = se'
  where
    se' =
      case ctxt_path_constraint ctxt of
        [] -> se
        pcs ->
          smtApply "=>" [(smtAndAll pcs), se]

smtAssert :: SMTCtxt -> SExpr -> SMTComp
smtAssert ctxt se = SMT.assert smt $ smtAddPathConstraints ctxt se
  where
    smt = ctxt_smt ctxt

verify1 :: SMTCtxt -> SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> SMTComp
verify1 ctxt at mf tk se mmsg = SMT.inNewScope smt $ do
  forM_ (ctxt_path_constraint ctxt) $ SMT.assert smt
  SMT.assert smt $ if isPossible then se else smtNot se
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
      dspd <- readIORef $ ctxt_displayed ctxt
      display_fail ctxt at mf tk se mmsg (elem se dspd) mm
      modifyIORef (ctxt_displayed ctxt) (S.insert se)
      modifyIORef (ctxt_res_fail ctxt) $ (1 +)
    isPossible =
      case tk of
        TClaim CT_Possible -> True
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

pathAddUnbound :: SMTCtxt -> SrcLoc -> Maybe DLVar -> BindingOrigin -> SMTComp
pathAddUnbound _ _ Nothing _ = mempty
pathAddUnbound ctxt at_dv (Just dv) bo = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddUnbound_v ctxt (Just dv) at_dv v t bo

pathAddBound :: SMTCtxt -> SrcLoc -> Maybe DLVar -> BindingOrigin -> SExpr -> SMTComp
pathAddBound _ _ Nothing _ _ = mempty
pathAddBound ctxt at_dv (Just dv) bo se = do
  let DLVar _ _ t _ = dv
  let v = smtVar ctxt dv
  pathAddBound_v ctxt (Just dv) at_dv v t bo se

smt_lt :: SMTCtxt -> SrcLoc -> DLLiteral -> SExpr
smt_lt _ctxt _at_de dc =
  case dc of
    DLL_Null -> Atom "null"
    DLL_Bool b ->
      case b of
        True -> Atom "true"
        False -> Atom "false"
    DLL_Int _ i ->
      case use_bitvectors of
        True ->
          List
            [ List [Atom "_", Atom "int2bv", Atom "256"]
            , Atom (show i)
            ]
        False -> Atom $ show i
    DLL_Bytes bs ->
      smtApply "bytes" [Atom (show $ crc32 bs)]

smt_v :: SMTCtxt -> SrcLoc -> DLVar -> SExpr
smt_v ctxt at_de dv =
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

smt_a :: SMTCtxt -> SrcLoc -> DLArg -> SExpr
smt_a ctxt at_de da =
  case da of
    DLA_Var dv -> smt_v ctxt at_de dv
    DLA_Constant c -> ctxt_smt_con ctxt ctxt at_de c
    DLA_Literal c -> smt_lt ctxt at_de c
    DLA_Interact who i _ -> Atom $ smtInteract ctxt who i

smt_la :: SMTCtxt -> SrcLoc -> DLLargeArg -> SExpr
smt_la ctxt at_de dla =
  case dla of
    DLLA_Array _ as -> cons as
    DLLA_Tuple as -> cons as
    DLLA_Obj m -> cons $ M.elems m
    DLLA_Data _ vn vv -> smtApply (s ++ "_" ++ vn) [smt_a ctxt at_de vv]
  where
    t = largeArgTypeOf dla
    s = smtTypeSort ctxt t
    cons as =
      smtApply (s ++ "_cons") (map (smt_a ctxt at_de) as)

smt_e :: SMTCtxt -> SrcLoc -> Maybe DLVar -> DLExpr -> SMTComp
smt_e ctxt at_dv mdv de =
  case de of
    DLE_Arg at da ->
      pathAddBound ctxt at_dv mdv bo $ smt_a ctxt at da
    DLE_LArg at dla ->
      pathAddBound ctxt at_dv mdv bo $ smt_la ctxt at dla
    DLE_Impossible _ _ ->
      pathAddUnbound ctxt at_dv mdv bo
    DLE_PrimOp at cp args ->
      pathAddBound ctxt at_dv mdv bo se
      where
        args' = map (smt_a ctxt at) args
        se = smtPrimOp ctxt cp args args'
    DLE_ArrayRef at arr_da idx_da -> do
      pathAddBound ctxt at_dv mdv bo se
      where
        se = smtApply "select" [arr_da', idx_da']
        arr_da' = smt_a ctxt at arr_da
        idx_da' = smt_a ctxt at idx_da
    DLE_ArraySet at arr_da idx_da val_da -> do
      pathAddBound ctxt at_dv mdv bo se
      where
        se = smtApply "store" [arr_da', idx_da', val_da']
        arr_da' = smt_a ctxt at arr_da
        idx_da' = smt_a ctxt at idx_da
        val_da' = smt_a ctxt at val_da
    DLE_ArrayConcat {} ->
      --- FIXME: This might be possible to do by generating a function
      impossible "array_concat"
    DLE_ArrayZip {} ->
      --- FIXME: This might be possible to do by using `map`
      impossible "array_zip"
    DLE_TupleRef at arr_da i ->
      pathAddBound ctxt at_dv mdv bo se
      where
        se = smtApply (s ++ "_elem" ++ show i) [arr_da']
        s = smtTypeSort ctxt t
        t = argTypeOf arr_da
        arr_da' = smt_a ctxt at arr_da
    DLE_ObjectRef at obj_da f ->
      pathAddBound ctxt at_dv mdv bo se
      where
        se = smtApply (s ++ "_" ++ f) [obj_da']
        s = smtTypeSort ctxt t
        t = argTypeOf obj_da
        obj_da' = smt_a ctxt at obj_da
    DLE_Interact {} ->
      pathAddUnbound ctxt at_dv mdv bo
    DLE_Digest at args ->
      pathAddBound ctxt at mdv bo se
      where
        se = smtApply "digest" [smtDigestCombine ctxt at args]
    DLE_Claim at f ct ca mmsg -> this_m
      where
        this_m =
          case ct of
            CT_Assert -> check_m <> assert_m
            CT_Assume -> assert_m
            CT_Require ->
              case ctxt_mode ctxt of
                VM_Honest -> check_m <> assert_m
                VM_Dishonest {} -> assert_m
            CT_Possible -> check_m
            CT_Unknowable {} -> mempty
        ca' = smt_a ctxt at ca
        check_m =
          verify1 ctxt at f (TClaim ct) ca' mmsg
        assert_m =
          smtAssert ctxt ca'
    DLE_Transfer {} ->
      mempty
    DLE_Wait {} ->
      mempty
    DLE_PartSet at who a ->
      pathAddBound ctxt at mdv bo (smt_a ctxt at a)
        <> case (mdv, shouldSimulate ctxt who) of
          (Just psv, True) ->
            smtAssert ctxt (smtEq (Atom $ smtVar ctxt psv) (Atom $ smtAddress who))
          _ ->
            mempty
  where
    bo = O_Expr de

data SwitchMode
  = SM_Local
  | SM_Consensus

smtSwitch :: SwitchMode -> SMTCtxt -> SrcLoc -> DLVar -> SwitchCases a -> (SMTCtxt -> a -> SMTComp) -> SMTComp
smtSwitch sm ctxt at ov csm iter =
  mconcatMap cm1 (M.toList csm)
  where
    ova = DLA_Var ov
    ovp = smt_a ctxt at ova
    ovt = argTypeOf ova
    ovtm = case ovt of
      T_Data m -> m
      _ -> impossible "switch"
    pc = ctxt_path_constraint ctxt
    cm1 (vn, (mov', l)) =
      case sm of
        SM_Local ->
          udef_m <> iter ctxt' l
        SM_Consensus ->
          udef_m <> smtAssert ctxt eqc <> iter ctxt l
      where
        ctxt' = ctxt {ctxt_path_constraint = eqc : pc}
        eqc =
          case mov' of
            Just ov' -> smtEq ovp ov'p
              where
                ov'p = smt_la ctxt at (DLLA_Data ovtm vn (DLA_Var ov'))
            Nothing -> Atom "true"
        udef_m = pathAddUnbound ctxt at mov' (O_SwitchCase vn)

smt_m :: (SMTCtxt -> a -> SMTComp) -> SMTCtxt -> LLCommon a -> SMTComp
smt_m iter ctxt m =
  case m of
    LL_Return {} -> mempty
    LL_Let at mdv de k -> smt_e ctxt at mdv de <> iter ctxt k
    LL_Var at dv k -> var_m <> iter ctxt k
      where
        var_m =
          pathAddUnbound ctxt at (Just dv) O_Var
    LL_ArrayMap {} ->
      --- FIXME: It might be possible to do this in Z3 by generating a function
      impossible "array_map"
    LL_ArrayReduce {} ->
      --- NOTE: I don't think this is possible
      impossible "array_reduce"
    LL_Set at dv va k -> set_m <> iter ctxt k
      where
        set_m =
          smtAssert ctxt (smtEq (smt_a ctxt at (DLA_Var dv)) (smt_a ctxt at va))
    LL_LocalIf at ca t f k ->
      smt_l ctxt_t t <> smt_l ctxt_f f <> iter ctxt k
      where
        ctxt_f = ctxt {ctxt_path_constraint = (smtNot ca_se) : pc}
        ctxt_t = ctxt {ctxt_path_constraint = ca_se : pc}
        pc = ctxt_path_constraint ctxt
        ca_se = smt_a ctxt at ca
    LL_LocalSwitch at ov csm k ->
      smtSwitch SM_Local ctxt at ov csm smt_l <> iter ctxt k

smt_l :: SMTCtxt -> LLLocal -> SMTComp
smt_l ctxt (LLL_Com m) = smt_m smt_l ctxt m

data BlockMode
  = B_Assume Bool
  | B_Prove

smt_block :: SMTCtxt -> BlockMode -> LLBlock -> SMTComp
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
          verify1 ctxt at f TInvariant da' Nothing

gatherDefinedVars_m :: (LLCommon LLLocal) -> S.Set DLVar
gatherDefinedVars_m m =
  case m of
    LL_Return {} -> mempty
    LL_Let _ mdv _ k ->
      maybe mempty S.singleton mdv <> gatherDefinedVars_l k
    LL_ArrayMap {} -> impossible "array_map"
    LL_ArrayReduce {} -> impossible "array_reduce"
    LL_Var _ dv k -> S.singleton dv <> gatherDefinedVars_l k
    LL_Set _ _ _ k -> gatherDefinedVars_l k
    LL_LocalIf _ _ t f k -> gatherDefinedVars_l t <> gatherDefinedVars_l f <> gatherDefinedVars_l k
    LL_LocalSwitch _ _ csm k ->
      mconcatMap cm1 (M.toList csm) <> gatherDefinedVars_l k
      where
        cm1 (_, (mov, cs)) = S.fromList (maybeToList mov) <> gatherDefinedVars_l cs

gatherDefinedVars_l :: LLLocal -> S.Set DLVar
gatherDefinedVars_l (LLL_Com m) = gatherDefinedVars_m m

gatherDefinedVars :: LLBlock -> S.Set DLVar
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
      pathAddUnbound ctxt at (Just dv) O_Assignment

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
          smtAssert ctxt (smtEq ca' v') <> smt_n ctxt k
          where
            v' = smt_a ctxt at (DLA_Literal (DLL_Bool v))
    LLC_Switch at ov csm ->
      smtSwitch SM_Consensus ctxt at ov csm smt_n
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

smt_s :: SMTCtxt -> LLStep -> SMTComp
smt_s ctxt s =
  case s of
    LLS_Com m -> smt_m smt_s ctxt m
    LLS_Stop _at -> mempty
    LLS_Only _at who loc k ->
      loc_m <> smt_s ctxt k
      where
        loc_m =
          case shouldSimulate ctxt who of
            True -> smt_l ctxt loc
            False -> mempty
    LLS_ToConsensus at send recv mtime ->
      mapM_ (ctxtNewScope ctxt) $ timeout : map go (M.toList send)
      where
        (whov, msgvs, amtv, next_n) = recv
        timeout = maybe mempty ((smt_s ctxt) . snd) mtime
        after = smt_n ctxt next_n
        go (from, (msgas, amta, _whena)) =
          -- XXX Potentially we need to look at whena to determine if we even
          -- do these bindings in honest mode
          bind_from <> bind_msg <> bind_amt <> after
          where
            bind_from = maybe_pathAdd whov (O_DishonestJoin from) (O_HonestJoin from) (Atom $ smtAddress from)
            bind_amt = maybe_pathAdd amtv (O_DishonestPay from) (O_HonestPay from amta) (smt_a ctxt at amta)
            bind_msg = zipWithM_ (\dv da -> maybe_pathAdd dv (O_DishonestMsg from) (O_HonestMsg from da) (smt_a ctxt at da)) msgvs msgas
            maybe_pathAdd v bo_no bo_yes se =
              case shouldSimulate ctxt from of
                False -> pathAddUnbound ctxt at (Just v) bo_no
                True -> pathAddBound ctxt at (Just v) bo_yes se

_smt_declare_toBytes :: Solver -> String -> IO ()
_smt_declare_toBytes smt n = do
  let an = Atom n
  let ntb = n ++ "_toBytes"
  void $ SMT.declareFun smt ntb [an] (Atom "Bytes")

--- FIXME The injective assertions cause Z3 to go off the
--- rails. Another strategy would be to make a datatype for all the
--- bytes variants. However, this would imply that an encoding of a
--- bytes can never be equal to the encoding of a string, and so
--- on. I think it may be safer to only do injectiveness like this
--- and figure out why it is breaking. However, if we leave it out
--- now, then we are doing a conservative approximation that is
--- sound, because more things are equal than really are.
{- Assert that _toBytes is injective
let x = Atom "x"
let y = Atom "y"
let xb = smtApply ntb [ x ]
let yb = smtApply ntb [ y ]
void $ SMT.assert smt $ smtApply "forall" [ List [ List [ x, an ], List [ y, an ] ]
                                          , smtApply "=>" [ smtNot (smtEq x y)
                                                          , smtNot (smtEq xb yb) ] ]
-}

_smtDefineTypes :: Solver -> S.Set SLType -> IO SMTTypeMap
_smtDefineTypes smt ts = do
  tnr <- newIORef (0 :: Int)
  let none _ = smtAndAll []
  tmr <-
    newIORef
      (M.fromList
         [ (T_Null, ("Null", none))
         , (T_Bool, ("Bool", none))
         , (T_UInt, ("UInt", uint256_inv))
         , (T_Digest, ("Digest", none))
         , (T_Address, ("Address", none))
         ])
  let base = impossible "default"
  let bind_type :: SLType -> String -> IO SMTTypeInv
      bind_type t n =
        case t of
          T_Null -> base
          T_Bool -> base
          T_UInt -> base
          T_Bytes {} -> base
          T_Digest -> base
          T_Address -> base
          T_Fun {} -> return none
          T_Forall {} -> impossible "forall in ll"
          T_Var {} -> impossible "var in ll"
          T_Type {} -> impossible "type in ll"
          T_Array et sz -> do
            tni <- type_name et
            let tn = fst tni
            let tinv = snd tni
            void $ SMT.command smt $ smtApply "define-sort" [Atom n, List [], smtApply "Array" [uint256_sort, Atom tn]]
            let z = "z_" ++ n
            void $ SMT.declare smt z $ Atom n
            let idxs = [0 .. (sz -1)]
            let idxses = map (smt_lt (error "no context") (error "no at") . DLL_Int srcloc_builtin) idxs
            let cons_vars = map (("e" ++) . show) idxs
            let cons_params = map (\x -> (x, Atom tn)) cons_vars
            let defn1 arrse (idxse, var) = smtApply "store" [arrse, idxse, Atom var]
            let cons_defn = foldl' defn1 (Atom z) $ zip idxses cons_vars
            void $ SMT.defineFun smt (n ++ "_cons") cons_params (Atom n) cons_defn
            _smt_declare_toBytes smt n
            let inv se = do
                  let invarg ise = tinv $ smtApply "select" [se, ise]
                  smtAndAll $ map invarg idxses
            return inv
          T_Tuple ats -> do
            ts_nis <- mapM type_name ats
            let mkargn _ (i :: Int) = n ++ "_elem" ++ show i
            let argns = zipWith mkargn ts_nis [0 ..]
            let mkarg (arg_tn, _) argn = (argn, Atom arg_tn)
            let args = zipWith mkarg ts_nis argns
            SMT.declareDatatype smt n [] [(n ++ "_cons", args)]
            _smt_declare_toBytes smt n
            let inv se = do
                  let invarg (_, arg_inv) argn = arg_inv $ smtApply argn [se]
                  smtAndAll $ zipWith invarg ts_nis argns
            return inv
          T_Data tm -> do
            tm_nis <- M.mapKeys ((n ++ "_") ++) <$> mapM type_name tm
            let mkvar (vn', (arg_tn, _)) = (vn', [(vn' <> "_v", Atom arg_tn)])
            let vars = map mkvar $ M.toList tm_nis
            SMT.declareDatatype smt n [] vars
            _smt_declare_toBytes smt n
            let inv_f = n ++ "_inv"
            let x = Atom "x"
            let mkvar_inv (vn', (_, arg_inv)) = List [List [(Atom vn'), x], arg_inv x]
            let vars_inv = map mkvar_inv $ M.toList tm_nis
            let inv_defn = smtApply "match" [x, List vars_inv]
            void $ SMT.defineFun smt inv_f [("x", Atom n)] (Atom "Bool") inv_defn
            let inv se = smtApply inv_f [se]
            return inv
          T_Object tm -> do
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
            _smt_declare_toBytes smt n
            let inv se = do
                  let invarg ((argn, _), arg_inv) = arg_inv $ smtApply argn [se]
                  smtAndAll $ map invarg args
            return inv
      type_name :: SLType -> IO (String, SMTTypeInv)
      type_name t = do
        tm <- readIORef tmr
        case M.lookup t tm of
          Just x -> return x
          Nothing ->
            case t of
              T_Bytes {} -> do
                let b = ("Bytes", none)
                modifyIORef tmr $ M.insert t b
                return b
              _ -> do
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

_verify_smt :: Maybe Connector -> VerifySt -> Solver -> LLProg -> IO ()
_verify_smt mc vst smt lp = do
  let mcs = case mc of
        Nothing -> "generic connector"
        Just c -> conName c <> " connector"
  putStrLn $ "Verifying for " <> T.unpack mcs
  dspdr <- newIORef mempty
  bindingsrr <- newIORefRef mempty
  typem <- _smtDefineTypes smt (cts lp)
  let smt_con ctxt at_de cn =
        case mc of
          Just c -> smt_lt ctxt at_de $ conCons c cn
          Nothing -> Atom $ smtConstant cn
  let LLProg at (LLOpts {..}) (SLParts pies_m) s = lp
  let ctxt =
        SMTCtxt
          { ctxt_smt = smt
          , ctxt_smt_con = smt_con
          , ctxt_typem = typem
          , ctxt_res_succ = vst_res_succ vst
          , ctxt_res_fail = vst_res_fail vst
          , ctxt_modem = Nothing
          , ctxt_path_constraint = []
          , ctxt_bindingsrr = bindingsrr
          , ctxt_while_invariant = Nothing
          , ctxt_loop_var_subst = mempty
          , ctxt_primed_vars = mempty
          , ctxt_displayed = dspdr
          }
  case mc of
    Just _ -> mempty
    Nothing ->
      pathAddUnbound_v ctxt Nothing at (smtConstant DLC_UInt_max) T_UInt O_BuiltIn
  -- FIXME it might make sense to assert that UInt_max is no less than
  -- something reasonable, like 64-bit?
  let defineIE who (v, it) =
        case it of
          T_Fun {} -> mempty
          _ ->
            pathAddUnbound_v ctxt Nothing at (smtInteract ctxt who v) it O_Interact
  let definePIE (who, InteractEnv iem) = do
        pathAddUnbound_v ctxt Nothing at (smtAddress who) T_Address O_BuiltIn
        mapM_ (defineIE who) $ M.toList iem
  mapM_ definePIE $ M.toList pies_m
  let smt_s_top mode = do
        putStrLn $ "  Verifying with mode = " ++ show mode
        let ctxt' = ctxt {ctxt_modem = Just mode}
        ctxtNewScope ctxt' $ smt_s ctxt' s
  let ms = VM_Honest : (map VM_Dishonest (RoleContract : (map RolePart $ M.keys pies_m)))
  mapM_ smt_s_top ms

hPutStrLn' :: Handle -> String -> IO ()
hPutStrLn' h s = do
  hPutStrLn h s
  hFlush h

newFileLogger :: FilePath -> IO (IO (), Logger)
newFileLogger p = do
  logh_xio <- openFile (p <> ".xio.smt") WriteMode
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
        hPutStrLn' logh_xio m'
        let (which, m) = splitAt (length send_tag) m'
        let short_which = if which == send_tag then "+" else "-"
        if (which == recv_tag && m == " success")
          then return ()
          else
            if (m == " (push 1 )")
              then do
                printTab
                hPutStrLn' logh $ "(push"
                logTab
              else
                if (m == " (pop 1 )")
                  then do
                    logUntab
                    printTab
                    hPutStrLn' logh $ ")"
                  else do
                    printTab
                    hPutStrLn' logh $ "(" ++ short_which ++ m ++ ")"
      close = do
        hClose logh
        hClose logh_xio
  return (close, Logger {..})

verify_smt :: Maybe FilePath -> Maybe [Connector] -> VerifySt -> LLProg -> String -> [String] -> IO ExitCode
verify_smt logpMay mvcs vst lp prog args = do
  let ulp = unrollLoops lp
  case logpMay of
    Nothing -> return ()
    Just x -> writeFile (x <> ".ulp") (show $ pretty ulp)
  let mkLogger = case logpMay of
        Just logp -> do
          (close, logpl) <- newFileLogger logp
          return (close, Just logpl)
        Nothing -> return (return (), Nothing)
  (close, logplMay) <- mkLogger
  smt <- SMT.newSolver prog args logplMay
  unlessM (SMT.produceUnsatCores smt) $
    impossible "Prover doesn't support possible?"
  SMT.loadString smt smtStdLib
  let go mc = SMT.inNewScope smt $ _verify_smt mc vst smt ulp
  case mvcs of
    Nothing -> go Nothing
    Just cs -> mapM_ (go . Just) cs
  zec <- SMT.stop smt
  close
  return $ zec
