{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Reach.VerifyZ3 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Extra
import SimpleSMT --- Maybe use Language.SMTLib2 in future
import System.IO
import System.Exit
import Data.Text.Prettyprint.Doc
import Data.Digest.CRC32

import Reach.AST
import Reach.Util

{- Collect Types of IL variables -}

type ILTypeMap = S.Set ILVar

class CollectTypes a where
  cvs :: a -> ILTypeMap

instance CollectTypes a => CollectTypes [a] where
  cvs = foldMap cvs

instance CollectTypes a => CollectTypes (M.Map b a) where
  cvs = foldMap cvs

instance CollectTypes ILVar where
  cvs v = S.singleton v

instance CollectTypes (ILTail a) where
  cvs (IL_Ret _ _) = mempty
  cvs (IL_If _ _ tt ft) = cvs tt <> cvs ft
  cvs (IL_Let _ _ v _ ct) = cvs v <> cvs ct
  cvs (IL_Do _ _ _ ct) = cvs ct
  cvs (IL_ToConsensus _ _ _ _ kt) = cvs kt
  cvs (IL_FromConsensus _ kt) = cvs kt
  cvs (IL_While _ loopv _ it ct bt kt) = cvs loopv <> cvs it <> cvs ct <> cvs bt <> cvs kt
  cvs (IL_Continue _ _) = mempty

instance CollectTypes (ILProgram a) where
  cvs (IL_Prog _ ps it) = cvs ps <> cvs it

{- Z3 Printing -}

z3_sortof :: BaseType -> SExpr
z3_sortof AT_UInt256 = Atom "Int"
z3_sortof AT_Bool = Atom "Bool"
z3_sortof AT_Bytes = Atom "Bytes"

z3Apply :: String -> [SExpr] -> SExpr
z3Apply f args = List (Atom f : args)

z3Eq :: SExpr -> SExpr -> SExpr
z3Eq x y = z3Apply "=" [ x, y ]

z3Var :: ILVar -> String
z3Var (n, _) = "v" ++ show n

z3VarRef :: ILVar -> SExpr
z3VarRef v = Atom $ z3Var v

z3CTCBalance :: Int -> String
z3CTCBalance i = "ctc_balance" ++ show i

z3CTCBalanceRef :: Int -> SExpr
z3CTCBalanceRef i = Atom $ z3CTCBalance i

z3TxnValue :: Int -> String
z3TxnValue i = "txn_value" ++ show i

z3TxnValueRef :: Int -> SExpr
z3TxnValueRef i = Atom $ z3TxnValue i

z3IntSort :: SExpr
z3IntSort = z3_sortof AT_UInt256

{- Model Rendering -}

pretty_se :: SExpr -> Doc a
pretty_se (List l) = group $ parens $ hsep $ map pretty_se l
pretty_se (Atom a) = pretty a

pretty_se_top :: SExpr -> Doc a
pretty_se_top (List l) = group $ parens $ nest 2 $ vsep $ map pretty_se l
pretty_se_top (Atom a) = pretty a

{- Z3 Interaction -}

display_fail :: Show ann => Bool -> Role -> TheoremKind -> ann -> SExpr -> IO ()
display_fail honest r tk ann a = do
  putStrLn $ "Verification failed:"
  putStrLn $ "\tin " ++ (if honest then "honest" else "dishonest") ++ " mode"
  putStrLn $ "\tfor " ++ show r
  putStrLn $ "\tof theorem " ++ show tk
  putStrLn $ "\tfrom " ++ show ann
  putStrLn $ "\tspecifically: " ++ (showsSExpr a ":")

z3_verify1 :: Show ann => Solver -> (Bool, Role, TheoremKind, ann) -> SExpr -> IO VerifyResult
z3_verify1 z3 (honest, who, tk, ann) a = inNewScope z3 $ do
  assert z3 (z3Apply "not" [ a ])
  r <- check z3
  case r of
    Unknown -> error "Z3 inconclusive result"
    Unsat -> return $ VR 1 0
    Sat -> do
      display_fail honest who tk ann a
      m <- command z3 $ List [ Atom "get-model" ]
      putStrLn $ show $ pretty_se_top m
      return $ VR 0 1

z3_sat1 :: Show ann => Solver -> (Bool, Role, TheoremKind, ann) -> SExpr -> IO VerifyResult
z3_sat1 z3 (honest, who, tk, ann) a = inNewScope z3 $ do
  assert z3 a
  r <- check z3
  case r of
    Unknown -> error "Z3 inconclusive result"
    Sat -> return $ VR 1 0
    Unsat -> do
      display_fail honest who tk ann a
      uc <- getUnsatCore z3
      mapM_ putStrLn uc
      return $ VR 0 1

{- Z3 Theory Generation

   The Z3 theory has to prove a few different things.

   1. The balance of CTC at the end of the protocol is 0. It will have
   to do this by employing something like the State monad to represent
   all the various modifications to the CTC value overtime and assert
   that it is 0 at the end. This ensures that the protocol doesn't
   "leave anything on the table".

   2. Verify claims (see ClaimType for details)

   SMT-LIB is documented here:
   http://smtlib.cs.uiowa.edu/standard.shtml

 -}

lookie :: (Show k, Ord k) => String -> k -> M.Map k a -> a
lookie err k m = case M.lookup k m of
  Nothing -> error $ err ++ ": " ++ show k ++ " not in map"
  Just v -> v

z3CPrim :: Int -> C_Prim -> [SExpr] -> SExpr
z3CPrim cbi cp =
  case cp of
    ADD -> app "+"
    SUB -> app "-"
    MUL -> app "*"
    DIV -> app "div"
    MOD -> app "mod"
    PLT -> app "<"
    PLE -> app "<="
    PEQ -> app "="
    PGE -> app ">="
    PGT -> app ">"
    IF_THEN_ELSE -> app "ite"
    UINT256_TO_BYTES -> app "uint256->bytes"
    DIGEST -> app "digest"
    BYTES_EQ -> app "="
    BYTES_LEN -> app "bytes-length"
    BCAT -> app "msg-cat"
    BCAT_LEFT -> app "msg-left"
    BCAT_RIGHT -> app "msg-right"
    BALANCE -> \[] -> z3CTCBalanceRef cbi
    TXN_VALUE -> \[] -> z3TxnValueRef cbi
  where app n = z3Apply n

z3PrimEq :: Solver -> Int -> EP_Prim -> [SExpr] -> ILVar -> IO ()
z3PrimEq z3 cbi pr alt out = case pr of
  CP cp -> assert z3 (z3Eq (z3VarRef out) (z3CPrim cbi cp alt))
  RANDOM -> return ()

data TheoremKind
  = TAssert
  | TRequire
  | TPossible
  | TBalanceZero
  | TInvariant
  deriving (Show)

type Theorem = (Bool, Role, TheoremKind)

data VerifyResult = VR Int Int

instance Semigroup VerifyResult where
  (VR s1 f1) <> (VR s2 f2) = VR (s1 + s2) (f1 + f2)

instance Monoid VerifyResult where
  mempty = VR 0 0

emit_z3_con :: Constant -> SExpr
emit_z3_con (Con_I i) = Atom $ show i
emit_z3_con (Con_B True) = Atom "true"
emit_z3_con (Con_B False) = Atom "false"
emit_z3_con (Con_BS bs) = z3Apply "bytes-literal" [ Atom (show $ crc32 bs) ]

emit_z3_arg :: ILArg a -> SExpr
emit_z3_arg (IL_Con _ c) = emit_z3_con c
emit_z3_arg (IL_Var _ v) = z3VarRef v

z3_vardecl :: Solver -> ILVar -> IO ()
z3_vardecl z3 iv@(_, (_, bt)) = void $ declare z3 (z3Var iv) s
  where s = z3_sortof bt

z3_expr :: Solver -> Int -> ILVar -> ILExpr a -> IO ()
z3_expr z3 cbi out how = case how of
  IL_Declassify _ a ->
    assert z3 (z3Eq (z3VarRef out) (emit_z3_arg a))
  IL_PrimApp _ pr al -> z3PrimEq z3 cbi pr alt out
    where alt = map emit_z3_arg al
  IL_Interact _ _ _ _ -> return ()

z3_stmt :: Show a => Solver -> Bool -> Role -> Int -> ILStmt a -> IO (Int, VerifyResult)
z3_stmt z3 honest r cbi how =
  case how of
    IL_Transfer _ _ amount -> do void $ define z3 cb' z3IntSort (z3Apply "-" [ (z3CTCBalanceRef cbi), amountt ])
                                 return (cbi', mempty)
      where cbi' = cbi + 1
            cb' = z3CTCBalance cbi'
            amountt = emit_z3_arg amount
    IL_Claim h CT_Possible a -> do vr <- z3_sat1 z3 (honest, r, TPossible, h) at
                                   return ( cbi, vr )
      where at = emit_z3_arg a
    IL_Claim h ct a -> do vr <- this_check
                          assert z3 at
                          return ( cbi, vr )
      where at = emit_z3_arg a
            this_check = case mct of
              Just tk -> z3_verify1 z3 (honest, r, tk, h) at
              Nothing -> return mempty
            mct = case ct of
              CT_Assert -> Just TAssert
              CT_Assume -> Nothing
              CT_Require | honest -> Just TRequire
              CT_Require -> Nothing
              CT_Possible -> error "Impossible"

data VerifyCtxt a
  = VC_Top
  | VC_AssignCheckInv ILVar (ILTail a)
  | VC_CheckRet
  | VC_WhileBody_AssumeNotUntil ILVar (ILTail a) (ILTail a)
  | VC_WhileBody_AssumeInv ILVar (ILTail a) (ILTail a)
  | VC_WhileBody_Eval ILVar (ILTail a)
  | VC_WhileTail_AssumeUntil (ILTail a) (VerifyCtxt a, (ILTail a))
  | VC_WhileTail_AssumeInv (VerifyCtxt a, (ILTail a))

z3_it_top :: Show a => Solver -> ILTail a -> (Bool, Role) -> IO VerifyResult
z3_it_top z3 it_top (honest, me) = inNewScope z3 $ do
  putStrLn $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
  void $ define z3 cb0 z3IntSort zero
  meta_iter mempty [(VC_Top, it_top)]
  where zero = emit_z3_con (Con_I 0)
        cb0 = z3CTCBalance 0
        meta_iter :: Show a => VerifyResult -> [(VerifyCtxt a, ILTail a)] -> IO VerifyResult
        meta_iter vr0 [] = return vr0
        meta_iter vr0 ( (ctxt, it) : more0 ) = do
          (more1, vr1) <- inNewScope z3 $ iter 0 ctxt it
          let vr = vr0 <> vr1
          let more = more0 ++ more1
          meta_iter vr more
        iter :: Show a => Int -> VerifyCtxt a -> ILTail a -> IO ([(VerifyCtxt a, ILTail a)], VerifyResult)
        iter cbi ctxt it = case it of
          IL_Ret h al ->
            case ctxt of
              VC_Top -> do
                let cbi_balance = z3Eq (z3CTCBalanceRef cbi) zero
                vr <- z3_verify1 z3 (honest, me, TBalanceZero, h) cbi_balance
                return ([], vr)
              VC_AssignCheckInv loopv invt -> do
                let [ a ] = al
                assert z3 (z3Eq (z3VarRef loopv) (emit_z3_arg a))
                iter cbi VC_CheckRet invt
              VC_CheckRet -> do
                let [ a ] = al
                vr <- z3_verify1 z3 (honest, me, TInvariant, h) (emit_z3_arg a)
                return ([], vr)
              VC_WhileBody_AssumeNotUntil loopv invt bodyt -> do
                let [ a ] = al
                assert z3 (z3Apply "not" [ emit_z3_arg a ])
                iter cbi (VC_WhileBody_AssumeInv loopv invt bodyt) invt
              VC_WhileBody_AssumeInv loopv invt bodyt -> do
                let [ a ] = al
                assert z3 (emit_z3_arg a)
                iter cbi (VC_WhileBody_Eval loopv invt) bodyt
              VC_WhileBody_Eval _ _ ->
                error $ "VerifyZ3 While must terminate in continue"
              VC_WhileTail_AssumeUntil invt ki -> do
                let [ a ] = al
                assert z3 (emit_z3_arg a)
                iter cbi (VC_WhileTail_AssumeInv ki) invt
              VC_WhileTail_AssumeInv (kctxt, kt) -> do
                let [ a ] = al
                assert z3 (emit_z3_arg a)
                iter cbi kctxt kt
          IL_If _ ca tt ft -> do
            mconcatMapM (inNewScope z3 . f) (zip [True, False] [tt, ft])
            where ca' = emit_z3_arg ca
                  f (v, kt) = do assert z3 (z3Eq ca' cav)
                                 iter cbi ctxt kt
                    where cav = emit_z3_con (Con_B v)
          IL_Let _ who what how kt ->
            do when (honest || role_me me who) $ z3_expr z3 cbi what how
               iter cbi ctxt kt
          IL_Do _ who how kt ->
            if (honest || role_me me who) then
              do (cbi', vr) <- z3_stmt z3 honest me cbi how
                 (mt, vr') <- iter cbi' ctxt kt
                 return (mt, vr <> vr')
            else
              iter cbi ctxt kt
          IL_ToConsensus _ _who _msg amount kt ->
            do void $ declare z3 pvv z3IntSort
               void $ define z3 cb'v z3IntSort (z3Apply "+" [cbr, pvr])
               assert z3 thisc
               iter cbi' ctxt kt
            where cbi' = cbi + 1
                  cb'v = z3CTCBalance cbi'
                  cbr = z3CTCBalanceRef cbi
                  amountt = emit_z3_arg amount
                  pvv = z3TxnValue cbi'
                  pvr = z3TxnValueRef cbi'
                  thisc = if honest then
                            z3Eq pvr amountt
                          else
                            z3Apply "<=" [ zero, pvr ]
          IL_FromConsensus _ kt -> iter cbi ctxt kt
          IL_While x loopv inita untilt invt bodyt kt -> do
            (mt, vr) <- iter cbi (VC_AssignCheckInv loopv invt) (IL_Ret x [inita])
            let bodyj = (VC_WhileBody_AssumeNotUntil loopv invt bodyt, untilt)
            let tailj = (VC_WhileTail_AssumeUntil invt (ctxt, kt), untilt)
            let mt' = mt ++ [ bodyj, tailj ]
            return (mt ++ mt', vr)
          IL_Continue x newa ->
            case ctxt of
              VC_WhileBody_Eval loopv invt ->
                iter cbi (VC_AssignCheckInv loopv invt) (IL_Ret x [newa])
              _ ->
                error $ "VerifyZ3 IL_Continue must only occur inside While"

z3StdLib :: String
z3StdLib = "../../z3/z3-runtime.smt2"

_verify_z3 :: Show a => Solver -> ILProgram a -> IO ExitCode
_verify_z3 z3 tp = do
  loadFile z3 z3StdLib
  mapM_ (z3_vardecl z3) $ S.toList $ cvs tp
  VR ss fs <- mconcatMapM (z3_it_top z3 it) (liftM2 (,) [True, False] ps)
  putStr $ "Checked " ++ (show $ ss + fs) ++ " theorems;"
  (if ( fs == 0 ) then
      do putStrLn $ " No failures!"
         return ExitSuccess
   else
      do putStrLn $ " " ++ show fs ++ " failures. :'("
         return $ ExitFailure 1)
  where IL_Prog _ ipi it = tp
        ps = RoleContract : (map RolePart $ M.keys ipi)

newFileLogger :: String -> IO (IO (), Logger)
newFileLogger p = do
  logh <- openFile p WriteMode
  let logLevel = return 0
      logSetLevel _ = return ()
      logTab = return ()
      logUntab = return ()
      logMessage m = do
        hPutStrLn logh m
        hFlush logh
      close = hClose logh
  return (close, Logger { .. })

verify_z3 :: Show a => String -> ILProgram a -> IO ()
verify_z3 logp tp = do
  (close, logpl) <- newFileLogger logp
  z3 <- newSolver "z3" ["-smt2", "-in"] (Just logpl)
  unlessM (produceUnsatCores z3) $ error "Prover doesn't support possible?"
  vec <- _verify_z3 z3 tp
  zec <- stop z3
  close
  maybeDie $ return zec
  maybeDie $ return vec
