{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.Verify.SMT (verify_smt) where

import Codec.Compression.GZip
import qualified Control.Exception as Exn
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Digest.CRC32
import Data.Foldable
import Data.Functor
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AddCounts (add_counts)
import Reach.CollectTypes
import Reach.Connector
import Reach.Counter
import Reach.EmbeddedFiles
import Reach.Freshen
import Reach.Pretty
import Reach.Texty
import Reach.UnrollLoops
import Reach.UnsafeUtil (unsafeTermSupportsColor, unsafeIsErrorFormatJson)
import Reach.Util
import Reach.Verify.SMTAst
import Reach.Verify.SMTParser
import Reach.Verify.Shared
import SimpleSMT (Logger (Logger), Result (..), SExpr (..), Solver (..))
import qualified SimpleSMT as SMT
import qualified System.Console.Pretty as TC
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Text.Read (readMaybe)
import GHC.Generics (Generic)

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
uint256_le lhs rhs = smtApply ple [lhs, rhs]
  where
    ple = if use_bitvectors then "bvule" else "<="

uint256_inv :: SMTTypeInv
uint256_inv v = uint256_le uint256_zero v

uintWord_inv :: SMTTypeInv
uintWord_inv = uint256_inv

smtApply :: String -> [SExpr] -> SExpr
smtApply f args = List (Atom f : args)

smtAndAll :: [SExpr] -> SExpr
smtAndAll = \case
  [] -> Atom "true"
  [x] -> x
  xs -> smtApply "and" xs

smtOrAll :: [SExpr] -> SExpr
smtOrAll = \case
  [] -> Atom "false"
  [x] -> x
  xs -> smtApply "or" xs

smtEq :: SExpr -> SExpr -> SExpr
smtEq x y = smtApply "=" [x, y]

smtGe :: SExpr -> SExpr -> SExpr
smtGe x y = smtApply ">=" [x, y]

smtNot :: SExpr -> SExpr
smtNot se = smtApply "not" [se]

smtImplies :: SExpr -> SExpr -> SExpr
smtImplies x y = smtApply "=>" [x, y]

smtAnd :: SExpr -> SExpr -> SExpr
smtAnd x y = smtApply "and" [x, y]

--- SMT conversion code

data Role
  = RoleContract
  | RolePart SLPart
  deriving (Eq)

data VerifyMode
  = VM_Honest
  | VM_Dishonest Role
  deriving (Eq)

instance Pretty VerifyMode where
  pretty = \case
    VM_Honest -> "ALL participants are honest"
    VM_Dishonest RoleContract -> "NO participants are honest"
    VM_Dishonest (RolePart p) -> "ONLY " <> pretty p <> " is honest"

type SMTTypeInv = SExpr -> SExpr

type SMTTypeMap =
  M.Map DLType (String, SMTTypeInv)

type App = ReaderT SMTCtxt IO

instance Semigroup a => Semigroup (App a) where
  x <> y = liftM2 (<>) x y

instance Monoid a => Monoid (App a) where
  mempty = return mempty

data SMTMapInfo = SMTMapInfo
  { sm_c :: Counter
  , sm_t :: DLType
  , sm_kt :: DLType
  , sm_rs :: IORef [SMTMapRecordReduce]
  , sm_us :: IORef SMTMapRecordUpdate
  }

data SMTMapRecordReduce = SMR_Reduce
  { smr_mri :: Int
  , smr_ans :: DLVar
  , smr_z :: DLArg
  , smr_b :: DLVar
  , smr_a :: DLVar
  , smr_f :: DLBlock
  }

instance Pretty SMTMapRecordReduce where
  pretty = \case
    SMR_Reduce _mri ans z b a f ->
      prettyReduce ans ("map" :: String) z b a () f

data SMTMapRecordUpdate
  = SMR_Update SExpr SExpr SExpr SMTMapRecordUpdate
  | SMR_New
  | SMR_Fresh

instance Pretty SMTMapRecordUpdate where
  pretty = \case
    SMR_Update ma fa' na' _xxx ->
      viaShow ma <> "[" <> viaShow fa' <> "]" <+> "=" <+> viaShow na'
    SMR_New -> "new"
    SMR_Fresh -> "fresh"

data SMTCtxt = SMTCtxt
  { ctxt_smt :: Solver
  , ctxt_untrustworthyMaps :: Bool
  , ctxt_idx :: Counter
  , ctxt_smt_con :: SrcLoc -> DLConstant -> SExpr
  , ctxt_typem :: SMTTypeMap
  , ctxt_smt_typem :: M.Map String DLType
  , ctxt_vst :: VerifySt
  , ctxt_modem :: Maybe VerifyMode
  , ctxt_path_constraint :: [SExpr]
  , ctxt_while_invariants :: [DLInvariant DLBlock]
  , ctxt_displayed :: IORef (S.Set SExpr)
  , ctxt_maps :: M.Map DLMVar SMTMapInfo
  , ctxt_addrs :: M.Map SLPart DLVar
  , ctxt_v_to_dv :: IORef (M.Map String DLVar)
  , ctxt_inv_mode :: BlockMode
  , ctxt_pay_amt :: Maybe (SExpr, SExpr)
  , ctxt_smt_trace :: IORef [SMTLet]
  , ctxt_map_vars :: IORef (S.Set String)
  }

instance HasCounter SMTCtxt where
  getCounter = ctxt_idx

ctxt_mode :: App VerifyMode
ctxt_mode =
  (ctxt_modem <$> ask) >>= \case
    Nothing -> impossible "uninitialized"
    Just x -> return $ x

smtNewScope :: App a -> App a
smtNewScope m = do
  smt <- ctxt_smt <$> ask
  liftIO $ SMT.push smt
  x <- m
  liftIO $ SMT.pop smt
  return $ x

ctxtNewScope :: App a -> App a
ctxtNewScope m = do
  SMTCtxt {..} <- ask
  ctxt_smt_trace' <- liftIO $ dupeIORef ctxt_smt_trace
  let dupeMapInfo (SMTMapInfo {..}) = do
        sm_c' <- liftIO $ dupeCounter sm_c
        sm_rs' <- lift $ dupeIORef sm_rs
        sm_us' <- lift $ dupeIORef sm_us
        return $
          SMTMapInfo
            { sm_t = sm_t
            , sm_kt = sm_kt
            , sm_c = sm_c'
            , sm_rs = sm_rs'
            , sm_us = sm_us'
            }
  ctxt_maps' <- mapM dupeMapInfo ctxt_maps
  smtNewScope $
    local
      (\e ->
         e
           { ctxt_maps = ctxt_maps'
           , ctxt_smt_trace = ctxt_smt_trace'
           })
      $ m

shouldSimulate :: SLPart -> App Bool
shouldSimulate p =
  ctxt_mode >>= \case
    VM_Honest -> return $ True
    VM_Dishonest which ->
      case which of
        RoleContract -> return $ False
        RolePart me -> return $ me == p

smtInteract :: SLPart -> String -> String
smtInteract who m = "interact_" ++ (bunpack who) ++ "_" ++ m

smtAddress :: SLPart -> String
smtAddress who = "address_" <> bunpack who

smtConstant :: DLConstant -> String
smtConstant = \case
  DLC_UInt_max  -> "dlc_UInt_max"
  DLC_Token_zero -> "dlc_Token_zero"

smt_c :: SrcLoc -> DLConstant -> App SExpr
smt_c at c = smt_a at $ DLA_Constant c

getVarName :: DLVar -> String
getVarName (DLVar _ _ _ i) = "v" ++ show i

smtVar :: DLVar -> App String
smtVar dv = do
  let name = getVarName dv
  v2dv <- ctxt_v_to_dv <$> ask
  liftIO $ modifyIORef v2dv $ M.insert name dv
  return $ name

smtTypeSort :: DLType -> App String
smtTypeSort t = do
  tm <- ctxt_typem <$> ask
  case M.lookup t tm of
    Just (s, _) -> return s
    Nothing -> impossible $ "smtTypeSort " <> show t

smtTypeInv :: DLType -> SExpr -> App ()
smtTypeInv t se = do
  tm <- ctxt_typem <$> ask
  case M.lookup t tm of
    Just (_, i) -> smtAssertCtxt $ i se
    Nothing -> impossible $ "smtTypeInv " <> show t

smtDeclare :: Solver -> String -> SExpr -> Maybe SMTLet -> App ()
smtDeclare smt v s ml = do
  smt_trace_r <- asks ctxt_smt_trace
  liftIO $ modifyIORef smt_trace_r (\st -> maybe st (: st) ml)
  liftIO $ void $ SMT.declare smt v s

smtDeclare_v :: String -> DLType -> Maybe SMTLet -> App ()
smtDeclare_v v t l = do
  smt <- ctxt_smt <$> ask
  s <- smtTypeSort t
  smtDeclare smt v (Atom s) l
  smtTypeInv t $ Atom v

smtMulDiv :: [SExpr] -> App SExpr
smtMulDiv = \case
  [x, y, den] -> return $ smtApply "div" [smtApply "*" [x, y], den]
  _ -> impossible "smtPrimOp: MUL_DIV args"

smtPrimOp :: SrcLoc -> PrimOp -> [DLArg] -> [SExpr] -> App SExpr
smtPrimOp at p dargs =
  case p of
    ADD _ _ -> bvapp "bvadd" "+"
    SUB _ _ -> bvapp "bvsub" "-"
    MUL _ _ -> bvapp "bvmul" "*"
    DIV _ _ -> bvapp "bvudiv" "div"
    MOD _ _ -> bvapp "bvumod" "mod"
    PLT _ -> bvapp "bvult" "<"
    PLE _ -> bvapp "bvule" "<="
    PEQ _ -> app "="
    PGE _ -> bvapp "bvuge" ">="
    PGT _ -> bvapp "bvugt" ">"
    SQRT _ -> app "UInt_sqrt"
    UCAST _ _ _ _ -> \case
      [x] -> return x
      _ -> impossible "ucast"
    LSH -> bvapp "bvshl" "UInt_lsh"
    RSH -> bvapp "bvlshr" "UInt_rsh"
    BAND _ -> bvapp "bvand" "UInt_band"
    BIOR _ -> bvapp "bvor" "UInt_bior"
    BXOR _ -> bvapp "bvxor" "UInt_bxor"
    DIGEST_XOR -> app "Digest_xor"
    BYTES_XOR -> app "Bytes_xor"
    BTOI_LAST8 False -> app "btoiLast8"
    BTOI_LAST8 True  -> app "dtoiLast8"
    IF_THEN_ELSE -> app "ite"
    DIGEST_EQ -> app "="
    ADDRESS_EQ -> app "="
    TOKEN_EQ -> app "="
    (BYTES_ZPAD xtra) -> \args -> do
      xtra' <- smt_la at $ bytesZeroLit xtra
      return $ smtApply "bytesAppend" (args <> [xtra'])
    STRINGDYN_CONCAT -> app "StringDyn_Concat"
    UINT_TO_STRINGDYN _ -> app "UInt_toStringDyn"
    MUL_DIV _ -> smtMulDiv
    SELF_ADDRESS pn isClass _ ->
      case dargs of
        [] -> \_ ->
          case isClass of
            False ->
              return $ Atom $ smtAddress pn
            True -> do
              shouldSimulate pn >>= \case
                True ->
                  Atom <$> smtCurrentAddress pn
                False -> do
                  ai <- allocVarIdx
                  let av = "classAddr" <> show ai
                  let dv = DLVar at Nothing T_Address ai
                  let smlet = SMTLet at dv (DLV_Let DVC_Once dv) Witness (SMTProgram $ DLE_PrimOp at p dargs)
                  smtDeclare_v av T_Address $ Just smlet
                  return $ Atom av
        se -> impossible $ "self address " <> show se
    CTC_ADDR_EQ -> app "Contract_addressEq"
    GET_CONTRACT -> impossible "GET_CONTRACT"
    GET_ADDRESS -> impossible "GET_ADDRESS"
    GET_COMPANION -> impossible "GET_COMPANION"
  where
    app n = return . smtApply n
    bvapp n_bv n_i = app $ if use_bitvectors then n_bv else n_i

smtTypeByteConverter :: DLType -> App String
smtTypeByteConverter t = (++ "_toBytes") <$> smtTypeSort t

smtArgByteConverter :: DLArg -> App String
smtArgByteConverter = smtTypeByteConverter . argTypeOf

smtArgBytes :: SrcLoc -> DLArg -> App SExpr
smtArgBytes at arg = do
  conv <- smtArgByteConverter arg
  arg' <- smt_a at arg
  return $ smtApply conv [arg']

smtDigestCombine :: SrcLoc -> [DLArg] -> App SExpr
smtDigestCombine at args =
  case args of
    [] -> return $ smtApply "bytes0" []
    [x] -> convert1 x
    (x : xs) -> do
      x' <- convert1 x
      xs' <- smtDigestCombine at xs
      return $ smtApply "bytesAppend" [x', xs']
  where
    convert1 = smtArgBytes at

--- Verifier
data ResultDesc
  = -- = RD_UnsatCore [String]
    RD_Model SExpr
  deriving (Show, Read)

type SResult = (SMT.Result, Maybe ResultDesc)

sv2dv :: String -> App (Maybe DLVar)
sv2dv v = do
  v2dv <- (liftIO . readIORef) =<< asks ctxt_v_to_dv
  return $ M.lookup v v2dv

data SDT
  = SDT_D DLType
  | SDT_SMap DLType DLType

mustBeSDT_D :: SDT -> DLType
mustBeSDT_D = \case
  SDT_D x -> x
  _ -> impossible "mustBeSDT_D"

parseType :: SExpr -> App SDT
parseType = \case
  Atom "Int" -> return $ SDT_D $ T_UInt UI_Word
  Atom "Bytes" -> return $ SDT_D $ T_Bytes 0
  Atom t -> do
    typem <- asks ctxt_smt_typem
    case M.lookup t typem of
      Just dt -> return $ SDT_D dt
      Nothing -> impossible $ "parseType: Atom: " <> show t
  List [Atom "Array", Atom "Int", etse] -> do
    et <- mustBeSDT_D <$> parseType etse
    return $ SDT_D $ T_Array et 0
  List [Atom "Map", List [Atom "Array", domse, rngse]] -> goMap domse rngse
  List [Atom "Array", domse, rngse] -> goMap domse rngse
  ty -> impossible $ "parseType: " <> show ty
  where
    goMap domse rngse = do
      dom <- mustBeSDT_D <$> parseType domse
      rng <- mustBeSDT_D <$> parseType rngse
      return $ SDT_SMap dom rng

_parseValType :: M.Map String SExpr -> SExpr -> App SDT
_parseValType env = \case
  Atom ident
    | M.member ident env -> do
      let mv = M.lookup ident env
      maybe (impossible "parseVal: lookup") (_parseValType env) mv
  v -> impossible $ "parseValType: " <> show v

parseVal :: M.Map String SExpr -> SDT -> SExpr -> App SMTVal
parseVal env sdt v = do
  case v of
    List [Atom "ite", ce, te, fe] -> do
      c' <- parseVal env (SDT_D T_Bool) ce
      t' <- parseVal env sdt te
      f' <- parseVal env sdt fe
      return $ SMV_ite c' t' f'
    List [Atom "=", _le, _re] -> do
      --ty <- parseValType env le
      --l' <- parseVal env ty le
      --r' <- parseVal env ty re
      return $ SMV_unknown v
    -- Parse let bindings and add them to the subst env
    List [Atom "let", List envs, e] -> do
      let env' =
            M.fromList $
              map
                (\case
                   List [Atom k, v'] -> (k, v')
                   _ -> impossible "parseVal: encountered non-pair let binding")
                envs
      parseVal (M.union env' env) sdt e
    -- Sub var if encountered
    Atom ident
      | M.member ident env -> do
        let mv = M.lookup ident env
        maybe (impossible "parseVal: lookup") (parseVal env sdt) mv
    _ ->
      case sdt of
        SDT_SMap dom rng ->
          case v of
            List [List (Atom "as" : Atom "const" : _), e] ->
              SMV_Map_Const <$> parseVal env (SDT_D rng) e
            List [Atom "store", m, f, ma] ->
              SMV_Map_Set <$> parseVal env sdt m <*> parseVal env (SDT_D dom) f <*> parseVal env (SDT_D rng) ma
            _ ->
              impossible $ "parseVal: SDT_SMap: " <> show v
        SDT_D t ->
          case t of
            T_Bool -> do
              case v of
                Atom "true" -> return $ SMV_Bool True
                Atom "false" -> return $ SMV_Bool False
                _ -> impossible $ "parseVal: Bool: " <> show v
            T_UInt _ -> do
              let err = impossible $ "parseVal: UInt: " <> show v
              let readInt i = fromMaybe err (readMaybe i :: Maybe Integer)
              case v of
                Atom i -> return $ SMV_Int $ readInt i
                -- SMT can produce negative values when dealing with unsafe arithmetic (sub wraparound etc.)
                List [Atom "-", Atom i] -> return $ SMV_Int $ - (readInt i)
                _ -> err
            T_Null -> return SMV_Null
            T_Digest -> do
              case v of
                Atom i -> return $ SMV_Digest i
                _ -> impossible $ "parseVal: Digest: " <> show v
            T_Token -> do
              case v of
                Atom i -> return $ SMV_Token i
                _ -> impossible $ "parseVal: Token: " <> show v
            T_Contract -> do
              case v of
                Atom i -> return $ SMV_Contract i
                _ -> impossible $ "parseVal: Contract: " <> show v
            T_Address -> do
              let err = impossible $ "parseVal: Address: " <> show v
              case v of
                Atom i ->
                  return $
                    SMV_Address $
                      fromMaybe err (readMaybe $ dropWhile (not . isDigit) i :: Maybe Integer)
                _ -> err
            T_Bytes _ -> do
              case v of
                Atom i -> return $ SMV_Bytes $ bpack i
                _ -> impossible $ "parseVal: Bytes: " <> show v
            T_BytesDyn -> do
              case v of
                Atom i -> return $ SMV_BytesDyn $ bpack i
                _ -> impossible $ "parseVal: BytesDyn: " <> show v
            T_StringDyn -> do
              case v of
                Atom i -> return $ SMV_StringDyn $ bpack i
                _ -> impossible $ "parseVal: StringDyn: " <> show v
            T_Array ty sz ->
              case v of
                List [List (Atom "as" : Atom "const" : _), e] ->
                  SMV_Array_Const ty <$> parseVal env (SDT_D ty) e
                List [Atom "store", arrse, idxse, vse] -> do
                  arrv <- parseVal env sdt arrse
                  idxv <- parseVal env (SDT_D $ T_UInt UI_Word) idxse
                  vv <- parseVal env (SDT_D ty) vse
                  case (arrv, idxv) of
                    (SMV_Array_Const _ vc, SMV_Int idx) -> do
                      let idx' = fromIntegral idx
                      return $ SMV_Array ty $ arraySet idx' vv (replicate idx' vc)
                    (SMV_Array _ vs, SMV_Int idx) -> do
                      return $ SMV_Array ty $ arraySet (fromIntegral idx) vv vs
                    _ -> impossible $ "parseVal: Array(" <> show sz <> ").store " <> show arrv <> " " <> show idxv
                List [Atom "_", Atom "as-array", _] ->
                  return $ SMV_unknown v
                Atom ('z' : '_' : t0) -> do
                  typem <- asks ctxt_smt_typem
                  case M.lookup t0 typem of
                    Just dt -> return $ SMV_Array dt $ []
                    Nothing -> impossible $ "parseVal: Array: Atom: z_" <> show t0 <> " is Nothing"
                _ -> impossible $ "parseVal: Array: " <> show v
            T_Tuple [] ->
              case v of
                Atom _ -> return $ SMV_Tuple []
                _ -> impossible $ "parseVal: Tuple mt " <> show v
            T_Tuple ts ->
              case v of
                List (_ : vs) -> do
                  SMV_Tuple <$> zipWithM (parseVal env) (map SDT_D ts) vs
                _ -> impossible $ "parseVal: Tuple " <> show v
            T_Object ts ->
              case v of
                List (_ : vs) -> do
                  fields <- mapM (\((s, vt), mv) -> parseVal env (SDT_D vt) mv <&> (s,)) $ zip (M.toAscList ts) vs
                  return $ SMV_Object $ M.fromList fields
                Atom _ ->
                  return $ SMV_Object $ mempty
                _ -> impossible $ "parseVal: Object " <> show v
            T_Struct ts ->
              case v of
                List (_ : vs) -> do
                  fields <- mapM (\((s, vt), mv) -> parseVal env (SDT_D vt) mv <&> (s,)) $ zip ts vs
                  return $ SMV_Struct $ fields
                _ -> impossible $ "parseVal: Struct " <> show v
            T_Data ts ->
              case v of
                List [Atom con, vv] -> do
                  let c = case dropWhile (/= '_') con of
                        _ : c' -> c'
                        _ -> impossible "parseVal: Data: Constructor name"
                  v' <- case M.lookup c ts of
                    Just vt -> parseVal env (SDT_D vt) vv
                    Nothing -> impossible $ "parseVal: Data: Constructor type"
                  return $ SMV_Data c [v']
                _ -> impossible $ "parseVal: Data " <> show v

isVarAMap :: String -> SExpr -> App Bool
isVarAMap v = \case
  List [Atom "Array", k, _]
    | k /= uint256_sort -> return True
  _ -> do
    map_vs <- (liftIO . readIORef) =<< asks ctxt_map_vars
    return $ S.member v map_vs

parseModel2 :: SMTModel -> App (M.Map String SMTVal)
parseModel2 pm = M.fromList <$> aux (M.toList pm)
  where
    aux = \case
      [] -> return []
      (v, (tyse, vse)) : tl -> do
        -- liftIO $ putStrLn $ show (v, tyse, vse)
        ty' <- isVarAMap v tyse >>= \case
                True  -> return $ List [Atom "Map", tyse]
                False -> return $ tyse
        ty <- parseType ty'
        ve <- parseVal mempty ty vse
        rst <- aux tl
        return $ (v, ve) : rst

showTrace :: M.Map DLVar SMTVal -> SMTTrace -> Doc
showTrace pm st = do
  let pm' = M.map pretty pm
  pretty_subst pm' st

data SMTError
  = SMTError String
  deriving (Generic, Show)

instance ErrorSuggestions SMTError

instance HasErrorCode SMTError where
  errPrefix = const "RV"
  errIndex = const 0

instance ErrorMessageForJson SMTError where
  errorMessageForJson (SMTError msg) = msg

display_fail :: SrcLoc -> [SLCtxtFrame] -> TheoremKind -> Maybe B.ByteString -> Maybe ResultDesc -> Maybe DLVar -> Bool -> App ()
display_fail tat f tk mmsg mrd mdv timeout = do
  messageRef <- liftIO $ newIORef ""
  let putLine s = liftIO $ modifyIORef messageRef (++ s ++ "\n")
  cwd <- liftIO $ getCurrentDirectory
  let hasColor = (not unsafeIsErrorFormatJson) && unsafeTermSupportsColor
  let color c = if hasColor then TC.color c else id
  let style s = if hasColor then TC.style s else id
  VerifyOpts {..} <- (vst_vo . ctxt_vst) <$> ask
  let lab =
        case timeout of
          True -> "timed out after " <> show vo_timeout <> " ms"
          False -> "failed"
  putLine $ color TC.Red $ style TC.Bold $ "Verification " <> lab <> ":"
  mode <- ctxt_mode
  putLine $ "  when " ++ (show $ pretty mode)
  putLine $ "  of theorem: " ++ (show $ pretty tk)
  case mmsg of
    Nothing -> mempty
    Just msg -> do
      putLine $ "  msg: " <> show msg
  putLine $ redactAbsStr cwd $ "  at " ++ show tat
  mapM_ (putLine . ("  " ++) . show) f
  putLine $ ""
  case mdv of
    Nothing -> do
      putLine $ show $ "  " <> pretty tk <> parens "false" <> ";" <> hardline
    Just dv -> do
      let pm = case mrd of
            Nothing -> mempty
            --- XXX Gather these and use them
            -- Just (RD_UnsatCore _uc) -> mempty
            Just (RD_Model m) -> parseModel m
      pm_str_val <- parseModel2 pm
      lets <- (liftIO . readIORef) =<< asks ctxt_smt_trace
      let lets' = reverse $ nubOrd $ dropConstants pm_str_val lets
      let tr = SMTTrace lets' tk dv
      let doAddCounts = True
      smtTrace <-
        case doAddCounts of
          True -> liftIO $ add_counts tr
          False -> return tr
      pm_dv_val <-
        M.fromList
          <$> foldM
            (\acc (k, v) -> do
               sv2dv k <&> \case
                 Just dv'' -> (dv'', v) : acc
                 Nothing -> acc)
            []
            (M.toList pm_str_val)
      putLine $ show $ showTrace pm_dv_val smtTrace
  liftIO $ do
    finishedMessage <- readIORef messageRef
    case unsafeIsErrorFormatJson of
      True -> hPutStrLn stderr $ "error: " ++ makeErrorJson tat (SMTError finishedMessage) (topOfStackTrace f)
      False -> do
        putStr finishedMessage
    when vo_first_fail_quit $
      exitWith $ ExitFailure 1

dropConstants :: M.Map String SMTVal -> [SMTLet] -> [SMTLet]
dropConstants pm = \case
  [] -> []
  SMTCon s Nothing se : tl ->
    case (s, M.lookup s pm) of
      (cs, Just v)
        -- Non-zero value indicates this constant is part of assertion failure
        | cs == smtConstant DLC_UInt_max && v == SMV_Int 0 -> dropConstants pm tl
        | cs == smtConstant DLC_Token_zero -> dropConstants pm tl
      (_, ow) -> SMTCon s ow se : dropConstants pm tl
  ow : tl -> ow : dropConstants pm tl

smtNewPathConstraint :: SExpr -> App a -> App a
smtNewPathConstraint se m = do
  pc <- ctxt_path_constraint <$> ask
  local (\e -> e {ctxt_path_constraint = se : pc}) $
    m

smtAddPathConstraints :: SExpr -> App SExpr
smtAddPathConstraints se =
  (ctxt_path_constraint <$> ask) >>= \case
    [] -> return $ se
    pcs -> return $ smtApply "=>" [(smtAndAll pcs), se]

smtAssertCtxt :: SExpr -> App ()
smtAssertCtxt se = smtAssert =<< smtAddPathConstraints se

-- Intercept failures to prevent showing "user error",
-- which is confusing to a Reach developer. The library
-- `fail`s if there's a problem with the compiler,
-- not a Reach program.
smtAssert :: SExpr -> App ()
smtAssert se = do
  smt <- ctxt_smt <$> ask
  liftIO $
    Exn.catch (do SMT.assert smt se) $
      \(e :: Exn.SomeException) ->
        impossible $ safeInit $ drop 12 $ show e

fromSExpr :: Read a => SExpr -> a
fromSExpr = \case
  Atom s ->
    case readMaybe s of
      Just x -> x
      Nothing -> impossible $ "fromSExpr: " <> show s
  List _ -> impossible "fromSExpr"

toSExpr :: Show a => a -> SExpr
toSExpr = Atom . show

deriving instance Read Result

deriving instance Read SExpr

checkUsing :: TheoremKind -> App SResult
checkUsing tk = do
  smt <- ctxt_smt <$> ask
  fromSExpr <$> (liftIO $ SMT.command smt $ List [Atom "reachCheckUsing", toSExpr $ isPossible tk])

verify1 :: SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> App ()
verify1 at mf tk se mmsg = smtNewScope $ do
  flip forM_ smtAssert =<< (ctxt_path_constraint <$> ask)
  smtAssert $ if isPossible tk then se else smtNot se
  r <- checkUsing tk
  verify1r at mf tk se mmsg r

verify1r :: SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> SResult -> App ()
verify1r at mf tk se mmsg r = do
  case isPossible tk of
    True ->
      case r of
        (Unknown, _) -> bady Nothing
        (Unsat, mrd) -> badn mrd
        (Sat, _) -> good
    False ->
      case r of
        (Unknown, _) -> bady Nothing
        (Unsat, _) -> good
        (Sat, mrd) -> badn mrd
  where
    good = void $ (liftIO . incCounter . vst_res_succ) =<< (ctxt_vst <$> ask)
    badn = bad_ False
    bady = bad_ True
    bad_ timeout mm = do
      dr <- ctxt_displayed <$> ask
      dspd <- liftIO $ readIORef dr
      mdv <-
        case se of
          Atom id' -> sv2dv id'
          _ -> return Nothing
      let repeated = elem se dspd
      case repeated of
        True ->
          void $ (liftIO . incCounter . vst_res_reps) =<< (ctxt_vst <$> ask)
        False ->
          display_fail at mf tk mmsg mm mdv timeout
      liftIO $ modifyIORef dr $ S.insert se
      let which = if timeout then vst_res_time else vst_res_fail
      void $ (liftIO . incCounter . which) =<< (ctxt_vst <$> ask)

isPossible :: TheoremKind -> Bool
isPossible = \case
  TClaim CT_Possible -> True
  _ -> False

pathAddUnbound_v :: String -> DLType -> Maybe SMTLet -> App ()
pathAddUnbound_v v t ml = do
  smtDeclare_v v t ml

pathAddUnbound :: SrcLoc -> Maybe DLVar -> Maybe SMTExpr -> App ()
pathAddUnbound _ Nothing _ = mempty
pathAddUnbound at_dv (Just dv) msmte = do
  let DLVar _ _ t _ = dv
  v <- smtVar dv
  let smlet = Just . SMTLet at_dv dv (DLV_Let DVC_Once dv) Witness =<< msmte
  pathAddUnbound_v v t smlet

assertInvariants :: SrcLoc -> DLType -> String -> App ()
assertInvariants at_dv t v =
  case t of
    T_UInt ut -> do
      rhs <- case ut of
               UI_Word -> smt_c at_dv DLC_UInt_max
               UI_256 -> return $ smt_lt at_dv $ DLL_Int at_dv ut uint256_Max
      smtAssert (smtApply "<=" [Atom v, rhs])
    _ -> return ()

pathAddBound :: SrcLoc -> Maybe DLVar -> Maybe SMTExpr -> SExpr -> SMTCat -> App ()
pathAddBound _ Nothing _ _ _ = mempty
pathAddBound at_dv (Just dv) de se sc = do
  let DLVar _ _ t _ = dv
  v <- smtVar dv
  let smlet = Just . SMTLet at_dv dv (DLV_Let DVC_Once dv) sc =<< de
  smtDeclare_v v t smlet
  assertInvariants at_dv t v
  --- Note: We don't use smtAssertCtxt because variables are global, so
  --- this variable isn't affected by the path.
  smtAssert $ smtEq (Atom v) se

smtMapVar :: DLMVar -> Int -> String
smtMapVar (DLMVar mi) ri = "map" <> show mi <> "_" <> show ri

smtMapRefresh :: SrcLoc -> App ()
smtMapRefresh at = do
  ms <- ctxt_maps <$> ask
  let go (mpv, SMTMapInfo {..}) = do
        (_, mOld) <- smtMapLookup mpv
        mi' <- liftIO $ incCounter sm_c
        liftIO $ writeIORef sm_rs $ mempty
        liftIO $ writeIORef sm_us $ SMR_Fresh
        let oldMap = fromMaybe (impossible "smtMapRefresh") mOld
        smtMapDeclare at mpv mi' $ SMTMapFresh oldMap
  mapM_ go $ M.toList ms

smtMapLookupC :: DLMVar -> App SMTMapInfo
smtMapLookupC mpv = do
  ms <- ctxt_maps <$> ask
  case M.lookup mpv ms of
    Just x -> return $ x
    Nothing -> impossible $ "smtMapLookupC unknown map"

smtMapSort :: DLMVar -> App SExpr
smtMapSort mpv = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  kt' <- smtTypeSort $ sm_kt
  sm_t' <- smtTypeSort sm_t
  return $ smtApply "Array" [Atom kt', Atom sm_t']

smtMapDeclare :: SrcLoc -> DLMVar -> Int -> SynthExpr -> App ()
smtMapDeclare at mpv mi se = do
  let mv = smtMapVar mpv mi
  t <- smtMapSort mpv
  smt <- asks ctxt_smt
  dv <- do
    newId <- allocVarIdx
    let dv = DLVar at (Just (at, mv)) T_Null newId
    v2dv <- asks ctxt_v_to_dv
    map_vs <- asks ctxt_map_vars
    liftIO $ modifyIORef v2dv (M.insert mv dv)
    liftIO $ modifyIORef map_vs $ S.insert mv
    return dv
  let cat = case se of
        SMTMapFresh _ -> Witness
        _ -> Context
  let l = SMTLet at dv (DLV_Let DVC_Once dv) cat $ SMTSynth se
  smtDeclare smt mv t $ Just l

smtMapLookup :: DLMVar -> App (SExpr, Maybe DLVar)
smtMapLookup mpv = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  mi <- liftIO $ readCounter sm_c
  let mv = smtMapVar mpv (mi - 1)
  dv <- sv2dv mv
  return (Atom mv, dv)

smtMapMkMaybe :: SrcLoc -> DLMVar -> Maybe DLArg -> App SExpr
smtMapMkMaybe at mpv mna = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  let mkna = DLLA_Data $ dataTypeMap sm_t
  let na = case mna of
        Just x -> mkna "Some" x
        Nothing -> mkna "None" $ DLA_Literal DLL_Null
  smt_la at na

smtMapUpdate :: SrcLoc -> DLMVar -> DLArg -> Maybe DLArg -> App ()
smtMapUpdate at mpv fa mna = do
  fa' <- smt_a at fa
  na' <- smtMapMkMaybe at mpv mna
  SMTMapInfo {..} <- smtMapLookupC mpv
  mdv <-
    smtMapLookup mpv >>= \case
      (_, Just dv) -> return dv
      (_, Nothing) -> impossible $ "smtMapLookup: " <> show (pretty mpv)
  mi' <- liftIO $ incCounter sm_c
  let mi = mi' - 1
  let mv = smtMapVar mpv mi
  let ma = Atom mv
  let se = smtApply "store" [ma, fa', na']
  smtMapDeclare at mpv mi' $ SMTMapSet mdv fa mna
  smtMapRecordUpdate mpv $ SMR_Update ma fa' na'
  let mv' = smtMapVar mpv mi'
  smtAssert $ smtEq (Atom mv') se

smtMapRecordReduce :: DLMVar -> SMTMapRecordReduce -> App ()
smtMapRecordReduce mpv r = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  liftIO $ modifyIORef sm_rs $ (r :)

smtMapRecordUpdate :: DLMVar -> (SMTMapRecordUpdate -> SMTMapRecordUpdate) -> App ()
smtMapRecordUpdate mpv r = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  liftIO $ modifyIORef sm_us r

smtMapReviewRecord :: DLMVar -> (SMTMapInfo -> IORef a) -> App a
smtMapReviewRecord mpv sm_x = do
  mi <- smtMapLookupC mpv
  liftIO $ readIORef $ sm_x mi

smt_freshen :: DLBlock -> [DLVar] -> App (DLBlock, [DLVar])
smt_freshen x vs = do
  c <- ctxt_idx <$> ask
  liftIO $ freshen_ c x vs

smtMapReduceApply :: SrcLoc -> DLVar -> DLVar -> DLBlock -> App (SExpr, SExpr, App SExpr)
smtMapReduceApply at b a f = do
  (f', b_f, a_f) <-
    smt_freshen f [b, a] >>= \case
      (f', [b_f, a_f]) -> return (f', b_f, a_f)
      _ -> impossible "smt_freshen bad"
  b' <- smt_v at b_f
  pathAddUnbound at (Just b_f) $ Just $ SMTModel O_ReduceVar
  a' <- smt_v at a_f
  pathAddUnbound at (Just a_f) $ Just $ SMTModel O_ReduceVar
  let call_f' = smt_block f'
  return $ (b', a', call_f')

smtMapReviewRecordRef :: SrcLoc -> DLMVar -> SExpr -> DLVar -> App ()
smtMapReviewRecordRef at x fse res = do
  let go = \case
        SMR_Update _ fa' _ prev ->
          -- We only learn something about what we've read from the map via the
          -- reduction if this field has not been modified, so we add negative
          -- path constraints and then apply the reduction function
          smtNewPathConstraint (smtNot $ smtEq fa' fse) . go prev
        SMR_Fresh ->
          -- The map is completely unknown
          id
        SMR_New ->
          -- XXX We know the map is empty, so we could assert that this ref is
          -- Nothing
          id
  add_us_constraints <- go <$> smtMapReviewRecord x sm_us
  rs <- smtMapReviewRecord x sm_rs
  res' <- smt_v at res
  add_us_constraints $
    forM_ rs $ \(SMR_Reduce _ ans _ b a f) -> do
      ans' <- smt_v at ans
      (_, a', f') <- smtMapReduceApply at b a f
      smtAssertCtxt $ smtEq a' res'
      fres' <- f'
      smtAssertCtxt $ smtEq ans' fres'

smtMapReviewRecordReduce :: SrcLoc -> Int -> DLVar -> DLMVar -> DLArg -> DLVar -> DLVar -> DLBlock -> App ()
smtMapReviewRecordReduce at mri ans x z b a f = do
  z' <- smt_a at z
  (me_rs, other_rs) <- List.partition ((==) mri . smr_mri) <$> smtMapReviewRecord x sm_rs
  let go_fresh ans_x (SMR_Reduce {..}) = do
        -- We doing one map reduce `m.sum()` and there was a different map
        -- reduce in the past `m.product()` and we are trying to learn if the
        -- result of that map reduce says anything about ours.
        --
        -- We can't look at the actual values, because it is a fresh map, so
        -- we've forgotten what actually went into it, so all we can do is look
        -- at the actual map formula:
        --
        -- ans_x = fold f_x z_x m
        -- ans_y = fold f_y z_y m
        --
        -- For now, we're going to the simplest dumbest thing: just check if
        -- f_x and f_y are exactly the same and if they are, then learn the
        -- ans_x and ans_y are the same.
        --
        -- This can only happen if the types are
        -- the same. We know they are the same if the accumulators are the
        -- same, because the elements are definitely the same and the final
        -- value is the same as the accumulators
        let varTypeEq v0 v1 = ((==) (varType v0) (varType v1))
        when ((varTypeEq b smr_b) && (varTypeEq a smr_a)) $ do
          (b_x, a_x, mkf_x) <- smtMapReduceApply at b a f
          (b_y, a_y, mkf_y) <- smtMapReduceApply at smr_b smr_a smr_f
          f_x <- mkf_x
          f_y <- mkf_y
          let z_x = z'
          z_y <- smt_a at smr_z
          ans_y <- smt_v at smr_ans
          smtAssert $ smtEq b_x b_y
          smtAssert $ smtEq a_x a_y
          smtAssert $ smtImplies (smtAnd (smtEq z_x z_y) (smtEq f_x f_y)) (smtEq ans_x ans_y)
  let go = \case
        SMR_New -> do
          -- We know that the map is empty, so it must equal to initial value
          return z'
        SMR_Fresh -> do
          case map smr_ans me_rs of
            (oa : _) ->
              -- It is fresh, so it could equal one of the past reductions
              smt_v at oa
            [] -> do
              -- Or, it could be related to a different reduction
              ans'_dv <- freshenVar ans
              pathAddUnbound at (Just ans'_dv) $ Just $ SMTModel O_ReduceVar
              ans' <- smt_v at ans'_dv
              mapM_ (go_fresh ans') other_rs
              return ans'
        SMR_Update ma fa' na' prev -> do
          -- We go through each one of the updates and inline the computation
          -- of the reduction function back to the last known value, which is
          -- either z in the beginning or the last value
          z'0 <- go prev
          (b'0, a'0, f'0) <- smtMapReduceApply at b a f
          smtAssertCtxt $ smtEq a'0 $ smtApply "select" [ma, fa']
          fres'0 <- f'0
          smtAssertCtxt $ smtEq fres'0 z'0
          -- n f( Z0, m[fa] ) = z'
          -- u f( Z0, na' ) = z''
          (b'1, a'1, f'1) <- smtMapReduceApply at b a f
          smtAssertCtxt $ smtEq b'1 b'0
          smtAssertCtxt $ smtEq a'1 na'
          f'1
  z'' <- go =<< smtMapReviewRecord x sm_us
  ans' <- smt_v at ans
  smtAssertCtxt $ smtEq ans' z''

smt_lt :: SrcLoc -> DLLiteral -> SExpr
smt_lt _at_de dc =
  case dc of
    DLL_Null -> Atom "null"
    DLL_Bool b ->
      case b of
        True -> Atom "true"
        False -> Atom "false"
    DLL_Int _ _ i ->
      case use_bitvectors of
        True ->
          List
            [ List [Atom "_", Atom "int2bv", Atom "256"]
            , Atom (show i)
            ]
        False -> Atom $ show i
    DLL_TokenZero -> Atom $ smtConstant DLC_Token_zero

smt_v :: SrcLoc -> DLVar -> App SExpr
smt_v _at_de dv = Atom <$> smtVar dv

smt_a :: SrcLoc -> DLArg -> App SExpr
smt_a at_de = \case
  DLA_Var dv -> smt_v at_de dv
  DLA_Constant c -> do
    smt_con <- ctxt_smt_con <$> ask
    return $ smt_con at_de c
  DLA_Literal c -> return $ smt_lt at_de c
  DLA_Interact who i _ -> return $ Atom $ smtInteract who i

smt_la :: SrcLoc -> DLLargeArg -> App SExpr
smt_la at_de dla = do
  let t = largeArgTypeOf dla
  s <- smtTypeSort t
  let cons = \case
        [] -> return $ Atom $ s <> "_cons"
        as -> smtApply (s ++ "_cons") <$> mapM (smt_a at_de) as
  case dla of
    DLLA_Array _ as -> cons as
    DLLA_Tuple as -> cons as
    DLLA_Obj m -> cons $ M.elems m
    DLLA_Data _ vn vv -> do
      vv' <- smt_a at_de vv
      return $ smtApply (s ++ "_" ++ vn) [vv']
    DLLA_Struct kvs -> cons $ map snd kvs
    DLLA_Bytes bs -> do
      return $ smtApply "bytes" [Atom (show $ crc32 bs)]
    DLLA_BytesDyn bs -> do
      return $ smtApply "bytesDyn" [Atom (show $ crc32 bs)]
    DLLA_StringDyn st -> do
      return $ smtApply "stringDyn" [Atom (show $ crc32 $ bpack $ T.unpack st)]

smt_e :: SrcLoc -> Maybe DLVar -> DLExpr -> App ()
smt_e at_dv mdv de = do
  case de of
    DLE_Arg at (DLA_Interact _ _ _) -> unbound at
    DLE_Arg at da -> bound at =<< smt_a at da
    DLE_LArg at dla -> bound at =<< smt_la at dla
    DLE_Impossible {} -> unbound at_dv
    DLE_VerifyMuldiv at f cl args _ -> do
      args' <- mapM (smt_a at) args
      md <- smtMulDiv args'
      rhs <- smt_c at DLC_UInt_max
      let lt = uint256_le md rhs
      doClaim at f cl lt Nothing
    DLE_PrimOp at cp args ->
      case cp of
        GET_CONTRACT -> unbound at
        GET_ADDRESS -> unbound at
        GET_COMPANION -> unbound at
        _ -> do
          let f = case cp of
                SELF_ADDRESS {} -> \se -> pathAddBound at mdv (Just $ SMTProgram de) se Witness
                _ -> bound at
          args' <- mapM (smt_a at) args
          f =<< smtPrimOp at cp args args'
    DLE_ArrayRef at arr_da idx_da -> do
      arr_da' <- smt_a at arr_da
      idx_da' <- smt_a at idx_da
      bound at $ smtApply "select" [arr_da', idx_da']
    DLE_ArraySet at arr_da idx_da val_da -> do
      arr_da' <- smt_a at arr_da
      idx_da' <- smt_a at idx_da
      val_da' <- smt_a at val_da
      bound at $ smtApply "store" [arr_da', idx_da', val_da']
    DLE_ArrayConcat {} ->
      --- FIXME: This might be possible to do by generating a function
      impossible "array_concat"
    DLE_BytesDynCast at a -> do
      a' <- smt_a at a
      bound at $ smtApply "Bytes_toBytesDyn" [a']
    DLE_TupleRef at arr_da i -> do
      let t = argTypeOf arr_da
      s <- smtTypeSort t
      arr_da' <- smt_a at arr_da
      bound at $ smtApply (s ++ "_elem" ++ show i) [arr_da']
    DLE_TupleSet at tup_a index_ val_a -> do
      forM_ mdv $ \(DLVar _ _ tup_t _) -> do
        tupSort <- smtTypeSort tup_t
        tup_se <- smt_a at tup_a
        val_se <- smt_a at val_a
        let index = fromInteger index_
        let tupCtor = tupSort ++ "_cons"
        let tupLen = length $ tupleTypes tup_t
        let copiedFields = map (\n -> smtApply (tupSort <> "_elem" <> show n) [tup_se]) $
                             filter (/= index) [0..tupLen-1]
        let (h, t) = splitAt index copiedFields
        let fields = h ++ [val_se] ++ t
        bound at $ smtApply tupCtor fields
    DLE_ObjectRef at obj_da f -> do
      let t = argTypeOf obj_da
      s <- smtTypeSort t
      obj_da' <- smt_a at obj_da
      bound at $ smtApply (s ++ "_" ++ f) [obj_da']
    DLE_ObjectSet at obj_a fieldName val_a ->
      forM_ mdv $ \(DLVar _ _ obj_t _) -> do
        objSort <- smtTypeSort obj_t
        obj_se <- smt_a at obj_a
        val_se <- smt_a at val_a
        let objCtor = objSort ++ "_cons"
        let copiedFields = map (\f -> smtApply (objSort <> "_" <> f) [obj_se]) $
                             filter (/= fieldName) $ map fst $ objstrTypes obj_t
        let fieldIndex = fromInteger $ objstrFieldIndex obj_t fieldName
        let (h, t) = splitAt fieldIndex copiedFields
        let fields = h ++ [val_se] ++ t
        bound at $ smtApply objCtor fields
    DLE_Interact at _ _ _ _ _ ->
      unbound at
    DLE_Digest at args -> do
      args' <- smtDigestCombine at args
      bound at $ smtApply "digest" [args']
    DLE_Claim at f ct ca mmsg -> do
      ca' <- smt_a at ca
      doClaim at f ct ca' mmsg
    DLE_Transfer {} ->
      mempty
    DLE_TokenInit {} ->
      mempty
    DLE_TokenAccepted at _ _ ->
      unbound at
    DLE_CheckPay at f amta mtok -> do
      (pv_net, pv_ks) <- fromMaybe (impossible "no ctxt_pay_amt") <$> (ctxt_pay_amt <$> ask)
      amta' <- smt_a at amta
      paya' <- case mtok of
        Nothing -> return $ pv_net
        Just tok -> do
          tok' <- smt_a at tok
          return $ smtApply "select" [pv_ks, tok']
      let ca' = smtEq amta' paya'
      let msg_ = maybe "" (const "non-") mtok
      let mmsg = Just $ msg_ <> "network token pay amount"
      doClaim at f CT_Require ca' mmsg
    DLE_Wait {} ->
      mempty
    DLE_PartSet at who a -> do
      bound at =<< smt_a at a
      sim <- shouldSimulate who
      case (mdv, sim) of
        (Just psv, True) -> do
          psv' <- smt_v at psv
          smtAssertCtxt (smtEq psv' (Atom $ smtAddress who))
        _ ->
          mempty
    DLE_MapRef at mpv fa -> do
      (ma, mapDv) <- smtMapLookup mpv
      fa' <- smt_a at fa
      let se = smtApply "select" [ma, fa']
      let smte = Just . SMTSynth . flip SMTMapRef fa =<< mapDv
      pathAddBound at mdv smte se Context
      forM_ mdv $ smtMapReviewRecordRef at mpv fa'
    DLE_MapSet at mpv fa mna ->
      smtMapUpdate at mpv fa mna
    DLE_Remote at _ _ _ _ -> unbound at
    DLE_TokenNew at _ -> unbound at
    DLE_TokenBurn at _ _ -> unbound at
    DLE_TokenDestroy at _ -> unbound at
    DLE_TimeOrder at op mo n -> do
      n' <- smt_v at n
      let go f = smtAssert . f n'
      case mo of
        Nothing -> go smtEq $ smt_lt at $ DLL_Int at UI_Word 0
        Just o -> do
          o' <- smt_a at o
          case op of
            PGT _ -> do
              let w = DLA_Literal $ DLL_Int at UI_Word 1
              w' <- smt_a at w
              go smtEq =<< smtPrimOp at (ADD UI_Word PV_Safe) [o, w] [o', w']
            PGE _ -> go smtGe o'
            _ -> impossible $ "timeOrder: bad op: " <> show op
    DLE_EmitLog at _ lv ->
      mapM_ (bound at <=< smt_v at) lv
    DLE_setApiDetails {} -> mempty
    DLE_GetUntrackedFunds at _ _ -> unbound at
    DLE_DataTag at d -> do
      d' <- smt_a at d
      n <- smtTypeSort $ argTypeOf d
      bound at $ smtApply (n <> "_dataTag") [d']
    DLE_FromSome at mo da -> do
      mo' <- smt_a at mo
      da' <- smt_a at da
      n <- smtTypeSort $ argTypeOf mo
      let someCtor = n <> "_Some"
      let noneCtor = n <> "_None"
      let somev = Atom $ someCtor <> "_pv"
      let somep = List [Atom someCtor, somev]
      let somec = List [somep, somev]
      let nonev = Atom $ noneCtor <> "_pv"
      let nonep = List [Atom noneCtor, nonev]
      let nonec = List [nonep, da']
      bound at $ smtApply "match" [mo', List [nonec, somec]]
    DLE_ContractNew at _ _ -> unbound at
    DLE_ContractFromAddress at _addr -> unbound at
  where
    bound at se = pathAddBound at mdv (Just $ SMTProgram de) se Context
    unbound at = pathAddUnbound at mdv (Just $ SMTProgram de)
    doClaim at f ct ca' mmsg = do
      let check_m = verify1 at f (TClaim ct) ca' mmsg
      let assert_m = smtAssertCtxt ca'
      case ct of
        CT_Assert -> check_m >> assert_m
        CT_Assume -> assert_m
        CT_Enforce -> assert_m
        CT_Require ->
          ctxt_mode >>= \case
            VM_Honest -> check_m >> assert_m
            VM_Dishonest {} -> assert_m
        CT_Possible -> check_m
        CT_Unknowable {} -> mempty

data SwitchMode
  = SM_Local
  | SM_Consensus

smtSwitch :: SwitchMode -> SrcLoc -> DLVar -> SwitchCases a -> (a -> App ()) -> App ()
smtSwitch sm at ov csm iter = do
  let ova = DLA_Var ov
  let ovt = argTypeOf ova
  let ovtm = case ovt of
        T_Data m -> m
        _ -> impossible "switch"
  ovp <- smt_a at ova
  let cm1 (vn, (ov', _, l)) = do
        let smte = SMTModel $ O_SwitchCase $ DLA_Var ov
        ov'p <- smt_la at $ DLLA_Data ovtm vn $ DLA_Var ov'
        let eqc = smtEq ovp ov'p
        let udef_m = pathAddUnbound at (Just ov') (Just smte)
        let with_pc = smtNewPathConstraint eqc
        let branch_m =
              case sm of
                SM_Local ->
                  udef_m <> with_pc (iter l)
                SM_Consensus ->
                  ctxtNewScope $ udef_m <> smtAssertCtxt eqc <> iter l
        return $ (branch_m, eqc)
  casesl <- mapM cm1 $ M.toList csm
  mapM_ fst casesl
  case sm of
    SM_Local -> smtAssertCtxt (smtOrAll $ map snd casesl)
    SM_Consensus -> mempty

smt_m :: DLStmt -> App ()
smt_m = \case
  DL_Nop _ -> mempty
  DL_Let at lv de -> smt_e at (lv2mdv lv) de
  DL_Var at dv -> pathAddUnbound at (Just dv) (Just $ SMTModel O_Var)
  DL_ArrayMap {} ->
    --- FIXME: It might be possible to do this in Z3 by generating a function
    impossible "array_map"
  DL_ArrayReduce {} ->
    --- NOTE: I don't think this is possible
    impossible "array_reduce"
  DL_Set at dv va -> do
    dv' <- smt_a at (DLA_Var dv)
    va' <- smt_a at va
    smtAssertCtxt (smtEq dv' va')
  DL_LocalIf at _ ca t f -> do
    ca_se <- smt_a at ca
    let with_f = smtNewPathConstraint $ smtNot ca_se
    let with_t = smtNewPathConstraint $ ca_se
    with_t (smt_l t) <> with_f (smt_l f)
  DL_LocalSwitch at ov csm ->
    smtSwitch SM_Local at ov csm smt_l
  DL_MapReduce at mri ans x z b a f -> do
    pathAddUnbound at (Just ans) $ Just $ SMTModel O_ReduceVar
    (ctxt_inv_mode <$> ask) >>= \case
      B_Assume _ -> do
        smtMapRecordReduce x $ SMR_Reduce mri ans z b a f
      B_Prove _ ->
        smtMapReviewRecordReduce at mri ans x z b a f
      _ -> impossible $ "Map.reduce outside invariant"
  DL_Only _at (Left who) loc -> smt_lm who loc
  DL_Only {} -> impossible $ "right only before EPP"
  DL_LocalDo _ _ t -> smt_l t

smt_l :: DLTail -> App ()
smt_l = \case
  DT_Return _ -> mempty
  DT_Com m k -> smt_m m <> smt_l k

smt_lm :: SLPart -> DLTail -> App ()
smt_lm who l =
  shouldSimulate who >>= \case
    True -> smt_l l
    False -> mempty

data BlockMode
  = B_Assume Bool
  | B_Prove Bool
  | B_None

smt_block :: DLBlock -> App SExpr
smt_block (DLBlock at _ l da) = do
  smt_l l
  smt_a at da

smt_invblock :: BlockMode -> DLBlock -> Maybe B.ByteString -> App ()
smt_invblock bm b@(DLBlock at f _ _) minv_lab = do
  da' <-
      local (\e -> e {ctxt_inv_mode = bm}) $
        smt_block b
  case bm of
    B_Assume True -> smtAssertCtxt da'
    B_Assume False -> smtAssertCtxt (smtNot da')
    B_Prove inCont -> verify1 at f (TInvariant inCont) da' minv_lab
    B_None -> mempty

smt_while_jump :: Bool -> DLAssignment -> App ()
smt_while_jump vars_are_primed asn = do
  let DLAssignment asnm = asn
  invs <-
    (ctxt_while_invariants <$> ask) >>= \case
      [] -> impossible "asn outside loop"
      xs -> return $ xs
  let add_asn_lets m (DLBlock at fs t ra) =
        DLBlock at fs t' ra
        where
          go (v, a) t_ = DT_Com (DL_Let at (DLV_Let DVC_Many v) (DLE_Arg at a)) t_
          t' = foldr go t $ M.toList m
  forM_ invs $ \ (DLInvariant inv minv_lab) -> smtNewScope $ do
    inv' <-
      case vars_are_primed of
        False -> return $ add_asn_lets asnm inv
        True -> do
          let lvars = M.keys asnm
          (inv_f, nlvars) <- smt_freshen inv lvars
          let rho = M.fromList $ zip nlvars lvars
          let mapCompose bc ab = M.mapMaybe (bc M.!?) ab
          let asnm' = mapCompose asnm rho
          return $ add_asn_lets asnm' inv_f
    smt_invblock (B_Prove vars_are_primed) inv' minv_lab

smt_asn_def :: SrcLoc -> DLAssignment -> App ()
smt_asn_def at asn = mapM_ def1 $ M.keys asnm
  where
    DLAssignment asnm = asn
    def1 dv = do
      pathAddUnbound at (Just dv) (Just $ SMTModel O_Assignment)
      assertInvariants at (varType dv) (getVarName dv)

freshAddrs :: App a -> App a
freshAddrs m = do
  let go dv@(DLVar at _ _ _) = do
        dv' <- freshenVar dv
        pathAddUnbound at (Just dv') (Just $ SMTModel O_BuiltIn)
        return dv'
  addrs' <- mapM go =<< (ctxt_addrs <$> ask)
  local (\e -> e {ctxt_addrs = addrs'}) m

smtCurrentAddress :: SLPart -> App String
smtCurrentAddress who = do
  am <- ctxt_addrs <$> ask
  case M.lookup who am of
    Just x -> smtVar x
    Nothing -> impossible "smtCurrentAddress"

smt_n :: LLConsensus -> App ()
smt_n = \case
  LLC_Com m k -> smt_m m <> smt_n k
  LLC_If at ca t f -> do
    ca' <- smt_a at ca
    let go (v, k) = do
          v' <- smt_a at (DLA_Literal (DLL_Bool v))
          --- FIXME Can we use path constraints to avoid this forking?
          smtAssertCtxt (smtEq ca' v') <> smt_n k
    mapM_ (ctxtNewScope . go) [(True, t), (False, f)]
  LLC_Switch at ov csm ->
    smtSwitch SM_Consensus at ov csm smt_n
  LLC_FromConsensus at _ _ s -> do
    um <- asks ctxt_untrustworthyMaps
    when um $ smtMapRefresh at
    smt_s s
  LLC_While at asn invs cond body k ->
    mapM_ ctxtNewScope [before_m, loop_m, after_m]
    where
      with_inv = local (\e -> e {ctxt_while_invariants = invs })
      before_m = with_inv $ smt_while_jump False asn
      loop_m = do
        smtMapRefresh at
        smt_asn_def at asn
        forM_ invs $ \ (DLInvariant inv minv_lab) -> do
          smt_invblock (B_Assume True) inv minv_lab
        smt_invblock (B_Assume True) cond Nothing
        (with_inv $ smt_n body)
      after_m = do
        smtMapRefresh at
        smt_asn_def at asn
        forM_ invs $ \ (DLInvariant inv minv_lab) -> do
          smt_invblock (B_Assume True) inv minv_lab
        smt_invblock (B_Assume False) cond Nothing
        smt_n k
  LLC_Continue _at asn -> smt_while_jump True asn
  LLC_ViewIs _ _ _ ma k -> do
    maybe mempty smt_eb ma
    smt_n k

smt_s :: LLStep -> App ()
smt_s = \case
  LLS_Com m k -> smt_m m <> smt_s k
  LLS_Stop _at -> mempty
  LLS_ToConsensus at _ send recv mtime -> do
    let DLRecv whov msgvs timev secsv didSendv next_n = recv
    let timeout = case mtime of
          Nothing -> mempty
          Just (_delay_a, delay_s) -> smt_s delay_s
    let bind_time = do
          let publishOrig = Just $ SMTModel O_Publish
          -- XXX technically, didSend is guaranteed to be true if send has one
          -- thing in it
          void $ traverse (flip (pathAddUnbound at) publishOrig . Just) [timev, secsv, didSendv]
    let after = freshAddrs $ bind_time <> smt_n next_n
    let go (from, DLSend isClass msgas amta whena) = do
          should <- shouldSimulate from
          let maybe_pathAdd v mde se = do
                case should of
                  False -> pathAddUnbound at (Just v) (Just $ SMTModel O_Publish)
                  True -> pathAddBound at (Just v) mde se Context
          let bind_from =
                case isClass of
                  True ->
                    case should of
                      False ->
                        pathAddUnbound at (Just whov) (Just $ SMTModel $ O_ClassJoin from)
                      True -> do
                        from' <- smtCurrentAddress from
                        pathAddBound at (Just whov) (Just $ SMTModel $ O_Join from True) (Atom $ from') Witness
                  _ -> maybe_pathAdd whov Nothing (Atom $ smtAddress from)
          let bind_msg = zipWithM_ (\dv da -> maybe_pathAdd dv (Just $ SMTProgram $ DLE_Arg at da) =<< (smt_a at da)) msgvs msgas
          let bind_amt m = do
                let DLPayAmt {..} = amta
                let mki f = do
                      i <- allocVarIdx
                      return (((<>) ("pv_" <> f) . show) i, i)
                (pv_net, pv_net_i) <- mki "net"
                let pv_net' = Atom pv_net
                (pv_ks, _) <- mki "ks"
                let pv_ks' = Atom pv_ks
                (pv_tok, _) <- mki "tok"
                let pv_tok' = Atom pv_tok
                smt <- ctxt_smt <$> ask
                let pv_net_dv = DLVar at (Just (at, pv_net)) (T_UInt UI_Word) pv_net_i
                let pv_net_let = SMTLet at pv_net_dv (DLV_Let DVC_Once pv_net_dv) Context $ SMTProgram (DLE_Arg at pa_net)
                smtDeclare smt pv_net (Atom "UInt") $ Just pv_net_let
                smtTypeInv (T_UInt UI_Word) $ pv_net'
                smtDeclare smt pv_tok (Atom "Token") Nothing
                smtTypeInv T_Token $ pv_tok'
                smtDeclare smt pv_ks (smtApply "Array" [Atom "Token", Atom "UInt"]) Nothing
                smtTypeInv (T_UInt UI_Word) $ smtApply "select" [pv_ks', pv_tok']
                let one v a = smtAssert =<< (smtEq v <$> smt_a at a)
                when should $ do
                  one pv_net' pa_net
                  forM_ pa_ks $ \(ka, kt) -> do
                    kt' <- smt_a at kt
                    one (smtApply "select" [pv_ks', kt']) ka
                local (\e -> e {ctxt_pay_amt = Just (pv_net', pv_ks')}) m
          let this_case = bind_from <> bind_msg <> bind_amt after
          when' <- smt_a at whena
          case should of
            True -> do
              r <-
                case when' of
                  -- It is possible that this is bad, because maybe there's
                  -- something wrong with the whole context
                  Atom "true" -> return (Sat, Nothing)
                  Atom "false" -> return (Unsat, Nothing)
                  _ -> smtAssert when' >> (checkUsing TWhen)
              case fst r of
                -- If this context is satisfiable, then whena can be true, so
                -- we need to evaluate it
                Sat -> this_case
                -- If this context is not-satisfiable, then whena will never
                -- be true, so if we go try to evaluate it, then we will fail
                -- all subsequent theorems
                Unsat -> return ()
                Unknown -> verify1r at [] TWhen when' Nothing r
            False -> this_case
    mapM_ ctxtNewScope $ timeout : map go (M.toList send)

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

_smtDefineTypes :: Solver -> S.Set DLType -> IO SMTTypeMap
_smtDefineTypes smt ts = do
  tnr <- newIORef (0 :: Int)
  let none _ = smtAndAll []
  tmr <-
    newIORef
      (M.fromList
         [ (T_Null, ("Null", none))
         , (T_Bool, ("Bool", none))
         , (T_UInt UI_Word, ("UInt", uintWord_inv))
         , (T_UInt UI_256, ("UInt", uint256_inv))
         , (T_BytesDyn, ("BytesDyn", none))
         , (T_StringDyn, ("StringDyn", none))
         , (T_Digest, ("Digest", none))
         , (T_Address, ("Address", none))
         , (T_Contract, ("Contract", none))
         , (T_Token, ("Token", none))
         ])
  let base = impossible "default"
  let bind_type :: DLType -> String -> IO SMTTypeInv
      bind_type t n =
        case t of
          T_Null -> base
          T_Bool -> base
          T_UInt _ -> base
          T_Bytes {} -> base
          T_BytesDyn -> base
          T_StringDyn -> base
          T_Digest -> base
          T_Address -> base
          T_Contract -> base
          T_Token -> base
          T_Array et sz -> do
            tni <- type_name et
            let tn = fst tni
            let tinv = snd tni
            SMT.ackCommand smt $ smtApply "define-sort" [Atom n, List [], smtApply "Array" [uint256_sort, Atom tn]]
            let z = "z_" ++ n
            void $ SMT.declare smt z $ Atom n
            let idxs = [0 .. (sz -1)]
            let idxses = map (smt_lt sb . DLL_Int sb UI_Word) idxs
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
            -- define helper function for DLE_DataTag
            let tag_f = n <> "_dataTag"
            let tagParam = n <> "_dataTagParam"
            let variants = map fst $ M.toAscList $ dataTypeMap t
            let variantCtors = map (\v -> n <> "_" <> v) variants
            let variantVs = map (\vCtor -> Atom $ vCtor <> "_pv") variantCtors
            let variantPs = map (\(vV, vC) -> List [Atom vC, vV])
                 $ zip variantVs variantCtors
            let variantCs = map (\(vP, i) -> List [vP, Atom $ show i])
                 $ zip variantPs ([0 ..] :: [Int])
            let tagBody = smtApply "match" [Atom tagParam, List variantCs]
            void $ SMT.defineFun smt tag_f [(tagParam, Atom n)] (Atom "UInt") tagBody

            let inv_f = n ++ "_inv"
            let x = Atom "x"
            let mkvar_inv (vn', (_, arg_inv)) = List [List [(Atom vn'), x], arg_inv x]
            let vars_inv = map mkvar_inv $ M.toList tm_nis
            let inv_defn = smtApply "match" [x, List vars_inv]
            void $ SMT.defineFun smt inv_f [("x", Atom n)] (Atom "Bool") inv_defn
            let inv se = smtApply inv_f [se]
            return inv
          T_Object tm ->
            bind_type (T_Struct $ M.toAscList tm) n
          T_Struct tml -> do
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
      type_name :: DLType -> IO (String, SMTTypeInv)
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

smt_eb :: DLExportBlock -> App ()
smt_eb (DLinExportBlock at margs b) = ctxtNewScope $
  freshAddrs $ do
    let args = map varLetVar $ fromMaybe [] margs
    forM_ args $ \arg ->
      pathAddUnbound at (Just arg) (Just $ SMTModel O_ExportArg)
    void $ smt_block b

_verify_smt :: Maybe Connector -> VerifySt -> Solver -> LLProg -> IO ()
_verify_smt mc ctxt_vst smt lp = do
  let mcs = case mc of
        Nothing -> "generic connector"
        Just c -> conName c <> " connector"
  putStrLn $ "Verifying for " <> T.unpack mcs
  ctxt_displayed <- newIORef mempty
  ctxt_v_to_dv <- newIORef mempty
  ctxt_typem <- _smtDefineTypes smt (cts lp)
  let ctxt_smt_typem = M.fromList $ map (\(k, (v, _)) -> (v, k)) $ M.toList ctxt_typem
  let ctxt_smt_con at_de cn =
        case mc of
          Just c -> smt_lt at_de $ conCons c cn
          Nothing -> Atom $ smtConstant cn
  let LLProg { llp_at = at, llp_opts = (LLOpts {..}), llp_parts = (SLParts {..}),
               llp_init = (DLInit {..}), llp_exports = dex, llp_step = s} = lp
  let pies_m = sps_ies
  let initMapInfo mi = do
        sm_c <- liftIO $ newCounter 0
        let sm_t = dlmi_tym mi
        let sm_kt = dlmi_kt mi
        sm_rs <- liftIO $ newIORef mempty
        sm_us <- liftIO $ newIORef SMR_New
        return $ SMTMapInfo {..}
  ctxt_maps <- mapM initMapInfo dli_maps
  let ctxt_addrs = M.fromSet (\p -> DLVar at (Just (at, bunpack p)) T_Address 0) $ M.keysSet pies_m
  let ctxt_while_invariants = []
  let ctxt_inv_mode = B_None
  let ctxt_path_constraint = []
  let ctxt_modem = Nothing
  let ctxt_smt = smt
  let ctxt_idx = llo_counter
  let ctxt_untrustworthyMaps = llo_untrustworthyMaps
  let ctxt_pay_amt = Nothing
  ctxt_smt_trace <- newIORef mempty
  ctxt_map_vars <- newIORef mempty
  flip runReaderT (SMTCtxt {..}) $ do
    let defineMap (mpv, SMTMapInfo {..}) = do
          mi <- liftIO $ incCounter sm_c
          smtMapDeclare at mpv mi $ SMTMapNew
          let mv = smtMapVar mpv mi
          t <- smtMapSort mpv
          na' <- smtMapMkMaybe at mpv Nothing
          let se = List [smtApply "as" [Atom "const", t], na']
          smtAssert $ smtEq (Atom mv) se
    mapM_ defineMap $ M.toList ctxt_maps
    case mc of
      Just _ -> mempty
      Nothing -> do
        flip mapM_ allConstants $ \ c -> do
              let con = smtConstant c
              let smlet = Just $ SMTCon con Nothing $ SMTProgram $ DLE_Arg at $ DLA_Constant c
              pathAddUnbound_v con (conTypeOf c) smlet
    -- FIXME it might make sense to assert that UInt_max is no less than
    -- something reasonable, like 64-bit?
    let defineIE who (v, it) =
          case it of
            IT_Fun {} -> mempty
            IT_UDFun {} -> mempty
            IT_Val itv ->
              pathAddUnbound_v (smtInteract who v) itv Nothing
    let definePIE (who, InteractEnv iem) = do
          pathAddUnbound_v (smtAddress who) T_Address Nothing
          mapM_ (defineIE who) $ M.toList iem
    mapM_ definePIE $ M.toList pies_m
    let smt_s_top mode = do
          liftIO $ putStrLn $ "  Verifying when " <> show (pretty mode)
          local (\e -> e {ctxt_modem = Just mode}) $ do
            forM_ dex smt_eb
            ctxtNewScope $ freshAddrs $ smt_s s
    let ms = [VM_Honest, VM_Dishonest RoleContract] -- <> (map (VM_Dishonest . RolePart) $ M.keys pies_m)
    mapM_ smt_s_top ms

hPutStrLn' :: Handle -> String -> IO ()
hPutStrLn' h s = do
  hPutStrLn h s
  hFlush h

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
        replicateM_ tab $ hPutStr logh "  "
      send_tag = "[send->] "
      -- recv_tag = "[<-recv]"
      logMessage m' = do
        let (which, m) = splitAt (length send_tag) m'
        let isSend = which == send_tag
        let isRecv = not isSend
        unless (isRecv && m == "success") $ do
          printTab
          when isRecv $ do
            hPutStr logh ";; "
          hPutStr logh $ m
          let f = hPutStrLn' logh
          case m of
            "(push 1 )" -> do
              logTab
              f " ;; {"
            "(pop 1 )" -> do
              logUntab
              f " ;; }"
            _ -> f ""
      close = do
        hClose logh
  return (close, Logger {..})

ribPush :: a -> Seq.Seq (Seq.Seq a) -> Seq.Seq (Seq.Seq a)
ribPush v = \case
  (Seq.:|>) l r -> (Seq.|>) l $ seqPush v r
  _ -> impossible $ "empty rib"

seqPush :: a -> Seq.Seq a -> Seq.Seq a
seqPush v = flip (Seq.|>) v

seqPop :: Seq.Seq a -> Seq.Seq a
seqPop = \case
  (Seq.:|>) x _ -> x
  _ -> impossible $ "empty seq"

newSolverSet :: VerifyOpts -> String -> [String] -> (String -> IO (IO (), Maybe Logger)) -> IO Solver
newSolverSet (VerifyOpts {..}) p a mkl = do
  let short_a = Atom $ "10"
  let long_a = Atom $ show $ vo_timeout
  let t_bin o x y = List [Atom o, x, y]
  let t_timeout = t_bin "try-for"
  let our_tactic = t_timeout (Atom "default")
  let reachCheckUsing t = List [Atom "check-sat-using", our_tactic t]
  subc <- newCounter 0
  rib <- newIORef (return mempty :: Seq.Seq (Seq.Seq SExpr))
  let doClose :: IO () -> IO a -> IO a
      doClose l e = do
        x <- e
        l
        return x
  (tlc, tl) <- mkl ""
  Solver tc te <- SMT.newSolver p a tl
  let c = \case
        List [Atom "reachCheckUsing", iptks] -> do
          let iptk :: Bool = fromSExpr iptks
          let after_ ac rs = do
                let r =
                      case rs of
                        Atom "sat" -> Sat
                        Atom "unsat" -> Unsat
                        Atom "unknown" -> Unknown
                        _ -> impossible $ "reachCheckUsing " <> show rs
                let f x y = (Just . x) <$> (ac $ List [Atom y])
                mrd <-
                  case (iptk, r) of
                    -- (True, Unsat) -> f RD_UnsatCore "get-unsat-core"
                    (False, Sat) -> f RD_Model "get-model"
                    _ -> return Nothing
                return $ show (r, mrd)
          let after ac rs = Atom <$> after_ ac rs
          tc (reachCheckUsing short_a) >>= \case
            Atom "unknown" -> do
              sn <- incCounter subc
              ss <- readIORef rib
              let doSS f = do
                    traverse_ (traverse_ f) ss
                    f (reachCheckUsing long_a)
              hr <- newIORef $ crc32 ("" :: B.ByteString)
              doSS $ \s -> do
                modifyIORef hr $ flip crc32Update (bpack $ show s)
              h <- readIORef hr
              let cdir = vo_dir </> "smt-cache"
              let cfile = cdir </> show h
              let after__ = return . Atom
              let w f = BL.unpack . f . BL.pack
              doesFileExist cfile >>= \case
                True -> do
                  (w decompress <$> readFile cfile) >>= after__
                False -> do
                  (slc, sl) <- mkl $ show sn
                  Solver sc se <- SMT.newSolver p a sl
                  r <- doSS sc
                  x <- after_ sc r
                  void $ doClose slc se
                  createDirectoryIfMissing True cdir
                  writeFile cfile $ (w compress) x
                  after__ x
            r -> after tc r
        s@(List [Atom "push", Atom "1"]) -> do
          modifyIORef rib $ seqPush mempty
          tc s
        s@(List [Atom "pop", Atom "1"]) -> do
          modifyIORef rib $ seqPop
          tc s
        s -> do
          modifyIORef rib $ ribPush s
          tc s
  let e = doClose tlc te
  return $ Solver c e

verify_smt :: VerifySt -> LLProg -> String -> [String] -> IO ExitCode
verify_smt vst lp prog args = do
  let vo@VerifyOpts {..} = vst_vo vst
  let logpMay = ($ "smt") <$> vo_out
  ulp <- unrollLoops lp
  case logpMay of
    Nothing -> return ()
    Just x -> writeFile (x <> ".ulp") (show $ pretty ulp)
  let mkLogger t = case fmap (<> t) logpMay of
        Just logp -> do
          (close, logpl) <- newFileLogger logp
          return (close, Just logpl)
        Nothing -> return (return (), Nothing)
  smt <- newSolverSet vo prog args mkLogger
  --unlessM (SMT.produceUnsatCores smt) $
  --  impossible "Prover doesn't support possible?"
  SMT.loadString smt smtStdLib
  let go mc = SMT.inNewScope smt $ _verify_smt mc vst smt ulp
  case vo_mvcs of
    Nothing -> go Nothing
    Just cs -> mapM_ (go . Just) cs
  zec <- SMT.stop smt
  return $ zec
