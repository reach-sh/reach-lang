module Reach.Verify.SMT (verify_smt) where

import qualified Control.Exception as Exn
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.Digest.CRC32
import Data.Foldable
import Data.IORef
import qualified Data.List as List
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.CollectTypes
import Reach.Connector
import Reach.Counter
import Reach.EmbeddedFiles
import Reach.Freshen
import Reach.Pretty
import Reach.Texty
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
uint256_le lhs rhs = smtApply ple [lhs, rhs]
  where
    ple = if use_bitvectors then "bvule" else "<="

uint256_lt :: SExpr -> SExpr -> SExpr
uint256_lt lhs rhs = smtApply plt [lhs, rhs]
  where
    plt = if use_bitvectors then "bvult" else "<"

uint256_inv :: SMTTypeInv
uint256_inv v = uint256_le uint256_zero v

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

smtNot :: SExpr -> SExpr
smtNot se = smtApply "not" [se]

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

data BindingOrigin
  = O_Join SLPart Bool
  | O_Msg SLPart (Maybe DLArg)
  | O_Pay SLPart (Maybe DLArg)
  | O_ClassJoin SLPart
  | O_ToConsensus
  | O_BuiltIn
  | O_Var
  | O_Interact
  | O_Expr DLExpr
  | O_Assignment
  | O_SwitchCase SLVar
  | O_ReduceVar
  deriving (Eq)

instance Show BindingOrigin where
  show bo =
    case bo of
      O_Join who False -> "a dishonest join from " ++ sp who
      O_Msg who Nothing -> "a dishonest message from " ++ sp who
      O_Pay who Nothing -> "a dishonest payment from " ++ sp who
      O_Join who True -> "an honest join from " ++ sp who
      O_Msg who (Just what) -> "an honest message from " ++ sp who ++ " of " ++ sp what
      O_Pay who (Just amt) -> "an honest payment from " ++ sp who ++ " of " ++ sp amt
      O_ClassJoin who -> "a join by a class member of " <> sp who
      O_ToConsensus -> "a consensus transfer"
      O_BuiltIn -> "builtin"
      O_Var -> "function return"
      O_Interact -> "interaction"
      O_Expr e -> "evaluating " ++ sp e
      O_Assignment -> "loop variable"
      O_SwitchCase vn -> "switch case " <> vn
      O_ReduceVar -> "map reduction"
    where
      sp :: Pretty a => a -> String
      sp = show . pretty

type SMTTypeInv = SExpr -> SExpr

type SMTTypeMap =
  M.Map DLType (String, SMTTypeInv)

type App = ReaderT SMTCtxt IO

instance Semigroup a => Semigroup (App a) where
  x <> y = liftM2 (<>) x y

instance Monoid a => Monoid (App a) where
  mempty = return mempty

type SMTComp = App ()

type BindingEnv = M.Map String (Maybe DLVar, SrcLoc, BindingOrigin, Maybe SExpr, Maybe DLExpr)

data SMTMapInfo = SMTMapInfo
  { sm_c :: Counter
  , sm_t :: DLType
  , sm_rs :: IORef [SMTMapRecordReduce]
  , sm_us :: IORef [SMTMapRecordUpdate]
  }

data SMTMapRecordReduce
  = SMR_Reduce Int DLVar DLArg DLVar DLVar LLBlock

instance Pretty SMTMapRecordReduce where
  pretty (SMR_Reduce _mri ans z b a f) =
    prettyReduce ans ("map" :: String) z b a f

data SMTMapRecordUpdate
  = SMR_Update SExpr SExpr SExpr

instance Pretty SMTMapRecordUpdate where
  pretty (SMR_Update ma fa' na') =
    viaShow ma <> "[" <> viaShow fa' <> "]" <+> "=" <+> viaShow na'

data SMTCtxt = SMTCtxt
  { ctxt_smt :: Solver
  , ctxt_idx :: Counter
  , ctxt_smt_con :: SrcLoc -> DLConstant -> SExpr
  , ctxt_typem :: SMTTypeMap
  , ctxt_vst :: VerifySt
  , ctxt_modem :: Maybe VerifyMode
  , ctxt_path_constraint :: [SExpr]
  , ctxt_bindingsr :: IORef BindingEnv
  , ctxt_while_invariant :: Maybe LLBlock
  , ctxt_displayed :: IORef (S.Set SExpr)
  , ctxt_vars_defdr :: IORef (S.Set String)
  , ctxt_maps :: M.Map DLMVar SMTMapInfo
  , ctxt_addrs :: M.Map SLPart DLVar
  , ctxt_v_to_dv :: IORef (M.Map String [DLVar])
  , ctxt_inv_mode :: BlockMode
  }

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
  ctxt_bindingsr' <- liftIO $ dupeIORef ctxt_bindingsr
  ctxt_vars_defdr' <- liftIO $ dupeIORef ctxt_vars_defdr
  let dupeMapInfo (SMTMapInfo {..}) = do
        sm_c' <- liftIO $ dupeCounter sm_c
        sm_rs' <- lift $ dupeIORef sm_rs
        sm_us' <- lift $ dupeIORef sm_us
        return $
          SMTMapInfo
            { sm_t = sm_t
            , sm_c = sm_c'
            , sm_rs = sm_rs'
            , sm_us = sm_us'
            }
  ctxt_maps' <- mapM dupeMapInfo ctxt_maps
  smtNewScope $
    local
      (\e ->
         e
           { ctxt_bindingsr = ctxt_bindingsr'
           , ctxt_vars_defdr = ctxt_vars_defdr'
           , ctxt_maps = ctxt_maps'
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
  DLC_UInt_max -> "dlc_UInt_max"

getVarName :: DLVar -> String
getVarName (DLVar _ _ _ i) = "v" ++ show i

smtVar :: DLVar -> App String
smtVar dv = do
  let name = getVarName dv
  v2dv <- ctxt_v_to_dv <$> ask
  liftIO $ modifyIORef v2dv $ M.insertWith (<>) name [dv]
  return $ name

smtTypeSort :: DLType -> App String
smtTypeSort t = do
  tm <- ctxt_typem <$> ask
  case M.lookup t tm of
    Just (s, _) -> return s
    Nothing -> impossible $ "smtTypeSort " <> show t

smtTypeInv :: DLType -> SExpr -> SMTComp
smtTypeInv t se = do
  tm <- ctxt_typem <$> ask
  case M.lookup t tm of
    Just (_, i) -> smtAssertCtxt $ i se
    Nothing -> impossible $ "smtTypeInv " <> show t

smtDeclare_v :: String -> DLType -> SMTComp
smtDeclare_v v t = do
  smt <- ctxt_smt <$> ask
  s <- smtTypeSort t
  liftIO $ void $ SMT.declare smt v $ Atom s
  smtTypeInv t $ Atom v

smtDeclare_v_memo :: String -> DLType -> App ()
smtDeclare_v_memo v t = do
  vds <- ctxt_vars_defdr <$> ask
  vars_defd <- liftIO $ readIORef vds
  case S.member v vars_defd of
    True -> return ()
    False -> do
      liftIO $ modifyIORef vds $ S.insert v
      smtDeclare_v v t

smtPrimOp :: PrimOp -> [DLArg] -> [SExpr] -> App SExpr
smtPrimOp p dargs =
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
        [ DLA_Literal (DLL_Bytes pn)
          , DLA_Literal (DLL_Bool isClass)
          , DLA_Literal (DLL_Int _ addrNum)
          ] -> \_ ->
            case isClass of
              False ->
                return $ Atom $ smtAddress pn
              True -> do
                should <- shouldSimulate pn
                case should of
                  True ->
                    Atom <$> smtCurrentAddress pn
                  False -> do
                    let addrVar = "classAddr" <> show addrNum
                    smtDeclare_v addrVar T_Address
                    return $ Atom addrVar
        se -> impossible $ "self address " <> show se
  where
    cant = impossible $ "Int doesn't support " ++ show p
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

data TheoremKind
  = TClaim ClaimType
  | TInvariant Bool
  | TWhenNotUnknown

instance Pretty TheoremKind where
  pretty = \case
    TClaim c -> pretty c
    TInvariant False -> "while invariant before loop"
    TInvariant True -> "while invariant after loop"
    TWhenNotUnknown -> "when is not unknown"

fmtAssert :: TheoremKind -> String
fmtAssert = \case
  TClaim c -> show $ pretty c
  TInvariant _ -> "invariant"
  TWhenNotUnknown -> "assert"

data ResultDesc
  = RD_UnsatCore [String]
  | RD_Model SExpr

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

-- Log every occurence of dlvars so we know what is used how many times
dlvOccurs :: [String] -> BindingEnv -> DLExpr -> [String]
dlvOccurs env bindings de =
  case de of
    DLE_Arg _ (DLA_Var dv) ->
      case v `List.elem` env of
        -- If we already expanded, mark that we've seen var again and go home
        True -> env'
        -- Otherwise, expand and mark all sub expressions
        False ->
          case v `M.lookup` bindings of
            Just (_, _, _, _, Just e) -> dlvOccurs env' bindings e
            _ -> env'
      where
        env' = v : env
        v = getVarName dv
    DLE_Arg {} -> env
    DLE_LArg at (DLLA_Array _ as) -> _recs at as
    DLE_LArg at (DLLA_Tuple as) -> _recs at as
    DLE_LArg at (DLLA_Obj as) -> _recs at $ map snd $ M.toList as
    DLE_LArg at (DLLA_Data _ _ a) -> _rec at a
    DLE_LArg at (DLLA_Struct kvs) -> _recs at $ map snd kvs
    DLE_Impossible {} -> env
    DLE_PrimOp at _ as -> _recs at as
    DLE_ArrayRef at x y -> _recs at [x, y]
    DLE_ArraySet at x y z -> _recs at [x, y, z]
    DLE_ArrayConcat at x y -> _recs at [x, y]
    DLE_ArrayZip at x y -> _recs at [x, y]
    DLE_TupleRef at a _ -> _rec at a
    DLE_ObjectRef at a _ -> _rec at a
    DLE_Interact at _ _ _ _ as -> _recs at as
    DLE_Digest at as -> _recs at as
    DLE_Claim at _ _ a _ -> _rec at a
    DLE_Transfer at x y -> _recs at [x, y]
    DLE_Wait at a -> _rec at a
    DLE_PartSet at _ a -> _rec at a
    DLE_MapRef at _ fa -> _rec at fa
    DLE_MapSet at _ fa na -> _recs at [fa, na]
    DLE_MapDel at _ fa -> _rec at fa
    DLE_Remote at _ _ _ av _ amta as -> _recs at $ av : amta : as
  where
    _recs at as = foldr (\a acc -> dlvOccurs acc bindings $ DLE_Arg at a) env as
    _rec at a = dlvOccurs env bindings $ DLE_Arg at a

displayDLAsJs :: M.Map String [DLVar] -> [(String, Either String DLExpr)] -> Bool -> DLExpr -> String
displayDLAsJs v2dv inlineCtxt nested = \case
  DLE_Arg _ (DLA_Interact p s _) -> List.intercalate "_" ["interact", B.unpack p, ps s]
  DLE_Arg _ a -> sub a
  DLE_LArg _ (DLLA_Array _ as) -> "array" <> args as
  DLE_LArg _ (DLLA_Tuple as) -> bracket $ commaSep (map sub as)
  DLE_LArg _ (DLLA_Obj env) ->
    curly $ commaSep (map (\(k, v) -> k <> ": " <> sub v) $ M.toList env)
  DLE_LArg _ (DLLA_Data _ _ a) -> sub a
  DLE_LArg _ (DLLA_Struct as) -> "struct" <> bracket (commaSep (map go as))
    where go (k, a) = bracket (k <> ", " <> sub a)
  d@(DLE_Impossible {}) -> ps d
  DLE_PrimOp _ IF_THEN_ELSE [c, t, el] ->
    mparen $ sub c <> " ? " <> sub t <> " : " <> sub el
  DLE_PrimOp _ o [a] -> mparen $ ps o <> sub a
  DLE_PrimOp _ o [a, b] ->
    case (o, sub a, sub b) of
      (ADD, "0", b') -> b'
      (ADD, a', "0") -> a'
      (_, a', b') -> mparen $ unwords [a', ps o, b']
  DLE_PrimOp _ o as -> ps o <> args as
  DLE_ArrayRef _ x y -> sub x <> bracket (sub y)
  DLE_ArraySet _ x y z -> "Array.set" <> args [x, y, z]
  DLE_ArrayConcat _ x y -> "Array.concat" <> args [x, y]
  DLE_ArrayZip _ x y -> "Array.zip" <> args [x, y]
  DLE_TupleRef _ a i -> sub a <> bracket (show i)
  DLE_ObjectRef _ a i -> sub a <> bracket i
  DLE_Interact _ _ pv f _ as -> "interact(" <> show pv <> ")." <> f <> args as
  DLE_Digest _ as -> "digest" <> args as
  DLE_Claim _ _ ty a m -> show ty <> paren (commaSep [sub a, show m])
  DLE_Transfer _ x y -> "transfer" <> paren (sub y) <> ".to" <> paren (sub x)
  DLE_Wait _ a -> "wait" <> paren (sub a)
  DLE_PartSet _ p a -> "Participant.set" <> paren (commaSep [show p, sub a])
  DLE_MapRef _ mv fa -> ps mv <> bracket (sub fa)
  DLE_MapSet _ mv fa na -> ps mv <> bracket (sub fa) <> " = " <> sub na
  DLE_MapDel _ mv fa -> "delete " <> ps mv <> bracket (sub fa)
  DLE_Remote _ _ rfm _ av f amta as -> "remote(" <> show av <> ", " <> show rfm <> ")." <> f <> ".pay" <> paren (sub amta) <> args as
  where
    commaSep = List.intercalate ", "
    args as = paren (commaSep (map sub as))
    ps o = show $ pretty o
    mparen = if nested then paren else id
    curly e = "{" <> e <> "}"
    paren e = "(" <> e <> ")"
    bracket e = "[" <> e <> "]"
    sub (DLA_Var v) =
      case getVarName v `List.lookup` inlineCtxt of
        Nothing -> show v
        Just (Right de) -> displayDLAsJs v2dv inlineCtxt True de
        Just (Left s) -> s
    sub e = ps e

displaySexpAsJs :: Bool -> SExpr -> String
displaySexpAsJs nested s =
  case s of
    Atom i -> i
    List (Atom w : rs)
      | "cons" `List.isSuffixOf` w ->
        "[" <> List.intercalate ", " (map r rs) <> "]"
    List xs -> lparen <> unwords (map r xs) <> rparen
  where
    lparen = if nested then "(" else ""
    rparen = if nested then ")" else ""
    r = displaySexpAsJs True

-- A computation is first stored into an unamed var, then `const`
-- assigned to a variable. So, choose the second variable inserted
-- into list, if it exists (This will be the user named variable).
-- If no user assigned var, display the name of the tmp var.
-- If the list is empty, it is an "unbound" var like an interact field.
getBindingOrigin :: String -> M.Map String [DLVar] -> String
getBindingOrigin v v2dv =
  case reverse $ fromMaybe [] $ M.lookup v v2dv of
    (_ : val : _) -> show val
    (val : _) -> show val
    [] -> v

subAllVars :: BindingEnv -> TheoremKind -> M.Map String (SExpr, SExpr) -> SExpr -> App String
subAllVars bindings tk pm (Atom ai) = do
  v2dv <- (liftIO . readIORef) =<< asks ctxt_v_to_dv
  case ai `M.lookup` bindings of
    Just (_, _, _, _, Just de) -> do
      let env = dlvOccurs [] bindings de
      let sortedEnv = List.group $ List.sort env
      -- Get variable/values to inline. Inline a DLExpr if available,
      -- or fallback to s-exp Atom value
      let inlineVars = List.foldr canInline [] sortedEnv
      let inlines = map (getInlineValue v2dv) inlineVars
      let toJs = displayDLAsJs v2dv inlines False
      -- Get let assignments
      let assignVars = List.foldr canAssign [] sortedEnv
      let assigns =
            List.foldr
              (\x acc ->
                 case x `M.lookup` bindings of
                   Just (_, _, _, _, Just dl) -> (x, Right dl) : acc
                   Just (_, _, _, Just se, _) -> (x, Left se) : acc
                   _ -> acc)
              []
              assignVars
      let assignStr =
            unlines $
              map
                (\(k, eds) ->
                   let kv = maybe "" (displaySexpAsJs False . snd) $ M.lookup k pm
                    in "  const " <> getBindingOrigin k v2dv <> " = " <> case eds of
                         Right v -> toJs v
                         Left v -> SMT.showsSExpr v ""
                         <> ";"
                         <> "\n  //    ^ would be "
                         <> kv)
                assigns
      let assertStr = "  " <> fmtAssert tk <> "(" <> toJs de <> ");"
      return $ assignStr <> assertStr
    -- Something like assert(false)
    _ -> return $ "  " <> fmtAssert tk <> "(" <> ai <> ");"
  where
    -- Variable can be inlined if it is used once or its value is a `DLArg`
    canInline [x] acc = x : acc
    canInline (x : _) acc =
      case x `M.lookup` bindings of
        Just (_, _, _, _, Just (DLE_Arg _ _)) -> x : acc
        _ -> acc
    canInline _ acc = acc

    -- Variable can be assigned if it is used many times and its value is not a `DLArg`
    canAssign (x : _ : _) acc =
      case x `M.lookup` bindings of
        Just (_, _, _, _, Just (DLE_Arg _ _)) -> acc
        _ -> x : acc
    canAssign _ acc = acc

    getInlineValue :: M.Map String [DLVar] -> String -> (String, Either String DLExpr)
    getInlineValue v2dv v =
      case v `M.lookup` bindings of
        Just (_, _, _, _, Just del) -> (v, Right del)
        Just (_, _, _, Just se, _) -> (v, Left $ SMT.showsSExpr se "")
        _ -> (v, Left $ getBindingOrigin v v2dv)
subAllVars _ _ _ _ = impossible "subAllVars: expected Atom"

--- FYI, the last version that had Dan's display code was
--- https://github.com/reach-sh/reach-lang/blob/ab15ea9bdb0ef1603d97212c51bb7dcbbde879a6/hs/src/Reach/Verify/SMT.hs

display_fail :: SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> Bool -> Maybe ResultDesc -> SMTComp
display_fail tat f tk tse mmsg repeated mrd = do
  v2dv <- (liftIO . readIORef) =<< (ctxt_v_to_dv <$> ask)
  let iputStrLn = liftIO . putStrLn
  cwd <- liftIO $ getCurrentDirectory
  iputStrLn $ "Verification failed:"
  mode <- ctxt_mode
  iputStrLn $ "  when " ++ (show $ pretty mode)
  iputStrLn $ "  of theorem: " ++ (show $ pretty tk)
  case mmsg of
    Nothing -> mempty
    Just msg -> do
      iputStrLn $ "  msg: " <> show msg
  iputStrLn $ redactAbsStr cwd $ "  at " ++ show tat
  mapM_ (iputStrLn . ("  " ++) . show) f
  iputStrLn $ ""
  case repeated of
    True -> do
      --- FIXME have an option to force these to display
      iputStrLn $ "  (details omitted on repeat)"
    False -> do
      --- FIXME Another way to think about this is to take `tse` and fully
      --- substitute everything that came from the program (the "context"
      --- below) and then just show the remaining variables found by the
      --- model.
      bindingsm <- (liftIO . readIORef) =<< (ctxt_bindingsr <$> ask)
      iputStrLn $ "  // Violation witness"
      let pm =
            case mrd of
              Nothing ->
                mempty
              Just (RD_UnsatCore _uc) -> do
                --- FIXME Do something useful here
                mempty
              Just (RD_Model m) -> do
                parseModel m
      let show_vars :: (S.Set String) -> (Seq.Seq String) -> App ()
          show_vars shown = \case
            Seq.Empty -> return ()
            (v0 Seq.:<| q') -> do
              v0vars <-
                case M.lookup v0 bindingsm of
                  Nothing ->
                    return $ mempty
                  Just (_, at, bo, mvse, _) -> do
                    let this se =
                          [("  const " ++ getBindingOrigin v0 v2dv ++ " = " ++ (displaySexpAsJs False se) ++ ";")]
                            ++ (map
                                  (redactAbsStr cwd)
                                  [ ("  //    ^ from " ++ show bo ++ " at " ++ show at)
                                  ])
                    case mvse of
                      Nothing ->
                        --- FIXME It might be useful to do `get-value` rather than parse
                        case M.lookup v0 pm of
                          Nothing ->
                            return $ mempty
                          Just (_ty, se) -> do
                            mapM_ iputStrLn (this se)
                            return $ seVars se
                      Just se ->
                        return $ seVars se
              let nvars = S.difference v0vars shown
              let shown' = S.union shown nvars
              let new_q = set_to_seq nvars
              let q'' = q' <> new_q
              show_vars shown' q''
      let tse_vars = seVars tse
      show_vars tse_vars $ set_to_seq $ tse_vars
      theorem_formalization <- subAllVars bindingsm tk pm tse
      iputStrLn ""
      iputStrLn $ "  // Theorem formalization"
      iputStrLn theorem_formalization
      iputStrLn ""

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

smtAssertCtxt :: SExpr -> SMTComp
smtAssertCtxt se = smtAssert =<< smtAddPathConstraints se

-- Intercept failures to prevent showing "user error",
-- which is confusing to a Reach developer. The library
-- `fail`s if there's a problem with the compiler,
-- not a Reach program.
smtAssert :: SExpr -> SMTComp
smtAssert se = do
  smt <- ctxt_smt <$> ask
  liftIO $
    Exn.catch (do SMT.assert smt se) $
      \(e :: Exn.SomeException) ->
        impossible $ safeInit $ drop 12 $ show e

checkUsing :: App SMT.Result
checkUsing = do
  smt <- ctxt_smt <$> ask
  let our_tactic = List [Atom "then", Atom "simplify", Atom "qflia"]
  res <- liftIO $ SMT.command smt (List [Atom "check-sat-using", our_tactic])
  case res of
    Atom "unsat" -> return Unsat
    Atom "unknown" -> return Unknown
    Atom "sat" -> return Sat
    _ ->
      impossible $
        unlines
          [ "Unexpected result from the SMT solver:"
          , "  Expected: unsat, unknown, or sat"
          , "  Result: " ++ SMT.showsSExpr res ""
          ]

verify1 :: SrcLoc -> [SLCtxtFrame] -> TheoremKind -> SExpr -> Maybe B.ByteString -> SMTComp
verify1 at mf tk se mmsg = smtNewScope $ do
  flip forM_ smtAssert =<< (ctxt_path_constraint <$> ask)
  smtAssert $ if isPossible then se else smtNot se
  r <- checkUsing
  smt <- ctxt_smt <$> ask
  case isPossible of
    True ->
      case r of
        Unknown -> bad $ return Nothing
        Unsat ->
          bad $
            liftM (Just . RD_UnsatCore) $
              liftIO $ SMT.getUnsatCore smt
        Sat -> good
    False ->
      case r of
        Unknown -> bad $ return Nothing
        Unsat -> good
        Sat ->
          bad $
            liftM (Just . RD_Model) $
              liftIO $ SMT.command smt $ List [Atom "get-model"]
  where
    good = void $ (liftIO . incCounter . vst_res_succ) =<< (ctxt_vst <$> ask)
    bad mgetm = do
      mm <- mgetm
      dr <- ctxt_displayed <$> ask
      dspd <- liftIO $ readIORef dr
      display_fail at mf tk se mmsg (elem se dspd) mm
      liftIO $ modifyIORef dr $ S.insert se
      void $ (liftIO . incCounter . vst_res_fail) =<< (ctxt_vst <$> ask)
    isPossible =
      case tk of
        TClaim CT_Possible -> True
        _ -> False

pathAddUnbound_v :: Maybe DLVar -> SrcLoc -> String -> DLType -> BindingOrigin -> SMTComp
pathAddUnbound_v mdv at_dv v t bo = do
  smtDeclare_v v t
  ctxt <- ask
  liftIO $
    modifyIORef (ctxt_bindingsr ctxt) $
      M.insert v (mdv, at_dv, bo, Nothing, Nothing)

pathAddBound_v :: Maybe DLVar -> SrcLoc -> String -> DLType -> BindingOrigin -> Maybe DLExpr -> SExpr -> SMTComp
pathAddBound_v mdv at_dv v t bo de se = do
  smtDeclare_v v t
  --- Note: We don't use smtAssertCtxt because variables are global, so
  --- this variable isn't affected by the path.
  smtAssert (smtEq (Atom $ v) se)
  ctxt <- ask
  liftIO $
    modifyIORef (ctxt_bindingsr ctxt) $
      M.insert v (mdv, at_dv, bo, Just se, de)

pathAddUnbound :: SrcLoc -> Maybe DLVar -> BindingOrigin -> SMTComp
pathAddUnbound _ Nothing _ = mempty
pathAddUnbound at_dv (Just dv) bo = do
  let DLVar _ _ t _ = dv
  v <- smtVar dv
  pathAddUnbound_v (Just dv) at_dv v t bo

pathAddBound :: SrcLoc -> Maybe DLVar -> BindingOrigin -> Maybe DLExpr -> SExpr -> SMTComp
pathAddBound _ Nothing _ _ _ = mempty
pathAddBound at_dv (Just dv) bo de se = do
  let DLVar _ _ t _ = dv
  v <- smtVar dv
  pathAddBound_v (Just dv) at_dv v t bo de se

smtMapVar :: DLMVar -> Int -> String
smtMapVar (DLMVar mi) ri = "map" <> show mi <> "_" <> show ri

smtMapRefresh :: App ()
smtMapRefresh = do
  ms <- ctxt_maps <$> ask
  let go (mpv, SMTMapInfo {..}) = do
        mi' <- liftIO $ incCounter sm_c
        smtMapDeclare mpv mi'
        liftIO $ writeIORef sm_rs $ mempty
        liftIO $ writeIORef sm_us $ mempty
  mapM_ go $ M.toList ms

smtMapLookupC :: DLMVar -> App SMTMapInfo
smtMapLookupC mpv = do
  ms <- ctxt_maps <$> ask
  case M.lookup mpv ms of
    Just x -> return $ x
    Nothing -> impossible $ "smtMapLookupC unknown map"

smtMapDeclare :: DLMVar -> Int -> App ()
smtMapDeclare mpv mi = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  let mv = smtMapVar mpv mi
  t_addr' <- smtTypeSort $ T_Address
  sm_t' <- smtTypeSort sm_t
  let t = smtApply "Array" [Atom t_addr', Atom sm_t']
  smt <- ctxt_smt <$> ask
  liftIO $ void $ SMT.declare smt mv t

smtMapLookup :: DLMVar -> App SExpr
smtMapLookup mpv = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  mi <- liftIO $ readCounter sm_c
  return $ Atom $ smtMapVar mpv (mi - 1)

smtMapUpdate :: SrcLoc -> DLMVar -> DLArg -> Maybe DLArg -> SMTComp
smtMapUpdate at mpv fa mna = do
  fa' <- smt_a at fa
  SMTMapInfo {..} <- smtMapLookupC mpv
  let mkna = DLLA_Data $ dataTypeMap sm_t
  let na = case mna of
        Just x -> mkna "Some" x
        Nothing -> mkna "None" $ DLA_Literal DLL_Null
  na' <- smt_la at na
  mi' <- liftIO $ incCounter sm_c
  let mi = mi' - 1
  smtMapDeclare mpv mi'
  let mv = smtMapVar mpv mi
  let ma = Atom mv
  smtMapRecordUpdate mpv $ SMR_Update ma fa' na'
  let mv' = smtMapVar mpv mi'
  let se = smtApply "store" [ma, fa', na']
  smtAssert $ smtEq (Atom mv') se

smtMapRecordReduce :: DLMVar -> SMTMapRecordReduce -> App ()
smtMapRecordReduce mpv r = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  liftIO $ modifyIORef sm_rs $ (r :)

smtMapRecordUpdate :: DLMVar -> SMTMapRecordUpdate -> App ()
smtMapRecordUpdate mpv r = do
  SMTMapInfo {..} <- smtMapLookupC mpv
  liftIO $ modifyIORef sm_us $ (r :)

smtMapReviewRecord :: DLMVar -> (SMTMapInfo -> IORef a) -> App a
smtMapReviewRecord mpv sm_x = do
  mi <- smtMapLookupC mpv
  liftIO $ readIORef $ sm_x mi

smt_freshen :: LLBlock -> [DLVar] -> App (LLBlock, [DLVar])
smt_freshen x vs = do
  c <- ctxt_idx <$> ask
  liftIO $ freshen_ c x vs

smtMapReduceApply :: SrcLoc -> DLVar -> DLVar -> LLBlock -> App (SExpr, SExpr, App SExpr)
smtMapReduceApply at b a f = do
  (f', b_f, a_f) <-
    smt_freshen f [b, a] >>= \case
      (f', [b_f, a_f]) -> return (f', b_f, a_f)
      _ -> impossible "smt_freshen bad"
  b' <- smt_v at b_f
  pathAddUnbound at (Just b_f) O_ReduceVar
  a' <- smt_v at a_f
  pathAddUnbound at (Just a_f) O_ReduceVar
  let call_f' = smt_block f'
  return $ (b', a', call_f')

smtMapReviewRecordRef :: SrcLoc -> DLMVar -> SExpr -> DLVar -> App ()
smtMapReviewRecordRef at x fse res = do
  us <- smtMapReviewRecord x sm_us
  -- We only learn something about what we've read from the map via the
  -- reduction if this field has not been modified, so we add negative path
  -- constraints and then apply the reduction function
  let go_u (SMR_Update _ fa' _) more =
        smtNewPathConstraint (smtNot $ smtEq fa' fse) . more
  let add_us_constraints = foldr go_u id us
  rs <- smtMapReviewRecord x sm_rs
  res' <- smt_v at res
  add_us_constraints $
    forM_ rs $ \(SMR_Reduce _ ans _ b a f) -> do
      ans' <- smt_v at ans
      (_, a', f') <- smtMapReduceApply at b a f
      smtAssertCtxt $ smtEq a' res'
      fres' <- f'
      smtAssertCtxt $ smtEq ans' fres'

smtMapReviewRecordReduce :: SrcLoc -> Int -> DLVar -> DLMVar -> DLArg -> DLVar -> DLVar -> LLBlock -> App ()
smtMapReviewRecordReduce at mri ans x z b a f = do
  rs <- smtMapReviewRecord x sm_rs
  let look (SMR_Reduce mri' ans' _ _ _ _) =
        if mri == mri' then Just ans' else Nothing
  let firstJusts = listToMaybe . catMaybes
  z' <-
    case firstJusts $ map look rs of
      Just oldAns ->
        Atom <$> smtVar oldAns
      Nothing ->
        smt_a at z
  -- We go through each one of the updates and inline the computation of the
  -- reduction function back to the last known value, which is either z in the
  -- beginning or the last value
  --
  -- NOTE: A question remains: what if we have two loops in a row that use
  -- different reductions? How will we be able to relate one to the next? I
  -- don't know and don't have a test case right now.
  let go (SMR_Update ma fa' na') z'0 = do
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
  z'' <- foldrM go z' =<< smtMapReviewRecord x sm_us
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
  let cons as = smtApply (s ++ "_cons") <$> mapM (smt_a at_de) as
  case dla of
    DLLA_Array _ as -> cons as
    DLLA_Tuple as -> cons as
    DLLA_Obj m -> cons $ M.elems m
    DLLA_Data _ vn vv -> do
      vv' <- smt_a at_de vv
      return $ smtApply (s ++ "_" ++ vn) [vv']
    DLLA_Struct kvs -> cons $ map snd kvs

smt_e :: SrcLoc -> Maybe DLVar -> DLExpr -> SMTComp
smt_e at_dv mdv de = do
  case de of
    DLE_Arg at da -> bound at =<< smt_a at da
    DLE_LArg at dla -> bound at =<< smt_la at dla
    DLE_Impossible _ _ ->
      pathAddUnbound at_dv mdv bo
    DLE_PrimOp at cp args -> do
      args' <- mapM (smt_a at) args
      bound at =<< smtPrimOp cp args args'
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
    DLE_ArrayZip {} ->
      --- FIXME: This might be possible to do by using `map`
      impossible "array_zip"
    DLE_TupleRef at arr_da i -> do
      let t = argTypeOf arr_da
      s <- smtTypeSort t
      arr_da' <- smt_a at arr_da
      bound at $ smtApply (s ++ "_elem" ++ show i) [arr_da']
    DLE_ObjectRef at obj_da f -> do
      let t = argTypeOf obj_da
      s <- smtTypeSort t
      obj_da' <- smt_a at obj_da
      bound at $ smtApply (s ++ "_" ++ f) [obj_da']
    DLE_Interact at _ _ _ _ _ ->
      pathAddUnbound at mdv bo
    DLE_Digest at args -> do
      args' <- smtDigestCombine at args
      bound at $ smtApply "digest" [args']
    DLE_Claim at f ct ca mmsg -> do
      ca' <- smt_a at ca
      let check_m = verify1 at f (TClaim ct) ca' mmsg
      let assert_m = smtAssertCtxt ca'
      case ct of
        CT_Assert -> check_m
        CT_Assume _ -> assert_m
        CT_Require ->
          ctxt_mode >>= \case
            VM_Honest -> check_m
            VM_Dishonest {} -> assert_m
        CT_Possible -> check_m
        CT_Unknowable {} -> mempty
    DLE_Transfer {} ->
      mempty
    DLE_Wait {} ->
      mempty
    DLE_PartSet at who a -> do
      bound at =<< smt_a at a
      sim <- shouldSimulate who
      case (mdv, sim) of
        (Just psv, True) -> do
          psv' <- smtVar psv
          smtAssertCtxt (smtEq (Atom psv') (Atom $ smtAddress who))
        _ ->
          mempty
    DLE_MapRef at mpv fa -> do
      ma <- smtMapLookup mpv
      fa' <- smt_a at fa
      bound at $ smtApply "select" [ma, fa']
      forM_ mdv $ smtMapReviewRecordRef at mpv fa'
    DLE_MapSet at mpv fa na ->
      smtMapUpdate at mpv fa $ Just na
    DLE_MapDel at mpv fa ->
      smtMapUpdate at mpv fa $ Nothing
    DLE_Remote at _ _ _ _ _ _ _ ->
      pathAddUnbound at mdv bo
  where
    bo = O_Expr de
    bound at = pathAddBound at mdv bo (Just de)

data SwitchMode
  = SM_Local
  | SM_Consensus

smtSwitch :: SwitchMode -> SrcLoc -> DLVar -> SwitchCases a -> (a -> SMTComp) -> SMTComp
smtSwitch sm at ov csm iter = do
  let ova = DLA_Var ov
  let ovt = argTypeOf ova
  let ovtm = case ovt of
        T_Data m -> m
        _ -> impossible "switch"
  ovp <- smt_a at ova
  let cm1 (vn, (mov', l)) = do
        ov_s <- smtVar ov
        let vnv = ov_s <> "_vn_" <> vn
        let vt = ovtm M.! vn
        let (ov'p_m, get_ov'p) =
              case mov' of
                Just ov' ->
                  ( mempty
                  , smt_la at $ DLLA_Data ovtm vn $ DLA_Var ov'
                  )
                -- Note It would be nice to ensure that this is always a Just
                -- and then make it so that EPP can remove them if they aren't
                -- actually used
                Nothing ->
                  ( smtDeclare_v_memo vnv vt
                  , smtTypeSort ovt >>= \s -> (return $ smtApply (s <> "_" <> vn) [Atom vnv])
                  )
        ov'p <- get_ov'p
        let eqc = smtEq ovp ov'p
        let udef_m = ov'p_m <> pathAddUnbound at mov' (O_SwitchCase vn)
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

smt_m :: LLCommon -> SMTComp
smt_m = \case
  DL_Nop _ -> mempty
  DL_Let at mdv de -> smt_e at mdv de
  DL_Var at dv -> pathAddUnbound at (Just dv) O_Var
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
  DL_LocalIf at ca t f -> do
    ca_se <- smt_a at ca
    let with_f = smtNewPathConstraint $ smtNot ca_se
    let with_t = smtNewPathConstraint $ ca_se
    with_t (smt_l t) <> with_f (smt_l f)
  DL_LocalSwitch at ov csm ->
    smtSwitch SM_Local at ov csm smt_l
  DL_MapReduce at mri ans x z b a f -> do
    pathAddUnbound at (Just ans) O_ReduceVar
    (ctxt_inv_mode <$> ask) >>= \case
      B_Assume _ -> do
        smtMapRecordReduce x $ SMR_Reduce mri ans z b a f
      B_Prove _ ->
        smtMapReviewRecordReduce at mri ans x z b a f
      _ -> impossible $ "Map.reduce outside invariant"

smt_l :: LLTail -> SMTComp
smt_l = \case
  DT_Return _ -> mempty
  DT_Com m k -> smt_m m <> smt_l k

smt_lm :: SLPart -> LLTail -> SMTComp
smt_lm who l =
  shouldSimulate who >>= \case
    True -> smt_l l
    False -> mempty

data BlockMode
  = B_Assume Bool
  | B_Prove Bool
  | B_None

smt_block :: LLBlock -> App SExpr
smt_block (DLinBlock at _ l da) = do
  smt_l l
  smt_a at da

smt_invblock :: BlockMode -> LLBlock -> SMTComp
smt_invblock bm b@(DLinBlock at f _ _) = do
  da' <-
    local (\e -> e {ctxt_inv_mode = bm}) $
      smt_block b
  case bm of
    B_Assume True -> smtAssertCtxt da'
    B_Assume False -> smtAssertCtxt (smtNot da')
    B_Prove inCont -> verify1 at f (TInvariant inCont) da' Nothing
    B_None -> mempty

smt_while_jump :: Bool -> DLAssignment -> SMTComp
smt_while_jump vars_are_primed asn = do
  let DLAssignment asnm = asn
  inv <-
    (ctxt_while_invariant <$> ask) >>= \case
      Just x -> return $ x
      Nothing -> impossible "asn outside loop"
  let add_asn_lets m (DLinBlock at fs t ra) =
        DLinBlock at fs t' ra
        where
          go (v, a) t_ = DT_Com (DL_Let at (Just v) (DLE_Arg at a)) t_
          t' = foldr go t $ M.toList m
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
  smt_invblock (B_Prove vars_are_primed) inv'

smt_asn_def :: SrcLoc -> DLAssignment -> SMTComp
smt_asn_def at asn = mapM_ def1 $ M.keys asnm
  where
    DLAssignment asnm = asn
    def1 dv = pathAddUnbound at (Just dv) O_Assignment

freshAddrs :: App a -> App a
freshAddrs m = do
  idxr <- ctxt_idx <$> ask
  let go (DLVar at lab t _) = do
        dv <- DLVar at lab t <$> (liftIO $ incCounter idxr)
        pathAddUnbound at (Just dv) O_BuiltIn
        return dv
  addrs' <- mapM go =<< (ctxt_addrs <$> ask)
  local (\e -> e {ctxt_addrs = addrs'}) m

smtCurrentAddress :: SLPart -> App String
smtCurrentAddress who = do
  am <- ctxt_addrs <$> ask
  case M.lookup who am of
    Just x -> smtVar x
    Nothing -> impossible "smtCurrentAddress"

smt_n :: LLConsensus -> SMTComp
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
  LLC_FromConsensus _ _ s -> smt_s s
  LLC_While at asn inv cond body k ->
    mapM_ ctxtNewScope [before_m, loop_m, after_m]
    where
      with_inv = local (\e -> e {ctxt_while_invariant = Just inv})
      before_m = with_inv $ smt_while_jump False asn
      loop_m = do
        smtMapRefresh
        smt_asn_def at asn
        smt_invblock (B_Assume True) inv
        smt_invblock (B_Assume True) cond
        (with_inv $ smt_n body)
      after_m = do
        smtMapRefresh
        smt_asn_def at asn
        smt_invblock (B_Assume True) inv
        smt_invblock (B_Assume False) cond
        smt_n k
  LLC_Continue _at asn -> smt_while_jump True asn
  LLC_Only _at who loc k -> smt_lm who loc <> smt_n k

smt_s :: LLStep -> SMTComp
smt_s = \case
  LLS_Com m k -> smt_m m <> smt_s k
  LLS_Stop _at -> mempty
  LLS_Only _at who loc k -> smt_lm who loc <> smt_s k
  LLS_ToConsensus at send recv mtime -> do
    let (last_timemv, whov, msgvs, amtv, timev, next_n) = recv
    timev' <- smt_v at timev
    let timeout = case mtime of
          Nothing -> mempty
          Just (_delay_a, delay_s) -> smt_s delay_s
    let bind_time = pathAddUnbound at (Just timev) O_ToConsensus
    let order_time =
          case last_timemv of
            Nothing -> mempty
            Just last_timev -> do
              last_timev' <- smt_v at last_timev
              smtAssertCtxt $ uint256_lt last_timev' timev'
    let after = freshAddrs $ bind_time <> order_time <> smt_n next_n
    let go (from, (isClass, msgas, amta, whena)) = do
          should <- shouldSimulate from
          let maybe_pathAdd v bo_no bo_yes mde se =
                case should of
                  False -> pathAddUnbound at (Just v) bo_no
                  True -> pathAddBound at (Just v) bo_yes mde se
          let bind_from =
                case isClass of
                  True ->
                    case should of
                      False ->
                        pathAddUnbound at (Just whov) (O_ClassJoin from)
                      True -> do
                        from' <- smtCurrentAddress from
                        pathAddBound at (Just whov) (O_Join from True) Nothing (Atom $ from')
                  _ -> maybe_pathAdd whov (O_Join from False) (O_Join from True) Nothing (Atom $ smtAddress from)
          let bind_msg = zipWithM_ (\dv da -> maybe_pathAdd dv (O_Msg from Nothing) (O_Msg from $ Just da) (Just $ DLE_Arg at da) =<< (smt_a at da)) msgvs msgas
          let bind_amt = maybe_pathAdd amtv (O_Pay from Nothing) (O_Pay from $ Just amta) (Just $ DLE_Arg at amta) =<< (smt_a at amta)
          let this_case = bind_from <> bind_msg <> bind_amt <> after
          when' <- smt_a at whena
          case should of
            True -> do
              smtAssert $ when'
              r <- checkUsing
              case r of
                -- If this context is satisfiable, then whena can be true, so
                -- we need to evaluate it
                Sat -> this_case
                -- If this context is not-satisfiable, then whena will never
                -- be true, so if we go try to evaluate it, then we will fail
                -- all subsequent theorems
                Unsat -> return ()
                Unknown ->
                  verify1 at [] TWhenNotUnknown (Atom $ "false") Nothing
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
         , (T_UInt, ("UInt", uint256_inv))
         , (T_Digest, ("Digest", none))
         , (T_Address, ("Address", none))
         ])
  let base = impossible "default"
  let bind_type :: DLType -> String -> IO SMTTypeInv
      bind_type t n =
        case t of
          T_Null -> base
          T_Bool -> base
          T_UInt -> base
          T_Bytes {} -> base
          T_Digest -> base
          T_Address -> base
          T_Array et sz -> do
            tni <- type_name et
            let tn = fst tni
            let tinv = snd tni
            void $ SMT.command smt $ smtApply "define-sort" [Atom n, List [], smtApply "Array" [uint256_sort, Atom tn]]
            let z = "z_" ++ n
            void $ SMT.declare smt z $ Atom n
            let idxs = [0 .. (sz -1)]
            let idxses = map (smt_lt srcloc_builtin . DLL_Int srcloc_builtin) idxs
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

_verify_smt :: Maybe Connector -> VerifySt -> Solver -> LLProg -> IO ()
_verify_smt mc ctxt_vst smt lp = do
  let mcs = case mc of
        Nothing -> "generic connector"
        Just c -> conName c <> " connector"
  putStrLn $ "Verifying for " <> T.unpack mcs
  ctxt_displayed <- newIORef mempty
  ctxt_bindingsr <- newIORef mempty
  ctxt_vars_defdr <- newIORef mempty
  ctxt_v_to_dv <- newIORef mempty
  ctxt_typem <- _smtDefineTypes smt (cts lp)
  let ctxt_smt_con at_de cn =
        case mc of
          Just c -> smt_lt at_de $ conCons c cn
          Nothing -> Atom $ smtConstant cn
  let LLProg at (LLOpts {..}) (SLParts pies_m) (DLInit {..}) s = lp
  let initMapInfo (DLMapInfo {..}) = do
        sm_c <- liftIO $ newCounter 0
        let sm_t = maybeT dlmi_ty
        sm_rs <- liftIO $ newIORef mempty
        sm_us <- liftIO $ newIORef mempty
        return $ SMTMapInfo {..}
  ctxt_maps <- mapM initMapInfo dli_maps
  let ctxt_addrs = M.mapWithKey (\p _ -> DLVar at (Just (at, bunpack p)) T_Address 0) pies_m
  let ctxt_while_invariant = Nothing
  let ctxt_inv_mode = B_None
  let ctxt_path_constraint = []
  let ctxt_modem = Nothing
  let ctxt_smt = smt
  let ctxt_idx = llo_counter
  flip runReaderT (SMTCtxt {..}) $ do
    let defineMap (mpv, SMTMapInfo {..}) = do
          mi <- liftIO $ incCounter sm_c
          smtMapDeclare mpv mi
    mapM_ defineMap $ M.toList ctxt_maps
    case dli_ctimem of
      Nothing -> mempty
      Just ctimev -> pathAddUnbound at (Just ctimev) O_BuiltIn
    case mc of
      Just _ -> mempty
      Nothing ->
        pathAddUnbound_v Nothing at (smtConstant DLC_UInt_max) T_UInt O_BuiltIn
    -- FIXME it might make sense to assert that UInt_max is no less than
    -- something reasonable, like 64-bit?
    let defineIE who (v, it) =
          case it of
            IT_Fun {} -> mempty
            IT_Val itv ->
              pathAddUnbound_v Nothing at (smtInteract who v) itv O_Interact
    let definePIE (who, InteractEnv iem) = do
          pathAddUnbound_v Nothing at (smtAddress who) T_Address O_BuiltIn
          mapM_ (defineIE who) $ M.toList iem
    mapM_ definePIE $ M.toList pies_m
    let smt_s_top mode = do
          liftIO $ putStrLn $ "  Verifying when " <> show (pretty mode)
          local (\e -> e {ctxt_modem = Just mode}) $
            ctxtNewScope $ freshAddrs $ smt_s s
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
  ulp <- unrollLoops lp
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
