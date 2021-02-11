{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Eval.Core where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import Data.IORef
import Data.List (transpose, (\\))
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.SL
import Reach.Counter
import Reach.Eval.Error
import Reach.Eval.Types
import Reach.JSUtil
import Reach.Parser
import Reach.Util
import Safe (atMay)
import Text.ParserCombinators.Parsec.Number (numberValue)

--- New Types

type App = ReaderT Env IO

data Env = Env
  { e_id :: Counter
  , e_who :: Maybe SLPart
  , e_ios :: M.Map SLPart SLSSVal
  , e_at :: SrcLoc
  , e_sco :: SLScope
  , e_st :: IORef SLState
  , e_lifts :: IORef DLStmts
  , e_stack :: [SLCtxtFrame]
  , e_depth :: Int
  , e_dlo :: DLOpts
  , e_classes :: S.Set SLPart
  }

instance Semigroup a => Semigroup (App a) where
  x <> y = (<>) <$> x <*> y

instance Monoid a => Monoid (App a) where
  mempty = return mempty

-- XXX add something for SecurityLevel

withFrame :: SLCtxtFrame -> App a -> App a
withFrame f m = do
  d <- e_depth <$> ask
  when (d <= 0) $ expect_ $ Err_RecursionDepthLimit
  local
    (\e ->
       e
         { e_stack = f : e_stack e
         , e_depth = d - 1
         })
    m

locIOs :: M.Map SLPart SLSSVal -> App a -> App a
locIOs m = local (\e -> e {e_ios = m})

locDLO :: DLOpts -> App a -> App a
locDLO m = local (\e -> e {e_dlo = m})

locClasses :: S.Set SLPart -> App a -> App a
locClasses m = local (\e -> e {e_classes = m})

locWho :: SLPart -> App a -> App a
locWho w = local (\e -> e {e_who = Just w})

locAtf :: (SrcLoc -> SrcLoc) -> App a -> App a
locAtf f = local (\e -> e {e_at = f $ e_at e})

locAt :: SrcLoc -> App a -> App a
locAt = locAtf . const

withAt :: (SrcLoc -> a) -> App a
withAt f = do
  Env {..} <- ask
  return $ f e_at

locSco :: SLScope -> App a -> App a
locSco e_sco' = local (\e -> e {e_sco = e_sco'})

saveLifts :: DLStmts -> App ()
saveLifts ss = do
  Env {..} <- ask
  liftIO $ modifyIORef e_lifts $ (<> ss)

saveLift :: DLStmt -> App ()
saveLift = saveLifts . return

whenVerifyOverflow :: App () -> App ()
whenVerifyOverflow m = flip when m =<< (dlo_verifyOverflow . e_dlo <$> ask)

setSt :: SLState -> App ()
setSt x = do
  Env {..} <- ask
  liftIO $ writeIORef e_st x

readSt :: (SLState -> a) -> App a
readSt f = do
  Env {..} <- ask
  e_stv <- liftIO $ readIORef e_st
  return $ f e_stv

captureSt :: App a -> App (SLState, a)
captureSt m = do
  e_stv <- readSt id
  e_st' <- liftIO $ newIORef e_stv
  x <- local (\e -> e {e_st = e_st'}) m
  st <- liftIO $ readIORef e_st'
  return (st, x)

-- XXX this should be removed
locSt :: SLState -> App a -> App a
locSt x m = snd <$> (captureSt $ (setSt x >> m))

locStMode :: SLMode -> App a -> App a
locStMode x m = do
  st <- readSt id
  let st' = st {st_mode = x}
  locSt st' m

captureLifts :: App a -> App (DLStmts, a)
captureLifts m = do
  e_lifts' <- liftIO $ newIORef mempty
  x <- local (\e -> e {e_lifts = e_lifts'}) m
  ls <- liftIO $ readIORef e_lifts'
  return (ls, x)

captureRes :: App a -> App (SLRes a)
captureRes m = do
  (ls, (st, x)) <- captureLifts $ captureSt $ m
  return $ SLRes ls st x

mergeSt :: SLState -> App ()
mergeSt y = do
  x <- readSt id
  setSt =<< stMerge x y

unchangedSt :: App a -> App a
unchangedSt m = do
  (y, a) <- captureSt m
  mergeSt y
  return a

--- Old Types

data SLExits
  = NeverExits
  | AlwaysExits
  | MayExit
  deriving (Eq, Show)

type SLStmtRets = [(SrcLoc, Maybe Int, SLSVal, Bool)]

-- XXX move SLStmtRes in SLScope as an IORef
data SLStmtRes = SLStmtRes SLScope SLStmtRets

data SLAppRes = SLAppRes SLScope SLSVal

type SLObjEnv = M.Map SLVar (App SLSVal)

data ReturnStyle
  = RS_ImplicitNull
  | RS_NeedExplicit
  | RS_CannotReturn
  | RS_MayBeEmpty
  deriving (Eq, Show)

--- A scope is local to a single function body (i.e. sequence of
--- statements), so it is reset every function call.
data SLScope = SLScope
  { sco_ret :: Maybe Int
  , sco_must_ret :: ReturnStyle
  , sco_while_vars :: Maybe (M.Map SLVar DLVar)
  , -- One env per participant
    sco_penvs :: SLPartEnvs
  , -- One for the consensus
    sco_cenv :: SLEnv
  }
  deriving (Show)

data EnvInsertMode
  = AllowShadowing
  | DisallowShadowing
  | AllowShadowingSome (S.Set SLVar)
  | AllowShadowingRace (S.Set SLPart) (S.Set SLVar)

data SLRes a = SLRes DLStmts SLState a

type SLLibs = (M.Map ReachSource SLEnv)

type SLMod = (ReachSource, [JSModuleItem])

--- Utilities
expect_ :: (Show e, ErrorMessageForJson e, ErrorSuggestions e) => e -> App a
expect_ e = do
  Env {..} <- ask
  expect_throw (Just e_stack) e_at e

mkValType :: SLVal -> App SLValTy
mkValType v = do
  r <- typeOfM v
  return (v, fmap fst r)

expect_t :: (Show e, ErrorMessageForJson e, ErrorSuggestions e) => SLVal -> (SLValTy -> e) -> App a
expect_t v f = (expect_ . f) =<< mkValType v

expect_ts :: (Show e, ErrorMessageForJson e, ErrorSuggestions e) => [SLVal] -> ([SLValTy] -> e) -> App a
expect_ts vs f = (expect_ . f) =<< mapM mkValType vs

zipEq :: (Show e, ErrorMessageForJson e, ErrorSuggestions e) => (Int -> Int -> e) -> [a] -> [b] -> App [(a, b)]
zipEq ce x y =
  if lx == ly
    then return $ zip x y
    else expect_ $ ce lx ly
  where
    lx = length x
    ly = length y

ensure_public :: SLSVal -> App SLVal
ensure_public (lvl, v) =
  case lvl of
    Public -> return $ v
    Secret -> expect_t v $ Err_ExpectedPublic

-- | Certain idents are special and bypass the public/private
-- enforced naming convention.
isSpecialIdent :: SLVar -> Bool
isSpecialIdent "interact" = True
isSpecialIdent _ = False

-- | Secret idents start with _, but are not _.
isSecretIdent :: SLVar -> Bool
isSecretIdent ('_' : _ : _) = True
isSecretIdent _ = False

-- | The "_" never actually gets bound;
-- it is therefore only ident that may be "shadowed".
-- Secret idents must start with _.
-- Public idents must not start with _.
-- Special idents "interact" and "__decode_testing__" skip these rules.
env_insert_ :: HasCallStack => EnvInsertMode -> SLVar -> SLSSVal -> SLEnv -> App SLEnv
env_insert_ _ "_" _ env = return env
env_insert_ insMode k v env = case insMode of
  DisallowShadowing -> check
  AllowShadowing -> go
  AllowShadowingSome ok ->
    case S.member k ok of
      True -> go
      False -> check
  AllowShadowingRace {} -> impossible "env_insert_ race"
  where
    check :: App SLEnv
    check =
      case M.lookup k env of
        Nothing -> go
        Just v0 -> expect_ $ Err_Shadowed k v0 v
    go :: App SLEnv
    go = case v of
      -- Note: secret ident enforcement is limited to doOnly
      (SLSSVal _ Public _)
        | not (isSpecialIdent k) && isSecretIdent k ->
          expect_ $ Err_Eval_NotPublicIdent k
      _ -> return $ M.insert k v env

env_insert :: HasCallStack => SLVar -> SLSSVal -> SLEnv -> App SLEnv
env_insert = env_insert_ DisallowShadowing

env_insertp_ :: HasCallStack => EnvInsertMode -> SLEnv -> (SLVar, SLSSVal) -> App SLEnv
env_insertp_ imode = flip (uncurry (env_insert_ imode))

env_insertp :: HasCallStack => SLEnv -> (SLVar, SLSSVal) -> App SLEnv
env_insertp = env_insertp_ DisallowShadowing

env_merge_ :: HasCallStack => EnvInsertMode -> SLEnv -> SLEnv -> App SLEnv
env_merge_ imode left righte = foldlM (env_insertp_ imode) left $ M.toList righte

env_merge :: HasCallStack => SLEnv -> SLEnv -> App SLEnv
env_merge = env_merge_ DisallowShadowing

-- | The "_" ident may never be looked up.
env_lookup :: LookupCtx -> SLVar -> SLEnv -> App SLSSVal
env_lookup _ "_" _ = expect_ $ Err_Eval_LookupUnderscore
env_lookup ctx x env =
  case M.lookup x env of
    Just v -> return $ v
    Nothing ->
      expect_ $ Err_Eval_UnboundId ctx x $ M.keys $ M.filter (not . isKwd) env

isKwd :: SLSSVal -> Bool
isKwd (SLSSVal _ _ (SLV_Kwd _)) = True
isKwd _ = False

m_fromList_public_builtin :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public_builtin = m_fromList_public srcloc_builtin

base_env :: SLEnv
base_env =
  m_fromList_public_builtin $
    [ ("makeEnum", SLV_Prim SLPrim_makeEnum)
    , ("declassify", SLV_Prim SLPrim_declassify)
    , ("commit", SLV_Prim SLPrim_commit)
    , ("digest", SLV_Prim SLPrim_digest)
    , ("transfer", SLV_Prim SLPrim_transfer)
    , ("assert", SLV_Prim $ SLPrim_claim CT_Assert)
    , ("assume", SLV_Prim $ SLPrim_claim CT_Assume)
    , ("require", SLV_Prim $ SLPrim_claim CT_Require)
    , ("possible", SLV_Prim $ SLPrim_claim CT_Possible)
    , ("unknowable", SLV_Form $ SLForm_unknowable)
    , ("balance", SLV_Prim $ SLPrim_fluid_read $ FV_balance)
    , ("lastConsensusTime", SLV_Prim $ SLPrim_lastConsensusTime)
    , ("Digest", SLV_Type ST_Digest)
    , ("Null", SLV_Type ST_Null)
    , ("Bool", SLV_Type ST_Bool)
    , ("UInt", SLV_Type ST_UInt)
    , ("Bytes", SLV_Prim SLPrim_Bytes)
    , ("Address", SLV_Type ST_Address)
    , ("forall", SLV_Prim SLPrim_forall)
    , ("Data", SLV_Prim SLPrim_Data)
    , ("Array", SLV_Prim SLPrim_Array)
    , ("array", SLV_Prim SLPrim_array)
    , ("Tuple", SLV_Prim SLPrim_Tuple)
    , ("Object", SLV_Prim SLPrim_Object)
    , ("Fun", SLV_Prim SLPrim_Fun)
    , ("exit", SLV_Prim SLPrim_exit)
    , ("each", SLV_Form SLForm_each)
    , ("intEq", SLV_Prim $ SLPrim_op PEQ)
    , ("polyEq", SLV_Prim $ SLPrim_op PEQ)
    , --    , ("bytesEq", SLV_Prim $ SLPrim_op BYTES_EQ)
      ("digestEq", SLV_Prim $ SLPrim_op DIGEST_EQ)
    , ("addressEq", SLV_Prim $ SLPrim_op ADDRESS_EQ)
    , ("isType", SLV_Prim SLPrim_is_type)
    , ("typeEq", SLV_Prim SLPrim_type_eq)
    , ("typeOf", SLV_Prim SLPrim_typeOf)
    , ("wait", SLV_Form SLForm_wait)
    , ("race", SLV_Prim SLPrim_race)
    , ("fork", SLV_Form SLForm_fork)
    , ("parallel_reduce", SLV_Form SLForm_parallel_reduce)
    , ( "Participant"
      , (SLV_Object srcloc_builtin (Just $ "Participant") $
           m_fromList_public_builtin
             [("set", SLV_Prim SLPrim_part_set)])
      )
    , ( "Reach"
      , (SLV_Object srcloc_builtin (Just $ "Reach") $
           m_fromList_public_builtin
             [("App", SLV_Form SLForm_App)])
      )
    ]
      -- Add language keywords to env to prevent variables from using names.
      <> map (\t -> (show t, SLV_Kwd t)) allKeywords

jsClo :: HasCallStack => SrcLoc -> String -> String -> (M.Map SLVar SLVal) -> SLVal
jsClo at name js env_ = SLV_Clo at (Just name) args body cloenv
  where
    cloenv = SLCloEnv mempty env
    env = M.map (SLSSVal at Public) env_
    (args, body) =
      case readJsExpr js of
        JSArrowExpression aformals _ bodys -> (a_, b_)
          where
            b_ = jsArrowStmtToBlock bodys
            a_ = parseJSArrowFormals at aformals
        _ -> impossible "not arrow"

dtypeMeetM :: (SrcLoc, DLType) -> (SrcLoc, DLType) -> Maybe DLType
dtypeMeetM (_, T_Bytes xz) (_, T_Bytes yz) =
  Just $ T_Bytes (max xz yz)
dtypeMeetM _x@(_, xt) _y@(_, yt) =
  case xt == yt of
    True -> Just xt
    False -> Nothing

dtypeMeet :: (SrcLoc, DLType) -> (SrcLoc, DLType) -> App DLType
dtypeMeet x y = do
  case dtypeMeetM x y of
    Just t -> return t
    Nothing -> expect_ $ Err_dTypeMeets_Mismatch x y

dtypeMeets :: [(SrcLoc, DLType)] -> App DLType
dtypeMeets = \case
  [] -> expect_ $ Err_TypeMeets_None
  [(_, xt)] -> return $ xt
  [x, y] -> dtypeMeet x y
  (xa, xt) : more -> dtypeMeet (xa, xt) . ((,) xa) =<< dtypeMeets more

typeMeet :: (SrcLoc, SLType) -> (SrcLoc, SLType) -> App SLType
typeMeet (_, ST_Bytes xz) (_, ST_Bytes yz) =
  return $ ST_Bytes (max xz yz)
typeMeet x@(_, xt) y@(_, yt) =
  case xt == yt of
    True -> return $ xt
    False -> expect_ $ Err_TypeMeets_Mismatch x y

-- XXX remove the need for this
type TypeEnv = M.Map SLVar (IORef (Maybe SLType))

typeSubst :: TypeEnv -> SLType -> App SLType
typeSubst env ty =
  case ty of
    ST_Fun doms rng -> do
      doms' <- mapM iter doms
      rng' <- typeSubst env rng
      return $ ST_Fun doms' rng'
    ST_Array t sz -> do
      t' <- iter t
      return $ ST_Array t' sz
    ST_Tuple ts -> do
      ts' <- mapM iter ts
      return $ ST_Tuple ts'
    ST_Object oenv -> do
      oenv' <- mapM iter oenv
      return $ ST_Object oenv'
    ST_Var var ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeSubst: unbound type variable"
        Just var_ref -> do
          mvar <- liftIO $ readIORef var_ref
          case mvar of
            Nothing ->
              impossible $ "typeSubst: uninstantiated type variable"
            Just var_ty ->
              iter var_ty
    ST_Forall _ _ ->
      impossible $ "typeSubst: forall in output"
    _ -> return ty
  where
    iter = typeSubst env

typeCheck_help :: TypeEnv -> SLType -> SLVal -> SLType -> a -> App a
typeCheck_help env ty val val_ty res =
  case (val_ty, ty) of
    (ST_Var _, _) ->
      impossible $ "typeCheck: value has type var: " ++ show val
    (_, ST_Var var) ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeCheck: unbound type variable"
        Just var_ref -> do
          mvar_ty <- liftIO $ readIORef var_ref
          case mvar_ty of
            Nothing -> do
              liftIO $ writeIORef var_ref (Just val_ty)
              return res
            Just var_ty ->
              typeCheck_help env var_ty val val_ty res
    (_, _) -> do
      at <- withAt id
      _ <- typeMeet (at, val_ty) (at, ty)
      return res

slToDL :: SLVal -> App (Maybe DLArgExpr)
slToDL = \case
  SLV_Null _ _ -> yes $ DLAE_Arg $ DLA_Literal $ DLL_Null
  SLV_Bool _ b -> yes $ DLAE_Arg $ DLA_Literal $ DLL_Bool b
  SLV_Int at i -> yes $ DLAE_Arg $ DLA_Literal $ DLL_Int at i
  SLV_Bytes _ bs -> yes $ DLAE_Arg $ DLA_Literal $ DLL_Bytes bs
  SLV_Array _ t vs -> do
    mds <- all_just <$> mapM slToDL vs
    let dt = st2dt t
    return $ DLAE_Array <$> dt <*> mds
  SLV_Tuple _ vs -> do
    mds <- all_just <$> mapM slToDL vs
    return $ DLAE_Tuple <$> mds
  SLV_Object _ _ fenv -> do
    let f :: (SLVar, SLSSVal) -> App (Maybe (SLVar, DLArgExpr))
        f (x, y) = do
          y' <- slToDL $ sss_val y
          return $ (,) x <$> y'
    denvl <- all_just <$> (mapM f $ M.toList fenv)
    return $ DLAE_Obj . M.fromList <$> denvl
  SLV_Clo _ _ _ _ _ -> no
  SLV_Data _ t vn sv -> do
    let dt = traverse st2dt t
    msv <- slToDL sv
    return $ DLAE_Data <$> dt <*> pure vn <*> msv
  SLV_DLC c -> yes $ DLAE_Arg $ DLA_Constant c
  SLV_DLVar dv -> yes $ DLAE_Arg $ DLA_Var dv
  SLV_Type _ -> no
  SLV_Connector _ -> no
  SLV_Participant _ who _ mdv -> do
    pdvs <- readSt st_pdvs
    case (M.lookup who pdvs) <|> mdv of
      Just dv -> yes $ DLAE_Arg $ DLA_Var dv
      Nothing -> no
  SLV_RaceParticipant {} -> no
  SLV_Prim _ -> no
  SLV_Form _ -> no
  SLV_Kwd _ -> no
  where
    yes = return . Just
    no = return Nothing

typeOfM :: SLVal -> App (Maybe (DLType, DLArgExpr))
typeOfM v = do
  mdae <- slToDL v
  return $ fmap (\x -> (argExprTypeOf x, x)) mdae

typeOf :: SLVal -> App (DLType, DLArgExpr)
typeOf v =
  typeOfM v >>= \case
    Just x -> return x
    Nothing -> expect_ $ Err_Type_None v

typeCheck :: TypeEnv -> SLType -> SLVal -> App DLArgExpr
typeCheck env ty val = do
  (val_ty, res) <- typeOf val
  let vty = dt2st val_ty
  typeCheck_help env ty val vty res

typeChecks :: TypeEnv -> [SLType] -> [SLVal] -> App [DLArgExpr]
typeChecks env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t : ts'), (v : vs')) -> do
      d <- typeCheck env t v
      ds' <- typeChecks env ts' vs'
      return $ d : ds'
    ((_ : _), _) ->
      expect_ $ Err_Type_TooFewArguments ts
    (_, (_ : _)) ->
      expect_ $ Err_Type_TooManyArguments vs

checkAndConvert_i :: TypeEnv -> SLType -> [SLVal] -> App (SLType, [DLArgExpr])
checkAndConvert_i env t args =
  case t of
    ST_Fun dom rng -> do
      dargs <- typeChecks env dom args
      return (rng, dargs)
    ST_Forall var ft -> do
      var_ref <- liftIO $ newIORef Nothing
      let env' = M.insert var var_ref env
      (vrng, dargs) <- checkAndConvert_i env' ft args
      rng <- typeSubst env' vrng
      return (rng, dargs)
    _ -> expect_ $ Err_Type_NotApplicable t

checkAndConvert :: SLType -> [SLVal] -> App (DLType, [DLArgExpr])
checkAndConvert t args = do
  (st, exprs) <- checkAndConvert_i mempty t args
  case st2dt st of
    Just dt -> pure (dt, exprs)
    Nothing -> expect_ $ Err_Type_NotDT st

checkType :: SLType -> SLVal -> App DLArgExpr
checkType et v = do
  (t, da) <- typeOf v
  at <- withAt id
  _ <- typeMeet (at, et) (at, dt2st t)
  return $ da

is_class :: SLPart -> App Bool
is_class who = S.member who . e_classes <$> ask

ctxt_alloc :: App Int
ctxt_alloc = do
  Env {..} <- ask
  liftIO $ incCounter e_id

ctxt_mkvar :: (Int -> DLVar) -> App DLVar
ctxt_mkvar mkvar = mkvar <$> ctxt_alloc

ctxt_lift_expr :: (Int -> DLVar) -> DLExpr -> App DLVar
ctxt_lift_expr mkvar e = do
  at <- withAt id
  dv <- ctxt_mkvar mkvar
  saveLift $ DLS_Let at (Just dv) e
  return dv

compileArgExpr :: DLArgExpr -> App DLArg
compileArgExpr = \case
  DLAE_Arg a -> return a
  DLAE_Array t aes -> do
    as <- compileArgExprs aes
    mk $ DLLA_Array t as
  DLAE_Tuple aes -> do
    as <- compileArgExprs aes
    mk $ DLLA_Tuple as
  DLAE_Obj me -> do
    m <- compileArgExprMap me
    mk $ DLLA_Obj m
  DLAE_Data t v ae -> do
    a <- compileArgExpr ae
    mk $ DLLA_Data t v a
  where
    mk la = do
      at <- withAt id
      let t = largeArgTypeOf la
      let mkvar = DLVar at "large arg" t
      DLA_Var <$> ctxt_lift_expr mkvar (DLE_LArg at la)

compileArgExprs :: [DLArgExpr] -> App [DLArg]
compileArgExprs = mapM compileArgExpr

compileArgExprMap :: M.Map a DLArgExpr -> App (M.Map a DLArg)
compileArgExprMap = mapM compileArgExpr

slvParticipant_part :: SLVal -> App SLPart
slvParticipant_part = \case
  SLV_Participant _ x _ _ -> return x
  x -> expect_t x $ Err_NotParticipant

compileCheckAndConvert :: SLType -> [SLVal] -> App (DLType, [DLArg])
compileCheckAndConvert t argvs = do
  (res, arges) <- checkAndConvert t argvs
  args <- compileArgExprs arges
  return (res, args)

compileTypeOf :: SLVal -> App (DLType, DLArg)
compileTypeOf v = do
  (t, dae) <- typeOf v
  da <- compileArgExpr dae
  return (t, da)

compileTypeOfs :: [SLVal] -> App ([DLType], [DLArg])
compileTypeOfs vs = unzip <$> mapM compileTypeOf vs

compileCheckType :: SLType -> SLVal -> App DLArg
compileCheckType et v = compileArgExpr =<< checkType et v

-- Modes

ensure_modes :: [SLMode] -> String -> App ()
ensure_modes ems msg = do
  am <- readSt st_mode
  case elem am ems of
    True -> return ()
    False -> expect_ $ Err_Eval_IllegalMode am msg ems

ensure_mode :: SLMode -> String -> App ()
ensure_mode em = ensure_modes [em]

ensure_live :: String -> App ()
ensure_live msg =
  readSt st_live >>= \case
    True -> return ()
    False -> expect_ $ Err_Eval_MustBeLive msg

ensure_can_wait :: App ()
ensure_can_wait = do
  dm <- dlo_deployMode . e_dlo <$> ask
  readSt st_after_first >>= \case
    True -> return ()
    False -> expect_ $ Err_Eval_IllegalWait dm

sco_to_cloenv :: SLScope -> SLCloEnv
sco_to_cloenv SLScope {..} =
  SLCloEnv sco_penvs sco_cenv

sco_lookup_penv :: SLPart -> App SLEnv
sco_lookup_penv who = do
  Env {..} <- ask
  return $
    case M.lookup who $ sco_penvs e_sco of
      Nothing -> sco_cenv e_sco
      Just x -> x

penvs_update :: (SLPart -> SLEnv -> App SLEnv) -> App SLPartEnvs
penvs_update f = do
  ps <- M.keys . e_ios <$> ask
  M.fromList
    <$> mapM (\p -> (,) p <$> (f p =<< sco_lookup_penv p)) ps

sco_set :: SLVar -> SLSSVal -> SLScope -> SLScope
sco_set v sv sco@(SLScope {..}) =
  sco
    { sco_penvs = M.map go sco_penvs
    , sco_cenv = go sco_cenv
    }
  where
    go = M.insert v sv

sco_update_and_mod :: EnvInsertMode -> SLEnv -> (SLEnv -> App SLEnv) -> App SLScope
sco_update_and_mod imode addl_env env_mod =
  (e_sco <$> ask) >>= c >>= ps
  where
    h :: (SLMode -> a -> App a) -> a -> App a
    h f x = do
      m <- readSt st_mode
      f m x
    c :: SLScope -> App SLScope
    c = h $ \case
      SLM_Module -> c_mod
      SLM_Step -> c_mod
      SLM_LocalStep -> return
      SLM_LocalPure -> return
      SLM_ConsensusStep -> c_mod
      SLM_ConsensusPure -> c_mod
    ps :: SLScope -> App SLScope
    ps = h $ \case
      SLM_Module -> return
      SLM_Step -> ps_all_mod
      SLM_LocalStep -> ps_one_mod
      SLM_LocalPure -> ps_one_mod
      SLM_ConsensusStep -> ps_all_mod
      SLM_ConsensusPure -> ps_all_mod
    p_do_merge_ :: SLPart -> SLEnv -> App SLEnv
    p_do_merge_ p = do_merge imode'
      where
        imode' =
          case imode of
            AllowShadowingRace racers ok ->
              case S.member p racers of
                True ->
                  AllowShadowingSome ok
                False ->
                  DisallowShadowing
            _ -> imode
    p_do_merge :: Maybe SLPart -> SLPart -> SLEnv -> App SLEnv
    p_do_merge = \case
      Nothing -> p_do_merge_
      Just x -> \y ->
        case x == y of
          True -> p_do_merge_ y
          False -> return
    ps_mod :: Maybe SLPart -> SLScope -> App SLScope
    ps_mod mone x = do
      penvs' <- penvs_update (p_do_merge mone)
      return $ x {sco_penvs = penvs'}
    ps_one_mod :: SLScope -> App SLScope
    ps_one_mod sco =
      (e_who <$> ask) >>= \case
        Just who -> ps_mod (Just who) sco
        Nothing -> impossible $ "no ctxt_only in local mode"
    ps_all_mod :: SLScope -> App SLScope
    ps_all_mod = ps_mod Nothing
    c_mod :: SLScope -> App SLScope
    c_mod x = do
      ce' <- do_merge imode'_top $ sco_cenv x
      return $ x {sco_cenv = ce'}
    imode'_top =
      case imode of
        AllowShadowingRace {} -> DisallowShadowing
        _ -> imode
    do_merge :: EnvInsertMode -> SLEnv -> App SLEnv
    do_merge imode' env = env_mod =<< env_merge_ imode' env addl_env

sco_update_ :: EnvInsertMode -> SLEnv -> App SLScope
sco_update_ imode addl_env = sco_update_and_mod imode addl_env return

sco_update :: SLEnv -> App SLScope
sco_update = sco_update_ DisallowShadowing

stMerge :: SLState -> SLState -> App SLState
stMerge old new =
  -- This is a little bit suspicious. What's going on?
  --
  -- Basically, the point of merge is so that afterwards, when we look at the
  -- state, we are making consistent assumptions about what is true. However,
  -- if either side is an exit, then that branch of code can't return past this
  -- point, so the only one who made it thus far would be the one that lived,
  -- so we can just go on with their assumptions
  case (st_live old, st_live new) of
    (_, False) -> return old
    (False, _) -> return new
    (True, True) ->
      case old == new of
        True -> return new
        False -> expect_ $ Err_Eval_IncompatibleStates new old

stEnsureMode :: SLMode -> App ()
stEnsureMode slm = do
  st <- readSt id
  mergeSt $ st {st_mode = slm}

binaryToPrim :: JSBinOp -> App SLVal
binaryToPrim = \case
  JSBinOpAnd _ -> impossible "and"
  JSBinOpDivide a -> prim a DIV
  JSBinOpEq a -> fun a "polyEq" "=="
  JSBinOpGe a -> prim a PGE
  JSBinOpGt a -> prim a PGT
  JSBinOpLe a -> prim a PLE
  JSBinOpLt a -> prim a PLT
  JSBinOpMinus a -> prim a SUB
  JSBinOpMod a -> prim a MOD
  JSBinOpNeq a -> fun a "polyNeq" "!="
  JSBinOpOr _ -> impossible "or"
  JSBinOpPlus a -> prim a ADD
  JSBinOpStrictEq a -> fun a "polyEq" "==="
  JSBinOpStrictNeq a -> fun a "polyNeq" "!=="
  JSBinOpTimes a -> prim a MUL
  JSBinOpLsh a -> prim a LSH
  JSBinOpRsh a -> prim a RSH
  JSBinOpBitAnd a -> prim a BAND
  JSBinOpBitOr a -> prim a BIOR
  JSBinOpBitXor a -> prim a BXOR
  j -> expect_ $ Err_Parse_IllegalBinOp j
  where
    fun a s ctx = snd <$> (locAtf (srcloc_jsa "binop" a) $ evalId ctx s)
    prim _a p = return $ SLV_Prim $ SLPrim_op p

unaryToPrim :: JSUnaryOp -> App SLVal
unaryToPrim = \case
  JSUnaryOpMinus a -> fun a "minus" "-"
  JSUnaryOpPlus a -> fun a "plus" "+"
  JSUnaryOpNot a -> fun a "not" "!"
  JSUnaryOpTypeof a -> fun a "typeOf" "typeOf"
  j -> expect_ $ Err_Parse_IllegalUnaOp j
  where
    fun a s ctx = snd <$> (locAtf (srcloc_jsa "unop" a) $ evalId ctx s)

infectWithId_sv :: SLVar -> SLVal -> SLVal
infectWithId_sv v = \case
  SLV_Participant at who _ mdv ->
    SLV_Participant at who (Just v) mdv
  x -> x

infectWithId_sss :: SLVar -> SLSSVal -> SLSSVal
infectWithId_sss v (SLSSVal at lvl sv) =
  SLSSVal at lvl $ infectWithId_sv v sv

infectWithId_sls :: SLVar -> SLSVal -> SLSVal
infectWithId_sls v (lvl, sv) = (lvl, infectWithId_sv v sv)

evalObjEnv :: SLObjEnv -> App SLEnv
evalObjEnv = mapM go
  where
    go :: App SLSVal -> App SLSSVal
    go getv = do
      at <- withAt id
      sls_sss at <$> getv

evalAsEnvM :: SLVal -> Maybe SLObjEnv
evalAsEnvM obj = case obj of
  SLV_Object _ _ env ->
    Just $ M.map (retV . sss_sls) env
  SLV_DLVar obj_dv@(DLVar _ _ (T_Object tm) _) ->
    Just $ retDLVar tm (DLA_Var obj_dv) Public
  SLV_Participant _ who vas _ ->
    Just $
      M.fromList
        [ ("only", retV $ public $ SLV_Form (SLForm_Part_Only who vas))
        , ("publish", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos vas (Just TCM_Publish) Nothing Nothing Nothing Nothing))
        , ("pay", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos vas (Just TCM_Pay) Nothing Nothing Nothing Nothing))
        , ("set", delayCall SLPrim_part_set)
        ]
    where
      whos = S.singleton who
  SLV_RaceParticipant _ whos ->
    Just $
      M.fromList
        [ ("publish", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos Nothing (Just TCM_Publish) Nothing Nothing Nothing Nothing))
        , ("pay", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos Nothing (Just TCM_Pay) Nothing Nothing Nothing Nothing))
        ]
  SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mwhen mtime) ->
    Just $
      M.fromList $
        gom "publish" TCM_Publish mpub
          <> gom "pay" TCM_Pay mpay
          <> gom "when" TCM_When mwhen
          <> gom "timeout" TCM_Timeout mtime
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just mode) mpub mpay mwhen mtime))]
  SLV_Form (SLForm_fork_partial fat Nothing cases mtime) ->
    Just $
      M.fromList $
        go "case" FM_Case
          <> gom "timeout" FM_Timeout mtime
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_fork_partial fat (Just mode) cases mtime))]
  SLV_Form (SLForm_parallel_reduce_partial pr_at Nothing pr_init pr_minv pr_mwhile pr_cases pr_mtime) ->
    Just $
      M.fromList $
        gom "invariant" PRM_Invariant pr_minv
          <> gom "while" PRM_While pr_mwhile
          <> go "case" PRM_Case
          <> gom "timeout" PRM_Timeout pr_mtime
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_parallel_reduce_partial pr_at (Just mode) pr_init pr_minv pr_mwhile pr_cases pr_mtime))]
  --- FIXME rewrite the rest to look at the type and go from there
  SLV_Tuple _ _ ->
    tupleValueEnv
  SLV_DLVar (DLVar _ _ (T_Tuple _) _) ->
    tupleValueEnv
  SLV_Prim SLPrim_Tuple ->
    Just $
      M.fromList
        [ ("set", retV $ public $ SLV_Prim $ SLPrim_tuple_set)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_tuple_length)
        ]
  SLV_Array {} ->
    arrayValueEnv
  SLV_DLVar (DLVar _ _ (T_Array _ _) _) ->
    arrayValueEnv
  SLV_Data {} ->
    Just $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_DLVar (DLVar _ _ (T_Data _) _) ->
    Just $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_Prim SLPrim_Array ->
    Just $
      M.fromList
        [ ("empty", retStdLib "Array_empty")
        , ("forEach", retStdLib "Array_forEach")
        , ("min", retStdLib "Array_min")
        , ("max", retStdLib "Array_max")
        , ("all", retStdLib "Array_all")
        , ("any", retStdLib "Array_any")
        , ("or", retStdLib "Array_or")
        , ("and", retStdLib "Array_and")
        , ("sum", retStdLib "Array_sum")
        , ("average", retStdLib "Array_average")
        , ("product", retStdLib "Array_product")
        , ("includes", retStdLib "Array_includes")
        , ("indexOf", retStdLib "Array_indexOf")
        , ("findIndex", retStdLib "Array_findIndex")
        , ("count", retStdLib "Array_count")
        , ("replicate", retStdLib "Array_replicate")
        , ("length", retV $ public $ SLV_Prim $ SLPrim_array_length)
        , ("set", retV $ public $ SLV_Prim $ SLPrim_array_set)
        , ("iota", retV $ public $ SLV_Prim $ SLPrim_Array_iota)
        , ("concat", retV $ public $ SLV_Prim $ SLPrim_array_concat)
        , ("map", retV $ public $ SLV_Prim $ SLPrim_array_map)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_array_reduce)
        , ("zip", retV $ public $ SLV_Prim $ SLPrim_array_zip)
        ]
  SLV_Prim SLPrim_Object ->
    Just $
      M.fromList
        [ ("set", retStdLib "Object_set")
        , ("setIfUnset", retStdLib "Object_setIfUnset")
        , ("has", retV $ public $ SLV_Prim $ SLPrim_Object_has)
        ]
  SLV_Type ST_UInt ->
    Just $
      M.fromList
        [("max", retV $ public $ SLV_DLC DLC_UInt_max)]
  SLV_Type (ST_Data varm) ->
    Just $ M.mapWithKey (\k t -> retV $ public $ SLV_Prim $ SLPrim_Data_variant varm k t) varm
  _ -> Nothing
  where
    tupleValueEnv =
      Just $
        M.fromList
          [ ("set", delayCall SLPrim_tuple_set)
          , ("length", doCall SLPrim_tuple_length)
          ]
    arrayValueEnv =
      Just $
        M.fromList
          [ ("set", delayCall SLPrim_array_set)
          , ("length", doCall SLPrim_array_length)
          , ("concat", delayCall SLPrim_array_concat)
          , ("forEach", doStdlib "Array_forEach1")
          , ("min", doStdlib "Array_min1")
          , ("max", doStdlib "Array_max1")
          , ("all", doStdlib "Array_all1")
          , ("any", doStdlib "Array_any1")
          , ("or", doStdlib "Array_or1")
          , ("and", doStdlib "Array_and1")
          , ("sum", doStdlib "Array_sum1")
          , ("average", doStdlib "Array_average1")
          , ("product", doStdlib "Array_product1")
          , ("indexOf", doStdlib "Array_indexOf1")
          , ("findIndex", doStdlib "Array_findIndex1")
          , ("includes", doStdlib "Array_includes1")
          , ("count", doStdlib "Array_count1")
          , ("map", delayCall SLPrim_array_map)
          , ("reduce", delayCall SLPrim_array_reduce)
          , ("zip", delayCall SLPrim_array_zip)
          ]
    delayCall :: SLPrimitive -> App SLSVal
    delayCall p = do
      at <- withAt id
      retV $ public $ SLV_Prim $ SLPrim_PrimDelay at p [(public obj)] []
    doStdlib :: SLVar -> App SLSVal
    doStdlib = doApply <=< lookStdlib
    lookStdlib :: SLVar -> App SLVal
    lookStdlib n = sss_val <$> ((env_lookup (LC_RefFrom "stdlib") n) =<< (sco_cenv . e_sco) <$> ask)
    doCall p = doApply $ SLV_Prim p
    doApply :: SLVal -> App SLSVal
    doApply f = evalApplyVals' f [(public obj)]
    retDLVar tm obj_dla slvl = do
      let retk field t = do
            at <- withAt id
            let mkv = DLVar at "object ref" t
            let e = DLE_ObjectRef at obj_dla field
            dv <- ctxt_lift_expr mkv e
            let ansv = SLV_DLVar dv
            return $ (slvl, ansv)
      M.mapWithKey retk tm
    retV = return
    retStdLib :: SLVar -> App SLSVal
    retStdLib n = (retV . public) =<< lookStdlib n

evalAsEnv :: SLVal -> App SLObjEnv
evalAsEnv obj = case evalAsEnvM obj of
  Nothing -> expect_t obj $ Err_Eval_NotObject
  Just x -> return $ x

evalDot_ :: SLVal -> SLObjEnv -> String -> App SLSVal
evalDot_ obj env field = do
  case M.lookup field env of
    Just gv -> gv
    Nothing -> expect_t obj $ \o -> Err_Dot_InvalidField o (M.keys env) field

evalDot :: SLVal -> String -> App SLSVal
evalDot obj s = flip (evalDot_ obj) s =<< evalAsEnv obj

st2dte :: SLType -> App DLType
st2dte t =
  case st2dt t of
    Just x -> return $ x
    Nothing -> expect_ $ Err_Type_NotDT t

compileInteractResult :: SLVar -> SLType -> (DLType -> DLExpr) -> App SLSVal
compileInteractResult m t de = do
  at <- withAt id
  dt <- st2dte t
  idv <- ctxt_lift_expr (DLVar at m dt) (de dt)
  let isv = SLV_DLVar idv
  return $ secret isv

makeInteract :: SLPart -> SLEnv -> App (SLSSVal, InteractEnv)
makeInteract who spec = do
  at <- withAt id
  let lab = Just $ (bunpack who) <> "'s interaction interface"
  let specl = M.toList spec
  let wrap_ty :: SLVar -> SLSSVal -> App (SLSSVal, IType)
      wrap_ty k (SLSSVal idAt Public (SLV_Type t)) = locAt idAt $ case t of
        ST_Fun dom rng -> do
          dom' <- mapM st2dte dom
          rng' <- st2dte rng
          return $
            ( (sls_sss idAt $ secret $ SLV_Prim $ SLPrim_interact at who k dom rng)
            , IT_Fun dom' rng'
            )
        _ -> do
          t' <- st2dte t
          isv <- compileInteractResult k t (\dt -> DLE_Arg idAt $ DLA_Interact who k dt)
          return $ (sls_sss idAt isv, IT_Val t')
      wrap_ty _ v = do
        _ <- ensure_public $ sss_sls v
        locAt (sss_at v) $ expect_t (sss_val v) $ Err_App_InvalidInteract
  (lifts, spec'l) <-
    captureLifts $
      mapM (\(k, v) -> (\v' -> (k, v')) <$> wrap_ty k v) specl
  let spec' = M.fromList spec'l
  let io = SLSSVal at Secret $ SLV_Object at lab $ M.map fst spec'
  let ienv = InteractEnv $ M.map (\(_, y) -> y) spec'
  saveLift $ DLS_Only at who lifts
  return $ (io, ienv)

make_partio :: SLVal -> App SLCompiledPartInfo
make_partio = check_partio
  where
    check_partio v =
      case v of
        SLV_Tuple p_at [SLV_Bytes bs_at bs, SLV_Object io_at _ io] ->
          make_partio_ p_at False bs_at bs io_at io
        SLV_Tuple p_at [SLV_Bytes _ "class", SLV_Bytes bs_at bs, SLV_Object io_at _ io] ->
          make_partio_ p_at True bs_at bs io_at io
        _ -> expect_ $ Err_App_InvalidPartSpec v
    make_partio_ slcpi_at slcpi_isClass bs_at slcpi_who io_at iov =
      case "_" `B.isPrefixOf` slcpi_who of
        True -> locAt bs_at $ expect_ $ Err_App_PartUnderscore slcpi_who
        False -> locAt io_at $ do
          (slcpi_lifts, (slcpi_io, slcpi_ienv)) <-
            captureLifts $ makeInteract slcpi_who iov
          return $ SLCompiledPartInfo {..}

evalForm :: SLForm -> [JSExpression] -> App SLSVal
evalForm f args = do
  case f of
    SLForm_App ->
      case args of
        [opte, partse, JSArrowExpression top_formals _ top_s] -> do
          sargs <- evalExprs [opte, partse]
          case map snd sargs of
            [(SLV_Object _ _ opts), (SLV_Tuple _ parts)] -> do
              at <- withAt id
              part_ios <- mapM make_partio parts
              env <- (sco_cenv . e_sco) <$> ask
              retV $ public $ SLV_Prim $ SLPrim_App_Delay at opts part_ios (parseJSArrowFormals at top_formals) top_s env
            _ -> expect_ $ Err_App_InvalidArgs args
        _ -> expect_ $ Err_App_InvalidArgs args
    SLForm_Part_Only who mv -> do
      at <- withAt id
      x <- one_arg
      env <- (sco_to_cloenv . e_sco) <$> ask
      return $ public $ SLV_Form $ SLForm_EachAns [(who, mv)] at env x
    SLForm_fork -> do
      zero_args
      at <- withAt id
      retV $ public $ SLV_Form $ SLForm_fork_partial at Nothing [] Nothing
    SLForm_fork_partial fat mmode cases mtime ->
      case mmode of
        Just FM_Case -> do
          a <- withAt srcloc2annot
          let sp = JSSemi a
          let default_pay = JSArrowExpression (JSParenthesizedArrowParameterList a JSLNil a) a (JSExpressionStatement (JSDecimal a "0") sp)
          at <- withAt id
          case_args <-
            case args of
              [w, x, y, z] -> return $ (w, x, y, z)
              [w, x, z] -> return $ (w, x, default_pay, z)
              _ -> illegal_args 4
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing (cases <> [(at, case_args)]) mtime
        Just FM_Timeout -> do
          x <- parsedTimeout
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing cases x
        Nothing -> expect_t rator $ Err_Eval_NotApplicable
    SLForm_parallel_reduce -> do
      at <- withAt id
      x <- one_arg
      retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial at Nothing x Nothing Nothing [] Nothing
    SLForm_parallel_reduce_partial pr_at pr_mode pr_init pr_minv pr_mwhile pr_cases pr_mtime -> do
      aa <- withAt $ \at -> (at, args)
      let jaa = Just aa
      case pr_mode of
        Just PRM_Invariant -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial pr_at Nothing pr_init (Just x) pr_mwhile pr_cases pr_mtime
        Just PRM_While -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial pr_at Nothing pr_init pr_minv (Just x) pr_cases pr_mtime
        Just PRM_Case ->
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial pr_at Nothing pr_init pr_minv pr_mwhile (pr_cases <> [aa]) pr_mtime
        Just PRM_Timeout ->
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial pr_at Nothing pr_init pr_minv pr_mwhile pr_cases jaa
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
    SLForm_Part_ToConsensus to_at who vas mmode mpub mpay mwhen mtime ->
      case mmode of
        Just TCM_Publish ->
          case mpub of
            Nothing -> do
              at <- withAt id
              let msg = map (jse_expect_id at) args
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing (Just msg) mpay mwhen mtime
            Just _ ->
              expect_ $ Err_ToConsensus_Double TCM_Publish
        Just TCM_Pay -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub (Just x) mwhen mtime
        Just TCM_When -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay (Just x) mtime
        Just TCM_Timeout -> do
          x <- parsedTimeout
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mwhen x
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
    SLForm_each -> do
      (partse, thunke) <- two_args
      (_, parts_v) <- evalExpr partse
      case parts_v of
        SLV_Tuple _ part_vs -> do
          parts <-
            mapM
              (\case
                 SLV_Participant _ who mv _ -> return (who, mv)
                 v -> expect_t v $ Err_NotParticipant)
              part_vs
          ce <- sco_to_cloenv . e_sco <$> ask
          at <- withAt id
          return $ public $ SLV_Form $ SLForm_EachAns parts at ce thunke
        _ ->
          expect_t parts_v $ Err_Each_NotTuple
    -- This case occurs when the "result" of `each` is used as an expression.
    SLForm_EachAns _ ea_at _ _ ->
      locAt ea_at $
        expect_ $ Err_Invalid_Statement "each/only"
    SLForm_unknowable -> do
      ensure_mode SLM_Step "unknowable"
      (notter_e, snd_part, mmsg_e) <-
        case args of
          [x, y] -> return $ (x, y, Nothing)
          [x, y, z] -> return $ (x, y, Just z)
          _ -> illegal_args 2
      (knower_e, whats_e) <- withAt $ \at -> jsCallLike at snd_part
      (mmsg :: Maybe B.ByteString) <-
        traverse (mustBeBytes <=< ensure_public <=< evalExpr) mmsg_e
      (_, v_n) <- evalExpr notter_e
      let participant_who = \case
            SLV_Participant _ who _ _ -> return $ who
            v -> expect_t v $ Err_NotParticipant
      notter <- participant_who v_n
      (_, v_kn) <- evalExpr knower_e
      knower <- participant_who v_kn
      whats_sv <-
        locWho knower $
          locStMode SLM_LocalStep $
            evalExprs whats_e
      (_, whats_das) <- compileTypeOfs $ map snd whats_sv
      let whats_da = DLA_Literal $ DLL_Bool False
      let ct = CT_Unknowable notter whats_das
      fs <- e_stack <$> ask
      at <- withAt id
      saveLift $
        DLS_Let at Nothing $
          DLE_Claim at fs ct whats_da mmsg
      return $ public $ SLV_Null at "unknowable"
    SLForm_wait -> do
      ensure_can_wait
      ensure_mode SLM_Step "wait"
      amt_e <- one_arg
      amt_sv <- locStMode SLM_ConsensusPure $ evalExpr amt_e
      amt_da <- compileCheckType ST_UInt =<< ensure_public amt_sv
      at <- withAt id
      saveLift $ DLS_Let at Nothing (DLE_Wait at amt_da)
      return $ public $ SLV_Null at "wait"
  where
    illegal_args n = expect_ $ Err_Form_InvalidArgs f n args
    rator = SLV_Form f
    retV = return
    zero_args = case args of
      [] -> return ()
      _ -> illegal_args 0
    one_arg = case args of
      [x] -> return $ x
      _ -> illegal_args 1
    two_args = case args of
      [x, y] -> return $ (x, y)
      _ -> illegal_args 2
    _three_args = case args of
      [x, y, z] -> return $ (x, y, z)
      _ -> illegal_args 3
    parsedTimeout =
      case args of
        [de, JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s] ->
          withAt $ \at -> Just (at, de, (jsStmtToBlock dt_s))
        _ -> expect_ $ Err_ToConsensus_TimeoutArgs args

evalPrimOp :: PrimOp -> [SLSVal] -> App SLSVal
evalPrimOp p sargs = do
  case p of
    ADD -> intOrBinop (+) True
    SUB -> intOrBinop (-) False
    MUL -> nn2n (*)
    DIV -> nn2n (div)
    MOD -> nn2n (mod)
    PLT -> nn2b (<)
    PLE -> nn2b (<=)
    PEQ -> polyEq args
    PGE -> nn2b (>=)
    PGT -> nn2b (>)
    IF_THEN_ELSE ->
      case args of
        [SLV_Bool _ b, t, f] ->
          static $ if b then t else f
        _ -> make_var args
    DIGEST_EQ -> make_var args
    ADDRESS_EQ -> make_var args
    -- FIXME fromIntegral may overflow the Int
    LSH -> nn2n (\a b -> shift a (fromIntegral b))
    RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    BAND -> nn2n (.&.)
    BIOR -> nn2n (.|.)
    BXOR -> nn2n (xor)
    SELF_ADDRESS -> impossible "self address"
  where
    andMap f ls rs = do
      xs <- zipWithM (\l r -> snd <$> f [l, r]) ls rs
      case xs of
        [] -> return $ SLV_Bool srcloc_builtin True
        h : t -> foldrM (/\) h t
    -- Logical and for SL bool values
    (/\) (SLV_Bool bAt l) (SLV_Bool _ r) = return $ SLV_Bool bAt $ l && r
    (/\) l@SLV_DLVar {} r@SLV_DLVar {} =
      snd <$> evalPrimOp IF_THEN_ELSE [(lvl, l), (lvl, r), public $ SLV_Bool srcloc_builtin False]
    (/\) (SLV_Bool bAt False) _ = return $ SLV_Bool bAt False
    (/\) (SLV_Bool _ True) r@SLV_DLVar {} = return $ r
    -- Flip args & process
    (/\) l@(SLV_DLVar _) r@SLV_Bool {} = r /\ l
    -- Values not supported
    (/\) l r = impossible $ "/\\ expecting SLV_Bool or SLV_DLVar: " <> show l <> ", " <> show r
    polyEq = \case
      -- Both args static
      [SLV_Int _ l, SLV_Int _ r] -> retBool $ l == r
      [SLV_Bool _ l, SLV_Bool _ r] -> retBool $ l == r
      [SLV_Bytes _ l, SLV_Bytes _ r] -> retBool $ l == r
      [SLV_Type l, SLV_Type r] -> retBool $ l == r
      [SLV_Null {}, SLV_Null {}] -> retBool True
      [SLV_Array _ _ ls, SLV_Array _ _ rs] -> do
        at <- withAt id
        let lengthEquals = SLV_Bool at $ length ls == length rs
        elemEquals <- andMap polyEq ls rs
        andVal <- lengthEquals /\ elemEquals
        return (lvl, andVal)
      [SLV_Tuple _ ls, SLV_Tuple _ rs] -> do
        elemEquals <- andMap polyEq ls rs
        return $ (lvl, elemEquals)
      [SLV_Object _ _ lEnv, SLV_Object _ _ rEnv] -> do
        let elems = map sss_val . M.elems
        elemEquals <- andMap polyEq (elems lEnv) (elems rEnv)
        return $ (lvl, elemEquals)
      [SLV_Data _ lCons lCon lVal, SLV_Data _ rCons rCon rVal]
        | lCons == rCons && lCon == rCon -> do
          elemEquals <- snd <$> polyEq [lVal, rVal]
          return $ (lvl, elemEquals)
        | otherwise -> retBool False
      -- If atleast one arg is dynamic
      args'@[l, r] -> do
        lty <- getType l
        rty <- getType r
        case (lty, rty) of
          (T_Null, T_Null) -> retBool True
          (T_UInt, T_UInt) -> make_var args'
          (T_Digest, T_Digest) -> make_var args'
          (T_Address, T_Address) -> make_var args'
          (T_Bool, T_Bool) -> do
            notR <- evalPrimOp IF_THEN_ELSE [(lvl, r), public $ SLV_Bool srcloc_builtin False, public $ SLV_Bool srcloc_builtin True]
            evalPrimOp IF_THEN_ELSE [(lvl, l), (lvl, r), notR]
          _ ->
            case dtypeMeetM (srclocOf l, lty) (srclocOf r, rty) of
              Nothing -> retBool False
              Just _ -> hashAndCmp (lvl, l) (lvl, r)
      _ -> impossible "polyEq called with more than 2 args"
    hashAndCmp :: SLSVal -> SLSVal -> App SLSVal
    hashAndCmp l r = do
      l' <- getHash l
      r' <- getHash r
      digestEq2 l' r'
    digestEq2 x y = digestEq [x, y]
    lookupFn f = do
      env <- (sco_cenv . e_sco) <$> ask
      sss_val <$> env_lookup (LC_RefFrom "polyEq") f env
    digestEq as = lookupFn "digestEq" >>= flip evalApplyVals' as
    getHash :: SLSVal -> App SLSVal
    getHash x = lookupFn "digest" >>= flip evalApplyVals' [x]
    retBool v = do
      at <- withAt id
      static $ SLV_Bool at v
    getType x = fst <$> typeOf x
    args = map snd sargs
    lvl = mconcat $ map fst sargs
    intOrBinop op sign =
      case sargs of
      [h] ->
        let sv = snd h in
        let at = srclocOf sv in
          return (lvl, SLV_Tuple at [SLV_Bool at sign, sv])
      _    -> nn2n op
    nn2b op =
      case args of
        [SLV_Int _ lhs, SLV_Int _ rhs] -> do
          at <- withAt id
          static $ SLV_Bool at $ op lhs rhs
        _ -> make_var args
    nn2n op =
      case args of
        [SLV_Int _ lhs, SLV_Int _ rhs] -> do
          at <- withAt id
          static $ SLV_Int at $ op lhs rhs
        _ -> make_var args
    static v = return $ (lvl, v)
    make_var args' = do
      at <- withAt id
      (rng, dargs) <- compileCheckAndConvert (primOpType p) args'
      let doClaim ca msg = do
            fs <- e_stack <$> ask
            saveLift $
              DLS_Let at Nothing $
                DLE_Claim at fs CT_Assert ca $ Just msg
      let mkvar t = DLVar at "overflow" t
      let doOp t cp cargs = DLA_Var <$> (ctxt_lift_expr (mkvar t) $ DLE_PrimOp at cp cargs)
      let doCmp = doOp T_Bool
      let lim_maxUInt_a = DLA_Constant DLC_UInt_max
      whenVerifyOverflow $
        case p of
          ADD -> do
            let (a, b) = case dargs of
                  [a_, b_] -> (a_, b_)
                  _ -> impossible "add args"
            ra <- doOp T_UInt SUB [lim_maxUInt_a, b]
            ca <- doCmp PLE [a, ra]
            doClaim ca "add overflow"
          SUB -> do
            ca <- doCmp PGE dargs
            doClaim ca "sub wraparound"
          _ -> return ()
      dv <- ctxt_lift_expr (DLVar at "prim" rng) (DLE_PrimOp at p dargs)
      let da = DLA_Var dv
      whenVerifyOverflow $
        case p of
          MUL -> do
            ca <- doCmp PLE [da, lim_maxUInt_a]
            doClaim ca "mul overflow"
          _ -> return $ mempty
      return $ (lvl, SLV_DLVar dv)

explodeTupleLike :: String -> SLVal -> App [SLVal]
explodeTupleLike lab = \case
  SLV_Tuple _ vs -> return vs
  SLV_Array _ _ vs -> return vs
  SLV_DLVar tupdv@(DLVar _ _ (T_Tuple tuptys) _) ->
    mconcatMap (uncurry (flip (mkdv tupdv DLE_TupleRef))) $ zip [0 ..] tuptys
  SLV_DLVar tupdv@(DLVar _ _ (T_Array t sz) _) -> do
    at <- withAt id
    let mkde _ da i = DLE_ArrayRef at da (DLA_Literal $ DLL_Int at i)
    mconcatMap (mkdv tupdv mkde t) [0 .. sz -1]
  tuplv ->
    expect_ $ Err_Eval_NotSpreadable lab tuplv
  where
    mkdv tupdv mkde t i = do
      at <- withAt id
      let de = mkde at (DLA_Var tupdv) i
      let mdv = DLVar at lab t
      dv <- ctxt_lift_expr mdv de
      return $ [SLV_DLVar dv]

doFluidRef_dv :: FluidVar -> App DLVar
doFluidRef_dv fv = do
  at <- withAt id
  ensure_modes (all_slm_modes \\ [SLM_Module]) "fluid ref"
  let fvt = fluidVarType fv
  dv <- ctxt_mkvar (DLVar at "fluid" fvt)
  saveLift $ DLS_FluidRef at dv fv
  return dv

doFluidRef :: FluidVar -> App SLSVal
doFluidRef fv = (public . SLV_DLVar) <$> doFluidRef_dv fv

doFluidSet :: FluidVar -> SLSVal -> App ()
doFluidSet fv ssv = do
  at <- withAt id
  sv <- ensure_public ssv
  da <- compileCheckType (dt2st $ fluidVarType fv) sv
  saveLift $ DLS_FluidSet at fv da

doAssertBalance :: SLVal -> PrimOp -> App ()
doAssertBalance lhs op = do
  at <- withAt id
  let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [(Public, lhs)] []
  balance_v <- doFluidRef FV_balance
  cmp_v <- evalApplyVals' cmp_rator [balance_v]
  let ass_rator = SLV_Prim $ SLPrim_claim CT_Assert
  void $
    evalApplyVals' ass_rator $
      [cmp_v, public $ SLV_Bytes at "balance assertion"]

doArrayBoundsCheck :: Integer -> SLVal -> App ()
doArrayBoundsCheck sz idxv = do
  at <- withAt id
  cmp_v <- evalApplyVals' (SLV_Prim $ SLPrim_op PLT) [public idxv, public $ SLV_Int at sz]
  void $
    evalApplyVals' (SLV_Prim $ SLPrim_claim CT_Assert) $
      [cmp_v, public $ SLV_Bytes at "array bounds check"]

doBalanceUpdate :: PrimOp -> SLVal -> App ()
doBalanceUpdate op rhs = do
  at <- withAt id
  let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
  balance_v <- doFluidRef FV_balance
  balance_v' <- evalApplyVals' up_rator [balance_v]
  doFluidSet FV_balance balance_v'

mustBeBytes :: SLVal -> App B.ByteString
mustBeBytes = \case
  SLV_Bytes _ x -> return $ x
  v -> expect_t v $ Err_Expected_Bytes

evalPrim :: SLPrimitive -> [SLSVal] -> App SLSVal
evalPrim p sargs =
  case p of
    SLPrim_race -> do
      at <- withAt id
      ps <- mapM slvParticipant_part $ map snd sargs
      retV $ (lvl, SLV_RaceParticipant at $ S.fromList ps)
    SLPrim_fluid_read fv -> doFluidRef fv
    SLPrim_lastConsensusTime -> do
      ensure_can_wait
      zero_args
      evalPrim (SLPrim_fluid_read $ FV_lastConsensusTime) []
    SLPrim_op op -> evalPrimOp op sargs
    SLPrim_Fun ->
      case map snd sargs of
        [(SLV_Tuple _ dom_arr), (SLV_Type rng)] -> do
          dom <- mapM expect_ty dom_arr
          retV $ (lvl, SLV_Type $ ST_Fun dom rng)
        _ -> illegal_args
    SLPrim_is_type -> do
      at <- withAt id
      one_arg >>= \case
        SLV_Type _ ->
          retV $ (lvl, SLV_Bool at True)
        _ ->
          retV $ (lvl, SLV_Bool at False)
    SLPrim_type_eq -> do
      at <- withAt id
      case map snd sargs of
        [(SLV_Type ty1), (SLV_Type ty2)] ->
          retV $ (lvl, SLV_Bool at (ty1 == ty2))
        _ -> illegal_args
    SLPrim_typeOf ->
      case map snd sargs of
        [(SLV_Type ty)] ->
          retV $ (lvl, SLV_Type (ST_Type ty))
        [val] -> do
          (ty, _) <- typeOf val
          retV $ (lvl, SLV_Type (dt2st ty))
        _ -> illegal_args
    SLPrim_Bytes ->
      case map snd sargs of
        [(SLV_Int _ sz)] -> retV $ (lvl, SLV_Type $ ST_Bytes sz)
        _ -> illegal_args
    SLPrim_Array ->
      case map snd sargs of
        [(SLV_Type ty), (SLV_Int _ sz)] ->
          retV $ (lvl, SLV_Type $ ST_Array ty sz)
        _ -> illegal_args
    SLPrim_tuple_length -> do
      at <- withAt id
      one_arg >>= \case
        SLV_Tuple _ vs ->
          retV $ public $ SLV_Int at $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Tuple ts) _) ->
          retV $ public $ SLV_Int at $ fromIntegral $ length ts
        _ -> illegal_args
    SLPrim_array_length -> do
      at <- withAt id
      one_arg >>= \case
        SLV_Array _ _ vs ->
          retV $ public $ SLV_Int at $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Array _ sz) _) ->
          retV $ public $ SLV_Int at $ fromIntegral $ sz
        _ -> illegal_args
    SLPrim_Array_iota -> do
      at <- withAt id
      case map snd sargs of
        [SLV_Int _ sz] ->
          retV $ (lvl, SLV_Array at ST_UInt $ map (SLV_Int at) [0 .. (sz -1)])
        [_] -> expect_ $ Err_Prim_InvalidArg_Dynamic p
        _ -> illegal_args
    SLPrim_array ->
      case map snd sargs of
        [(SLV_Type elem_ty), elems_v] ->
          case elems_v of
            SLV_Tuple _ elem_vs -> do
              at <- withAt id
              let check1 sv = checkType elem_ty sv >> return sv
              elem_vs_checked <- mapM check1 elem_vs
              retV $ (lvl, SLV_Array at elem_ty elem_vs_checked)
            --- FIXME we could support turning a DL Tuple into an array.
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_concat -> do
      at <- withAt id
      case map snd sargs of
        [SLV_Array x_at x_ty x_vs, SLV_Array y_at y_ty y_vs] -> do
          ty <- typeMeet (x_at, x_ty) (y_at, y_ty)
          retV $ (lvl, SLV_Array at ty $ x_vs ++ y_vs)
        [x, y] -> do
          (xt, xa) <- compileTypeOf x
          (yt, ya) <- compileTypeOf y
          case (xt, yt) of
            (T_Array x_ty x_sz, T_Array y_ty y_sz) -> do
              meet_ty <- dtypeMeet (at, x_ty) (at, y_ty)
              let t = T_Array meet_ty (x_sz + y_sz)
              let mkdv = DLVar at "array_concat" t
              dv <- ctxt_lift_expr mkdv $ DLE_ArrayConcat at xa ya
              return $ (lvl, SLV_DLVar dv)
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_zip -> do
      at <- withAt id
      (x, y) <- two_args
      (xt, x_da) <- compileTypeOf x
      (x_ty, x_sz) <- mustBeArray xt
      (yt, y_da) <- compileTypeOf y
      (y_ty, y_sz) <- mustBeArray yt
      let ty' = T_Tuple [x_ty, y_ty]
      let sty' = dt2st ty'
      unless (x_sz == y_sz) $ do
        expect_ $ Err_Zip_ArraysNotEqualLength x_sz y_sz
      let sz' = x_sz
      case isLiteralArray x && isLiteralArray y of
        True -> do
          x_vs <- explodeTupleLike "zip" x
          y_vs <- explodeTupleLike "zip" y
          let vs' = zipWith (\xe ye -> SLV_Tuple at [xe, ye]) x_vs y_vs
          return $ (lvl, SLV_Array at sty' vs')
        False -> do
          let t = T_Array ty' sz'
          let mkdv = (DLVar at "array_zip" t)
          dv <- ctxt_lift_expr mkdv $ DLE_ArrayZip at x_da y_da
          return $ (lvl, SLV_DLVar dv)
    SLPrim_array_map ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [x, f] -> do
          at <- withAt id
          (xt, x_da) <- compileTypeOf x
          (x_ty, x_sz) <- mustBeArray xt
          let f' a = evalApplyVals' f [(lvl, a)]
          (a_dv, a_dsv) <- make_dlvar at "map in" x_ty
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ (f_lvl, f_ty, f_da) <-
            captureRes $ do
              (f_lvl, f_v) <- f' a_dsv
              (f_ty, f_da) <- compileTypeOf f_v
              return (f_lvl, f_ty, f_da)
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              x_vs <- explodeTupleLike "map" x
              let evalem xv = snd <$> f' xv
              vs' <- mapM evalem x_vs
              return $ (f_lvl, SLV_Array at (dt2st f_ty) vs')
            False -> do
              let t = T_Array f_ty x_sz
              (ans_dv, ans_dsv) <- make_dlvar at "array_map" t
              let f_bl = DLBlock at [] f_lifts f_da
              saveLift $ DLS_ArrayMap at ans_dv x_da a_dv f_bl
              return $ (lvl, ans_dsv)
        x : y : args' -> do
          let (f, more) = case reverse args' of
                f_ : rmore -> (f_, reverse rmore)
                _ -> impossible "array_map"
          xy_v <- evalApplyVals' (SLV_Prim $ SLPrim_array_zip) $ map public [x, y]
          let clo_args = concatMap ((",c" <>) . show) [0 .. (length more - 1)]
          f' <- withAt $ \at -> jsClo at "zip" ("(ab" <> clo_args <> ") => f(ab[0], ab[1]" <> clo_args <> ")") (M.fromList [("f", f)])
          evalApplyVals' (SLV_Prim $ SLPrim_array_map) (xy_v : (map public $ more ++ [f']))
    SLPrim_array_reduce ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [_, _] -> illegal_args
        [x, z, f] -> do
          at <- withAt id
          (xt, x_da) <- compileTypeOf x
          (x_ty, _) <- mustBeArray xt
          let f' b a = evalApplyVals' f [(lvl, b), (lvl, a)]
          (z_ty, z_da) <- compileTypeOf z
          (b_dv, b_dsv) <- make_dlvar at "reduce acc" z_ty
          (a_dv, a_dsv) <- make_dlvar at "reduce in" x_ty
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ (f_lvl, f_ty, f_da) <-
            captureRes $ do
              (f_lvl, f_v) <- f' b_dsv a_dsv
              (f_ty, f_da) <- compileTypeOf f_v
              return (f_lvl, f_ty, f_da)
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              x_vs <- explodeTupleLike "reduce" x
              let evalem :: SLSVal -> SLVal -> App SLSVal
                  evalem prev_z xv = do
                    xv_v' <- f' (snd prev_z) xv
                    --- Note: We are artificially restricting reduce
                    --- to be parameteric in the state. We also ensure
                    --- that they type is the same as the anonymous
                    --- version.
                    _ <- checkType (dt2st f_ty) (snd xv_v')
                    return $ xv_v'
              foldM evalem (f_lvl, z) x_vs
            False -> do
              (ans_dv, ans_dsv) <- make_dlvar at "array_reduce" f_ty
              let f_bl = DLBlock at [] f_lifts f_da
              saveLift $ DLS_ArrayReduce at ans_dv x_da z_da b_dv a_dv f_bl
              return $ (lvl, ans_dsv)
        x : y : args' -> do
          let (f, z, more) = case reverse args' of
                f_ : z_ : rmore -> (f_, z_, reverse rmore)
                _ -> impossible "array_reduce"
          xy_v <- evalApplyVals' (SLV_Prim $ SLPrim_array_zip) $ map public [x, y]
          let clo_args = concatMap ((",c" <>) . show) [0 .. (length more - 1)]
          f' <- withAt $ \at -> jsClo at "zip" ("(z,ab" <> clo_args <> ") => f(z, ab[0], ab[1]" <> clo_args <> ")") (M.fromList [("f", f)])
          evalApplyVals' (SLV_Prim $ SLPrim_array_reduce) (xy_v : (map public $ more ++ [z, f']))
    SLPrim_array_set ->
      case map snd sargs of
        [arrv, idxv, valv] -> do
          (idxty, idxda) <- compileTypeOf idxv
          case (idxty, idxda) of
            (T_UInt, (DLA_Literal (DLL_Int _ idxi))) ->
              case arrv of
                SLV_Array _ elem_ty arrvs ->
                  case idxi' < length arrvs of
                    True -> do
                      at <- withAt id
                      let valv_checked = checkType elem_ty valv `seq` valv
                      let arrvs' = take (idxi' - 1) arrvs ++ [valv_checked] ++ drop (idxi' + 1) arrvs
                      let arrv' = SLV_Array at elem_ty arrvs'
                      retV $ (lvl, arrv')
                    False ->
                      expect_ $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                SLV_DLVar arrdv@(DLVar _ _ arr_ty@(T_Array elem_ty sz) _) ->
                  case idxi < sz of
                    True -> do
                      valda <- compileCheckType (dt2st elem_ty) valv
                      doArrayBoundsCheck sz idxv
                      at <- withAt id
                      retArrDV arr_ty $ DLE_ArraySet at (DLA_Var arrdv) idxda valda
                    False ->
                      expect_ $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                _ -> illegal_args
              where
                idxi' = fromIntegral idxi
            (T_UInt, _) -> do
              (arr_ty, arrda) <- compileTypeOf arrv
              case arr_ty of
                T_Array elem_ty sz -> do
                  valda <- compileCheckType (dt2st elem_ty) valv
                  doArrayBoundsCheck sz idxv
                  at <- withAt id
                  retArrDV arr_ty $ DLE_ArraySet at arrda idxda valda
                _ -> illegal_args
            _ -> illegal_args
        _ -> illegal_args
      where
        retArrDV t de = do
          at <- withAt id
          dv <- ctxt_lift_expr (DLVar at "array_set" t) de
          return $ (lvl, SLV_DLVar dv)
    SLPrim_Tuple -> do
      vs <- mapM expect_ty $ map snd sargs
      retV $ (lvl, SLV_Type $ ST_Tuple vs)
    SLPrim_tuple_set -> do
      at <- withAt id
      case map snd sargs of
        [tup, (SLV_Int _ idxi), val] -> do
          tupvs <- explodeTupleLike "tuple_set" tup
          let go i v = if idxi == i then val else v
          let tupvs' = zipWith go [0 ..] tupvs
          let len = length tupvs
          unless (idxi < fromIntegral len) $
            expect_ $ Err_Eval_RefOutOfBounds len idxi
          return $ (lvl, SLV_Tuple at tupvs')
        _ -> illegal_args
    SLPrim_Object ->
      case map snd sargs of
        [(SLV_Object _ _ objm)] -> do
          vm <- mapM (expect_ty . sss_val) objm
          retV $ (lvl, SLV_Type $ ST_Object vm)
        _ -> illegal_args
    SLPrim_Object_has -> do
      at <- withAt id
      case map snd sargs of
        [obj, (SLV_Bytes _ bs)] -> do
          vm <- evalAsEnv obj
          retV $ (lvl, SLV_Bool at $ M.member (bunpack bs) vm)
        _ -> illegal_args
    SLPrim_makeEnum -> do
      at' <- withAt $ srcloc_at "makeEnum" Nothing
      case map snd sargs of
        [iv@(SLV_Int _ i)] ->
          retV $ (lvl, SLV_Tuple at' (enum_pred : map (SLV_Int at') [0 .. (i -1)]))
          where
            enum_pred = jsClo at' "makeEnum" "(x) => ((0 <= x) && (x < M))" (M.fromList [("M", iv)])
        _ -> illegal_args
    SLPrim_App_Delay {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_interact iat who m dom rng -> do
      ensure_mode SLM_LocalStep "interact"
      arges <- mapM (uncurry $ checkType) =<< zipEq (Err_Apply_ArgCount iat) dom (map snd sargs)
      dargs <- compileArgExprs arges
      fs <- e_stack <$> ask
      at <- withAt id
      compileInteractResult m rng (\drng -> DLE_Interact at fs who m drng dargs)
    SLPrim_declassify ->
      case map snd sargs of
        [val] ->
          case lvl of
            Secret -> retV $ public $ val
            Public -> expect_t val $ Err_ExpectedPrivate
        _ -> illegal_args
    SLPrim_commit ->
      case sargs of
        [] -> retV $ public $ SLV_Prim SLPrim_committed
        _ -> illegal_args
    SLPrim_committed -> illegal_args
    SLPrim_digest -> do
      let rng = T_Digest
      darges <- map snd <$> mapM (typeOf . snd) sargs
      dargs <- compileArgExprs darges
      at <- withAt id
      dv <- ctxt_lift_expr (DLVar at "digest" rng) (DLE_Digest at dargs)
      return $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct -> do
      let barg = compileCheckType ST_Bool
      (dargm, mmsg) <- case map snd sargs of
        [arg] -> return $ (barg arg, Nothing)
        [arg, marg] -> do
          bs <- mustBeBytes marg
          return $ (barg arg, Just bs)
        _ -> illegal_args
      darg <- dargm
      at <- withAt id
      fs <- e_stack <$> ask
      saveLift $
        DLS_Let at Nothing $
          DLE_Claim at fs ct darg mmsg
      let good = return $ public $ SLV_Null at "claim"
      let some_good ems = ensure_modes ems ("assert " <> show ct) >> good
      case ct of
        CT_Assume -> some_good [SLM_LocalStep]
        CT_Require -> some_good [SLM_ConsensusStep, SLM_ConsensusPure]
        CT_Assert -> good
        CT_Possible -> good
        CT_Unknowable {} -> impossible "unknowable"
    SLPrim_transfer ->
      mapM ensure_public sargs >>= \case
        [amt_sv] -> do
          at <- withAt id
          let transferToPrim = SLV_Prim (SLPrim_transfer_amt_to amt_sv)
          return $
            public $
              SLV_Object at (Just "transfer") $
                M.fromList [("to", SLSSVal srcloc_builtin Public transferToPrim)]
        _ -> illegal_args
    SLPrim_transfer_amt_to amt_sv -> do
      at <- withAt id
      ensure_mode SLM_ConsensusStep "transfer"
      amt_dla <- compileCheckType ST_UInt amt_sv
      doAssertBalance amt_sv PLE
      let convert = compileCheckType ST_Address
      who_dla <-
        case map snd sargs of
          [one] -> convert one
          _ -> illegal_args
      saveLift $ DLS_Let at Nothing $ DLE_Transfer at who_dla amt_dla
      doBalanceUpdate SUB amt_sv
      return $ public $ SLV_Null at "transfer.to"
    SLPrim_exit ->
      case sargs of
        [] -> do
          doExit
          return $ public $ SLV_Prim $ SLPrim_exitted
        _ -> illegal_args
    SLPrim_exitted -> illegal_args
    SLPrim_forall {} ->
      case sargs of
        [(olvl, one)] -> do
          dt <- st2dte =<< expect_ty one
          at <- withAt id
          dv <- ctxt_lift_expr (DLVar at "forall" dt) (DLE_Impossible at $ "cannot inspect value from forall")
          return $ (olvl, SLV_DLVar dv)
        [one, (tlvl, two)] -> do
          one' <- evalPrim SLPrim_forall [one]
          lvlMeet tlvl <$> evalApplyVals' two [one']
        _ -> illegal_args
    SLPrim_PrimDelay _at dp bargs aargs ->
      evalPrim dp $ bargs <> sargs <> aargs
    SLPrim_part_set ->
      case map snd sargs of
        [(SLV_Participant _ who _ _), addr] -> do
          addr_da <- compileCheckType ST_Address addr
          withAt $ \at -> (lvl, (SLV_Prim $ SLPrim_part_setted at who addr_da))
        _ -> illegal_args
    SLPrim_part_setted {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_Data -> do
      argm <- case args of
        [SLV_Object _ _ m] -> return m
        _ -> illegal_args
      varm <- mapM (expect_ty . sss_val) argm
      retV $ (lvl, SLV_Type $ ST_Data varm)
    SLPrim_Data_variant t vn vt -> do
      at <- withAt id
      vv <- case (vt, args) of
        (ST_Null, []) -> return $ SLV_Null at "variant"
        _ -> one_arg
      void $ checkType vt vv
      retV $ (lvl, SLV_Data at t vn vv)
    SLPrim_data_match -> do
      -- Expect two arguments to function
      (obj, cases) <- two_args
      -- Get the key/value pairs for the case object
      objEnv <- case cases of
        SLV_Object _ _ env -> return $ env
        ow ->
          locAtf (flip getSrcLocOrDefault ow) $
            expect_t ow $ Err_Decl_NotType "object"
      -- Keep a map of type constructor - args, for each case
      args_x_case <-
        mapM
          (\v ->
             case sss_val v of
               SLV_Clo _ _ case_args _ _ -> return $ case_args
               ow ->
                 locAtf (flip getSrcLocOrDefault ow) $
                   expect_t ow $ Err_Decl_NotType "closure")
          objEnv
      -- Generate the function to call
      fnv <- evalExpr $ do
        let ann = JSNoAnnot
        let semi = JSSemiAuto
        let data_param = JSIdentifier ann "data_id"
        let case_param = JSIdentifier ann "cases_id"
        let switch_parts =
              map
                (\(tycon, tycon_args) ->
                   let case_id = JSIdentifier ann tycon
                    in let fn = JSMemberDot case_param ann $ JSIdentifier ann tycon
                        in let js_args = case tycon_args of
                                 [] -> JSLNil
                                 _ -> JSLOne data_param
                            in let ret = JSCallExpression fn ann js_args ann
                                in let case_body = [JSReturn ann (Just ret) semi]
                                    in case tycon == "default" of
                                         True -> JSDefault ann ann case_body
                                         False -> JSCase ann case_id ann case_body)
                $ M.toList args_x_case
        let body = JSSwitch ann ann data_param ann ann switch_parts ann semi
        let params = mkArrowParameterList [data_param, case_param]
        JSArrowExpression params ann body
      -- Apply the object and cases to the newly created function
      let fn = snd fnv
      evalApplyVals' fn [public obj, public cases]
  where
    lvl = mconcatMap fst sargs
    args = map snd sargs
    illegal_args = expect_ts args $ Err_Prim_InvalidArgs p
    retV = return
    rator = SLV_Prim p
    getSrcLocOrDefault def sv =
      let loc = srclocOf sv
       in if loc == srcloc_builtin then def else loc
    expect_ty = \case
      SLV_Type t -> return $ t
      _ -> illegal_args
    zero_args = case args of
      [] -> return ()
      _ -> illegal_args
    one_arg = case args of
      [x] -> return $ x
      _ -> illegal_args
    two_args = case args of
      [x, y] -> return $ (x, y)
      _ -> illegal_args
    _three_args = case args of
      [x, y, z] -> return $ (x, y, z)
      _ -> illegal_args
    mustBeArray = \case
      T_Array ty sz -> return $ (ty, sz)
      _ -> illegal_args
    make_dlvar at' lab ty = do
      dv <- ctxt_mkvar $ DLVar at' lab ty
      return $ (dv, SLV_DLVar dv)

evalApplyVals' :: SLVal -> [SLSVal] -> App SLSVal
evalApplyVals' rator randvs = do
  SLAppRes _ val <- evalApplyVals rator randvs
  return $ val

evalApplyVals :: SLVal -> [SLSVal] -> App SLAppRes
evalApplyVals rator randvs =
  case rator of
    SLV_Prim p -> do
      sco <- e_sco <$> ask
      SLAppRes sco <$> evalPrim p randvs
    SLV_Clo clo_at mname formals (JSBlock body_a body _) (SLCloEnv clo_penvs clo_cenv) -> do
      ret <- ctxt_alloc
      let body_at = srcloc_jsa "block" body_a clo_at
      at <- withAt id
      arg_env <-
        evalDeclLHSs mempty
          =<< zipEq (Err_Apply_ArgCount clo_at) formals randvs
      let clo_sco =
            (SLScope
               { sco_ret = Just ret
               , sco_must_ret = RS_MayBeEmpty
               , sco_while_vars = Nothing
               , sco_penvs = clo_penvs
               , sco_cenv = clo_cenv
               })
      clo_sco' <- locSco clo_sco $ sco_update arg_env
      (body_lifts, (SLStmtRes clo_sco'' rs)) <-
        captureLifts $
          withFrame (SLC_CloApp at clo_at mname) $
            locAt body_at $
              locSco clo_sco' $
                evalStmt body
      let no_prompt (lvl, v) = do
            let lifts' =
                  case body_lifts of
                    body_lifts' Seq.:|> (DLS_Return _ the_ret_label _the_val)
                      | the_ret_label == ret ->
                        --- We don't check that the_val is v, because
                        --- we're relying on the invariant that there
                        --- was only one Return... this should be
                        --- true, but if something changes in the
                        --- future, this might be a place that an
                        --- error could be introduced.
                        body_lifts'
                    _ ->
                      return $ DLS_Prompt body_at (Left ret) body_lifts
            saveLifts lifts'
            return $ SLAppRes clo_sco'' $ (lvl, v)
      case rs of
        [] -> no_prompt $ public $ SLV_Null body_at "clo app"
        [(_, _, x, False)] -> no_prompt $ x
        (_, _, (xlvl, xv), _) : more -> do
          let msvs = map (\(_a, _b, c, _d) -> c) more
          let mlvls = map fst msvs
          let mvs = map snd msvs
          let lvl = mconcat $ xlvl : mlvls
          -- Note: This test might be too expensive, so try it
          let all_same = False && all (== xv) mvs
          case all_same of
            True -> no_prompt $ (lvl, xv)
            False -> do
              let go (r_at, rmi, (_, rv), _) = do
                    (rlifts, (rty, rda)) <-
                      captureLifts $ locAt r_at $ compileTypeOf rv
                    let retsm =
                          case rmi of
                            Nothing -> mempty
                            Just ri -> M.singleton ri (rlifts, rda)
                    return $ (retsm, (r_at, rty))
              (retsms, tys) <- unzip <$> mapM go rs
              let retsm = mconcat retsms
              r_ty <- locAt body_at $ dtypeMeets tys
              let dv = DLVar body_at "clo app" r_ty ret
              saveLift $ DLS_Prompt body_at (Right (dv, retsm)) body_lifts
              return $ SLAppRes clo_sco'' (lvl, (SLV_DLVar dv))
    v -> expect_t v $ Err_Eval_NotApplicableVals

evalApply :: SLVal -> [JSExpression] -> App SLSVal
evalApply rator rands =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm f rands
    v -> do
      fs <- e_stack <$> ask
      let modAt = case fs of
            [] -> id
            h : _ -> locAt (srclocOf h)
      modAt $ expect_t v $ Err_Eval_NotApplicable
  where
    vals = evalApplyVals' rator =<< evalExprs rands

getKwdOrPrim :: Ord k => k -> M.Map k SLSSVal -> Maybe SLSSVal
getKwdOrPrim ident env =
  case M.lookup ident env of
    -- Hack: Allow `default` to be used as object property name for `match`
    -- expr. Keywords are allowed as property names in JS anyway, *shrug*
    Just (SLSSVal _ _ (SLV_Kwd SLK_default)) -> Nothing
    Just s@(SLSSVal _ _ (SLV_Kwd _)) -> Just s
    Just s@(SLSSVal _ _ (SLV_Prim _)) -> Just s
    _ -> Nothing

evalPropertyName :: JSPropertyName -> App (SecurityLevel, String)
evalPropertyName = \case
  JSPropertyIdent an s -> do
    dummy_at <- withAt $ \at -> SLSSVal at Public $ SLV_Null at ""
    locAtf (srcloc_jsa "field" an) $
      -- Do not allow keywords or primitives to be used as property names
      case getKwdOrPrim s base_env of
        Just s' -> expect_ $ Err_Shadowed s s' dummy_at
        _ -> return $ public s
  JSPropertyString _ s -> return $ public $ trimQuotes s
  pn@(JSPropertyNumber an _) ->
    locAtf (srcloc_jsa "number" an) $
      expect_ $ Err_Obj_IllegalNumberField pn
  JSPropertyComputed an e _ ->
    locAtf (srcloc_jsa "computed field name" an) $ do
      (elvl, ev) <- evalExpr e
      case ev of
        SLV_Bytes _ fb -> return $ (elvl, bunpack fb)
        _ -> expect_t ev $ Err_Obj_IllegalComputedField

evalPropertyPair :: SLEnv -> JSObjectProperty -> App (SecurityLevel, SLEnv)
evalPropertyPair fenv = \case
  JSPropertyNameandValue pn a vs ->
    locAtf (srcloc_jsa "property binding" a) $ do
      (flvl, f) <- evalPropertyName pn
      case vs of
        [e] -> do
          at' <- withAt id
          sv' <- sls_sss at' <$> evalExpr e
          env' <- env_insert_ AllowShadowing f sv' fenv
          return $ (flvl, env')
        _ -> expect_ $ Err_Obj_IllegalFieldValues vs
  JSPropertyIdentRef a v -> evalPropertyPair fenv p'
    where
      p' = JSPropertyNameandValue pn a vs
      pn = JSPropertyIdent a v
      vs = [JSIdentifier a v]
  JSObjectSpread a se -> locAtf (srcloc_jsa "...obj" a) $ do
    (slvl, sv) <- evalExpr se
    let mkRes env = do
          env' <- env_merge_ AllowShadowing fenv env
          return $ (slvl, env')
    case sv of
      SLV_Object _ _ senv -> mkRes senv
      SLV_DLVar dlv@(DLVar _at _s (T_Object tenv) _i) -> do
        let mkOneEnv k t = do
              at' <- withAt id
              let de = DLE_ObjectRef at' (DLA_Var dlv) k
              let mdv = DLVar at' "obj_ref" t
              dv <- ctxt_lift_expr mdv de
              return $ M.singleton k $ SLSSVal at' slvl $ SLV_DLVar dv
        -- mconcat over SLEnvs is safe here b/c each is a singleton w/ unique key
        mkRes =<< (mconcatMapM (uncurry mkOneEnv) $ M.toList tenv)
      _ -> expect_ $ Err_Obj_SpreadNotObj sv
  p@(JSObjectMethod {}) ->
    --- FIXME support these
    expect_ $ Err_Obj_IllegalMethodDefinition p

evalId_ :: String -> SLVar -> App SLSSVal
evalId_ lab x = do
  sco <- e_sco <$> ask
  let c = ("consensus environment", return $ sco_cenv sco)
  let s = ("step environment", return $ sco_cenv sco)
  mwho <- e_who <$> ask
  let l = case mwho of
        Just who ->
          ( (show who <> "'s environment")
          , sco_lookup_penv who
          )
        Nothing -> impossible $ "no ctxt_only in local mode"
  m <- readSt st_mode
  let (elab, env) = case m of
        SLM_Module -> s
        SLM_Step -> s
        SLM_LocalStep -> l
        SLM_LocalPure -> l
        SLM_ConsensusStep -> c
        SLM_ConsensusPure -> c
  let _lab' = lab <> " in " <> elab
  infectWithId_sss x <$> (env_lookup (LC_RefFrom lab) x =<< env)

evalId :: String -> SLVar -> App SLSVal
evalId lab x = sss_sls <$> evalId_ lab x

evalExpr :: JSExpression -> App SLSVal
evalExpr e = case e of
  JSIdentifier a x ->
    locAtf (srcloc_jsa "id ref" a) $
      evalId "expression" x
  JSDecimal a ns -> locAtf (srcloc_jsa "decimal" a) $
    withAt $ \at -> public $ SLV_Int at $ numberValue 10 ns
  JSLiteral a l -> locAtf (srcloc_jsa "literal" a) $ do
    at' <- withAt id
    case l of
      "null" -> return $ public $ SLV_Null at' "null"
      "true" -> return $ public $ SLV_Bool at' True
      "false" -> return $ public $ SLV_Bool at' False
      "this" -> evalExpr $ JSIdentifier a l
      _ -> expect_ $ Err_Parse_IllegalLiteral l
  JSHexInteger a ns -> locAtf (srcloc_jsa "hex" a) $
    withAt $ \at -> public $ SLV_Int at $ numberValue 16 ns
  JSOctal a ns -> locAtf (srcloc_jsa "octal" a) $
    withAt $ \at -> public $ SLV_Int at $ numberValue 8 ns
  JSStringLiteral a s -> locAtf (srcloc_jsa "string" a) $
    withAt $ \at -> public $ SLV_Bytes at (bpack (trimQuotes s))
  JSRegEx _ _ -> illegal
  JSArrayLiteral a as _ -> locAtf (srcloc_jsa "tuple" a) $ do
    at' <- withAt id
    svs <- evalExprs $ jsa_flatten as
    let vs = map snd svs
    let lvl = mconcat $ map fst svs
    return $ (lvl, SLV_Tuple at' vs)
  JSAssignExpression _ _ _ -> illegal
  JSAwaitExpression _ _ -> illegal
  JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
  JSCallExpressionDot obj a field -> doDot obj a field
  JSCallExpressionSquare arr a idx _ -> doRef arr a idx
  JSClassExpression _ _ _ _ _ _ -> illegal
  JSCommaExpression _ _ _ -> illegal
  JSExpressionBinary lhs op rhs ->
    case op of
      JSBinOpAnd a -> tern a True
      JSBinOpOr a -> tern a False
      _ ->
        binaryToPrim op >>= \x ->
          doCallV x JSNoAnnot [lhs, rhs]
    where
      tern a isAnd = evalExpr $ JSExpressionTernary lhs a te a fe
        where
          (te, fe) = case isAnd of
            True -> (rhs, JSLiteral a "false")
            False -> (JSLiteral a "true", rhs)
  JSExpressionParen a ie _ -> locAtf (srcloc_jsa "paren" a) $ evalExpr ie
  JSExpressionPostfix _ _ -> illegal
  JSExpressionTernary ce a te fa fe -> locAtf (srcloc_jsa "?:" a) $ do
    t_at' <- withAt $ srcloc_jsa "?: > true" a
    f_at' <- withAt $ srcloc_jsa "?: > false" fa
    csv@(clvl, cv) <- evalExpr ce
    case cv of
      SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
        SLRes tlifts st_t tsv@(tlvl, tv) <-
          captureRes $ locAt t_at' $ evalExpr te
        SLRes flifts st_f fsv@(flvl, fv) <-
          captureRes $ locAt f_at' $ evalExpr fe
        let lvl = clvl <> tlvl <> flvl
        setSt =<< stMerge st_t st_f
        let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
        case isPure sa of
          True -> do
            saveLifts (tlifts <> flifts)
            lvlMeet lvl
              <$> evalPrim (SLPrim_op $ IF_THEN_ELSE) [csv, tsv, fsv]
          False -> do
            ret <- ctxt_alloc
            let add_ret e_at' elifts ev = do
                  (dlifts, (e_ty, da)) <-
                    captureLifts $ locAt e_at' $ compileTypeOf ev
                  let elifts' =
                        elifts <> dlifts
                          <> (return $ DLS_Return e_at' ret $ Right da)
                  return $ (elifts', e_ty)
            (tlifts', t_ty) <- add_ret t_at' tlifts tv
            (flifts', f_ty) <- add_ret f_at' flifts fv
            ty <- dtypeMeet (t_at', t_ty) (f_at', f_ty)
            at' <- withAt id
            let ans_dv = DLVar at' "clo app" ty ret
            saveLift $
              DLS_Prompt at' (Right (ans_dv, mempty)) $
                return $ DLS_If at' (DLA_Var cond_dv) sa tlifts' flifts'
            return $ (lvl, SLV_DLVar ans_dv)
      _ -> do
        let (n_at', ne) = case cv of
              SLV_Bool _ False -> (f_at', fe)
              _ -> (t_at', te)
        lvlMeet clvl <$> (locAt n_at' $ evalExpr ne)
  JSArrowExpression aformals a bodys -> locAtf (srcloc_jsa "arrow" a) $ do
    let body = jsArrowStmtToBlock bodys
    fformals <- withAt $ flip jsArrowFormalsToFunFormals aformals
    evalExpr $ JSFunctionExpression a JSIdentNone a fformals a body
  JSFunctionExpression a name _ jsformals _ body ->
    locAtf (srcloc_jsa "function exp" a) $ do
      at' <- withAt id
      let formals = parseJSFormals at' jsformals
      fname <-
        case name of
          JSIdentNone -> return $ Nothing
          JSIdentName na _ ->
            locAtf (srcloc_jsa "function name" na) $
              expect_ Err_Fun_NamesIllegal
      ce <- sco_to_cloenv . e_sco <$> ask
      return $ public $ SLV_Clo at' fname formals body ce
  JSGeneratorExpression _ _ _ _ _ _ _ -> illegal
  JSMemberDot obj a field -> doDot obj a field
  JSMemberExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
  JSMemberNew _ _ _ _ _ -> illegal
  JSMemberSquare arr a idx _ -> doRef arr a idx
  JSNewExpression _ _ -> illegal
  JSObjectLiteral a plist _ -> locAtf (srcloc_jsa "obj" a) $ do
    at' <- withAt id
    let f (lvl, oenv) pp = lvlMeet lvl <$> evalPropertyPair oenv pp
    (lvl, fenv) <- foldlM f (mempty, mempty) $ jsctl_flatten plist
    return $ (lvl, SLV_Object at' Nothing fenv)
  JSSpreadExpression _ _ -> illegal
  JSTemplateLiteral _ _ _ _ -> illegal
  JSUnaryExpression op ue ->
    unaryToPrim op
      >>= \x -> doCallV x JSNoAnnot [ue]
  JSVarInitExpression _ _ -> illegal
  JSYieldExpression _ _ -> illegal
  JSYieldFromExpression _ _ _ -> illegal
  where
    illegal = expect_ $ Err_Eval_IllegalJS e
    doCallV ratorv a rands =
      locAtf (srcloc_jsa "application" a) $
        evalApply ratorv rands
    doCall rator a rands = do
      (rator_lvl, ratorv) <-
        locAtf (srcloc_jsa "application, rator" a) $ evalExpr rator
      lvlMeet rator_lvl <$> doCallV ratorv a rands
    doDot obj a field = do
      at' <- withAt $ srcloc_jsa "dot" a
      (obj_lvl, objv) <- locAt at' $ evalExpr obj
      let fields = (jse_expect_id at') field
      fieldAt <-
        case field of
          JSIdentifier iat _ -> withAt $ srcloc_jsa "dot" iat
          _ -> return $ at'
      lvlMeet obj_lvl <$> (locAt fieldAt $ evalDot objv fields)
    doRef arr a idxe = locAtf (srcloc_jsa "array ref" a) $ do
      at' <- withAt id
      (arr_lvl, arrv) <- evalExpr arr
      (idx_lvl, idxv) <- evalExpr idxe
      let lvl = arr_lvl <> idx_lvl
      let retRef t de = do
            dv <- ctxt_lift_expr (DLVar at' "ref" t) de
            let ansv = SLV_DLVar dv
            return $ (lvl, ansv)
      let retArrayRef t sz arr_dla idx_dla = do
            doArrayBoundsCheck sz idxv
            retRef t $ DLE_ArrayRef at' arr_dla idx_dla
      let retTupleRef t arr_dla idx =
            retRef t $ DLE_TupleRef at' arr_dla idx
      let retVal idxi arrvs =
            case fromIntegerMay idxi >>= atMay arrvs of
              Nothing ->
                expect_ $ Err_Eval_RefOutOfBounds (length arrvs) idxi
              Just ansv ->
                return $ (lvl, ansv)
      case idxv of
        SLV_Int _ idxi ->
          case arrv of
            SLV_Array _ _ arrvs -> retVal idxi arrvs
            SLV_Tuple _ tupvs -> retVal idxi tupvs
            SLV_DLVar adv@(DLVar _ _ (T_Tuple ts) _) ->
              case fromIntegerMay idxi >>= atMay ts of
                Nothing ->
                  expect_ $ Err_Eval_RefOutOfBounds (length ts) idxi
                Just t -> retTupleRef t arr_dla idxi
                  where
                    arr_dla = DLA_Var adv
            SLV_DLVar adv@(DLVar _ _ (T_Array t sz) _) ->
              case idxi < sz of
                False ->
                  expect_ $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                True -> do
                  let arr_dla = DLA_Var adv
                  idx_dla <- withAt $ \at -> DLA_Literal (DLL_Int at idxi)
                  retArrayRef t sz arr_dla idx_dla
            _ -> expect_t arrv $ Err_Eval_RefNotRefable
        SLV_DLVar idxdv@(DLVar _ _ T_UInt _) -> do
          (arr_ty, arr_dla) <- compileTypeOf arrv
          case arr_ty of
            T_Array elem_ty sz ->
              retArrayRef elem_ty sz arr_dla $ DLA_Var idxdv
            _ -> expect_t arrv $ Err_Eval_IndirectRefNotArray
        _ -> expect_t idxv $ Err_Eval_RefNotInt

evalExprs :: [JSExpression] -> App [SLSVal]
evalExprs = \case
  [] -> return $ []
  (e0 : randN) -> do
    svals0 <-
      case e0 of
        JSSpreadExpression a rand0 ->
          locAtf (srcloc_jsa "spread" a) $ do
            (lvl, v0) <- evalExpr rand0
            let addlvl v = (lvl, v)
            map addlvl <$> explodeTupleLike "spread" v0
        rand0 -> (\x -> [x]) <$> evalExpr rand0
    svalN <- evalExprs randN
    return $ (svals0 <> svalN)

evalDeclLHSArray :: SecurityLevel -> SLEnv -> [SLVal] -> [JSExpression] -> App SLEnv
evalDeclLHSArray rhs_lvl lhs_env vs es =
  case (vs, es) of
    ([], []) ->
      return $ lhs_env
    (_, (JSSpreadExpression a e) : es') -> do
      locAtf (srcloc_jsa "array spread" a) $ do
        v <- withAt $ \at -> SLV_Tuple at vs
        case es' of
          [] -> evalDeclLHS rhs_lvl lhs_env v e
          _ -> expect_ $ Err_Decl_ArraySpreadNotLast
    (v : vs', e : es') -> do
      lhs_env' <- evalDeclLHS rhs_lvl lhs_env v e
      evalDeclLHSArray rhs_lvl lhs_env' vs' es'
    (_, _) ->
      expect_ $ Err_Decl_WrongArrayLength (length es) (length vs)

evalDeclLHSObject :: SecurityLevel -> SLEnv -> SLVal -> SLObjEnv -> [JSObjectProperty] -> App SLEnv
evalDeclLHSObject rhs_lvl lhs_env orig_v vm = \case
  [] -> return $ lhs_env
  (JSObjectSpread a e) : os' -> do
    locAtf (srcloc_jsa "object spread" a) $
      case os' of
        [] -> do
          vom <- evalObjEnv vm
          vo <- withAt $ \at_ -> SLV_Object at_ Nothing vom
          evalDeclLHS rhs_lvl lhs_env vo e
        _ -> expect_ $ Err_Decl_ObjectSpreadNotLast
  o : os' -> do
    let go x e = do
          (v_lvl, v) <- evalDot_ orig_v vm x
          let lvl' = rhs_lvl <> v_lvl
          lhs_env' <- evalDeclLHS lvl' lhs_env v e
          let vm' = M.delete x vm
          evalDeclLHSObject rhs_lvl lhs_env' orig_v vm' os'
    case o of
      JSPropertyIdentRef a x -> do
        go x $ JSIdentifier a x
      JSPropertyNameandValue pn _ [e] ->
        flip go e =<< (snd <$> evalPropertyName pn)
      _ ->
        expect_ $ Err_Parse_ExpectIdentifierProp o

evalDeclLHS :: SecurityLevel -> SLEnv -> SLVal -> JSExpression -> App SLEnv
evalDeclLHS rhs_lvl lhs_env v = \case
  JSIdentifier a x -> do
    locAtf (srcloc_jsa "id" a) $ do
      at_ <- withAt id
      env_insert x (SLSSVal at_ rhs_lvl v) lhs_env
  JSArrayLiteral a xs _ -> do
    locAtf (srcloc_jsa "array" a) $ do
      vs <- explodeTupleLike "lhs array" v
      evalDeclLHSArray rhs_lvl lhs_env vs (jsa_flatten xs)
  JSObjectLiteral a props _ -> do
    locAtf (srcloc_jsa "object" a) $ do
      vm <- evalAsEnv v
      evalDeclLHSObject rhs_lvl lhs_env v vm (jso_flatten props)
  e -> expect_ $ Err_DeclLHS_IllegalJS e

evalDeclLHSs :: SLEnv -> [(JSExpression, SLSVal)] -> App SLEnv
evalDeclLHSs lhs_env = \case
  [] -> return $ lhs_env
  (e, (rhs_lvl, v)) : more ->
    flip evalDeclLHSs more =<< evalDeclLHS rhs_lvl lhs_env v e

evalDecl :: JSExpression -> JSExpression -> App SLEnv
evalDecl lhs rhs = do
  (rhs_lvl, rhs_v) <- evalExpr rhs
  evalDeclLHS rhs_lvl mempty rhs_v lhs

destructDecls :: (JSCommaList JSExpression) -> App (JSExpression, JSExpression)
destructDecls = \case
  (JSLOne (JSVarInitExpression lhs (JSVarInit _ rhs))) -> return (lhs, rhs)
  es -> expect_ $ Err_Decls_IllegalJS es

-- | Make sure all bindings in this SLEnv respect the rule that
-- private vars must be named with a leading underscore.
enforcePrivateUnderscore :: SLEnv -> App ()
enforcePrivateUnderscore = mapM_ enf . M.toList
  where
    enf (k, (SLSSVal _ secLev _)) = case secLev of
      Secret
        | not (isSpecialIdent k)
            && not (isSecretIdent k) ->
          expect_ $ Err_Eval_NotSecretIdent k
      _ -> return ()

doOnlyExpr :: ((SLPart, Maybe SLVar), SrcLoc, SLCloEnv, JSExpression) -> App (SLEnv, DLType, SLVal)
doOnlyExpr ((who, vas), only_at, only_cloenv, only_synarg) = do
  st <- readSt id
  sco <- e_sco <$> ask
  let SLCloEnv only_penvs only_cenv = only_cloenv
  let st_localstep = st {st_mode = SLM_LocalStep}
  let sco_only_pre =
        sco
          { sco_penvs = only_penvs
          , sco_cenv = only_cenv
          }
  locWho who $
    locSco sco_only_pre $
      locSt st_localstep $ do
        penv__ <- sco_lookup_penv who
        ios <- e_ios <$> ask
        let penv_ =
              -- Ensure that only has access to "interact" if it didn't before,
              -- such as when an only occurs inside of a closure in a module body
              case M.member "interact" penv__ of
                True -> penv__
                False -> M.insert "interact" (ios M.! who) penv__
        me_dv <- doGetSelfAddress who
        let me_v = SLV_DLVar me_dv
        let ssv_here = SLSSVal only_at Public
        let add_this v env = M.insert "this" (ssv_here v) env
        isClass <- is_class who
        penv <-
          case vas of
            Nothing -> return $ add_this me_v penv_
            Just v ->
              case M.lookup v penv_ of
                Just (SLSSVal pv_at pv_lvl pv@(SLV_Participant at_ who_ mv_ mdv)) ->
                  case mdv of
                    Just _ | not isClass -> return $ add_this pv penv_
                    _ -> do
                      -- If this is a class, then we will always overwrite the DV,
                      -- because each instance of an Only could be a different one.
                      -- This might be a little weird, because for any given
                      -- participant, their self address is actually always the
                      -- same, but the whole point is to ensure that we can't
                      -- require(P == P) where P is a class.
                      let pv' = SLV_Participant at_ who_ mv_ (Just me_dv)
                      return $
                        add_this pv' $
                          M.insert v (SLSSVal pv_at pv_lvl pv') penv_
                _ -> return $ add_this me_v $ M.insert v (ssv_here me_v) penv_
        let sco_only =
              sco_only_pre {sco_penvs = M.insert who penv (sco_penvs sco_only_pre)}
        locAt only_at $
          locSco sco_only $ do
            only_arg <- snd <$> evalExpr only_synarg
            case only_arg of
              only_clo@(SLV_Clo _ _ [] _ _) -> do
                SLAppRes sco' (_, only_v) <- evalApplyVals only_clo []
                let penv' = (sco_penvs sco') M.! who
                --- TODO: check less things
                enforcePrivateUnderscore penv'
                only_ty <- fst <$> typeOf only_v
                return (penv', only_ty, only_v)
              _ -> expect_t only_arg $ Err_Only_NotOneClosure

doOnly :: SLScope -> ((SLPart, Maybe SLVar), SrcLoc, SLCloEnv, JSExpression) -> App SLScope
doOnly sco ((who, vas), only_at, only_cloenv, only_synarg) = locAt only_at $ do
  (alifts, (penv', only_ty, only_v)) <- captureLifts $ doOnlyExpr ((who, vas), only_at, only_cloenv, only_synarg)
  case only_ty of
    T_Null -> do
      saveLift $ DLS_Only only_at who alifts
      return $ sco {sco_penvs = M.insert who penv' $ sco_penvs sco}
    ty -> expect_ $ Err_Block_NotNull ty only_v

doGetSelfAddress :: SLPart -> App DLVar
doGetSelfAddress who = do
  let whos = bunpack who
  isClass <- is_class who
  addrNum <-
    case isClass of
      False -> return $ 0
      True -> ctxt_alloc
  Env {..} <- ask
  ctxt_lift_expr
    (DLVar e_at whos T_Address)
    (DLE_PrimOp
       e_at
       SELF_ADDRESS
       [ DLA_Literal (DLL_Bytes who)
       , DLA_Literal (DLL_Bool isClass)
       , DLA_Literal (DLL_Int e_at $ fromIntegral addrNum)
       ])

all_just :: [Maybe a] -> Maybe [a]
all_just = \case
  [] -> Just []
  Nothing : _ -> Nothing
  Just x : xs ->
    case all_just xs of
      Just xs' -> Just $ x : xs'
      Nothing -> Nothing

doToConsensus :: [JSStatement] -> S.Set SLPart -> Maybe SLVar -> [SLVar] -> JSExpression -> JSExpression -> Maybe (SrcLoc, JSExpression, JSBlock) -> App SLStmtRes
doToConsensus ks whos vas msg amt_e when_e mtime = do
  at <- withAt id
  st <- readSt id
  ensure_mode SLM_Step "to consensus"
  ensure_live "to consensus"
  let st_pure = st {st_mode = SLM_ConsensusPure}
  let pdvs = st_pdvs st
  let ctepee t e = compileCheckType t =<< ensure_public =<< evalExpr e
  -- We go back to the original env from before the to-consensus step
  -- Handle sending
  let tc_send1 who = do
        let st_lpure = st {st_mode = SLM_LocalPure}
        (only_lifts, res) <-
          locWho who $
            locSt st_lpure $
              captureLifts $ do
                let repeat_dv = M.lookup who pdvs
                isClass <- is_class who
                msg_das <- mapM (\v -> snd <$> (compileTypeOf =<< ensure_public =<< evalId "publish msg" v)) msg
                amt_da <- ctepee ST_UInt amt_e
                when_da <- ctepee ST_Bool when_e
                return (repeat_dv, isClass, msg_das, amt_da, when_da)
        saveLift $ DLS_Only at who only_lifts
        return $ res
  tc_send' <- sequence $ M.fromSet tc_send1 whos
  let tc_send = fmap (\(_, a, b, c, d) -> (a, b, c, d)) tc_send'
  let msg_dass = fmap (\(_, _, b, _, _) -> b) tc_send'
  let msg_dass_t = transpose $ M.elems $ msg_dass
  let get_msg_t msg_das = do
        let ts = map argTypeOf msg_das
        dtypeMeets $ map ((,) at) ts
  msg_ts <- mapM get_msg_t msg_dass_t
  let mrepeat_dvs = all_just $ M.elems $ M.map (\(x, _, _, _, _) -> x) tc_send'
  -- Handle receiving / consensus
  winner_dv <- ctxt_mkvar $ DLVar at "race winner" T_Address
  let recv_imode = AllowShadowingRace whos (S.fromList msg)
  whosc <- mapM (\w -> (,) w <$> is_class w) $ S.toList whos
  (who_env_mod, pdvs_recv) <-
    case whosc of
      [(who, False)] -> do
        let who_dv = fromMaybe winner_dv (M.lookup who pdvs)
        let pdvs' = M.insert who who_dv pdvs
        let add_who_env env =
              case vas of
                Nothing -> return $ env
                Just whov ->
                  evalId_ "publish who binding" whov >>= \case
                    (SLSSVal idAt lvl_ (SLV_Participant at_ who_ as_ _)) ->
                      return $ M.insert whov (SLSSVal idAt lvl_ (SLV_Participant at_ who_ as_ (Just who_dv))) env
                    _ ->
                      impossible $ "participant is not participant"
        return $ (add_who_env, pdvs')
      _ -> do
        return $ (return, pdvs)
  let st_recv =
        st
          { st_mode = SLM_ConsensusStep
          , st_pdvs = pdvs_recv
          }
  msg_dvs <- mapM (\t -> ctxt_mkvar (DLVar at "msg" t)) msg_ts
  msg_env <- foldlM env_insertp mempty $ zip msg $ map (sls_sss at . public . SLV_DLVar) $ msg_dvs
  let recv_env_mod = who_env_mod . (M.insert "this" (SLSSVal at Public $ SLV_DLVar winner_dv))
  let recv_env = msg_env
  (tc_recv, k_st, k_cr) <- do
    SLRes conlifts k_st (mktc_recv, k_cr) <- captureRes $ do
      setSt st_recv
      sco_recv <- sco_update_and_mod recv_imode recv_env recv_env_mod
      locSco sco_recv $ do
        amt_dv <- ctxt_mkvar $ DLVar at "amt" T_UInt
        let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op PEQ) [(Public, SLV_DLVar amt_dv)] []
        cmp_v <- locSt st_pure $ evalApply cmp_rator [amt_e]
        let req_rator = SLV_Prim $ SLPrim_claim CT_Require
        _ <-
          locSt st_pure $
            evalApplyVals req_rator $
              [cmp_v, public $ SLV_Bytes at $ "pay amount correct"]
        let check_repeat whoc_v repeat_dv = do
              repeat_cmp_v <-
                evalPrimOp ADDRESS_EQ $
                  map (public . SLV_DLVar) [repeat_dv, winner_dv]
              evalPrimOp IF_THEN_ELSE $
                [whoc_v, (public $ SLV_Bool at True), repeat_cmp_v]
        whoc_v <-
          locSt st_pure $
            case mrepeat_dvs of
              Just repeat_dvs@(_ : _) ->
                foldM check_repeat (public $ SLV_Bool at False) repeat_dvs
              _ ->
                return $ (public $ SLV_Bool at True)
        void $
          locSt st_pure $
            evalApplyVals' req_rator $
              [whoc_v, public $ SLV_Bytes at $ "sender correct"]
        doBalanceUpdate ADD (SLV_DLVar amt_dv)
        time_dv <- ctxt_mkvar $ DLVar at "ctime" T_UInt
        doFluidSet FV_thisConsensusTime $ public $ SLV_DLVar time_dv
        k_cr <- evalStmt ks
        let mktc_recv x = (winner_dv, msg_dvs, amt_dv, time_dv, x)
        return $ (mktc_recv, k_cr)
    return $ (mktc_recv conlifts, k_st, k_cr)
  -- Handle timeout
  let not_true_send = \case
        (_, (_, _, _, _, DLA_Literal (DLL_Bool True))) -> False
        (_, (_, _, _, _, _)) -> True
  let not_all_true_send =
        getAny $ mconcat $ map (Any . not_true_send) (M.toList tc_send')
  let mustHaveTimeout = S.null whos || not_all_true_send
  (tc_mtime, fcr) <-
    case mtime of
      Nothing -> do
        when mustHaveTimeout $ expect_ $ Err_ToConsensus_WhenNoTimeout
        setSt k_st
        return $ (Nothing, k_cr)
      Just (time_at, delay_e, (JSBlock _ time_ss _)) -> locAt time_at $ do
        ensure_can_wait
        delay_da <- locSt st_pure $ ctepee ST_UInt delay_e
        SLRes time_lifts time_st time_cr <- captureRes $ evalStmt time_ss
        setSt k_st
        mergeSt time_st
        fcr <- combineStmtRes Public k_cr time_st time_cr
        return $ (Just (delay_da, time_lifts), fcr)
  -- Prepare final result
  saveLift $ DLS_ToConsensus at tc_send tc_recv tc_mtime
  return $ fcr

srcloc2annot :: SrcLoc -> JSAnnot
srcloc2annot = \case
  SrcLoc _ (Just x) _ -> JSAnnot x []
  _ -> JSNoAnnot

typeToExpr :: DLType -> JSExpression
typeToExpr = \case
  T_Null -> var "Null"
  T_Bool -> var "Bool"
  T_UInt -> var "UInt"
  T_Bytes i -> call "Bytes" [ie i]
  T_Digest -> var "Digest"
  T_Address -> var "Address"
  T_Array t i -> call "Array" [r t, ie i]
  T_Tuple ts -> call "Tuple" $ map r ts
  T_Object m -> call "Object" [rm m]
  T_Data m -> call "Data" [rm m]
  where
    call f es = JSCallExpression (var f) a (toJSCL es) a
    var = JSIdentifier a
    r = typeToExpr
    a = JSNoAnnot
    ie = JSDecimal a . show
    rm m = JSObjectLiteral a (JSCTLNone $ toJSCL $ map rm1 $ M.toList m) a
    rm1 (k, t) = JSPropertyNameandValue (JSPropertyIdent a k) a [r t]

data CompiledForkCase = CompiledForkCase
  { cfc_part :: JSExpression
  , cfc_data_def :: JSObjectProperty
  , cfc_pay_prop :: JSObjectProperty
  , cfc_req_prop :: JSObjectProperty
  , cfc_switch_case :: JSSwitchParts
  , cfc_only :: JSStatement
  }

doFork :: [JSStatement] -> [(SrcLoc, (JSExpression, JSExpression, JSExpression, JSExpression))] -> Maybe (SrcLoc, JSExpression, JSBlock) -> App SLStmtRes
doFork ks cases mtime = do
  idx <- ctxt_alloc
  let fid x = ".fork" <> (show idx) <> "." <> x
  a <- withAt srcloc2annot
  let jid = JSIdentifier a
  let sp = JSSemi a
  let fd_e = jid (fid "data")
  let res_e = jid (fid "res")
  let msg_e = jid (fid "msg")
  let when_e = jid (fid "when")
  let thunkify e = JSArrowExpression (JSParenthesizedArrowParameterList a JSLNil a) a (JSExpressionStatement e sp)
  seenr <- liftIO $ newIORef (mempty :: M.Map SLPart SrcLoc)
  let go (c_at, (who_e, before_e, pay_e, after_e)) = locAt c_at $ do
        let cfc_part = who_e
        let who = jse_expect_id c_at who_e
        let who_s = bpack who
        seen <- liftIO $ readIORef seenr
        case M.lookup who_s seen of
          Just prev_at ->
            expect_ $ Err_Fork_CaseAppearsTwice who_s prev_at c_at
          Nothing -> do
            liftIO $ modifyIORef seenr (M.insert who_s c_at)
        let var_i = who
        let var_e = jid var_i
        let mkobjp x = JSPropertyNameandValue (JSPropertyIdent a var_i) a [x]
        let before_call_e = JSCallExpression before_e a JSLNil a
        let only_before_call_e = JSCallExpression (JSMemberDot who_e a (jid "only")) a (JSLOne $ before_e) a
        (_, (_, res_sv)) <- captureLifts $ evalExpr only_before_call_e
        (_, (_, res_ty, _)) <-
          case res_sv of
            SLV_Form (SLForm_EachAns [(who_, vas)] only_at only_cloenv only_synarg) ->
              captureLifts $
                doOnlyExpr ((who_, vas), only_at, only_cloenv, only_synarg)
            _ -> impossible $ "not each"
        isBound <- readSt $ M.member who_s . st_pdvs
        res_ty_m <-
          case res_ty of
            T_Object m -> return $ m
            _ -> expect_ $ Err_Fork_ResultNotObject res_ty
        let resHas = flip M.member res_ty_m
        let msg_ty = fromMaybe T_Null $ M.lookup "msg" res_ty_m
        let this_eq_who = JSExpressionBinary (jid "this") (JSBinOpEq a) who_e
        let req_e =
              case isBound of
                True -> this_eq_who
                False -> JSLiteral a "true"
        let cfc_req_prop = mkobjp $ thunkify req_e
        let cfc_pay_prop = mkobjp $ pay_e
        let cfc_data_def = mkobjp $ typeToExpr msg_ty
        let when_de =
              case resHas "when" of
                True -> JSMemberDot res_e a (jid "when")
                False -> JSLiteral a "true"
        let msg_de =
              case resHas "msg" of
                True -> JSMemberDot res_e a (jid "msg")
                False -> JSLiteral a "null"
        let msg_vde = JSCallExpression (JSMemberDot fd_e a var_e) a (JSLOne msg_de) a
        let defcon l r =
              JSConstant a (JSLOne $ JSVarInitExpression l $ JSVarInit a r) sp
        let only_body =
              [ defcon res_e before_call_e
              , defcon msg_e msg_vde
              , defcon when_e when_de
              ]
        isClass <- is_class who_s
        let who_is_this_ss =
              case (isBound, isClass) of
                (True, _) ->
                  [JSMethodCall (jid "assert") a (JSLOne $ this_eq_who) a sp]
                (False, False) ->
                  [JSMethodCall (JSMemberDot who_e a (jid "set")) a (JSLOne (jid "this")) a sp]
                (False, True) -> []
        let getAfter = \case
              JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ s -> return [s]
              JSArrowExpression (JSParenthesizedArrowParameterList _ (JSLOne ae) _) _ s -> return [defcon ae msg_e, s]
              JSExpressionParen _ e _ -> getAfter e
              _ -> expect_ $ Err_Fork_ConsensusBadArrow after_e
        after_ss <- getAfter after_e
        let cfc_switch_case = JSCase a var_e a $ who_is_this_ss <> after_ss
        let cfc_only = JSMethodCall (JSMemberDot who_e a (jid "only")) a (JSLOne (JSArrowExpression (JSParenthesizedArrowParameterList a JSLNil a) a (JSStatementBlock a only_body a sp))) a sp
        return $ CompiledForkCase {..}
  casel <- mapM go cases
  let cases_data_def = map cfc_data_def casel
  let cases_parts = map cfc_part casel
  let cases_pay_props = map cfc_pay_prop casel
  let cases_req_props = map cfc_req_prop casel
  let cases_switch_cases = map cfc_switch_case casel
  let cases_onlys = map cfc_only casel
  let mkobj l = JSObjectLiteral a (JSCTLNone $ toJSCL l) a
  let fd_def = JSCallExpression (jid "Data") a (JSLOne $ mkobj cases_data_def) a
  let data_decls = JSLOne $ JSVarInitExpression fd_e $ JSVarInit a fd_def
  let data_ss = [JSConstant a data_decls sp]
  let race_e = JSCallExpression (jid "race") a (toJSCL cases_parts) a
  let tc_pub_e = JSCallExpression (JSMemberDot race_e a (jid "publish")) a (JSLOne msg_e) a
  let tc_when_e = JSCallExpression (JSMemberDot tc_pub_e a (jid "when")) a (JSLOne when_e) a
  let pay_e = JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_pay_props) a
  let tc_pay_e = JSCallExpression (JSMemberDot tc_when_e a (jid "pay")) a (JSLOne pay_e) a
  let tc_time_e =
        case mtime of
          Nothing -> tc_pay_e
          Just (_, td, JSBlock tbb tbs tba) ->
            JSCallExpression (JSMemberDot tc_pay_e a (jid "timeout")) tbb (toJSCL [td, (JSArrowExpression (JSParenthesizedArrowParameterList tbb JSLNil tba) tba (JSStatementBlock tbb tbs tba sp))]) tba
  let tc_e = tc_time_e
  let tc_ss = [JSExpressionStatement tc_e sp]
  let req_arg = JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_req_props) a
  let req_e = JSCallExpression (jid "require") a (JSLOne $ req_arg) a
  let req_ss = [JSExpressionStatement req_e sp]
  let switch_ss = [JSSwitch a a msg_e a a cases_switch_cases a sp]
  let before_tc_ss = data_ss <> cases_onlys
  let after_tc_ss = req_ss <> switch_ss
  let exp_ss = before_tc_ss <> tc_ss <> after_tc_ss
  evalStmt $ exp_ss <> ks

doParallelReduce :: JSExpression -> SrcLoc -> Maybe ParallelReduceMode -> JSExpression -> Maybe JSExpression -> Maybe JSExpression -> [(SrcLoc, [JSExpression])] -> Maybe (SrcLoc, [JSExpression]) -> App [JSStatement]
doParallelReduce lhs pr_at pr_mode init_e pr_minv pr_mwhile pr_cases pr_mtime = locAt pr_at $ do
  idx <- ctxt_alloc
  let prid x = ".pr" <> (show idx) <> "." <> x
  case pr_mode of
    Just x -> expect_ $ Err_ParallelReduceIncomplete $ "unapplied component: " <> show x
    Nothing -> return ()
  let ao = srcloc2annot
  let a = ao pr_at
  let jid = JSIdentifier a
  let sp = JSSemi a
  let want lab = \case
        Just x -> return x
        Nothing -> expect_ $ Err_ParallelReduceIncomplete $ "missing " <> lab
  inv_e <- want "invariant" pr_minv
  while_e <- want "while" pr_mwhile
  let var_decls = JSLOne (JSVarInitExpression lhs (JSVarInit a init_e))
  let var_s = JSVariable a var_decls sp
  let inv_s = JSMethodCall (JSIdentifier a "invariant") a (JSLOne inv_e) a sp
  let fork_e0 = JSCallExpression (jid "fork") a JSLNil a
  let fork_e1 =
        case pr_mtime of
          Nothing -> fork_e0
          Just (t_at, t_es) ->
            JSCallExpression (JSMemberDot fork_e0 ta (jid "timeout")) ta (toJSCL t_es) ta
            where
              ta = ao t_at
  let forkcase fork_eN (case_at, case_es) = JSCallExpression (JSMemberDot fork_eN ca (jid "case")) ca (toJSCL case_es) ca
        where
          ca = ao case_at
  let fork_e = foldl' forkcase fork_e1 pr_cases
  let fork_arr_e = JSFunctionExpression a JSIdentNone a JSLNil a (JSBlock a [JSExpressionStatement fork_e sp] a)
  let call_e = JSCallExpression fork_arr_e a JSLNil a
  let commit_s = JSMethodCall (jid "commit") a JSLNil a sp
  let def_e = jid $ prid "res"
  let def_s = JSConstant a (JSLOne $ JSVarInitExpression def_e $ JSVarInit a call_e) sp
  let asn_s = JSAssignStatement lhs (JSAssign a) def_e sp
  let continue_s = JSContinue a JSIdentNone sp
  let while_body = [commit_s, def_s, asn_s, continue_s]
  let while_s = JSWhile a a while_e a $ JSStatementBlock a while_body a sp
  let pr_ss = [var_s, inv_s, while_s]
  return $ pr_ss

evalStmtTrampoline :: JSSemi -> [JSStatement] -> SLVal -> App SLStmtRes
evalStmtTrampoline sp ks = \case
  SLV_Prim (SLPrim_part_setted at' who addr_da) -> locAt at' $ do
    ensure_mode SLM_ConsensusStep "participant set"
    st <- readSt id
    let pdvs = st_pdvs st
    isClass <- is_class who
    when isClass $
      expect_ $ Err_Eval_PartSet_Class who
    case M.lookup who pdvs of
      Just olddv ->
        case DLA_Var olddv == addr_da of
          True -> evalStmt ks
          False -> expect_ $ Err_Eval_PartSet_Bound who
      Nothing -> do
        let who_s = bunpack who
        whodv <- ctxt_lift_expr (DLVar at' who_s T_Address) (DLE_PartSet at' who addr_da)
        let pdvs' = M.insert who whodv pdvs
        let st' = st {st_pdvs = pdvs'}
        setSt st'
        evalStmt ks
  SLV_Prim SLPrim_exitted -> do
    expect_empty_tail "exit" JSNoAnnot sp ks
    sco <- e_sco <$> ask
    return $ SLStmtRes sco []
  SLV_Form (SLForm_EachAns parts only_at only_cloenv only_synarg) -> do
    ensure_modes [SLM_Step, SLM_ConsensusStep] "local action (only or each)"
    sco <- e_sco <$> ask
    sco' <-
      foldM doOnly sco $
        map (\who -> (who, only_at, only_cloenv, only_synarg)) parts
    locSco sco' $ evalStmt ks
  SLV_Form (SLForm_Part_ToConsensus to_at whos vas Nothing mmsg mamt mwhen mtime) -> locAt to_at $ do
    let msg = fromMaybe [] mmsg
    let amt = fromMaybe (JSDecimal JSNoAnnot "0") mamt
    let whene = fromMaybe (JSLiteral JSNoAnnot "true") mwhen
    doToConsensus ks whos vas msg amt whene mtime
  SLV_Form (SLForm_fork_partial f_at Nothing cases mtime) ->
    locAt f_at $ doFork ks cases mtime
  SLV_Prim SLPrim_committed -> do
    ensure_mode SLM_ConsensusStep "commit"
    sco <- e_sco <$> ask
    at <- withAt id
    st <- readSt id
    setSt $
      st
        { st_mode = SLM_Step
        , st_after_first = True
        }
    let sco' = sco_set "this" (SLSSVal at Public $ SLV_Kwd $ SLK_this) sco
    (steplifts, cr) <-
      captureLifts $
        locSco sco' $ evalStmt ks
    saveLift $ DLS_FromConsensus at steplifts
    return $ cr
  ev ->
    typeOf ev >>= \case
      (T_Null, _) -> evalStmt ks
      (ty, _) -> expect_ $ Err_Block_NotNull ty ev

doExit :: App ()
doExit = do
  ensure_mode SLM_Step "exit"
  ensure_live "exit"
  at <- withAt id
  let zero = SLV_Int at 0
  doAssertBalance zero PEQ
  saveLift $ DLS_Stop at
  st <- readSt id
  setSt $ st {st_live = False}

doWhileLikeInitEval :: JSExpression -> JSExpression -> App (M.Map SLVar DLVar, DLAssignment, SLScope)
doWhileLikeInitEval lhs rhs = do
  at <- withAt id
  vars_env <- unchangedSt $ evalDecl lhs rhs
  let help v (SLSSVal _ _ val) = do
        (t, da) <- typeOf val
        dv <- ctxt_mkvar $ DLVar at v t
        return $ (dv, da)
  helpm <- M.traverseWithKey help vars_env
  let unknown_var_env = M.map (sls_sss at . public . SLV_DLVar . fst) helpm
  sco_env' <- sco_update unknown_var_env
  let init_daem = M.fromList $ M.elems helpm
  let init_vars = M.map fst helpm
  init_dam <- compileArgExprMap init_daem
  let init_dl = DLAssignment init_dam
  return $ (init_vars, init_dl, sco_env')

doWhileLikeContinueEval :: JSExpression -> M.Map SLVar DLVar -> SLSVal -> App ()
doWhileLikeContinueEval lhs whilem (rhs_lvl, rhs_v) = do
  at <- withAt id
  decl_env <- evalDeclLHS rhs_lvl mempty rhs_v lhs
  stEnsureMode SLM_ConsensusStep
  forM_
    (M.keys decl_env)
    (\v ->
       case M.lookup v whilem of
         Nothing -> expect_ $ Err_Eval_ContinueNotLoopVariable v
         Just _ -> return ())
  let f (v, dv) = do
        let sv = case M.lookup v decl_env of
              Nothing -> SLSSVal at Public $ SLV_DLVar dv
              Just x -> x
        let DLVar _ _ et _ = dv
        val <- ensure_public $ sss_sls sv
        dae <- checkType (dt2st et) val
        return $ (dv, dae)
  cont_daem <- M.fromList <$> (mapM f $ M.toList whilem)
  cont_dam' <- compileArgExprMap cont_daem
  let cont_das = DLAssignment cont_dam'
  saveLift $ DLS_Continue at cont_das

evalPureExprToBlock :: JSExpression -> SLType -> App DLBlock
evalPureExprToBlock e rest = do
  at <- withAt id
  st <- readSt id
  let pure_st = st {st_mode = SLM_ConsensusPure}
  fs <- e_stack <$> ask
  (e_lifts, e_da) <-
    captureLifts $
      locSt pure_st $
        compileCheckType rest =<< (snd <$> evalExpr e)
  return $ DLBlock at fs e_lifts e_da

evalStmt :: [JSStatement] -> App SLStmtRes
evalStmt = \case
  [] -> do
    sco <- e_sco <$> ask
    at <- withAt id
    let ret = return . SLStmtRes sco
    case sco_must_ret sco of
      RS_CannotReturn -> ret []
      RS_ImplicitNull -> ret [(at, Nothing, (public $ SLV_Null at "implicit null"), False)]
      RS_NeedExplicit -> do
        --- In the presence of `exit()`, it is okay to have a while
        --- that ends in an empty tail, if the empty tail is
        --- dominated by an exit(). Here we really on two properties
        --- of the linearizer and the verifier: first, the
        --- linearizer will completely drop the continuation of
        --- DLS_Continue and DLS_Stop, so if this assert is not
        --- removed, then ti will error.
        fs <- e_stack <$> ask
        saveLift $ DLS_Let at Nothing $ DLE_Claim at fs CT_Assert (DLA_Literal $ DLL_Bool False) (Just "unreachable")
        ret []
      RS_MayBeEmpty -> ret []
  ((JSStatementBlock a ss' _ sp) : ks) -> do
    br <- locAtf (srcloc_jsa "block" a) $ evalStmt ss'
    locAtf (srcloc_after_semi "block" a sp) $ retSeqn br ks
  (s@(JSBreak a _ _) : _) -> illegal a s "break"
  (s@(JSLet a _ _) : _) -> illegal a s "let"
  (s@(JSClass a _ _ _ _ _ _) : _) -> illegal a s "class"
  ((JSConstant a decls sp) : ks) -> do
    let lab = "const"
    locAtf (srcloc_jsa lab a) $ do
      (lhs, rhs) <- destructDecls decls
      (rhs_lvl, rhs_v) <- evalExpr rhs
      case rhs_v of
        SLV_Form (SLForm_parallel_reduce_partial {..}) -> do
          pr_ss <- doParallelReduce lhs slpr_at slpr_mode slpr_init slpr_minv slpr_mwhile slpr_cases slpr_mtime
          evalStmt (pr_ss <> ks)
        _ -> do
          addl_env <- evalDeclLHS rhs_lvl mempty rhs_v lhs
          sco' <- sco_update addl_env
          locAtf (srcloc_after_semi lab a sp) $ locSco sco' $ evalStmt ks
  (cont@(JSContinue a _ sp) : cont_ks) ->
    evalStmt $ assign : cont : cont_ks
    where
      assign = JSAssignStatement lhs op rhs sp
      lhs = JSArrayLiteral a [] a
      op = JSAssign a
      rhs = lhs
  --- FIXME We could desugar all these to certain while patterns
  (s@(JSDoWhile a _ _ _ _ _ _) : _) -> illegal a s "do while"
  (s@(JSFor a _ _ _ _ _ _ _ _) : _) -> illegal a s "for"
  (s@(JSForIn a _ _ _ _ _ _) : _) -> illegal a s "for in"
  (s@(JSForVar a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for var"
  (s@(JSForVarIn a _ _ _ _ _ _ _) : _) -> illegal a s "for var in"
  (s@(JSForLet a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for let"
  (s@(JSForLetIn a _ _ _ _ _ _ _) : _) -> illegal a s "for let in"
  (s@(JSForLetOf a _ _ _ _ _ _ _) : _) -> illegal a s "for let of"
  (s@(JSForConst a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for const"
  (s@(JSForConstIn a _ _ _ _ _ _ _) : _) -> illegal a s "for const in"
  (s@(JSForConstOf a _ _ _ _ _ _ _) : _) -> illegal a s "for const of"
  (s@(JSForOf a _ _ _ _ _ _) : _) -> illegal a s "for of"
  (s@(JSForVarOf a _ _ _ _ _ _ _) : _) -> illegal a s "for var of"
  (s@(JSAsyncFunction a _ _ _ _ _ _ _) : _) -> illegal a s "async function"
  ((JSFunction a name lp jsformals rp body sp) : ks) ->
    locAtf (srcloc_jsa "fun" a) $ do
      f <- case name of
        JSIdentNone -> expect_ (Err_TopFun_NoName)
        JSIdentName _ x -> return x
      let lhs = JSIdentifier a f
      let rhs = JSFunctionExpression a JSIdentNone lp jsformals rp body
      let decl = JSVarInitExpression lhs (JSVarInit a rhs)
      let decls = JSLOne decl
      let ss' = (JSConstant a decls sp) : ks
      evalStmt ss'
  (s@(JSGenerator a _ _ _ _ _ _ _) : _) -> illegal a s "generator"
  ((JSIf a la ce ra ts) : ks) -> do
    evalStmt $ (JSIfElse a la ce ra ts ea fs) : ks
    where
      ea = ra
      fs = (JSEmptyStatement ea)
  ((JSIfElse a _ ce ta ts fa fs) : ks) -> locAtf (srcloc_jsa "if" a) $ do
    t_at' <- withAt $ srcloc_jsa "if > true" ta
    f_at' <- withAt $ srcloc_jsa "if > false" fa
    (clvl, cv) <- evalExpr ce
    let ks_ne = dropEmptyJSStmts ks
    sco <- e_sco <$> ask
    let sco' =
          case ks_ne of
            [] -> sco
            _ -> sco {sco_must_ret = RS_MayBeEmpty}
    locSco sco' $
      case cv of
        SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
          SLRes tlifts st_t (SLStmtRes _ trets) <-
            captureRes $ locAt t_at' $ evalStmt [ts]
          SLRes flifts st_f (SLStmtRes _ frets) <-
            captureRes $ locAt f_at' $ evalStmt [fs]
          let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
          at <- withAt id
          saveLift $ DLS_If at (DLA_Var cond_dv) sa tlifts flifts
          let levelHelp = SLStmtRes sco . map (\(r_at, rmi, (r_lvl, r_v), _) -> (r_at, rmi, (clvl <> r_lvl, r_v), True))
          ir <- locSt st_t $ combineStmtRes clvl (levelHelp trets) st_f (levelHelp frets)
          setSt st_t
          mergeSt st_f
          retSeqn ir ks_ne
        _ -> do
          let (n_at', ns) = case cv of
                SLV_Bool _ False -> (f_at', fs)
                _ -> (t_at', ts)
          nr <- locAt n_at' $ evalStmt [ns]
          retSeqn nr ks_ne
  (s@(JSLabelled _ a _) : _) ->
    --- FIXME We could allow labels on whiles and have a mapping in
    --- sco_while_vars from a while label to the set of variables
    --- that should be modified, plus a field in sco for the default
    --- (i.e. closest label)
    illegal a s "labelled"
  ((JSEmptyStatement a) : ks) ->
    locAtf (srcloc_jsa "empty" a) $ evalStmt ks
  ((JSExpressionStatement e sp) : ks) -> do
    sev <- snd <$> evalExpr e
    locAtf (srcloc_after_semi "expr stmt" JSNoAnnot sp) $
      evalStmtTrampoline sp ks sev
  ((JSAssignStatement lhs op rhs _asp) : ks) ->
    case (op, ks) of
      ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) -> do
        let lab = "continue"
        ensure_mode SLM_ConsensusStep lab
        rhs_sv <- locAtf (srcloc_jsa lab var_a) $ evalExpr rhs
        sco <- e_sco <$> ask
        locAtf (srcloc_jsa lab cont_a) $ do
          whilem <-
            case sco_while_vars sco of
              Nothing -> expect_ $ Err_Eval_ContinueNotInWhile
              Just x -> return $ x
          doWhileLikeContinueEval lhs whilem rhs_sv
        -- NOTE We could/should look at sco_must_ret and see if it is
        -- RS_MayBeEmpty which means that the outside scope has an empty
        -- tail?
        expect_empty_tail lab cont_a cont_sp cont_ks
        return $ SLStmtRes sco []
      (jsop, stmts) ->
        locAtf (srcloc_jsa "assign" JSNoAnnot) $
          expect_ $ Err_Block_Assign jsop stmts
  ((JSMethodCall e a args ra sp) : ks) -> evalStmt ss'
    where
      ss' = (JSExpressionStatement e' sp) : ks
      e' = (JSCallExpression e a args ra)
  ((JSReturn a me sp) : ks) -> do
    let lab = "return"
    locAtf (srcloc_jsa lab a) $ do
      at' <- withAt id
      sco <- e_sco <$> ask
      sev <- case me of
        Nothing -> return $ public $ SLV_Null at' "empty return"
        Just e -> evalExpr e
      ret <- case sco_ret sco of
        Just x ->
          case sco_must_ret sco of
            RS_CannotReturn -> expect_ $ Err_CannotReturn
            _ -> return $ x
        Nothing -> expect_ $ Err_Eval_NoReturn
      evi <- ctxt_alloc
      saveLift $ DLS_Return at' ret (Left evi)
      expect_empty_tail lab a sp ks
      return $ SLStmtRes sco [(at', Just evi, sev, False)]
  (JSSwitch a _ de _ _ cases _ sp : ks) -> do
    locAtf (srcloc_jsa "switch" a) $ do
      at' <- withAt id
      let de_v = jse_expect_id at' de
      (de_lvl, de_val) <- evalId "switch" de_v
      (de_ty, _) <- typeOf de_val
      varm <- case de_ty of
        T_Data m -> return $ m
        _ -> expect_t de_val $ Err_Switch_NotData
      let ks_ne = dropEmptyJSStmts ks
      sco <- e_sco <$> ask
      let sco' =
            case ks_ne of
              [] -> sco
              _ -> sco {sco_must_ret = RS_MayBeEmpty}
      let case_insert k v@(at1, _, _) m =
            case M.lookup k m of
              Nothing -> return $ M.insert k v m
              Just (at0, _, _) ->
                expect_ $ Err_Switch_DoubleCase at0 at1 (Just k)
      let case_minserts cs v m = M.unions $ m : map (flip M.singleton v) cs
      let add_case (seenDefault, casem0) = \case
            JSCase ca ve _ body ->
              (,) seenDefault <$> case_insert vn (at_c, True, body) casem0
              where
                at_c = srcloc_jsa "case" ca at'
                vn = jse_expect_id at_c ve
            JSDefault ca _ body ->
              case seenDefault of
                Just at_c' ->
                  expect_ $ Err_Switch_DoubleCase at_c at_c' Nothing
                Nothing -> return $ ((Just at_c), case_minserts (M.keys varm) (at_c, False, body) casem0)
              where
                at_c = srcloc_jsa "case" ca at'
      (_, casesm) <- foldlM add_case (Nothing, mempty) cases
      let all_cases = M.keysSet varm
      let given_cases = M.keysSet casesm
      let missing_cases = all_cases S.\\ given_cases
      unless (S.null missing_cases) $ do
        expect_ $ Err_Switch_MissingCases $ S.toList missing_cases
      let extra_cases = given_cases S.\\ all_cases
      unless (S.null extra_cases) $ do
        expect_ $ Err_Switch_ExtraCases $ S.toList extra_cases
      let select at_c body mvv = locAt at_c $ do
            let addl_env = case mvv of
                  Just vv -> M.singleton de_v (sls_sss at_c (de_lvl, vv))
                  Nothing -> mempty
            sco'' <- locSco sco' $ sco_update_ AllowShadowing addl_env
            locSco sco'' $ evalStmt body
      let select_one vn (at_c, shouldBind, body) = do
            (mdv', mvv) <-
              case shouldBind of
                True -> do
                  let vt = varm M.! vn
                  case vt of
                    T_Null ->
                      return (Nothing, Just $ SLV_Null at_c "case")
                    _ -> do
                      dv' <- ctxt_mkvar $ DLVar at_c ("switch " <> vn) vt
                      return (Just dv', Just $ SLV_DLVar dv')
                False ->
                  return (Nothing, Nothing)
            return $ (mdv', at_c, select at_c body mvv)
      let select_all sv = do
            dv <- case sv of
              SLV_DLVar dv -> return dv
              _ -> impossible "select_all: not dlvar or interact field"
            let casemm = M.mapWithKey select_one casesm
            let cmb :: (Maybe SLState, StmtAnnot, Maybe SLStmtRets, M.Map SLVar (Maybe DLVar, DLStmts)) -> (SLVar, App (Maybe DLVar, SrcLoc, App SLStmtRes)) -> App (Maybe SLState, StmtAnnot, Maybe SLStmtRets, M.Map SLVar (Maybe DLVar, DLStmts))
                cmb (mst', sa', mrets', casemm') (vn, casem) = do
                  (mdv', at_c, casem') <- casem
                  locAt at_c $ do
                    SLRes case_lifts case_st (SLStmtRes _ case_rets) <-
                      captureRes casem'
                    let case_rets' = map (\(ra, rb, rc, _) -> (ra, rb, rc, True)) case_rets
                    let sa'' = sa' <> mkAnnot case_lifts
                    (mst'', rets'') <-
                      case (mst', mrets') of
                        (Nothing, Nothing) ->
                          return $ (Just case_st, case_rets')
                        (Just st', Just rets') -> do
                          st'' <- stMerge st' case_st
                          rets'' <- locSt st'' $ combineStmtRets de_lvl rets' case_st case_rets'
                          return $ (Just st'', rets'')
                        _ -> impossible $ "switch"
                    let casemm'' = M.insert vn (mdv', case_lifts) casemm'
                    return $ (mst'', sa'', Just rets'', casemm'')
            (mst', sa', mrets', casemm') <-
              foldM cmb (Nothing, mempty, Nothing, mempty) $ M.toList casemm
            let rets' = maybe mempty id mrets'
            maybe (return ()) setSt mst'
            saveLift =<< withAt (\at -> DLS_Switch at dv sa' casemm')
            return $ SLStmtRes sco rets'
      fr <-
        case de_val of
          SLV_Data _ _ vn vv -> do
            let (at_c, shouldBind, body) = (casesm M.! vn)
            let mvv = if shouldBind then Just vv else Nothing
            select at_c body mvv
          SLV_DLVar {} -> select_all de_val
          _ -> impossible "switch mvar"
      locAtf (srcloc_after_semi "switch" a sp) $ retSeqn fr ks_ne
  (s@(JSThrow a _ _) : _) -> illegal a s "throw"
  (s@(JSTry a _ _ _) : _) -> illegal a s "try"
  ((JSVariable var_a while_decls _vsp) : var_ks) -> do
    locAtf (srcloc_jsa "var" var_a) $
      case var_ks of
        ( (JSMethodCall (JSIdentifier inv_a "invariant") _ (JSLOne invariant_e) _ _isp)
            : (JSWhile while_a cond_a while_cond _ while_body)
            : ks
          ) -> locAtf (srcloc_jsa "while" while_a) $ do
            ensure_mode SLM_ConsensusStep "while"
            (while_lhs, while_rhs) <- destructDecls while_decls
            (init_vars, init_dl, sco_env') <- doWhileLikeInitEval while_lhs while_rhs
            inv_b <-
              locAtf (srcloc_jsa "invariant" inv_a) $
                locSco sco_env' $ evalPureExprToBlock invariant_e ST_Bool
            cond_b <-
              locAtf (srcloc_jsa "cond" cond_a) $
                locSco sco_env' $ evalPureExprToBlock while_cond ST_Bool
            let while_sco =
                  sco_env'
                    { sco_while_vars = Just init_vars
                    , sco_must_ret = RS_NeedExplicit
                    }
            (body_lifts, (SLStmtRes _ body_rets)) <-
              unchangedSt $
                locSco while_sco $
                  captureLifts $ evalStmt [while_body]
            saveLift
              =<< withAt
                (\at ->
                   DLS_While at init_dl inv_b cond_b body_lifts)
            SLStmtRes k_sco' k_rets <- locSco sco_env' $ evalStmt ks
            let rets' = body_rets <> k_rets
            return $ SLStmtRes k_sco' rets'
        _ -> expect_ $ Err_Block_Variable
  ((JSWhile a _ _ _ _) : _) ->
    locAtf (srcloc_jsa "while" a) $ expect_ $ Err_Block_While
  (s@(JSWith a _ _ _ _ _) : _) -> illegal a s "with"
  where
    illegal a s lab =
      locAtf (srcloc_jsa lab a) $ expect_ $ Err_Block_IllegalJS s

retSeqn :: SLStmtRes -> [JSStatement] -> App SLStmtRes
retSeqn sr ks = do
  case dropEmptyJSStmts ks of
    [] -> return $ sr
    ks' -> do
      sco <- e_sco <$> ask
      let SLStmtRes _ rets0 = sr
      let sco' =
            case rets0 of
              [] -> sco
              (_ : _) -> sco {sco_must_ret = RS_ImplicitNull}
      SLStmtRes sco1 rets1 <- locSco sco' $ evalStmt ks'
      return $ SLStmtRes sco1 (rets0 <> rets1)

combineStmtRes :: SecurityLevel -> SLStmtRes -> SLState -> SLStmtRes -> App SLStmtRes
combineStmtRes lvl (SLStmtRes _ lrets) rst (SLStmtRes sco rrets) =
  SLStmtRes sco <$> combineStmtRets lvl lrets rst rrets

combineStmtRets :: SecurityLevel -> SLStmtRets -> SLState -> SLStmtRets -> App SLStmtRets
combineStmtRets lvl lrets rst rrets = do
  at <- withAt id
  lst <- readSt id
  let mnull lab st =
        case st_live st of
          True -> [(at, Nothing, (lvl, SLV_Null at $ "empty " <> lab), False)]
          False -> []
  case (lrets, rrets) of
    ([], []) -> return []
    ([], _) -> return $ mnull "left" lst <> rrets
    (_, []) -> return $ lrets <> mnull "right" rst
    (_, _) -> return $ lrets <> rrets

expect_empty_tail :: String -> JSAnnot -> JSSemi -> [JSStatement] -> App ()
expect_empty_tail lab a sp = \case
  [] -> return ()
  ks -> locAtf (srcloc_after_semi lab a sp) $ expect_ $ Err_TailNotEmpty ks
