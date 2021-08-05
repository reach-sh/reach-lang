{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Eval.Core where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.Either
import Data.Foldable
import Data.Functor ((<&>))
import Data.IORef
import Data.List (elemIndex, groupBy, intercalate, transpose, unzip5, (\\), intersperse)
import Data.List.Extra (mconcatMap, splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (fromList, mapWithIndex)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
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
import Reach.Texty (pretty)
import Reach.Util
import Reach.Warning
import Safe (atMay)
import Text.ParserCombinators.Parsec.Number (numberValue)
import Text.RE.TDFA (RE, compileRegex, matched, (?=~))

--- New Types

type App = ReaderT Env IO

data ExnEnv = ExnEnv
  { e_exn_in_throw :: Bool
  , e_exn_ty :: Maybe DLType
  , e_exn_st :: Maybe SLState
  , e_exn_mode :: SLMode
  }

data AppEnv = AppEnv
  { ae_ios :: M.Map SLPart SLSSVal
  , ae_dlo :: DLOpts
  , ae_classes :: S.Set SLPart
  }

data AppRes = AppRes
  { ar_pie :: M.Map SLPart InteractEnv
  , ar_views :: DLViews
  , ar_ctimem :: Maybe (DLVar, DLVar)
  }

data AppInitSt
  = AIS_Init
      { aisi_env :: IORef AppEnv
      , aisi_res :: IORef AppRes
      }
  | AIS_Deployed
      {aisd_env :: AppEnv}

data Env = Env
  { e_id :: Counter
  , e_who :: Maybe SLPart
  , e_at :: SrcLoc
  , e_sco :: SLScope
  , e_st :: IORef SLState
  , e_lifts :: IORef DLStmts
  , e_stack :: [SLCtxtFrame]
  , e_depth :: Int
  , e_mape :: MapEnv
  , e_unused_variables :: IORef (S.Set (SrcLoc, SLVar))
  , e_while_invariant :: Bool
  , e_exn :: IORef ExnEnv
  , e_appr :: Either DLOpts (IORef AppInitSt)
  , e_droppedAsserts :: Counter
  }

instance Semigroup a => Semigroup (App a) where
  x <> y = (<>) <$> x <*> y

instance Monoid a => Monoid (App a) where
  mempty = return mempty

-- XXX add something for SecurityLevel

e_app :: App (Maybe AppInitSt)
e_app =
  (e_appr <$> ask) >>= \case
    Left _ -> return $ Nothing
    Right r -> Just <$> (liftIO $ readIORef r)

aisd_ :: App (Maybe AppEnv)
aisd_ =
  e_app >>= \case
    Just (AIS_Deployed d) -> return $ Just d
    _ -> return $ Nothing

aisd :: HasCallStack => App AppEnv
aisd = fromMaybe (impossible $ "aisd") <$> aisd_

aisiDo :: (a -> App b) -> (AppInitSt -> a) -> App b
aisiDo g f =
  e_app >>= \case
    Just i@(AIS_Init {}) -> g $ f i
    _ -> impossible $ "aisiDo"

aisiGet :: (AppInitSt -> IORef a) -> App a
aisiGet = aisiDo (liftIO . readIORef)

aisiPut :: (AppInitSt -> IORef a) -> (a -> a) -> App ()
aisiPut f g = aisiDo (liftIO . flip modifyIORef g) f

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

locMap :: App a -> App a
locMap m = do
  MapEnv {..} <- asks e_mape :: App MapEnv
  m' <- liftIO $ dupeIORef me_ms
  local (\e -> e {e_mape = MapEnv me_id m'}) m

locWho :: SLPart -> App a -> App a
locWho w = local (\e -> e {e_who = Just w})

locWhileInvariant :: App a -> App a
locWhileInvariant = local (\e -> e {e_while_invariant = True})

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

locUseStrict :: Bool -> App a -> App a
locUseStrict v m = do
  old_sco <- asks e_sco
  local (\e -> e {e_sco = old_sco {sco_use_strict = v}}) m

saveLifts :: DLStmts -> App ()
saveLifts ss = do
  Env {..} <- ask
  liftIO $ modifyIORef e_lifts $ (<> ss)

saveLift :: DLSStmt -> App ()
saveLift = saveLifts . return

readDlo :: (DLOpts -> b) -> App b
readDlo f =
  (e_appr <$> ask) >>= \case
    Left d -> return $ f d
    Right r ->
      (liftIO $ readIORef r) >>= \case
        AIS_Init er _ -> f . ae_dlo <$> (liftIO $ readIORef er)
        AIS_Deployed d -> return $ f (ae_dlo d)

whenVerifyArithmetic :: App () -> App ()
whenVerifyArithmetic m = do
  verifyOn <- readDlo dlo_verifyArithmetic
  when verifyOn $ m

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

type SLStmtRets = [(SrcLoc, Maybe DLType, SLSVal, Bool)]

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
  , sco_use_strict :: Bool
  }

data EnvInsertMode
  = AllowShadowing
  | DisallowShadowing
  | AllowShadowingSome (S.Set SLVar)
  | AllowShadowingRace (S.Set SLPart) (S.Set SLVar)

data SLRes a = SLRes DLStmts SLState a

type SLLibs = (M.Map ReachSource SLEnv)

type SLMod = (ReachSource, [JSModuleItem])

defaultApp :: SLSSVal
defaultApp =
  SLSSVal srcloc_builtin Public $
    SLV_Prim $
      SLPrim_App_Delay
        srcloc_builtin
        (JSStatementBlock JSNoAnnot [] JSNoAnnot JSSemiAuto)
        (mempty, False)

app_default_opts :: Counter -> [T.Text] -> DLOpts
app_default_opts idxr cns =
  DLOpts
    { dlo_deployMode = DM_constructor
    , dlo_verifyArithmetic = False
    , dlo_verifyPerConnector = False
    , dlo_connectors = cns
    , dlo_counter = idxr
    , dlo_bals = 1
    , dlo_droppedAsserts = 0
    }

app_options :: M.Map SLVar (DLOpts -> SLVal -> Either String DLOpts)
app_options =
  M.fromList
    [ ("deployMode", opt_deployMode)
    , ("verifyArithmetic", opt_bool (\opts b -> opts {dlo_verifyArithmetic = b}))
    , ("verifyPerConnector", opt_bool (\opts b -> opts {dlo_verifyPerConnector = b}))
    , ("connectors", opt_connectors)
    ]
  where
    opt_bool f opts v =
      case v of
        SLV_Bool _ b -> Right $ f opts b
        _ -> Left $ "expected boolean"
    opt_connectors opts v =
      case v of
        SLV_Tuple _ vs ->
          case traverse f vs of
            Left x -> Left x
            Right y -> Right $ opts {dlo_connectors = y}
          where
            f (SLV_Connector cn) = Right $ cn
            f _ = Left $ "expected connector"
        _ -> Left $ "expected tuple"
    opt_deployMode opts v =
      case v of
        SLV_Bytes _ "firstMsg" -> up DM_firstMsg
        SLV_Bytes _ "constructor" -> up DM_constructor
        SLV_Bytes _ bs -> Left $ bss <> " is not a deployMode" <> didYouMean bss ["firstMsg", "constructor"] 2
          where
            bss = bunpack bs
        _ -> Left $ "expected bytes"
      where
        up m = Right $ opts {dlo_deployMode = m}

--- Utilities
expect_ :: (HasCallStack, HasErrorCode e, Show e, ErrorMessageForJson e, ErrorSuggestions e) => e -> App a
expect_ e = do
  Env {..} <- ask
  expect_throw (Just e_stack) e_at e

mkValType :: SLVal -> App SLValTy
mkValType v = do
  r <- typeOfM v
  return (v, fmap fst r)

expect_t :: (HasCallStack, HasErrorCode e, Show e, ErrorMessageForJson e, ErrorSuggestions e) => SLVal -> (SLValTy -> e) -> App a
expect_t v f = (expect_ . f) =<< mkValType v

expect_ts :: (HasCallStack, HasErrorCode e, Show e, ErrorMessageForJson e, ErrorSuggestions e) => [SLVal] -> ([SLValTy] -> e) -> App a
expect_ts vs f = (expect_ . f) =<< mapM mkValType vs

zipEq :: (Show e, HasErrorCode e, ErrorMessageForJson e, ErrorSuggestions e) => (Int -> Int -> e) -> [a] -> [b] -> App [(a, b)]
zipEq ce x y =
  if lx == ly
    then return $ zip x y
    else expect_ $ ce lx ly
  where
    lx = length x
    ly = length y

ensure_public :: SLSVal -> App SLVal
ensure_public (lvl, v) = do
  ensure_level Public lvl
  return $ v

ensure_level :: SecurityLevel -> SecurityLevel -> App ()
ensure_level x y =
  case x == y of
    True -> return ()
    False -> expect_ $ Err_ExpectedLevel x

verifyName :: String -> String -> App ()
verifyName ty ns = do
  when (isSpecialBackendIdent ns) $
    expect_ $ Err_InvalidNameExport ty ns
  regex <- nameRegex
  unless (matched $ ns ?=~ regex) $
    expect_ $ Err_InvalidNameRegex ty ns

isSpecialBackendIdent :: [Char] -> Bool
isSpecialBackendIdent = flip elem ["getExports"]

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
      (SLSSVal _ _ ev) -> do
        case findStmtTrampoline ev of
          Just _ -> expect_t ev $ Err_IllegalEffPosition
          Nothing ->
            case k of
              "_" -> return env
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

-- Utilities to track variables.
-- We could track variable usage with `e_unused_vars :: [(SrcLoc, SLVar, IORef Bool)]`
-- and extend the env to hold `Map SLVar (SLSSVal, Maybe (IORef Bool))`.
-- Then, when looking up a variable, set the ref to `False`.
-- Any `e_unused_vars` with Bool `True` is unused.

useStrict :: App Bool
useStrict = asks (sco_use_strict . e_sco)

whenUsingStrict :: App () -> App ()
whenUsingStrict e = useStrict >>= flip when e

shouldNotTrackVariable :: (SrcLoc, SLVar) -> Bool
shouldNotTrackVariable (_, "main") = True
shouldNotTrackVariable (_, "_") = True
shouldNotTrackVariable _ = False

trackVariable :: (SrcLoc, SLVar) -> App ()
trackVariable el =
  whenUsingStrict $ do
    unused_vars <- asks e_unused_variables
    unless (shouldNotTrackVariable el) $
      liftIO $ modifyIORef unused_vars $ S.insert el

markVarUsed :: (SrcLoc, SLVar) -> App ()
markVarUsed v = do
  unused_vars <- asks e_unused_variables
  liftIO $ modifyIORef unused_vars $ S.filter (v /=)

-- | The "_" ident may never be looked up.
env_lookup :: LookupCtx -> SLVar -> SLEnv -> App SLSSVal
env_lookup _ "_" _ = expect_ $ Err_Eval_LookupUnderscore
env_lookup ctx x env =
  case M.lookup x env of
    Just sv -> do
      markVarUsed (sss_at sv, x)
      case sv of
        SLSSVal {sss_val = SLV_Deprecated d v} -> do
          liftIO $ emitWarning $ W_Deprecated d
          return $ sv {sss_val = v}
        v -> return $ v
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
    , ("assume", SLV_Prim $ SLPrim_claim $ CT_Assume False)
    , ("require", SLV_Prim $ SLPrim_claim CT_Require)
    , ("possible", SLV_Prim $ SLPrim_claim CT_Possible)
    , ("unknowable", SLV_Form $ SLForm_unknowable)
    , ("balance", SLV_Prim $ SLPrim_balance)
    , ("lastConsensusTime", SLV_Prim $ SLPrim_fluid_read_canWait FV_lastConsensusTime)
    , ("baseWaitTime", SLV_Prim $ SLPrim_fluid_read_canWait FV_baseWaitTime)
    , ("lastConsensusSecs", SLV_Prim $ SLPrim_fluid_read_canWait FV_lastConsensusSecs)
    , ("baseWaitSecs", SLV_Prim $ SLPrim_fluid_read_canWait FV_baseWaitSecs)
    , ("Digest", SLV_Type ST_Digest)
    , ("Null", SLV_Type ST_Null)
    , ("Bool", SLV_Type ST_Bool)
    , ("UInt", SLV_Type ST_UInt)
    , ("Bytes", SLV_Prim SLPrim_Bytes)
    , ("Address", SLV_Type ST_Address)
    , ("Token", SLV_Type ST_Token)
    , ("forall", SLV_Prim SLPrim_forall)
    , ("Data", SLV_Prim SLPrim_Data)
    , ("Array", SLV_Prim SLPrim_Array)
    , ("array", SLV_Prim SLPrim_array)
    , ("Tuple", SLV_Prim SLPrim_Tuple)
    , ("Object", SLV_Prim SLPrim_Object)
    , ("Struct", SLV_Prim SLPrim_Struct)
    , ("Foldable", SLV_Prim SLPrim_Foldable)
    , ("Fun", SLV_Prim SLPrim_Fun)
    , ("Refine", SLV_Prim SLPrim_Refine)
    , ("exit", SLV_Prim SLPrim_exit)
    , ("each", SLV_Form SLForm_each)
    , ("intEq", SLV_Prim $ SLPrim_op PEQ)
    , ("polyEq", SLV_Prim $ SLPrim_op PEQ)
    , ("digestEq", SLV_Prim $ SLPrim_op DIGEST_EQ)
    , ("addressEq", SLV_Prim $ SLPrim_op ADDRESS_EQ)
    , ("isType", SLV_Prim SLPrim_is_type)
    , ("typeEq", SLV_Prim SLPrim_type_eq)
    , ("typeOf", SLV_Prim SLPrim_typeOf)
    , ("is", SLV_Prim SLPrim_is)
    , ("wait", SLV_Form SLForm_wait)
    , ("race", SLV_Prim SLPrim_race)
    , ("fork", SLV_Form SLForm_fork)
    , ("parallelReduce", SLV_Form SLForm_parallel_reduce)
    , ("parallel_reduce", SLV_Deprecated (D_SnakeToCamelCase "parallel_reduce") $ SLV_Form SLForm_parallel_reduce)
    , ("Map", SLV_Prim SLPrim_Map)
    , ("Anybody", SLV_Anybody)
    , ("remote", SLV_Prim SLPrim_remote)
    , ("Participant", SLV_Prim SLPrim_Participant)
    , ("ParticipantClass", SLV_Prim SLPrim_ParticipantClass)
    , ("View", SLV_Prim SLPrim_View)
    , ("deploy", SLV_Prim SLPrim_deploy)
    , ("setOptions", SLV_Prim SLPrim_setOptions)
    , (".adaptReachAppTupleArgs", SLV_Prim SLPrim_adaptReachAppTupleArgs)
    , ( "Reach"
      , (SLV_Object srcloc_builtin (Just $ "Reach") $
           m_fromList_public_builtin
             [("App", SLV_Form SLForm_App)])
      )
    ]
      -- Add language keywords to env to prevent variables from using names.
      <> map (\t -> (show t, SLV_Kwd t)) allKeywords

jsClo :: HasCallStack => SrcLoc -> String -> String -> (M.Map SLVar SLVal) -> SLVal
jsClo at name js env_ = SLV_Clo at Nothing $ SLClo (Just name) args body cloenv
  where
    -- Since we're generating closure, don't fuss about unused vars
    cloenv = SLCloEnv mempty env False
    env = M.map (SLSSVal at Public) env_
    (args, body) =
      case readJsExpr js of
        JSArrowExpression aformals _ bodys -> (a_, b_)
          where
            b_ = jsArrowStmtToBlock bodys
            a_ = parseJSArrowFormals at aformals
        _ -> impossible "not arrow"

typeEqb :: DLType -> DLType -> Bool
typeEqb = (==)

typeEq :: DLType -> DLType -> App ()
typeEq x y = do
  unless (typeEqb x y) $
    expect_ $ Err_Type_Mismatch x y

typeEqs :: [DLType] -> App DLType
typeEqs = \case
  [] -> impossible $ "typeEqs none"
  [xt] -> return xt
  x : y : more -> typeEq x y >> typeEqs (y : more)

slToDL :: SLVal -> App (Maybe DLArgExpr)
slToDL v = slToDLV v <&> maybe Nothing dlvToDL

dlvToDL :: DLValue -> Maybe DLArgExpr
dlvToDL = \case
  DLV_Arg _ a -> return $ DLAE_Arg a
  DLV_Array _ dt mdvs -> DLAE_Array dt <$> all_just (map dlvToDL mdvs)
  DLV_Tuple _ mdvs -> DLAE_Tuple <$> all_just (map dlvToDL mdvs)
  DLV_Obj _ menv -> DLAE_Obj . M.fromList <$> all_just (map recAssoc $ M.toList menv)
  DLV_Struct _ menv -> DLAE_Struct <$> all_just (map recAssoc menv)
  DLV_Data _ t s mdv -> DLAE_Data t s <$> dlvToDL mdv
  _ -> Nothing
  where
    recAssoc (k, v) = (,) k <$> dlvToDL v

slToDLExportVal :: SLVal -> App (Maybe DLSExportBlock)
slToDLExportVal v = slToDLV v >>= maybe (return Nothing) (dlvToEV >=> (return . Just))

dlvToEV :: DLValue -> App DLSExportBlock
dlvToEV = \case
  DLV_Fun at vs b ->
    return $ DLinExportBlock at (Just vs) b
  ow ->
    case dlvToDL ow of
      Nothing -> impossible "dlvToEV"
      Just i -> do
        let at = srclocOf ow
        (stmts, da) <- captureLifts $ compileArgExpr i
        return $ DLinExportBlock at Nothing (DLSBlock at [] stmts da)

expectDLVar :: SLVal -> DLVar
expectDLVar = \case
  SLV_DLVar dv -> dv
  _ -> impossible "expectDLVar"

slToDLV :: SLVal -> App (Maybe DLValue)
slToDLV = \case
  SLV_Null at _ -> lit at DLL_Null
  SLV_Bool at b -> lit at $ DLL_Bool b
  SLV_Int at i -> lit at $ DLL_Int at i
  SLV_Bytes at bs -> lit at $ DLL_Bytes bs
  SLV_DLC c -> arg srcloc_builtin $ DLA_Constant c
  SLV_DLVar dv -> arg (srclocOf dv) $ DLA_Var dv
  SLV_Array at dt vs -> do
    fmap (DLV_Array at dt) . all_just <$> recs vs
  SLV_Tuple at vs ->
    fmap (DLV_Tuple at) . all_just <$> recs vs
  SLV_Object at _ fenv -> do
    env' <- mapM (slToDLV . sss_val) fenv
    let env'' = all_just $ map (\(k, v) -> (,) k <$> v) $ M.toList env'
    return $ DLV_Obj at . M.fromList <$> env''
  SLV_Struct at vs -> do
    mds <- mapM slToDLV $ M.fromList vs
    let mds' = all_just $ map (\(k, v) -> (,) k <$> v) $ M.toList mds
    return $ DLV_Struct at <$> mds'
  SLV_Data at dt vn sv -> do
    msv <- slToDLV sv
    return $ DLV_Data at dt vn <$> msv
  SLV_Participant at who _ mdv -> do
    pdvs <- readSt st_pdvs
    case M.lookup who pdvs <|> mdv of
      Just dv -> arg at $ DLA_Var dv
      Nothing -> no
  SLV_Clo at (Just tf) sc@(SLClo _ args _ _) -> do
    sargs <-
      zipEq (Err_Apply_ArgCount at) (stf_dom tf) args
        >>= mapM
          (\(ty, _) ->
             public . SLV_DLVar
               <$> (ctxt_mkvar . DLVar at Nothing =<< st2dte ty))
    block <- evalExportClosureToBlock sc sargs (Just tf) =<< st2dte (stf_rng tf)
    yes $ DLV_Fun at (map (expectDLVar . snd) sargs) block
  SLV_Clo {} -> no
  SLV_Type _ -> no
  SLV_Connector _ -> no
  SLV_RaceParticipant {} -> no
  SLV_Anybody -> no
  SLV_Prim {} -> no
  SLV_Form {} -> no
  SLV_Kwd {} -> no
  SLV_Map {} -> no
  SLV_Deprecated {} -> no
  where
    recs = mapM slToDLV
    lit at = arg at . DLA_Literal
    arg at = yes . DLV_Arg at
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

typeCheck_d :: DLType -> SLVal -> App DLArgExpr
typeCheck_d ty val = do
  (val_ty, res) <- typeOf val
  typeEq val_ty ty
  return res

applyRefinement :: ClaimType -> SLVal -> [SLVal] -> Maybe SLVal -> App ()
applyRefinement ct p args mmsg = do
  pres <- evalApplyVals' p $ map public args
  void $ evalPrim (SLPrim_claim ct) $ pres : catMaybes [public <$> mmsg]

applyType :: ClaimType -> SLVal -> SLType -> App ()
applyType ct v = \case
  ST_Refine _ p msg -> applyRefinement ct p [v] msg
  _ -> return ()

typeCheck_s :: SLType -> SLVal -> App DLArgExpr
typeCheck_s st val = do
  dt <- st2dte st
  res <- typeCheck_d dt val
  applyType CT_Assert val st
  return $ res

is_class :: SLPart -> App Bool
is_class who = S.member who <$> (ae_classes <$> aisd)

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
  saveLift $ DLS_Let at (DLV_Let DVC_Many dv) e
  return dv

ctxt_lift_eff :: DLExpr -> App ()
ctxt_lift_eff e = do
  at <- withAt id
  saveLift $ DLS_Let at DLV_Eff e

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
  DLAE_Struct kvs -> do
    let go (k, v) = (,) k <$> compileArgExpr v
    kvs' <- mapM go kvs
    mk $ DLLA_Struct kvs'
  where
    mk la = do
      at <- withAt id
      let t = largeArgTypeOf la
      let mkvar = DLVar at Nothing t
      DLA_Var <$> ctxt_lift_expr mkvar (DLE_LArg at la)

compileArgExprs :: [DLArgExpr] -> App [DLArg]
compileArgExprs = mapM compileArgExpr

compileArgExprMap :: M.Map a DLArgExpr -> App (M.Map a DLArg)
compileArgExprMap = mapM compileArgExpr

slvParticipant_part :: SLVal -> App SLPart
slvParticipant_part = \case
  SLV_Participant _ x _ _ -> return x
  x -> expect_t x $ Err_NotParticipant

compileTypeOf :: SLVal -> App (DLType, DLArg)
compileTypeOf v = do
  (t, dae) <- typeOf v
  da <- compileArgExpr dae
  return (t, da)

compileTypeOfs :: [SLVal] -> App ([DLType], [DLArg])
compileTypeOfs vs = unzip <$> mapM compileTypeOf vs

compileCheckType :: DLType -> SLVal -> App DLArg
compileCheckType et v = compileArgExpr =<< typeCheck_d et v

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

ensure_while_invariant :: String -> App ()
ensure_while_invariant msg =
  (e_while_invariant <$> ask) >>= \case
    True -> return ()
    False -> expect_ $ Err_Eval_MustBeInWhileInvariant msg

ensure_can_wait :: App ()
ensure_can_wait = do
  dm <- readDlo dlo_deployMode
  readSt st_after_ctor >>= \case
    True -> return ()
    False -> expect_ $ Err_Eval_IllegalWait dm

sco_to_cloenv :: SLScope -> App SLCloEnv
sco_to_cloenv SLScope {..} = do
  return $ SLCloEnv sco_penvs sco_cenv sco_use_strict

sco_lookup_penv :: SLPart -> App SLEnv
sco_lookup_penv who = do
  Env {..} <- ask
  return $
    case M.lookup who $ sco_penvs e_sco of
      Nothing -> sco_cenv e_sco
      Just x -> x

penvs_update :: (SLPart -> SLEnv -> App SLEnv) -> App SLPartEnvs
penvs_update f = do
  im <- fmap ae_ios <$> aisd_
  let ps = M.keys $ fromMaybe mempty im
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
      SLM_AppInit -> c_mod
      SLM_Step -> c_mod
      SLM_LocalStep -> return
      SLM_LocalPure -> return
      SLM_ConsensusStep -> c_mod
      SLM_ConsensusPure -> c_mod
    ps :: SLScope -> App SLScope
    ps = h $ \case
      SLM_Module -> return
      SLM_AppInit -> ps_all_mod
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
    fun a s ctxt = snd <$> (locAtf (srcloc_jsa "binop" a) $ evalId ctxt s)
    prim _a p = return $ SLV_Prim $ SLPrim_op p

unaryToPrim :: JSUnaryOp -> App SLVal
unaryToPrim = \case
  JSUnaryOpMinus a -> fun a "minus" "-"
  JSUnaryOpPlus a -> fun a "plus" "+"
  JSUnaryOpNot a -> fun a "not" "!"
  JSUnaryOpTypeof a -> fun a "typeOf" "typeOf"
  JSUnaryOpVoid a -> clo a "void" "(_) => null"
  j -> expect_ $ Err_Parse_IllegalUnaOp j
  where
    fun a s ctxt = snd <$> (locAtf (srcloc_jsa "unop" a) $ evalId ctxt s)
    clo a s js = do
      at <- withAt $ srcloc_jsa "unop" a
      return $ jsClo at s js mempty

infectWithId_clo :: SLVar -> SLClo -> SLClo
infectWithId_clo v (SLClo _ e b c) =
  SLClo (Just v) e b c

infectWithId_sv :: SrcLoc -> SLVar -> SLVal -> SLVal
infectWithId_sv at v = \case
  SLV_Participant a who _ mdv ->
    SLV_Participant a who (Just v) mdv
  SLV_Clo a mt c ->
    SLV_Clo a mt $ infectWithId_clo v c
  SLV_DLVar (DLVar a _ t i) ->
    SLV_DLVar $ DLVar a (Just (at, v)) t i
  x -> x

infectWithId_sss :: SLVar -> SLSSVal -> SLSSVal
infectWithId_sss v (SLSSVal at lvl sv) =
  SLSSVal at lvl $ infectWithId_sv at v sv

infectWithId_sls :: SrcLoc -> SLVar -> SLSVal -> SLSVal
infectWithId_sls at v (lvl, sv) = (lvl, infectWithId_sv at v sv)

evalObjEnv :: SLObjEnv -> App SLEnv
evalObjEnv = mapM go
  where
    go :: App SLSVal -> App SLSSVal
    go getv = do
      at <- withAt id
      sls_sss at <$> getv

evalAsEnv :: SLVal -> App SLObjEnv
evalAsEnv obj = case obj of
  SLV_Object _ _ env ->
    return $ M.map (retV . sss_sls) env
  SLV_DLVar obj_dv@(DLVar _ _ (T_Object tm) _) ->
    return $ retDLVar tm (DLA_Var obj_dv) Public
  SLV_Participant _ who vas _ -> do
    -- `<part>.interact.<field>(...)` is a shorthand for `<part>.only(() => interact.<field>(...))`
    -- Wrap each field of the participant's interface in a form, which will expand as described.
    let makeInteractField = do
          ios <- ae_ios <$> aisd
          case M.lookup who ios of
            Just sv@(SLSSVal _ _ (SLV_Object oa ol env)) -> do
              let object =
                    SLV_Object oa ol $
                      M.mapWithKey
                        (\k _ ->
                           sv {sss_val = SLV_Form $ SLForm_liftInteract who vas k})
                        env
              return (Secret, object)
            _ -> impossible "participant has no interact interface"
    return $
      M.fromList
        [ ("only", retV $ public $ SLV_Form (SLForm_Part_Only who vas))
        , ("publish", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos vas (Just TCM_Publish) Nothing Nothing Nothing Nothing))
        , ("pay", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos vas (Just TCM_Pay) Nothing Nothing Nothing Nothing))
        , ("set", delayCall SLPrim_part_set)
        , ("interact", makeInteractField)
        ]
    where
      whos = S.singleton who
  SLV_Anybody -> do
    whos <- S.fromList . M.keys <$> (ae_ios <$> aisd)
    evalAsEnv (SLV_RaceParticipant srcloc_builtin whos)
  SLV_RaceParticipant _ whos ->
    return $
      M.fromList
        [ ("publish", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos Nothing (Just TCM_Publish) Nothing Nothing Nothing Nothing))
        , ("pay", withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus at whos Nothing (Just TCM_Pay) Nothing Nothing Nothing Nothing))
        ]
  SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mwhen mtime) ->
    return $
      M.fromList $
        gom "publish" TCM_Publish mpub
          <> gom "pay" TCM_Pay mpay
          <> gom "when" TCM_When mwhen
          <> gom "timeout" TCM_Timeout mtime
          <> gom "throwTimeout" TCM_ThrowTimeout mtime
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just mode) mpub mpay mwhen mtime))]
  SLV_Form (SLForm_fork_partial fat Nothing cases mtime mpay) ->
    return $
      M.fromList $
        go "case" FM_Case
          <> gom "timeout" FM_Timeout mtime
          <> gom "throwTimeout" FM_ThrowTimeout mtime
          <> gom "paySpec" FM_PaySpec mpay
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_fork_partial fat (Just mode) cases mtime mpay))]
  SLV_Form (SLForm_parallel_reduce_partial pr_at Nothing pr_init pr_minv pr_mwhile pr_cases pr_mtime pr_mpay pr_mdef) ->
    return $
      M.fromList $
        gom "invariant" PRM_Invariant pr_minv
          <> gom "while" PRM_While pr_mwhile
          <> go "case" PRM_Case
          <> gom "timeout" PRM_Timeout pr_mtime
          <> gom "timeRemaining" PRM_TimeRemaining pr_mtime
          <> gom "throwTimeout" PRM_ThrowTimeout pr_mtime
          <> gom "paySpec" PRM_PaySpec pr_mpay
          <> gom "define" PRM_Def pr_mdef
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form (SLForm_parallel_reduce_partial pr_at (Just mode) pr_init pr_minv pr_mwhile pr_cases pr_mtime pr_mpay pr_mdef))]
  --- FIXME rewrite the rest to look at the type and go from there
  SLV_Tuple _ _ -> return tupleValueEnv
  SLV_DLVar (DLVar _ _ (T_Tuple _) _) -> return tupleValueEnv
  SLV_DLVar (DLVar _ _ T_Token _) ->
    return $
      M.fromList $
        [ ("burn", delayCall SLPrim_Token_burn)
        , ("destroy", delayCall SLPrim_Token_destroy)
        , ("destroyed", delayCall SLPrim_Token_destroyed)
        , ("supply", delayCall SLPrim_Token_supply) ]
  SLV_Type ST_Token ->
    return $
      M.fromList $
        [ ("new", retV $ public $ SLV_Prim $ SLPrim_Token_new)
        , ("burn", retV $ public $ SLV_Prim $ SLPrim_Token_burn)
        , ("destroy", retV $ public $ SLV_Prim $ SLPrim_Token_destroy)
        , ("destroyed", retV $ public $ SLV_Prim $ SLPrim_Token_destroyed)
        , ("supply", retV $ public $ SLV_Prim $ SLPrim_Token_supply) ]
  SLV_Prim SLPrim_Struct -> return structValueEnv
  SLV_Type (ST_Struct ts) ->
    return $
      structValueEnv
        <> M.fromList
          [ ("fromTuple", retV $ public $ SLV_Prim $ SLPrim_Struct_fromTuple ts)
          , ("fromObject", retV $ public $ SLV_Prim $ SLPrim_Struct_fromObject ts)
          ]
  SLV_Struct _ kvs ->
    return $ M.map (retV . public) $ M.fromList kvs
  SLV_DLVar obj_dv@(DLVar _ _ (T_Struct tml) _) ->
    return $ retDLVarl tml (DLA_Var obj_dv) Public
  SLV_Prim SLPrim_Tuple ->
    return $
      M.fromList
        [ ("set", retV $ public $ SLV_Prim $ SLPrim_tuple_set)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_tuple_length)
        ]
  SLV_Array {} -> return arrayValueEnv
  SLV_DLVar (DLVar _ _ (T_Array _ _) _) -> return arrayValueEnv
  SLV_Data {} ->
    return $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_DLVar (DLVar _ _ (T_Data _) _) ->
    return $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_Type (ST_Bytes len) -> do
    return $
      M.fromList
        [("pad", retV $ public $ SLV_Prim $ SLPrim_padTo len)]
  SLV_Prim SLPrim_Participant ->
    return $
      M.fromList
        [("set", retV $ public $ SLV_Prim SLPrim_part_set)]
  SLV_Prim SLPrim_Foldable ->
    return $
      M.fromList foldableValueEnv
  SLV_Prim SLPrim_Array ->
    return $
      M.fromList $
        [ ("empty", retStdLib "Array_empty")
        , ("findIndex", retStdLib "Array_findIndex")
        , ("find", retStdLib "Array_find")
        , ("withIndex", retStdLib "Array_withIndex")
        , ("mapWithIndex", retStdLib "Array_mapWithIndex")
        , ("reduceWithIndex", retStdLib "Array_reduceWithIndex")
        , ("indexOf", retStdLib "Array_indexOf")
        , ("replicate", retStdLib "Array_replicate")
        , ("slice", retStdLib "Array_slice")
        , ("elemType", retV $ public $ SLV_Prim $ SLPrim_array_elemType)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_array_length)
        , ("set", retV $ public $ SLV_Prim $ SLPrim_array_set)
        , ("iota", retV $ public $ SLV_Prim $ SLPrim_Array_iota)
        , ("concat", retV $ public $ SLV_Prim $ SLPrim_array_concat)
        , ("map", retV $ public $ SLV_Prim $ SLPrim_array_map)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_array_reduce)
        , ("zip", retV $ public $ SLV_Prim $ SLPrim_array_zip)
        ]
          <> foldableValueEnv
  SLV_Prim SLPrim_Object ->
    return $
      M.fromList
        [ ("set", retStdLib "Object_set")
        , ("setIfUnset", retStdLib "Object_setIfUnset")
        , ("has", retV $ public $ SLV_Prim $ SLPrim_Object_has)
        ]
  SLV_Type ST_UInt ->
    return $
      M.fromList
        [("max", retV $ public $ SLV_DLC DLC_UInt_max)]
  SLV_Type (ST_Data varm) ->
    return $
      flip M.mapWithKey varm $ \k t ->
        retV $ public $ SLV_Prim $ SLPrim_Data_variant varm k t
  SLV_Prim SLPrim_Map ->
    return $
      M.fromList $
        [("new", retV $ public $ SLV_Prim $ SLPrim_Map_new)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_Map_reduce)] <> foldableValueEnv
  SLV_Map _ ->
    return $
      M.fromList $
        [("reduce", delayCall SLPrim_Map_reduce)] <> foldableObjectEnv
  SLV_Prim (SLPrim_remotef rat aa m stf mpay mbill Nothing) ->
    return $
      M.fromList $
        gom "pay" RFM_Pay mpay
          <> gom "bill" RFM_Bill mbill
          <> gom "withBill" RFM_WithBill mbill
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Prim $ SLPrim_remotef rat aa m stf mpay mbill $ Just mode)]
  _ -> expect_t obj $ Err_Eval_NotObject
  where
    foldableMethods = ["forEach", "min", "max", "imin", "imax", "all", "any", "or", "and", "sum", "average", "product", "includes", "size", "count"]
    foldableObjectEnv :: [(SLVar, App SLSVal)]
    foldableObjectEnv = map (\m -> (m, delayStdlib $ "Foldable_" <> m <> "1")) foldableMethods
    foldableValueEnv :: [(SLVar, App SLSVal)]
    foldableValueEnv = map (\m -> (m, retStdLib $ "Foldable_" <> m)) foldableMethods
    tupleValueEnv :: SLObjEnv
    tupleValueEnv =
      M.fromList
        [ ("set", delayCall SLPrim_tuple_set)
        , ("length", doCall SLPrim_tuple_length)
        ]
    arrayValueEnv :: SLObjEnv
    arrayValueEnv =
      M.fromList $
        foldableObjectEnv
          <> [ ("elemType", doCall SLPrim_array_elemType)
             , ("set", delayCall SLPrim_array_set)
             , ("length", doCall SLPrim_array_length)
             , ("concat", delayCall SLPrim_array_concat)
             , ("indexOf", delayStdlib "Array_indexOf1")
             , ("findIndex", delayStdlib "Array_findIndex1")
             , ("find", delayStdlib "Array_find1")
             , ("withIndex", delayStdlib "Array_withIndex1")
             , ("mapWithIndex", delayStdlib "Array_mapWithIndex1")
             , ("reduceWithIndex", delayStdlib "Array_reduceWithIndex1")
             , ("slice", delayStdlib "Array_slice1")
             , ("map", delayCall SLPrim_array_map)
             , ("reduce", delayCall SLPrim_array_reduce)
             , ("zip", delayCall SLPrim_array_zip)
             ]
    structValueEnv :: SLObjEnv
    structValueEnv =
      M.fromList $
        [ ("toTuple", retV $ public $ SLV_Prim $ SLPrim_Struct_toTuple)
        , ("toObject", retV $ public $ SLV_Prim $ SLPrim_Struct_toObject)
        ]
    delayCall :: SLPrimitive -> App SLSVal
    delayCall p = do
      at <- withAt id
      retV $ public $ SLV_Prim $ SLPrim_PrimDelay at p [(public obj)] []
    delayStdlib :: SLVar -> App SLSVal
    delayStdlib = doApply <=< lookStdlib
    doCall :: SLPrimitive -> App SLSVal
    doCall p = doApply $ SLV_Prim p
    doApply :: SLVal -> App SLSVal
    doApply f = evalApplyVals' f [(public obj)]
    retDLVarl tml obj_dla slvl = do
      let retk (field, t) = (,) field $ do
            at <- withAt id
            let mkv = DLVar at Nothing t
            let e = DLE_ObjectRef at obj_dla field
            dv <- ctxt_lift_expr mkv e
            let ansv = SLV_DLVar dv
            return $ (slvl, ansv)
      M.fromList $ map retk tml
    retDLVar tm = retDLVarl $ M.toAscList tm
    retV = return
    retStdLib :: SLVar -> App SLSVal
    retStdLib n = (retV . public) =<< lookStdlib n

lookStdlib :: SLVar -> App SLVal
lookStdlib n = sss_val <$> ((env_lookup (LC_RefFrom "stdlib") n) =<< (sco_cenv . e_sco) <$> ask)

evalDot_ :: SLVal -> SLObjEnv -> String -> App SLSVal
evalDot_ obj env field =
  case M.lookup field env of
    Just gv -> gv
    Nothing -> expect_t obj $ \o -> Err_Dot_InvalidField o (M.keys env) field

evalDot :: SLVal -> String -> App SLSVal
evalDot obj s = flip (evalDot_ obj) s =<< evalAsEnv obj

st2dte :: HasCallStack => SLType -> App DLType
st2dte t =
  case st2dt t of
    Just x -> return $ x
    Nothing -> expect_ $ Err_Type_NotDT t

compileInteractResult :: ClaimType -> String -> SLType -> (DLType -> DLExpr) -> App SLVal
compileInteractResult ct lab st de = do
  at <- withAt id
  dt <- st2dte st
  let de' = de dt
  isv <-
    case dt of
      T_Null -> do
        ctxt_lift_eff de'
        return $ SLV_Null at lab
      _ ->
        SLV_DLVar <$> ctxt_lift_expr (DLVar at Nothing dt) (de dt)
  applyType ct isv st
  return isv

makeInteract :: SLPart -> SLInterface -> App (SLSSVal, InteractEnv)
makeInteract who (SLInterface spec) = do
  at <- withAt id
  let lab = Just $ (bunpack who) <> "'s interaction interface"
  let wrap_ty :: SLVar -> SLType -> App (SLSSVal, IType)
      wrap_ty k = \case
        ST_Fun stf@(SLTypeFun {..}) -> do
          dom' <- mapM st2dte stf_dom
          rng' <- st2dte stf_rng
          return $
            ( (sls_sss at $ secret $ SLV_Prim $ SLPrim_localf at who k $ Left stf)
            , IT_Fun dom' rng'
            )
        ST_UDFun rng -> do
          rng' <- st2dte rng
          return $
            ( (sls_sss at $ secret $ SLV_Prim $ SLPrim_localf at who k $ Right rng)
            , IT_UDFun rng'
            )
        t -> do
          t' <- st2dte t
          isv <- secret <$> compileInteractResult (CT_Assume False) "interact" t (\dt -> DLE_Arg at $ DLA_Interact who k dt)
          return $ (sls_sss at isv, IT_Val t')
  (lifts, spec') <- captureLifts $ mapWithKeyM wrap_ty spec
  let io = SLSSVal at Secret $ SLV_Object at lab $ M.map fst spec'
  let ienv = InteractEnv $ M.map (\(_, y) -> y) spec'
  saveLift $ DLS_Only at who lifts
  return $ (io, ienv)

convertTernaryReachApp :: SrcLoc -> JSAnnot -> JSExpression -> JSArrowParameterList -> JSExpression -> JSStatement -> JSStatement
convertTernaryReachApp at a opte top_formals partse top_s = top_s'
  where
    pis = parseJSArrowFormals at top_formals
    sp = JSSemi a
    lhs = JSArrayLiteral a (map JSArrayElement pis) a
    rhs = jsCall a (JSIdentifier a ".adaptReachAppTupleArgs") [partse]
    top_ss =
      [ JSExpressionStatement (JSCallExpression (JSIdentifier a "setOptions") a (JSLOne opte) a) sp
      , JSConstant a (JSLOne $ JSVarInitExpression lhs $ JSVarInit a rhs) sp
      , JSExpressionStatement (JSCallExpression (JSIdentifier a "deploy") a JSLNil a) sp
      , top_s
      ]
    top_s' = JSStatementBlock a top_ss a sp

doClaim :: ClaimType -> DLArg -> Maybe B.ByteString -> App ()
doClaim ct ca mmsg =
  case ca of
    DLA_Literal (DLL_Bool True) -> do
      Env {..} <- ask
      void $ liftIO $ incCounter e_droppedAsserts
    _ -> do
      at <- withAt id
      fs <- e_stack <$> ask
      ctxt_lift_eff $ DLE_Claim at fs ct ca mmsg

compileTimeArg :: SLVal -> App DLTimeArg
compileTimeArg = \case
  SLV_Data _ dm "Left" v | correctData dm ->
    Left <$> compileCheckType T_UInt v
  SLV_Data _ dm "Right" v | correctData dm ->
    Right <$> compileCheckType T_UInt v
  SLV_DLVar (DLVar _ _ (T_Data dm) _) | correctData dm ->
    expect_ Err_TimeArg_NotStatic
  v -> do
    f <- lookStdlib "relativeTime"
    -- liftIO $ emitWarning $ W_Deprecated D_UntypedTimeArg
    compileTimeArg =<< ensure_public =<< evalApplyVals' f [public v]
  where
    correctData = (==) (dataTypeMap $ eitherT T_UInt T_UInt)

evalForm :: SLForm -> [JSExpression] -> App SLSVal
evalForm f args = do
  case f of
    SLForm_App -> do
      at <- withAt id
      SLScope {..} <- asks e_sco
      top_s <-
        case args of
          [JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ top_s] -> return $ top_s
          [opte, partse, JSArrowExpression top_formals a top_s] -> do
            -- liftIO $ emitWarning $ W_Deprecated D_ReachAppArgs
            let at' = srcloc_jsa "app arrow" a at
            return $ convertTernaryReachApp at' a opte top_formals partse top_s
          _ -> expect_ $ Err_App_InvalidArgs args
      retV $ public $ SLV_Prim $ SLPrim_App_Delay at top_s (sco_cenv, sco_use_strict)
    SLForm_Part_Only who mv -> do
      at <- withAt id
      x <- one_arg
      env <- ask >>= sco_to_cloenv . e_sco
      return $ public $ SLV_Form $ SLForm_EachAns [(who, mv)] at env x
    SLForm_liftInteract _ msv field -> do
      let who = fromMaybe (impossible "lifted interact: participant unbound") msv
      annot <- at2a <$> withAt id
      -- Enclose the generated `only` with a closure that binds all the arguments to the interact call,
      -- which allows `this` to point to the originator of the consensus transfer instead of `who`.
      let param_ids = toList $ mapWithIndex (\i _ -> JSIdentifier annot $ "liftedInteractArg" <> show i) $ fromList args
      let no_params = JSParenthesizedArrowParameterList annot JSLNil annot
      let dot = JSMemberDot (JSIdentifier annot "interact") annot (JSIdentifier annot field)
      let call = JSCallExpression dot annot (toJSCL param_ids) annot
      let stmt = JSExpressionStatement call JSSemiAuto
      let only_thunk = JSArrowExpression no_params annot stmt
      let only_mem = JSMemberDot (JSIdentifier annot who) annot (JSIdentifier annot "only")
      let only_stmt = JSExpressionStatement (JSCallExpression only_mem annot (toJSCL [only_thunk]) annot) JSSemiAuto
      let params = JSParenthesizedArrowParameterList annot (toJSCL param_ids) annot
      let fun = JSArrowExpression params annot only_stmt
      evalExpr $ JSCallExpression fun annot (toJSCL args) annot
    SLForm_fork -> do
      zero_args
      at <- withAt id
      retV $ public $ SLV_Form $ SLForm_fork_partial at Nothing [] Nothing Nothing
    SLForm_fork_partial fat mmode cases mtime mpay ->
      case mmode of
        Just FM_Case -> do
          a <- withAt srcloc2annot
          at <- withAt id
          let def_pay =
                case mpay of
                  Just (JSArrayLiteral aa ts ae) -> do
                    let tok_ids = map (jse_expect_id at) $ jsa_flatten ts
                    let tok_pays = JSDecimal a "0" : map (\ i ->
                          JSArrayLiteral aa [
                            JSArrayElement (JSDecimal aa "0"), JSArrayComma aa, JSArrayElement (JSIdentifier aa i)
                          ] ae) tok_ids
                    JSArrayLiteral aa (intersperse (JSArrayComma aa) $ map JSArrayElement tok_pays) ae
                  _ -> JSDecimal a "0"
          let default_pay = jsArrowExpr a [JSIdentifier a "_"] def_pay
          case_args <-
            case args of
              [w, x, y, z] -> return $ ForkCase at w x y z
              [w, x, z] -> return $ ForkCase at w x default_pay z
              _ -> illegal_args 4
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing (cases <> [case_args]) mtime mpay
        Just FM_Timeout -> do
          at <- withAt id
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing cases (Just (at, args)) mpay
        Just FM_ThrowTimeout -> do
          at <- withAt id
          (d, arg) <- case args of
            [d] -> return (d, JSLiteral JSNoAnnot "null")
            [d, x] -> return (d, x)
            _ -> illegal_args 2
          let ta = srcloc2annot at
          let throwS = jsArrow ta [] $ JSThrow ta arg (a2sp ta)
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing cases (Just (at, [d, throwS])) mpay
        Just FM_PaySpec -> do
          nnts <- case args of
            [toks] -> return toks
            _ -> illegal_args 1
          retV $ public $ SLV_Form $ SLForm_fork_partial fat Nothing cases mtime (Just nnts)
        Nothing -> expect_t rator $ Err_Eval_NotApplicable
    SLForm_parallel_reduce -> do
      at <- withAt id
      x <- one_arg
      retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial at Nothing x Nothing Nothing [] Nothing Nothing Nothing
    SLForm_parallel_reduce_partial {..} -> do
      aa <- withAt $ \at -> (at, args)
      case slpr_mode of
        Just PRM_Invariant -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_minv = Just x, .. }
        Just PRM_While -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_mwhile = Just x, .. }
        Just PRM_Case ->
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_cases = slpr_cases <> [aa], .. }
        Just PRM_PaySpec -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_mpay = Just x, .. }
        Just PRM_Def -> do
          x <- one_arg
          retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_mdef = Just x, .. }
        Just PRM_Timeout -> retTimeout PRM_Timeout aa
        Just PRM_TimeRemaining -> retTimeout PRM_TimeRemaining aa
        Just PRM_ThrowTimeout -> retTimeout PRM_ThrowTimeout aa
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
      where
        makeTimeoutArgs mode aa = Just (mode, fst aa, snd aa)
        retTimeout prm aa =
          retV $
            public $
              SLV_Form $
                SLForm_parallel_reduce_partial { slpr_mode = Nothing, slpr_mtime = makeTimeoutArgs prm aa, .. }
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
          at <- withAt id
          x <-
            case args of
              [de] -> return $ (at, de, Nothing)
              [de, JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s] -> return $ (at, de, Just (jsStmtToBlock dt_s))
              [de, JSExpressionParen _ (JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s) _] ->
                return $ (at, de, Just (jsStmtToBlock dt_s))
              _ -> expect_ $ Err_ToConsensus_TimeoutArgs args
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mwhen $ Just x
        Just TCM_ThrowTimeout -> do
          at <- withAt id
          let ta = srcloc2annot at
          (de, x) <-
            case args of
              [de] -> return (de, JSLiteral ta "null")
              [de, e] -> return (de, e)
              _ -> illegal_args 2
          let throwS = JSThrow ta x JSSemiAuto
          retV $
            public $
              SLV_Form $
                SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mwhen $
                  Just (at, de, Just (jsStmtToBlock throwS))
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
          ce <- ask >>= sco_to_cloenv . e_sco
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
      at <- withAt id
      let whats_v = map (jse_expect_id at) whats_e
      whats_sv <-
        locWho knower $
          locStMode SLM_LocalStep $
            mapM (evalId_ "unknown") whats_v
      whats_dae <- map snd <$> (mapM typeOf $ map sss_val whats_sv)
      let whats_das = argExprToArgs $ DLAE_Tuple whats_dae
      let whats_da = DLA_Literal $ DLL_Bool False
      let ct = CT_Unknowable notter whats_das
      doClaim ct whats_da mmsg
      return $ public $ SLV_Null at "unknowable"
    SLForm_wait -> do
      ensure_can_wait
      ensure_mode SLM_Step "wait"
      amt_e <- one_arg
      amt_sv <- locStMode SLM_ConsensusPure $ evalExpr amt_e
      amt_ta <- compileTimeArg =<< ensure_public amt_sv
      doBaseWaitUpdate amt_ta
      at <- withAt id
      ctxt_lift_eff $ DLE_Wait at amt_ta
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

evalPolyEq :: SecurityLevel -> SLVal -> SLVal -> App SLSVal
evalPolyEq lvl x y =
  case (x, y) of
    -- Both args static
    (SLV_Int _ l, SLV_Int _ r) -> retBool $ l == r
    (SLV_Bool _ l, SLV_Bool _ r) -> retBool $ l == r
    (SLV_Bytes _ l, SLV_Bytes _ r) -> retBool $ l == r
    (SLV_Type l, SLV_Type r) -> retBool $ l == r
    (SLV_Null {}, SLV_Null {}) -> retBool True
    (SLV_Array _ _ ls, SLV_Array _ _ rs) -> do
      at <- withAt id
      let lengthEquals = SLV_Bool at $ length ls == length rs
      elemEquals <- andMapEq ls rs
      andVal <- evalAnd lengthEquals elemEquals
      return (lvl, andVal)
    (SLV_Tuple _ ls, SLV_Tuple _ rs) -> do
      elemEquals <- andMapEq ls rs
      return $ (lvl, elemEquals)
    (SLV_Object _ _ lEnv, SLV_Object _ _ rEnv) -> do
      let elems = map sss_val . M.elems
      elemEquals <- andMapEq (elems lEnv) (elems rEnv)
      return $ (lvl, elemEquals)
    (SLV_Data _ lCons lCon lVal, SLV_Data _ rCons rCon rVal)
      | lCons == rCons && lCon == rCon -> do
        evalPolyEq lvl lVal rVal
      | otherwise -> retBool False
    -- If atleast one arg is dynamic
    (l, r) -> do
      at <- withAt id
      (lty, la) <- compileTypeOf l
      (rty, ra) <- compileTypeOf r
      let make_var pop = do
            dv <- ctxt_lift_expr (DLVar at Nothing T_Bool) (DLE_PrimOp at pop [la, ra])
            return $ (lvl, SLV_DLVar dv)
      case (lty, rty) of
        (T_Null, T_Null) -> retBool True
        (T_UInt, T_UInt) -> make_var PEQ
        (T_Digest, T_Digest) -> make_var DIGEST_EQ
        (T_Address, T_Address) -> make_var ADDRESS_EQ
        (T_Token, T_Token) -> make_var TOKEN_EQ
        (T_Bool, T_Bool) -> do
          notR <- evalPrimOp IF_THEN_ELSE [(lvl, r), public $ SLV_Bool srcloc_builtin False, public $ SLV_Bool srcloc_builtin True]
          evalPrimOp IF_THEN_ELSE [(lvl, l), (lvl, r), notR]
        _ ->
          case typeEqb lty rty of
            False -> retBool False
            True -> hashAndCmp (lvl, l) (lvl, r)
  where
    andMapEq ls rs = do
      xs <- zipWithM (\l r -> snd <$> evalPolyEq lvl l r) ls rs
      foldrM evalAnd (SLV_Bool srcloc_builtin True) xs
    -- Logical and for SL bool values
    hashAndCmp :: SLSVal -> SLSVal -> App SLSVal
    hashAndCmp l r = do
      l' <- getHash l
      r' <- getHash r
      digestEq2 l' r'
    digestEq2 l r = digestEq [l, r]
    lookupFn f = do
      env <- (sco_cenv . e_sco) <$> ask
      sss_val <$> env_lookup (LC_RefFrom "polyEq") f env
    digestEq as = lookupFn "digestEq" >>= flip evalApplyVals' as
    getHash :: SLSVal -> App SLSVal
    getHash l = lookupFn "digest" >>= flip evalApplyVals' [l]
    retBool v = do
      at <- withAt id
      return $ (lvl, SLV_Bool at v)

evalAnd :: SLVal -> SLVal -> App SLVal
evalAnd l r =
  case (l, r) of
    (SLV_Bool bAt lb, SLV_Bool _ rb) ->
      return $ SLV_Bool bAt $ lb && rb
    (SLV_DLVar {}, SLV_DLVar {}) ->
      snd <$> evalPrimOp IF_THEN_ELSE [(Public, l), (Public, r), public $ SLV_Bool srcloc_builtin False]
    (SLV_Bool bAt False, _) -> return $ SLV_Bool bAt False
    (SLV_Bool _ True, SLV_DLVar {}) -> return $ r
    (SLV_DLVar _, SLV_Bool {}) -> evalAnd r l
    _ -> impossible $ "evalAnd expecting SLV_Bool or SLV_DLVar"

evalAndMap :: (SLVal -> App SLVal) -> [SLVal] -> App SLVal
evalAndMap f = \case
  [] -> return $ SLV_Bool srcloc_builtin True
  h : t -> do
    h' <- f h
    t' <- evalAndMap f t
    evalAnd h' t'

evalAllDistinct :: [SLVal] -> App SLVal
evalAllDistinct = \case
  [] -> withAt $ flip SLV_Bool True
  (v : vs) -> evalAndMap (\v' -> snd <$> (evalNeg =<< (snd <$> evalPolyEq Public v v'))) vs

evalITE :: SecurityLevel -> SLVal -> SLVal -> SLVal -> App SLSVal
evalITE lvl c t f = do
  at <- withAt id
  ca <- compileCheckType T_Bool c
  (tt, ta) <- compileTypeOf t
  fa <- compileCheckType tt f
  dv <- ctxt_lift_expr (DLVar at Nothing tt) (DLE_PrimOp at IF_THEN_ELSE [ca, ta, fa])
  return $ (lvl, SLV_DLVar dv)

evalNeg :: SLVal -> App SLSVal
evalNeg v = evalITE Public v (b False) (b True)
  where
    b = SLV_Bool srcloc_builtin

evalPrimOp :: PrimOp -> [SLSVal] -> App SLSVal
evalPrimOp p sargs = do
  at <- withAt id
  let zero = SLV_Int at 0
  case p of
    BYTES_CONCAT ->
      case args of
        [SLV_Bytes _ "", rhs] -> static rhs
        [lhs, SLV_Bytes _ ""] -> static lhs
        [SLV_Bytes _ lhs, SLV_Bytes _ rhs] -> do
          static $ SLV_Bytes at $ lhs <> rhs
        [lhs, rhs] -> do
          (lhs_l, lhs_ae) <- typeOfBytes lhs
          (rhs_l, rhs_ae) <- typeOfBytes rhs
          let rng = T_Bytes $ lhs_l + rhs_l
          make_var_ rng [lhs_ae, rhs_ae]
        _ -> expect_ $ Err_Apply_ArgCount at 2 (length args)
    ADD ->
      case args of
        [SLV_Int _ 0, rhs] -> static rhs
        [lhs, SLV_Int _ 0] -> static lhs
        _ -> nn2n (+)
    SUB ->
      case args of
        [lhs, SLV_Int _ 0] -> static lhs
        _ -> nn2n (-)
    MUL ->
      case args of
        [SLV_Int _ 1, rhs] -> static rhs
        [lhs, SLV_Int _ 1] -> static lhs
        [SLV_Int _ 0, _] -> static zero
        [_, SLV_Int _ 0] -> static zero
        _ -> nn2n (*)
    DIV ->
      case args of
        [lhs, SLV_Int _ 1] -> static lhs
        _ -> nn2n (div)
    MOD -> nn2n (mod)
    PLT -> nn2b (<)
    PLE -> nn2b (<=)
    PEQ ->
      case args of
        [x, y] -> evalPolyEq lvl x y
        _ -> expect_ $ Err_Apply_ArgCount at 2 (length args)
    PGE -> nn2b (>=)
    PGT -> nn2b (>)
    IF_THEN_ELSE ->
      case args of
        [SLV_Bool _ b, t, f] ->
          static $ if b then t else f
        [b, t, f] -> evalITE lvl b t f
        _ -> expect_ $ Err_Apply_ArgCount at 3 (length args)
    DIGEST_EQ -> make_var args
    ADDRESS_EQ -> make_var args
    TOKEN_EQ -> make_var args
    -- FIXME fromIntegral may overflow the Int
    LSH -> nn2n (\a b -> shift a (fromIntegral b))
    RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    BAND -> nn2n (.&.)
    BIOR -> nn2n (.|.)
    BXOR -> nn2n (xor)
    SELF_ADDRESS -> impossible "self address"
  where
    args = map snd sargs
    lvl = mconcat $ map fst sargs
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
      let (dom, rng) = primOpType p
      args'e <-
        mapM (uncurry typeCheck_d)
          =<< zipEq (Err_Apply_ArgCount at) dom args'
      make_var_ rng args'e
    make_var_ rng args'e = do
      at <- withAt id
      dargs <- compileArgExprs args'e
      let dopClaim ca msg = doClaim CT_Assert ca $ Just msg
      let mkvar t = DLVar at Nothing t
      let doOp t cp cargs = DLA_Var <$> (ctxt_lift_expr (mkvar t) $ DLE_PrimOp at cp cargs)
      let doCmp = doOp T_Bool
      let lim_maxUInt_a = DLA_Constant DLC_UInt_max
      dv <- ctxt_lift_expr (DLVar at Nothing rng) (DLE_PrimOp at p dargs)
      let da = DLA_Var dv
      whenVerifyArithmetic $
        case p of
          ADD -> do
            let (a, b) = case dargs of
                  [a_, b_] -> (a_, b_)
                  _ -> impossible "add args"
            ra <- doOp T_UInt SUB [lim_maxUInt_a, b]
            ca <- doCmp PLE [a, ra]
            dopClaim ca "add overflow"
          SUB -> do
            ca <- doCmp PGE dargs
            dopClaim ca "sub wraparound"
          DIV -> do
            let denom = case dargs of
                  [_, b] -> b
                  _ -> impossible "div args"
            ca <- doCmp PGT [denom, DLA_Literal $ DLL_Int srcloc_builtin 0]
            dopClaim ca "div by zero"
          MUL -> do
            ca <- doCmp PLE [da, lim_maxUInt_a]
            dopClaim ca "mul overflow"
          _ -> return $ mempty
      return $ (lvl, SLV_DLVar dv)

explodeTupleLike :: String -> SLVal -> App [SLVal]
explodeTupleLike lab = \case
  SLV_Tuple _ vs -> return vs
  SLV_Array _ _ vs -> return vs
  SLV_Struct _ kvs -> return $ map snd kvs
  SLV_DLVar strdv@(DLVar _ _ (T_Struct kts) _) ->
    mconcatMap (uncurry (flip (mkdv strdv DLE_ObjectRef))) $ kts
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
      let mdv = DLVar at Nothing t
      dv <- ctxt_lift_expr mdv de
      return $ [SLV_DLVar dv]

doFluidRef_dv :: FluidVar -> App DLVar
doFluidRef_dv fv = do
  at <- withAt id
  ensure_modes (all_slm_modes \\ [SLM_Module]) "fluid ref"
  let fvt = fluidVarType fv
  dv <- ctxt_mkvar (DLVar at Nothing fvt)
  saveLift $ DLS_FluidRef at dv fv
  return dv

doFluidRef_da :: FluidVar -> App DLArg
doFluidRef_da fv = DLA_Var <$> doFluidRef_dv fv

doFluidRef :: FluidVar -> App SLSVal
doFluidRef fv = (public . SLV_DLVar) <$> doFluidRef_dv fv

doFluidSet_ :: FluidVar -> DLArg -> App ()
doFluidSet_ fv da = do
  at <- withAt id
  typeEq (fluidVarType fv) (argTypeOf da)
  saveLift $ DLS_FluidSet at fv da

doFluidSet :: FluidVar -> SLSVal -> App ()
doFluidSet fv ssv = do
  sv <- ensure_public ssv
  da <- compileCheckType (fluidVarType fv) sv
  doFluidSet_ fv da

doBaseWaitUpdate :: DLTimeArg -> App ()
doBaseWaitUpdate = \case
  Left x -> doFluidSet_ FV_baseWaitTime x
  Right x -> doFluidSet_ FV_baseWaitSecs x

lookupBalanceFV :: HasCallStack => (Int -> FluidVar) -> Maybe DLArg -> App FluidVar
lookupBalanceFV = lookupBalanceFV_

lookupBalanceFV_ :: HasCallStack => (Int -> FluidVar) -> Maybe DLArg -> App FluidVar
lookupBalanceFV_ fv mtok = do
  toks <- readSt st_toks
  let bad = expect_ $ Err_Token_DynamicRef
  i <-
    case mtok of
      Nothing -> return $ 0
      Just (DLA_Var v) ->
        case elemIndex v toks of
          Nothing -> bad
          Just x -> return $ 1 + x
      _ -> bad
  return $ fv i

ensureCreatedToken :: String -> DLArg -> App ()
ensureCreatedToken lab a = do
  toks_c <- readSt st_toks_c
  case a of
    DLA_Var v ->
      case S.member v toks_c of
        True -> return ()
        False -> expect_ $ Err_Token_NotCreated lab
    _ -> expect_ $ Err_Token_DynamicRef

doBalanceInit_ :: (Int -> FluidVar) -> Maybe DLArg -> DLArg -> App ()
doBalanceInit_ fv mtok amta = do
  b <- lookupBalanceFV_ fv mtok
  doFluidSet_ b amta

doBalanceInit :: Maybe DLArg -> App ()
doBalanceInit mtok = do
  at <- withAt id
  doBalanceInit_ FV_balance mtok (DLA_Literal $ DLL_Int at 0)

doBalanceAssert :: Maybe DLArg -> SLVal -> PrimOp -> B.ByteString -> App ()
doBalanceAssert = doBalanceAssert_ FV_balance

doBalanceAssert_ :: (Int -> FluidVar) -> Maybe DLArg -> SLVal -> PrimOp -> B.ByteString -> App ()
doBalanceAssert_ fv mtok lhs op msg = do
  at <- withAt id
  let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [(Public, lhs)] []
  b <- lookupBalanceFV_ fv mtok
  balance_v <- doFluidRef b
  cmp_v <- evalApplyVals' cmp_rator [balance_v]
  let ass_rator = SLV_Prim $ SLPrim_claim CT_Assert
  void $
    evalApplyVals' ass_rator $
      [cmp_v, public $ SLV_Bytes at msg]

doBalanceUpdate :: Maybe DLArg -> PrimOp -> SLVal -> App ()
doBalanceUpdate = doBalanceUpdate_ FV_balance

doBalanceUpdate_ :: (Int -> FluidVar) -> Maybe DLArg -> PrimOp -> SLVal -> App ()
doBalanceUpdate_ fv mtok op rhs = do
  at <- withAt id
  let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
  b <- lookupBalanceFV_ fv mtok
  balance_v <- doFluidRef b
  balance_v' <- evalApplyVals' up_rator [balance_v]
  doFluidSet b balance_v'

doArrayBoundsCheck :: Integer -> SLVal -> App ()
doArrayBoundsCheck sz idxv = do
  at <- withAt id
  cmp_v <- evalApplyVals' (SLV_Prim $ SLPrim_op PLT) [public idxv, public $ SLV_Int at sz]
  void $
    evalApplyVals' (SLV_Prim $ SLPrim_claim CT_Assert) $
      [cmp_v, public $ SLV_Bytes at "array bounds check"]

mustBeBytes :: SLVal -> App B.ByteString
mustBeBytes = \case
  SLV_Bytes _ x -> return $ x
  v -> expect_t v $ Err_Expected "bytes"

mustBeTuple :: SLVal -> App [SLVal]
mustBeTuple = \case
  SLV_Tuple _ vs -> return vs
  ow ->
    locAtf (flip srclocOf_ ow) $
      expect_t ow $ Err_Expected "tuple"

mustBeObject :: SLVal -> App SLEnv
mustBeObject = \case
  SLV_Object _ _ m -> return m
  ow ->
    locAtf (flip srclocOf_ ow) $
      expect_t ow $ Err_Expected "object"

mustBeDataTy :: (DLType -> EvalError) -> DLType -> App (M.Map SLVar DLType)
mustBeDataTy err = \case
  T_Data x -> return $ x
  t -> expect_ $ err t

mustBeObjectTy :: (DLType -> EvalError) -> DLType -> App (M.Map SLVar DLType)
mustBeObjectTy err = \case
  T_Object x -> return $ x
  t -> expect_ $ err t

structKeyRegex :: App RE
structKeyRegex = liftIO $ compileRegex "^([_a-zA-Z][_a-zA-Z0-9]*)$"

nameRegex :: App RE
nameRegex = liftIO $ compileRegex "^([a-zA-Z][_a-zA-Z0-9]*)$"

tokenPay :: Maybe DLArg -> DLArg -> B.ByteString -> ReaderT Env IO ()
tokenPay mtok_a amt_a msg = do
  amt_sv <- argToSV amt_a
  doBalanceAssert mtok_a amt_sv PLE msg
  doBalanceUpdate mtok_a SUB amt_sv

getBillTokens :: Maybe (Either SLVal SLVal) -> DLPayAmt -> App (SLType, [DLArg])
getBillTokens mbill billAmt = do
  case mbill of
    Just (Left arg) -> do
      (ty, ae) <- typeOf arg
      case (ty, ae) of
        (T_Tuple ts, DLAE_Tuple tas)
          | all (== T_Token) ts -> do
            tas' <- mapM compileArgExpr tas
            return (ST_Tuple $ map (const ST_UInt) ts, tas')
        _ -> expect_ $ Err_WithBill_Type ty
    Just (Right _) -> do
      let nnBilled = pa_ks billAmt
      return (ST_Tuple $ map (const ST_UInt) nnBilled, map snd nnBilled)
    _ -> return (ST_Null, [])

typeOfBytes :: SLVal -> App (Integer, DLArgExpr)
typeOfBytes v = do
  (t, ae) <- typeOf v
  case t of
    T_Bytes l -> return (l, ae)
    _ -> expect_t v $ Err_Expected "bytes"

evalPrim :: SLPrimitive -> [SLSVal] -> App SLSVal
evalPrim p sargs =
  case p of
    SLPrim_Token_burn -> do
      (tokv, mamtv) <-
        case args of
          [ x ] -> return (x, Nothing)
          [ x, y ] -> return (x, Just y)
          _ -> illegal_args
      toka <- compileCheckType T_Token tokv
      at <- withAt id
      let lab = "Token.burn"
      ensureCreatedToken lab toka
      amta <-
        case mamtv of
          Just v -> compileCheckType T_UInt v
          Nothing -> doFluidRef_da =<< lookupBalanceFV FV_balance (Just toka)
      ensure_mode SLM_ConsensusStep lab
      let mtok_a = Just toka
      amt_sv <- argToSV amta
      doBalanceAssert mtok_a amt_sv PLE (bpack lab)
      doBalanceUpdate mtok_a SUB amt_sv
      doBalanceUpdate_ FV_supply mtok_a SUB amt_sv
      ctxt_lift_eff $ DLE_TokenBurn at toka amta
      return $ public $ SLV_Null at lab
    SLPrim_Token_destroy -> do
      toka <- compileCheckType T_Token =<< one_arg
      at <- withAt id
      let lab = "Token.destroy"
      ensureCreatedToken lab toka
      ensure_mode SLM_ConsensusStep lab
      let mtoka = Just toka
      doBalanceAssert_ FV_destroyed mtoka (SLV_Bool at False) PEQ $ bpack $ "token not yet destroyed at " <> lab
      doBalanceAssert_ FV_supply mtoka (SLV_Int at 0) PEQ $ bpack $ "token supply zero at " <> lab
      ctxt_lift_eff $ DLE_TokenDestroy at toka
      doBalanceInit_ FV_destroyed mtoka $ DLA_Literal $ DLL_Bool True
      return $ public $ SLV_Null at lab
    SLPrim_Token_new -> do
      at <- withAt id
      metam <- mustBeObject =<< one_arg
      metam' <- mapM (ensure_public . sss_sls) metam
      let go tns (k, v) = case k of
            "name" -> bytes (\x -> tns { dtn_name = x}) tokenNameLen
            "symbol" -> bytes (\x -> tns { dtn_sym = x}) tokenSymLen
            "url" -> bytes (\x -> tns { dtn_url = x}) tokenURLLen
            "metadata" -> bytes (\x -> tns { dtn_metadata = x}) tokenMetadataLen
            "supply" -> do
              a <- compileCheckType T_UInt v
              return $ tns { dtn_supply = a }
            _ -> expect_ $ Err_TokenNew_InvalidKey k
            where
              bytes u len = u <$> compileCheckType (T_Bytes len) v
      tns <- foldM go defaultTokenNew (M.toAscList metam')
      let supplya = dtn_supply tns
      ensure_mode SLM_ConsensusStep "new Token"
      tokdv <- ctxt_lift_expr (DLVar at Nothing T_Token) $
        DLE_TokenNew at tns
      st <- readSt id
      setSt $ st
        { st_toks = st_toks st <> [ tokdv ]
        , st_toks_c = S.insert tokdv (st_toks_c st) }
      let mtok_a = Just $ DLA_Var tokdv
      doBalanceInit_ FV_balance mtok_a supplya
      doBalanceInit_ FV_supply mtok_a supplya
      doBalanceInit_ FV_destroyed mtok_a (DLA_Literal $ DLL_Bool False)
      return $ public $ SLV_DLVar tokdv
    SLPrim_padTo len -> do
      at <- withAt id
      v <- one_arg
      (vl, _) <- typeOfBytes v
      let xtra = fromIntegral $ len - vl
      let z = public $ SLV_Bytes at $ B.replicate xtra 0
      evalPrimOp BYTES_CONCAT [public v, z]
    SLPrim_race -> do
      at <- withAt id
      ps <- mapM slvParticipant_part $ map snd sargs
      retV $ (lvl, SLV_RaceParticipant at (S.fromList ps))
    SLPrim_balance -> do
      fv <- case args of
        [] -> lookupBalanceFV FV_balance Nothing
        [v] -> lookupBalanceFV FV_balance . Just =<< compileCheckType T_Token v
        _ -> illegal_args
      doFluidRef fv
    SLPrim_Token_supply -> do
      v <- one_arg
      da <- compileCheckType T_Token v
      ensureCreatedToken "Token.supply" da
      sr <- lookupBalanceFV FV_supply $ Just da
      doFluidRef sr
    SLPrim_Token_destroyed -> do
      v <- one_arg
      da <- compileCheckType T_Token v
      ensureCreatedToken "Token.destroyed" da
      dr <- lookupBalanceFV FV_destroyed $ Just da
      doFluidRef dr
    SLPrim_fluid_read fv -> doFluidRef fv
    SLPrim_fluid_read_canWait fv -> do
      ensure_can_wait
      zero_args
      evalPrim (SLPrim_fluid_read $ fv) []
    SLPrim_op op -> evalPrimOp op sargs
    SLPrim_Fun ->
      case map snd sargs of
        [(SLV_Bool _ True), (SLV_Type rng)] -> do
          retV $ (lvl, SLV_Type $ ST_UDFun rng)
        [(SLV_Tuple _ dom_arr), (SLV_Type rng)] -> do
          dom <- mapM (expect_ty "Fun domain") dom_arr
          retV $ (lvl, SLV_Type $ ST_Fun (SLTypeFun dom rng Nothing Nothing Nothing Nothing))
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
    SLPrim_Foldable -> expect_ Err_Prim_Foldable
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
    SLPrim_array_elemType ->
      one_arg >>= getType <&> public . SLV_Type . dt2st
      where
        getType = \case
          SLV_Array _ et _ -> return et
          SLV_DLVar (DLVar _ _ (T_Array et _) _) -> return et
          _ -> illegal_args
    SLPrim_Array_iota -> do
      at <- withAt id
      case map snd sargs of
        [SLV_Int _ sz] ->
          retV $ (lvl, SLV_Array at T_UInt $ map (SLV_Int at) [0 .. (sz -1)])
        [_] -> expect_ $ Err_Prim_InvalidArg_Dynamic p
        _ -> illegal_args
    SLPrim_array ->
      case map snd sargs of
        [(SLV_Type elem_sty), elems_v] ->
          case elems_v of
            SLV_Tuple _ elem_vs -> do
              at <- withAt id
              elem_ty <- st2dte elem_sty
              let check1 sv = typeCheck_d elem_ty sv >> return sv
              elem_vs_checked <- mapM check1 elem_vs
              retV $ (lvl, SLV_Array at elem_ty elem_vs_checked)
            --- FIXME we could support turning a DL Tuple into an array.
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_concat -> do
      at <- withAt id
      case map snd sargs of
        [SLV_Array _ x_ty x_vs, SLV_Array _ y_ty y_vs] -> do
          typeEq x_ty y_ty
          retV $ (lvl, SLV_Array at x_ty $ x_vs ++ y_vs)
        [x, y] -> do
          (xt, xa) <- compileTypeOf x
          (yt, ya) <- compileTypeOf y
          case (xt, yt) of
            (T_Array x_ty x_sz, T_Array y_ty y_sz) -> do
              typeEq x_ty y_ty
              let t = T_Array x_ty (x_sz + y_sz)
              let mkdv = DLVar at Nothing t
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
      unless (x_sz == y_sz) $ do
        expect_ $ Err_Zip_ArraysNotEqualLength x_sz y_sz
      let sz' = x_sz
      case isLiteralArray x && isLiteralArray y of
        True -> do
          x_vs <- explodeTupleLike "zip" x
          y_vs <- explodeTupleLike "zip" y
          let vs' = zipWith (\xe ye -> SLV_Tuple at [xe, ye]) x_vs y_vs
          return $ (lvl, SLV_Array at ty' vs')
        False -> do
          let t = T_Array ty' sz'
          let mkdv = (DLVar at Nothing t)
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
          (a_dv, a_dsv) <- make_dlvar at x_ty
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ (f_lvl, f_ty, f_da) <-
            captureRes $ do
              (f_lvl, f_v) <- f' a_dsv
              (f_ty, f_da) <- compileTypeOf f_v
              return (f_lvl, f_ty, f_da)
          let shouldUnroll = not (hasIsPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              x_vs <- explodeTupleLike "map" x
              let evalem xv = snd <$> f' xv
              vs' <- mapM evalem x_vs
              return $ (f_lvl, SLV_Array at f_ty vs')
            False -> do
              let t = T_Array f_ty x_sz
              (ans_dv, ans_dsv) <- make_dlvar at t
              let f_bl = DLSBlock at [] f_lifts f_da
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
          (b_dv, b_dsv) <- make_dlvar at z_ty
          (a_dv, a_dsv) <- make_dlvar at x_ty
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ f_da <-
            captureRes $ do
              (f_lvl, f_v) <- f' b_dsv a_dsv
              ensure_level lvl f_lvl
              (f_ty, f_da) <- compileTypeOf f_v
              typeEq z_ty f_ty
              return $ f_da
          let shouldUnroll = not (hasIsPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              -- liftIO $ putStrLn $ "unrolling at " <> show at <> ": " <> (show $ isLiteralArray x) <> " " <> show (hasIsPure f_lifts) <> " " <> show (pretty $ hasPurity f_lifts) <> " " <> show (isLocal f_lifts) <> ":\n" <> (show $ pretty f_lifts) <> "\n"
              x_vs <- explodeTupleLike "reduce" x
              let evalem :: SLSVal -> SLVal -> App SLSVal
                  evalem prev_z xv = do
                    xv_v' <- f' (snd prev_z) xv
                    --- Note: We are artificially restricting reduce
                    --- to be parameteric in the state. We also ensure
                    --- that they type is the same as the anonymous
                    --- version.
                    _ <- typeCheck_d z_ty (snd xv_v')
                    return $ xv_v'
              foldM evalem (lvl, z) x_vs
            False -> do
              (ans_dv, ans_dsv) <- make_dlvar at z_ty
              let f_bl = DLSBlock at [] f_lifts f_da
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
                      let valv_checked = typeCheck_d elem_ty valv `seq` valv
                      let arrvs' = (take idxi' arrvs) ++ [valv_checked] ++ (drop (idxi' + 1) arrvs)
                      let arrv' = SLV_Array at elem_ty arrvs'
                      retV $ (lvl, arrv')
                    False ->
                      expect_ $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                SLV_DLVar arrdv@(DLVar _ _ arr_ty@(T_Array elem_ty sz) _) ->
                  case idxi < sz of
                    True -> do
                      valda <- compileCheckType elem_ty valv
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
                  valda <- compileCheckType elem_ty valv
                  doArrayBoundsCheck sz idxv
                  at <- withAt id
                  retArrDV arr_ty $ DLE_ArraySet at arrda idxda valda
                _ -> illegal_args
            _ -> illegal_args
        _ -> illegal_args
      where
        retArrDV t de = do
          at <- withAt id
          dv <- ctxt_lift_expr (DLVar at Nothing t) de
          return $ (lvl, SLV_DLVar dv)
    SLPrim_Struct -> do
      as <-
        one_arg >>= \case
          SLV_Tuple _ x -> return x
          _ -> illegal_args
      regex <- structKeyRegex
      seenKeysRef <- liftIO $ newIORef S.empty
      let go = \case
            SLV_Tuple _ [kv, tv] -> do
              kbs <- mustBeBytes kv
              k <- verifyStructId regex $ bunpack kbs
              seenKeys <- liftIO $ readIORef seenKeysRef
              when (S.member k seenKeys) $
                expect_ $ Err_Struct_Key_Not_Unique (S.toList seenKeys) k
              liftIO $ writeIORef seenKeysRef $ S.insert k seenKeys
              t <- expect_ty ("value of " <> k) tv
              return (k, t)
            _ -> illegal_args
      kts <- mapM go as
      retV $ (lvl, SLV_Type $ ST_Struct kts)
      where
        verifyStructId r s = do
          at <- withAt id
          connectors <- readDlo dlo_connectors
          -- XXX extend `Connector` to convey this info we're checking
          when ("ETH" `elem` connectors && s `elem` map show solReservedNames) $
            expect_thrown at $ Err_Sol_Reserved s
          bool (expect_ $ Err_Struct_Key_Invalid s) (return s) $ matched $ s ?=~ r
    SLPrim_Struct_fromTuple ts -> do
      tv <- one_arg
      at <- withAt id
      let go (i, k) = (,) k <$> doArrRef_ tv (SLV_Int at i)
      kvs <- mapM go $ zip [0 ..] $ map fst ts
      return $ (lvl, SLV_Struct at kvs)
    SLPrim_Struct_fromObject ts -> do
      ov <- one_arg
      at <- withAt id
      let go k = (,) k <$> (snd <$> evalDot ov k)
      kvs <- mapM go $ map fst ts
      return $ (lvl, SLV_Struct at kvs)
    SLPrim_Struct_toTuple -> do
      at <- withAt id
      (,) lvl <$> (SLV_Tuple at <$> (explodeTupleLike "Struct.asTuple" =<< one_arg))
    SLPrim_Struct_toObject -> do
      sv <- one_arg
      at <- withAt id
      (,) lvl <$> (SLV_Object at Nothing <$> (evalObjEnv =<< evalAsEnv sv))
    SLPrim_Tuple -> do
      vs <- mapM (expect_ty "Tuple argument") $ map snd sargs
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
    SLPrim_Object -> do
      objm <- mustBeObject =<< one_arg
      vm <- mapWithKeyM (\ k v -> do
        locAt (sss_at v) $
          expect_ty ("value of " <> k) (sss_val v)) objm
      retV $ (lvl, SLV_Type $ ST_Object vm)
    SLPrim_Object_has -> do
      at <- withAt id
      (obj, bsv) <- two_args
      bs <- mustBeBytes bsv
      vm <- evalAsEnv obj
      retV $ (lvl, SLV_Bool at $ M.member (bunpack bs) vm)
    SLPrim_makeEnum -> do
      at' <- withAt $ srcloc_at "makeEnum" Nothing
      case map snd sargs of
        [iv@(SLV_Int _ i)] ->
          retV $ (lvl, SLV_Tuple at' (enum_pred : map (SLV_Int at') [0 .. (i -1)]))
          where
            enum_pred = jsClo at' "makeEnum" "(x) => ((0 <= x) && (x < M))" (M.fromList [("M", iv)])
        _ -> illegal_args
    SLPrim_App_Delay {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_localf iat who m stf ->
      secret <$> doInteractiveCall sargs iat stf SLM_LocalStep "interact" (CT_Assume False) (\at fs drng dargs -> DLE_Interact at fs who m drng dargs)
    SLPrim_declassify -> do
      val <- one_arg
      ensure_level Secret lvl
      retV $ public $ val
    SLPrim_commit -> do
      zero_args
      retV $ public $ SLV_Prim SLPrim_committed
    SLPrim_committed -> illegal_args
    SLPrim_digest -> do
      let rng = T_Digest
      darges <- map snd <$> mapM (typeOf . snd) sargs
      dargs <- compileArgExprs darges
      at <- withAt id
      dv <- ctxt_lift_expr (DLVar at Nothing rng) (DLE_Digest at dargs)
      return $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct -> do
      let barg = compileCheckType T_Bool
      (dargm, mmsg) <- case map snd sargs of
        [arg] -> return $ (barg arg, Nothing)
        [arg, marg] -> do
          bs <- mustBeBytes marg
          return $ (barg arg, Just bs)
        _ -> illegal_args
      darg <- dargm
      doClaim ct darg mmsg
      at <- withAt id
      let good = return $ public $ SLV_Null at "claim"
      let some_good ems = ensure_modes ems ("assert " <> show ct) >> good
      case ct of
        CT_Assume False -> some_good [SLM_LocalStep]
        CT_Assume True -> good
        CT_Require -> some_good [SLM_ConsensusStep, SLM_ConsensusPure]
        CT_Assert -> good
        CT_Possible -> good
        CT_Unknowable {} -> impossible "unknowable"
    SLPrim_transfer -> do
      at <- withAt id
      pay_sv <-
        case args of
          [a] -> return $ a
          [a, b] -> return $ SLV_Tuple at [SLV_Tuple at [a, b]]
          _ -> illegal_args
      let transferToPrim = SLV_Prim (SLPrim_transfer_amt_to pay_sv)
      return $
        public $
          SLV_Object at (Just "transfer") $
            M.fromList [("to", SLSSVal srcloc_builtin Public transferToPrim)]
    SLPrim_transfer_amt_to pay_sv -> do
      at <- withAt id
      ensure_mode SLM_ConsensusStep "transfer"
      part <- one_arg
      who_a <-
        typeOfM part >>= \case
          Just (ty, res) -> typeEq ty T_Address >> compileArgExpr res
          Nothing ->
            case part of
              SLV_Participant _ who _ _ ->
                is_class who
                  >>= expect_
                    . bool (Err_Transfer_NotBound who) (Err_Transfer_Class who)
              _ -> expect_ $ Err_Type_None part
      let staticallyZero = \case
            DLA_Literal (DLL_Int _ 0) -> True
            _ -> False
      DLPayAmt {..} <- compilePayAmt TT_Pay pay_sv
      let one amt_a mtok_a = unless (staticallyZero amt_a) $ do
            tokenPay mtok_a amt_a "balance sufficient for transfer"
            ctxt_lift_eff $ DLE_Transfer at who_a amt_a mtok_a
      one pa_net Nothing
      forM_ pa_ks $ \(a, t) -> one a $ Just t
      return $ public $ SLV_Null at "transfer.to"
    SLPrim_exit -> do
      zero_args
      doExit
      return $ public $ SLV_Prim $ SLPrim_exitted
    SLPrim_exitted -> illegal_args
    SLPrim_forall {} ->
      case sargs of
        [(olvl, one)] -> do
          dt <- st2dte =<< expect_ty "forall" one
          at <- withAt id
          tag <- ctxt_alloc
          dv <- ctxt_lift_expr (DLVar at Nothing dt) (DLE_Impossible at tag Err_Impossible_InspectForall)
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
          addr_da <- compileCheckType T_Address addr
          withAt $ \at -> (lvl, (SLV_Prim $ SLPrim_part_setted at who addr_da))
        _ -> illegal_args
    SLPrim_part_setted {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_Data -> do
      argm <- mustBeObject =<< one_arg
      varm <- mapWithKeyM (\ k v -> do
        locAt (sss_at v) $
          expect_ty ("value of " <> k) (sss_val v)) argm
      retV $ (lvl, SLV_Type $ ST_Data varm)
    SLPrim_Data_variant t vn vt -> do
      at <- withAt id
      dt <- mapM st2dte t
      vv <- case (vt, args) of
        (ST_Null, []) -> return $ SLV_Null at "variant"
        _ -> one_arg
      void $ typeCheck_s vt vv
      retV $ (lvl, SLV_Data at dt vn vv)
    SLPrim_data_match -> do
      -- Expect two arguments to function
      (obj, cases) <- two_args
      (obj_ty, _) <- typeOf obj
      obj_tys <- mustBeDataTy Err_Switch_NotData obj_ty
      cases_m <- mustBeObject cases
      args_x_case <-
        mapM
          (\v ->
             case sss_val v of
               SLV_Clo at _ (SLClo _ case_args _ _) ->
                 case case_args of
                   [] -> return $ False
                   [_] -> return $ True
                   xs -> expect_ $ Err_Apply_ArgCount at (length xs) 1
               ow ->
                 locAtf (flip srclocOf_ ow) $
                   expect_t ow $ Err_Decl_NotType "closure")
          cases_m
      -- Generate the function to call
      ann <- withAt at2a
      let semi = a2sp ann
      fnv <- evalExpr $ do
        let data_param = JSIdentifier ann "data_id"
        let case_param = JSIdentifier ann "cases_id"
        let go (tycon, case_args) =
              case tycon == "default" of
                True -> JSDefault ann ann case_body
                False -> JSCase ann case_id ann case_body
              where
                tycon_ty = M.lookup tycon obj_tys
                case_id = JSIdentifier ann tycon
                fn = JSMemberDot case_param ann $ JSIdentifier ann tycon
                one = [data_param]
                js_args = case tycon_ty of
                  Just T_Null ->
                    case case_args of
                      False -> []
                      True -> one
                  _ -> one
                ret = jsCall ann fn js_args
                case_body = [JSReturn ann (Just ret) semi]
        let switch_parts = map go $ M.toAscList args_x_case
        let body = JSSwitch ann ann data_param ann ann switch_parts ann semi
        jsArrow ann [data_param, case_param] body
      -- Apply the object and cases to the newly created function
      let fn = snd fnv
      evalApplyVals' fn [public obj, public cases]
    SLPrim_Participant -> makeParticipant False
    SLPrim_ParticipantClass -> makeParticipant True
    SLPrim_View -> do
      ensure_mode SLM_AppInit "View"
      at <- withAt id
      (nv, intv) <- two_args
      n <- mustBeBytes nv
      SLInterface im <- mustBeInterface intv
      sv <- ar_views <$> aisiGet aisi_res
      when (M.member n sv) $
        expect_ $ Err_View_DuplicateView n
      let ns = bunpack n
      verifyName "View" ns
      let go k t = do
            let vv = SLV_Prim $ SLPrim_viewis at n k t
            let vom = M.singleton "set" $ SLSSVal at Public vv
            let vo = SLV_Object at (Just $ ns <> " view, " <> k) vom
            let io = SLSSVal at Public vo
            di <-
              case t of
                ST_Fun (SLTypeFun {..}) ->
                  IT_Fun <$> mapM st2dte stf_dom <*> st2dte stf_rng
                ST_UDFun {} ->
                  expect_ $ Err_View_UDFun
                _ -> IT_Val <$> st2dte t
            return $ (di, io)
      ix <- mapWithKeyM go im
      let i' = M.map fst ix
      let io = M.map snd ix
      aisiPut aisi_res $ \ar ->
        ar {ar_views = M.insert n i' $ ar_views ar}
      retV $ (lvl, SLV_Object at (Just $ ns <> " view") io)
    SLPrim_Map -> illegal_args
    SLPrim_Map_new -> do
      t <- expect_ty "Map.new" =<< one_arg
      ensure_mode SLM_ConsensusStep "Map.new"
      mv <- mapNew =<< st2dte t
      retV $ public $ SLV_Map mv
    SLPrim_Map_reduce -> do
      at <- withAt id
      (m, z, f_) <- three_args
      mv <-
        case m of
          SLV_Map mv -> return $ mv
          _ -> expect_t m $ Err_Expected_Map
      mi <- mapLookup mv
      let x_tym = dlmi_tym mi
      let f = jsClo at "reduceWrapper" ("(b, ma) => ma.match({None: (() => b), Some: (a => f(b, a))})") (M.fromList [("f", f_)])
      ensure_while_invariant "Map.reduce"
      (z_ty, z_da) <- compileTypeOf z
      (b_dv, b_dsv) <- make_dlvar at z_ty
      (ma_dv, ma_dsv) <- make_dlvar at x_tym
      SLRes f_lifts _ f_da <-
        captureRes $ do
          (f_lvl, f_v) <- evalApplyVals' f [(lvl, b_dsv), (lvl, ma_dsv)]
          ensure_level lvl f_lvl
          (f_ty, f_da) <- compileTypeOf f_v
          typeEq z_ty f_ty
          return $ f_da
      (ans_dv, ans_dsv) <- make_dlvar at z_ty
      let f_bl = DLSBlock at [] f_lifts f_da
      mri <- ctxt_alloc
      saveLift $ DLS_MapReduce at mri ans_dv mv z_da b_dv ma_dv f_bl
      return $ (lvl, ans_dsv)
    SLPrim_Refine -> do
      at <- withAt id
      let mkClo = jsClo at "refine"
      t <- first_arg >>= expect_ty "Refine"
      t' <- case t of
        ST_Fun (SLTypeFun {..}) -> do
          (_, domp2, rngp2, mmsgs) <- three_mfourth_args
          let (domp_msg, rngp_msg) =
                case mmsgs of
                  Just (SLV_Tuple _ (dm : rm : _)) -> (Just dm, Just rm)
                  Just (SLV_Tuple _ (dm : _)) -> (Just dm, Nothing)
                  _ -> (Nothing, Nothing)
          let domp = case stf_pre of
                Nothing -> domp2
                Just domp1 ->
                  mkClo "(dom) => (domp1(dom) && domp2(dom))" $ M.fromList [("domp1", domp1), ("domp2", domp2)]
          let rngp = case stf_post of
                Nothing -> rngp2
                Just rngp1 ->
                  mkClo "(dom, rng) => (rngp1(dom, rng) && rngp2(dom, rng))" $ M.fromList [("rngp1", rngp1), ("rngp2", rngp2)]
          return $ ST_Fun $ SLTypeFun stf_dom stf_rng (Just domp) (Just rngp) domp_msg rngp_msg
        ST_Refine ot valp1 mmsg1 -> do
          (_, valp2, mmsg2) <- two_mthree_args
          let valp = mkClo "(x) => (valp1(x) && val1p2(x))" $ M.fromList [("valp1", valp1), ("valp2", valp2)]
          mmsg <-
            mapM mustBeBytes (catMaybes [mmsg1, mmsg2]) >>= \case
              [] -> return $ Nothing
              ow -> return $ Just $ SLV_Bytes at $ B.intercalate " and " ow
          return $ ST_Refine ot valp mmsg
        _ -> do
          (_, valp2, mmsg) <- two_mthree_args
          return $ ST_Refine t valp2 mmsg
      return $ public $ SLV_Type t'
    SLPrim_is -> do
      (x, y) <- two_args
      t <- expect_ty "is" y
      case t of
        ST_Fun tf ->
          case x of
            SLV_Clo at Nothing clo -> return (lvl, SLV_Clo at (Just tf) clo)
            -- XXX better error for SLV_Clo w/ type already
            _ -> expect_ $ Err_Decl_NotType "Fun" (x, Nothing)
        _ -> do
          void $ typeCheck_s t x
          return $ (lvl, x)
    SLPrim_remote -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote"
      (av, ri) <- two_args
      aa <- compileCheckType T_Address av
      rm_ <- mustBeObject ri
      rm <- mapWithKeyM (\ k v -> do
        locAt (sss_at v) $
          expect_ty ("value of " <> k) (sss_val v)) rm_
      at <- withAt id
      let go k = \case
            ST_Fun stf ->
              return $
                SLSSVal at Public $
                  SLV_Prim $
                    SLPrim_remotef at aa k stf Nothing Nothing Nothing
            t -> expect_ $ Err_Remote_NotFun k t
      om <- mapWithKeyM go rm
      return $ (lvl, SLV_Object at Nothing om)
    SLPrim_remotef rat aa m stf _ mbill (Just RFM_Pay) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote pay"
      payv <- one_arg
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa m stf (Just payv) mbill Nothing)
    SLPrim_remotef rat aa m stf mpay _ (Just RFM_Bill) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote bill"
      billv <- one_arg
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa m stf mpay (Just $ Right billv) Nothing)
    SLPrim_remotef rat aa m stf mpay _ (Just RFM_WithBill) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote withBill"
      at <- withAt id
      nonNetToks <- zero_mone_arg $ SLV_Tuple at []
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa m stf mpay (Just $ Left nonNetToks) Nothing)
    SLPrim_remotef rat aa m stf mpay mbill Nothing -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote"
      at <- withAt id
      let zero = SLV_Int at 0
      let amtv = fromMaybe zero mpay
      payAmt <- compilePayAmt TT_Pay amtv
      tokenPay Nothing (pa_net payAmt) "balance sufficient for payment to remote object"
      forM_ (pa_ks payAmt) $ \(a, t) -> do
        tokenPay (Just t) a "balance sufficient for payment to remote object"
      billAmt <- case fromMaybe (Left zero) mbill of
        Left _ -> return $ DLPayAmt (DLA_Literal $ DLL_Int at 0) []
        Right v -> compilePayAmt TT_Bill v
      (nnToksBilledType, nnToksBilledRecv) <- getBillTokens mbill billAmt
      let shouldRetNNToks = case fromMaybe (Right zero) mbill of
            Left (SLV_Tuple _ ts) -> not $ null ts
            _ -> False
      let SLTypeFun dom rng pre post pre_msg post_msg = stf
      let rng' = ST_Tuple $ if shouldRetNNToks then [ST_UInt, nnToksBilledType, rng] else [ST_UInt, rng]
      let postArg = if shouldRetNNToks then "(dom, [_, _, rng])" else "(dom, [_, rng])"
      let post' = flip fmap post $ \postv ->
            jsClo at "post" (postArg <> " => post(dom, rng)") $
              M.fromList [("post", postv)]
      let stf' = SLTypeFun dom rng' pre post' pre_msg post_msg
      nnTokReceived <- ctxt_mkvar $
        DLVar at Nothing $
          case nnToksBilledType of
            ST_Null -> T_Null
            ST_Tuple ts -> T_Tuple $ map (const T_UInt) ts
            _ -> impossible "Type of non-network tokens recieved is not [UInt]"
      allTokens <- fmap DLA_Var <$> readSt st_toks
      let nnToksNotBilled = allTokens \\ nnToksBilledRecv
      let withBill = DLWithBill nnTokReceived (if shouldRetNNToks then nnToksBilledRecv else []) nnToksNotBilled
      res' <-
        doInteractiveCall
          sargs
          rat
          (Left stf')
          SLM_ConsensusStep
          "remote"
          (CT_Assume True)
          (\_ fs _ dargs -> DLE_Remote at fs aa m payAmt dargs withBill)
      let getRemoteResults = do
            apdvv <- doArrRef_ res' zero
            case shouldRetNNToks of
              True -> do
                nnTokAmts <- doArrRef_ res' $ SLV_Int at 1
                res <- doArrRef_ res' $ SLV_Int at 2
                return (apdvv, res, Just nnTokAmts)
              False -> do
                res <- doArrRef_ res' $ SLV_Int at 1
                return (apdvv, res, Nothing)
      (apdvv, res, mNonNetToksRecv) <- getRemoteResults
      let withNonNetRecv f = case mNonNetToksRecv of Nothing -> return (); Just t -> f t
      doBalanceUpdate Nothing ADD apdvv
      case fromMaybe (Right zero) mbill of
        Left _ -> do
          forM_ (zip nnToksBilledRecv [0 .. length nnToksBilledRecv - 1]) $ \(t, i) ->
            withNonNetRecv $ \nnTokAmts -> do
              doBalanceUpdate (Just t) ADD
                =<< doArrRef_ nnTokAmts (SLV_Int at $ fromIntegral i)
          return $ public res'
        _ -> do
          sv <- argToSV $ pa_net billAmt
          forM_ (pa_ks billAmt) $ \(a, t) -> do
            a' <- argToSV a
            doBalanceUpdate (Just t) ADD a'
          -- Ensure we're paid expected network tokens
          cmp_v <- evalApplyVals' (SLV_Prim $ SLPrim_op PEQ) [public sv, public apdvv]
          void $
            evalApplyVals' (SLV_Prim $ SLPrim_claim $ CT_Assume True) $
              [cmp_v, public $ SLV_Bytes at "remote bill check"]
          return $ public res
    SLPrim_viewis _vat vn vk st -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "view.set"
      at <- withAt id
      mva <-
        case args of
          [] -> return Nothing
          [v] -> do
            v' <- snd <$> evalPrim SLPrim_is [public v, public $ SLV_Type st]
            mev <- slToDLExportVal v'
            when (mev == Nothing) $ do
              -- Might not be possible, because we already type checked
              expect_t v $ Err_View_CannotExpose
            return $ mev
          _ -> illegal_args
      saveLift $ DLS_ViewIs at vn vk mva
      return $ public $ SLV_Null at "viewis"
    SLPrim_deploy -> do
      at <- withAt id
      ensure_mode SLM_AppInit "deploy"
      e_appR <- fromRight (impossible "deploy") . e_appr <$> ask
      dlo <- readDlo id
      ctimem <- do
        let no = return $ Nothing
        let yes = do
              time_dv <- ctxt_mkvar (DLVar at Nothing T_UInt)
              doFluidSet FV_lastConsensusTime $ public $ SLV_DLVar time_dv
              secs_dv <- ctxt_mkvar (DLVar at Nothing T_UInt)
              doFluidSet FV_lastConsensusSecs $ public $ SLV_DLVar secs_dv
              return $ Just (time_dv, secs_dv)
        case dlo_deployMode dlo of
          DM_constructor -> yes
          DM_firstMsg -> no
      env <-
        (liftIO $ readIORef e_appR) >>= \case
          AIS_Init {..} -> do
            liftIO $ modifyIORef aisi_res $ \r -> r {ar_ctimem = ctimem}
            liftIO $ readIORef aisi_env
          _ -> impossible "deploy"
      liftIO $ writeIORef e_appR $ AIS_Deployed env
      st <- readSt id
      let after_ctor =
            case dlo_deployMode dlo of
              DM_constructor -> True
              DM_firstMsg -> False
      setSt $
        st
          { st_mode = SLM_Step
          , st_live = True
          , st_after_ctor = after_ctor
          }
      doBalanceInit Nothing
      return $ public $ SLV_Null at "deploy"
    SLPrim_setOptions -> do
      ensure_mode SLM_AppInit "setOptions"
      at <- withAt id
      opts <- mustBeObject =<< one_arg
      let use_opt k SLSSVal {sss_val = v, sss_at = opt_at} acc =
            case M.lookup k app_options of
              Nothing ->
                expect_thrown opt_at $
                  Err_App_InvalidOption k (S.toList $ M.keysSet app_options)
              Just opt ->
                case opt acc v of
                  Right x -> x
                  Left x -> expect_thrown opt_at $ Err_App_InvalidOptionValue k x
      aisiPut aisi_env $ \ae ->
        ae {ae_dlo = M.foldrWithKey use_opt (ae_dlo ae) opts}
      return $ public $ SLV_Null at "setOptions"
    SLPrim_adaptReachAppTupleArgs -> do
      tat <- withAt id
      tvs <- mustBeTuple =<< one_arg
      let adapt_tuple at p' who int = do
            liftIO $ emitWarning $ W_Deprecated $ D_ParticipantTuples at
            snd <$> evalPrim p' (map public $ [who, int])
      let go = \case
            SLV_Tuple at [who, int] ->
              adapt_tuple at SLPrim_Participant who int
            SLV_Tuple at [SLV_Bytes _ "class", who, int] ->
              adapt_tuple at SLPrim_ParticipantClass who int
            x -> return x
      tvs' <- mapM go tvs
      return (lvl, SLV_Tuple tat tvs')
  where
    lvl = mconcatMap fst sargs
    args = map snd sargs
    illegal_args = expect_ts args $ Err_Prim_InvalidArgs p
    retV = return
    rator = SLV_Prim p
    expect_ty lab v =
      case v of
        SLV_Type t -> return $ t
        _ -> expect_ $ Err_Expected_Type lab v
    zero_args = case args of
      [] -> return ()
      _ -> illegal_args
    first_arg = case args of
      x : _ -> return $ x
      _ -> illegal_args
    zero_mone_arg def = case args of
      [x] -> return x
      [] -> return def
      _ -> illegal_args
    one_arg = case args of
      [x] -> return $ x
      _ -> illegal_args
    two_args = case args of
      [x, y] -> return $ (x, y)
      _ -> illegal_args
    three_args = case args of
      [x, y, z] -> return $ (x, y, z)
      _ -> illegal_args
    two_mthree_args = case args of
      [x, y] -> return (x, y, Nothing)
      [x, y, z] -> return (x, y, Just z)
      _ -> illegal_args
    three_mfourth_args = case args of
      [x, y, z] -> return (x, y, z, Nothing)
      [x, y, z, a] -> return (x, y, z, Just a)
      _ -> illegal_args
    mustBeArray = \case
      T_Array ty sz -> return $ (ty, sz)
      _ -> illegal_args
    make_dlvar at' ty = do
      dv <- ctxt_mkvar $ DLVar at' Nothing ty
      return $ (dv, SLV_DLVar dv)
    mustBeInterface intv = do
      objEnv <- mustBeObject intv
      let checkint = \case
            SLSSVal _ _ (SLV_Type t) -> return $ t
            SLSSVal idAt _ idV -> locAt idAt $ expect_t idV $ Err_App_InvalidInteract
      SLInterface <$> mapM checkint objEnv
    makeParticipant :: Bool -> App SLSVal
    makeParticipant isClass = do
      ensure_mode SLM_AppInit "Participant Constructor"
      at <- withAt id
      (nv, intv) <- two_args
      n <- mustBeBytes nv
      let ns = bunpack n
      verifyName "Participant" ns
      ios <- ae_ios <$> aisiGet aisi_env
      when (M.member n ios) $
        expect_ $ Err_Part_DuplicatePart n
      int <- mustBeInterface intv
      (io, ienv) <- makeInteract n int
      aisiPut aisi_env $ \ae ->
        ae {ae_ios = M.insert n io $ ae_ios ae}
      aisiPut aisi_res $ \ar ->
        ar {ar_pie = M.insert n ienv $ ar_pie ar}
      when isClass $ do
        aisiPut aisi_env $ \ae ->
          ae {ae_classes = S.insert n $ ae_classes ae}
      return (lvl, SLV_Participant at n Nothing Nothing)

doInteractiveCall :: [SLSVal] -> SrcLoc -> Either SLTypeFun SLType -> SLMode -> String -> ClaimType -> (SrcLoc -> [SLCtxtFrame] -> DLType -> [DLArg] -> DLExpr) -> App SLVal
doInteractiveCall sargs iat estf mode lab ct mkexpr = do
  ensure_mode mode lab
  at <- withAt id
  (check_post, rng, arges) <-
    case estf of
      Left stf@(SLTypeFun {..}) -> do
        (dom_tupv, arges) <- assertRefinedArgs CT_Assert sargs iat stf
        let rng = stf_rng
        let check_post rng_v =
              forM_ stf_post $ \rngp ->
                applyRefinement ct rngp [dom_tupv, rng_v] stf_post_msg
        return (check_post, rng, arges)
      Right rng -> do
        let check_post = const $ return ()
        arges <- mapM (fmap snd . typeOf . snd) sargs
        return (check_post, rng, arges)
  dargs <- compileArgExprs arges
  fs <- e_stack <$> ask
  rng_v <- compileInteractResult ct lab rng $ \drng ->
    mkexpr at fs drng dargs
  check_post rng_v
  return rng_v

assertRefinedArgs :: ClaimType -> [SLSVal] -> SrcLoc -> SLTypeFun -> App (SLVal, [DLArgExpr])
assertRefinedArgs ct sargs iat (SLTypeFun {..}) = do
  let argvs = map snd sargs
  arges <-
    mapM (uncurry typeCheck_s)
      =<< zipEq (Err_Apply_ArgCount iat) stf_dom argvs
  at <- withAt id
  let dom_tupv = SLV_Tuple at argvs
  forM_ stf_pre $ \domp ->
    applyRefinement ct domp [dom_tupv] stf_pre_msg
  return (dom_tupv, arges)

evalApplyVals' :: SLVal -> [SLSVal] -> App SLSVal
evalApplyVals' rator randvs = do
  SLAppRes _ val <- evalApplyVals rator randvs
  return $ val

litToSV :: DLLiteral -> App SLVal
litToSV = \case
  DLL_Null -> withAt $ flip SLV_Null "litToSV"
  DLL_Bool b -> withAt $ flip SLV_Bool b
  DLL_Int a i -> return $ SLV_Int a i
  DLL_Bytes bs -> withAt $ flip SLV_Bytes bs

argToSV :: DLArg -> App SLVal
argToSV = \case
  DLA_Var dv -> return $ SLV_DLVar dv
  DLA_Constant dc -> return $ SLV_DLC dc
  DLA_Literal l -> litToSV l
  a@(DLA_Interact {}) -> alloc a
  where
    alloc a = do
      at <- withAt id
      SLV_DLVar <$> ctxt_lift_expr (DLVar at Nothing $ argTypeOf a) (DLE_Arg at a)

evalApplyArgs' :: SLVal -> [DLArg] -> App SLSVal
evalApplyArgs' rator randas =
  evalApplyVals' rator =<< mapM (liftM public . argToSV) randas

evalApplyClosureVals :: SrcLoc -> SLClo -> [SLSVal] -> App SLAppRes
evalApplyClosureVals clo_at (SLClo mname formals (JSBlock body_a body _) SLCloEnv {..}) randvs = do
  ret <- ctxt_alloc
  let body_at = srcloc_jsa "block" body_a clo_at
  let err = Err_Apply_ArgCount clo_at (length formals) (length randvs)
  let clo_sco =
        (SLScope
           { sco_ret = Just ret
           , sco_must_ret = RS_MayBeEmpty
           , sco_while_vars = Nothing
           , sco_penvs = clo_penvs
           , sco_cenv = clo_cenv
           , sco_use_strict = clo_use_strict
           })
  m <- readSt st_mode
  let arg_lvl = mconcat $ map fst randvs
  arg_env <-
    locStMode (pure_mode m) $
      locSco clo_sco $
        evalDeclLHSArray True (Just err) arg_lvl mempty (map snd randvs) formals
  at <- withAt id
  clo_sco' <- locSco clo_sco $ sco_update arg_env
  (body_lifts, (SLStmtRes clo_sco'' rs)) <-
    captureLifts $
      withFrame (SLC_CloApp at clo_at mname) $
        locAt body_at $
          locSco clo_sco' $
            evalStmt body
  let body_sa = mkAnnot body_lifts

  let saveAndRet lifts (lvl, v) = do
        saveLifts lifts
        return $ SLAppRes clo_sco'' (lvl, v)

  -- let promptAndStatic :: DLStmts -> (SecurityLevel, SLVal) -> App SLAppRes
  let promptAndStatic mt (lvl, v) =
        saveAndRet stmt (lvl, v)
          where
            stmt = return $ DLS_Prompt body_at (DLVar at Nothing t ret) body_sa body_lifts''
            (t, body_lifts'') =
              case mt of
                Just x -> (x, body_lifts)
                Nothing -> (T_Null, body_lifts <> (return $ DLS_Return body_at ret (DLA_Literal $ DLL_Null)))

  -- let maybeNoPromptStatic :: (SecurityLevel, SLVal) -> App SLAppRes
  let maybeNoPromptStatic mt (lvl, v) =
        case body_lifts of
          body_lifts' Seq.:|> (DLS_Return _ the_ret_label _the_val)
            | the_ret_label == ret ->
              --- We don't check that the_val is v, because
              --- we're relying on the invariant that there
              --- was only one Return... this should be
              --- true, but if something changes in the
              --- future, this might be a place that an
              --- error could be introduced.
              saveAndRet body_lifts' (lvl, v)
          _ ->
            promptAndStatic mt (lvl, v)
  case rs of
    [] -> maybeNoPromptStatic Nothing $ public $ SLV_Null body_at "clo app"
    [(_, mt, x, False)] -> maybeNoPromptStatic mt x
    (_, mt, (xlvl, xv), _) : more -> do
      let msvs = map (\(_a, _b, c, _d) -> c) more
      let mlvls = map fst msvs
      let mvs = map snd msvs
      let lvl = mconcat $ xlvl : mlvls
      -- We check equivalence here with Equiv typeclass. it's shallow checking but performant
      let all_same = all (equiv xv) mvs
      case all_same && not (containsVarNewerThan ret xv) of
        -- only remove the prompt if there was a single return
        True -> promptAndStatic mt (lvl, xv)
        False -> do
          let go (r_at, mrty, (_, v), _) =
                case mrty of
                  Nothing -> locAt r_at $ expect_ $ Err_Type_None v
                  Just t -> return $ t
          tys <- mapM go rs
          r_ty <- locAt body_at $ typeEqs tys
          let retv = DLVar body_at Nothing r_ty ret
          saveLift $ DLS_Prompt body_at retv body_sa body_lifts
          return $ SLAppRes clo_sco'' (lvl, (SLV_DLVar retv))

evalApplyVals :: SLVal -> [SLSVal] -> App SLAppRes
evalApplyVals = evalApplyValsAux False

evalApplyValsAux :: Bool -> SLVal -> [SLSVal] -> App SLAppRes
evalApplyValsAux assumePrecondition rator randvs =
  case rator of
    SLV_Prim p -> do
      sco <- e_sco <$> ask
      SLAppRes sco <$> evalPrim p randvs
    SLV_Clo clo_at Nothing sc ->
      evalApplyClosureVals clo_at sc randvs
    SLV_Clo clo_at (Just tf) sc -> do
      at <- withAt id
      let ct = if assumePrecondition then CT_Assume True else CT_Assert
      (dom_tupv, _) <- assertRefinedArgs ct randvs at tf
      res@(SLAppRes _ (_, ret_v)) <- evalApplyClosureVals clo_at sc randvs
      forM_ (stf_post tf) $ \rngp ->
        applyRefinement CT_Assert rngp [dom_tupv, ret_v] (stf_post_msg tf)
      return res
    v -> expect_t v $ Err_Eval_NotApplicableVals

evalApply :: SLVal -> [JSExpression] -> App SLSVal
evalApply rator rands =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo {} -> vals
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
    -- Allow `new` to be used as object property name for `Set`.
    Just (SLSSVal _ _ (SLV_Kwd SLK_new)) -> Nothing
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
              let mdv = DLVar at' Nothing t
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
        SLM_AppInit -> s
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
  JSDecimal a ns ->
    case splitOn "." ns of
      [iDigits, fDigits] ->
        let i = iDigits <> fDigits
         in let scale = '1' : replicate (length fDigits) '0'
             in let signV = \at ->
                      SLSSVal at Public $ SLV_Bool at True
                 in let signedInt = \at ->
                          SLV_Object at Nothing $
                            M.fromList
                              [ ("scale", SLSSVal at Public $ SLV_Int at $ numberValue 10 scale)
                              , ("i", SLSSVal at Public $ SLV_Int at $ numberValue 10 i)
                              ]
                     in let iV = \at -> SLSSVal at Public $ signedInt at
                         in locAtf (srcloc_jsa "decimal" a) $
                              withAt $ \at ->
                                public $
                                  SLV_Object at Nothing $
                                    M.fromList [("sign", signV at), ("i", iV at)]
      [_] -> locAtf (srcloc_jsa "decimal" a) $
        withAt $ \at -> public $ SLV_Int at $ numberValue 10 ns
      _ -> impossible "Number must have 0 or 1 decimal points."
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
          doCallV x (jsa op) [lhs, rhs]
    where
      tern a isAnd = evalExpr $ JSExpressionTernary lhs a te a fe
        where
          (te, fe) = case isAnd of
            True -> (rhs, JSLiteral a "false")
            False -> (JSLiteral a "true", rhs)
  JSExpressionParen a ie _ -> locAtf (srcloc_jsa "paren" a) $ evalExpr ie
  JSExpressionPostfix _ _ -> illegal
  JSExpressionTernary ce a te fa fe -> doTernary ce a te fa fe
  JSArrowExpression aformals a bodys -> locAtf (srcloc_jsa "arrow" a) $ do
    let body = jsArrowStmtToBlock bodys
    fformals <- withAt $ flip jsArrowFormalsToFunFormals aformals
    evalExpr $ JSFunctionExpression a JSIdentNone a fformals a body
  JSFunctionExpression a name _ jsformals _ body ->
    locAtf (srcloc_jsa "function exp" a) $ do
      at' <- withAt id
      let formals = parseJSFormals at' jsformals
      verifyFormals False formals
      fname <-
        case name of
          JSIdentNone -> return $ Nothing
          JSIdentName na _ ->
            locAtf (srcloc_jsa "function name" na) $
              expect_ Err_Fun_NamesIllegal
      ce <- ask >>= sco_to_cloenv . e_sco
      return $ public $ SLV_Clo at' Nothing $ SLClo fname formals body ce
  JSGeneratorExpression _ _ _ _ _ _ _ -> illegal
  JSMemberDot obj a field -> doDot obj a field
  JSMemberExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
  JSMemberNew a f lb args rb -> evalExpr obj
    where
      rator = JSMemberDot f a (JSIdentifier a "new")
      obj = JSMemberExpression rator lb args rb
  JSMemberSquare arr a idx _ -> doRef arr a idx
  JSNewExpression _ _ -> illegal
  JSObjectLiteral a plist _ -> locAtf (srcloc_jsa "obj" a) $ do
    at' <- withAt id
    let f (lvl, oenv) pp = lvlMeet lvl <$> evalPropertyPair oenv pp
    (lvl, fenv) <- foldlM f (mempty, mempty) $ jsctl_flatten plist
    return $ (lvl, SLV_Object at' Nothing fenv)
  JSSpreadExpression _ _ -> illegal
  JSTemplateLiteral _ _ _ _ -> illegal
  JSUnaryExpression op@JSUnaryOpMinus {} i -> castToSigned i op
  JSUnaryExpression op@JSUnaryOpPlus {} i -> castToSigned i op
  JSUnaryExpression (JSUnaryOpDelete a) ue ->
    locAtf (srcloc_jsa "delete" a) $ do
      at <- withAt id
      doDelete =<< evalLValue ue
      return $ public $ SLV_Null at "delete"
  JSUnaryExpression op ue ->
    unaryToPrim op
      >>= \x -> doCallV x (jsa op) [ue]
  JSVarInitExpression _ _ -> illegal
  JSYieldExpression ann _ -> illegalAt ann
  JSYieldFromExpression ann _ _ -> illegalAt ann
  where
    illegal = expect_ $ Err_Eval_IllegalJS e
    illegalAt annot = locAtf (srcloc_jsa "stmt" annot) illegal
    doCallV ratorv a rands =
      locAtf (srcloc_jsa "application" a) $
        evalApply ratorv rands
    doCall rator a rands = do
      (rator_lvl, ratorv) <-
        locAtf (srcloc_jsa "application, rator" a) $ evalExpr rator
      lvlMeet rator_lvl <$> doCallV ratorv a rands
    -- unary (-, +) work on signed types such as Int and FixedPoint
    -- which are of the shape: ∀ a . { sign: bool, i : a }
    -- This function will first cast UInt args to Int before applying op.
    -- Any arg that is already signed will just apply the op.
    castToSigned i op = do
      ex <- evalExpr i
      let mkField k v = JSPropertyNameandValue (JSPropertyString JSNoAnnot $ "'" <> k <> "'") JSNoAnnot [v]
      i' <-
        typeOf (snd ex) >>= \case
          (T_UInt, _) ->
            let i_field = JSLOne $ mkField "i" i
             in let sign_field = mkField "sign" $ JSLiteral JSNoAnnot "true"
                 in let si = JSObjectLiteral JSNoAnnot (JSCTLNone $ JSLCons i_field JSNoAnnot sign_field) JSNoAnnot
                     in return si
          _ -> return i
      unaryToPrim op >>= \o -> doCallV o JSNoAnnot [i']
    verifyFormals readDefaults = \case
      [] -> return ()
      JSAssignExpression _ (JSAssign _) _ : t -> verifyFormals True t
      _ : t
        | readDefaults -> expect_ Err_Default_Arg_Position
        | otherwise -> verifyFormals readDefaults t

doTernary :: JSExpression -> JSAnnot -> JSExpression -> JSAnnot -> JSExpression -> App SLSVal
doTernary ce a te fa fe = locAtf (srcloc_jsa "?:" a) $ do
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
      om <- readSt st_mode
      setSt =<< stMerge st_t st_f
      let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
      case hasIsPure sa of
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
                        <> (return $ DLS_Return e_at' ret da)
                return $ (elifts', e_ty)
          (tlifts', t_ty) <- add_ret t_at' tlifts tv
          (flifts', f_ty) <- add_ret f_at' flifts fv
          typeEq t_ty f_ty
          at' <- withAt id
          let ans_dv = DLVar at' Nothing t_ty ret
          theIf <- checkCond om $ DLS_If at' (DLA_Var cond_dv) sa tlifts' flifts'
          saveLift $ DLS_Prompt at' ans_dv sa $ return theIf
          return $ (lvl, SLV_DLVar ans_dv)
    _ -> do
      (n_at', ne, oe) <- case cv of
        SLV_Bool _ False -> return (f_at', fe, te)
        SLV_Bool _ True -> return (t_at', te, fe)
        _ -> do
          useStrict >>= \case
            True -> expect_ $ Err_Strict_Conditional cv
            False -> return (t_at', te, fe)
      whenUsingStrict $ ignoreAll $ evalExpr oe
      lvlMeet clvl <$> (locAt n_at' $ evalExpr ne)

checkCond :: SLMode -> DLSStmt -> App DLSStmt
checkCond om s = do
  unless (isLocal s) $
    locStMode om $
      ensure_mode SLM_ConsensusStep "conditional"
  return s

doDot :: JSExpression -> JSAnnot -> JSExpression -> App SLSVal
doDot obj a field = do
  at' <- withAt $ srcloc_jsa "dot" a
  (obj_lvl, objv) <- locAt at' $ evalExpr obj
  let fields = (jse_expect_id at') field
  fieldAt <-
    case field of
      JSIdentifier iat _ -> withAt $ srcloc_jsa "dot" iat
      _ -> return $ at'
  lvlMeet obj_lvl <$> (locAt fieldAt $ evalDot objv fields)

doDelete :: SLLValue -> App ()
doDelete = \case
  SLLV_MapRef _at mv mc -> do
    ensure_mode SLM_ConsensusStep "Map.set"
    mapDel mv mc

doRef :: JSExpression -> JSAnnot -> JSExpression -> App SLSVal
doRef arre a idxe = locAtf (srcloc_jsa "ref" a) $ do
  arrsv <- evalExpr arre
  case snd arrsv of
    SLV_Map mv -> do
      iv <- ensure_public =<< evalExpr idxe
      (public . SLV_DLVar) <$> mapRef mv iv
    _ ->
      doArrRef arrsv a idxe

doArrRef_ :: SLVal -> SLVal -> App SLVal
doArrRef_ arrv idxv = do
  at' <- withAt id
  let retRef t de = SLV_DLVar <$> ctxt_lift_expr (DLVar at' Nothing t) de
  let retArrayRef t sz arr_dla idx_dla = do
        doArrayBoundsCheck sz idxv
        retRef t $ DLE_ArrayRef at' arr_dla idx_dla
  let retTupleRef t arr_dla idx =
        retRef t $ DLE_TupleRef at' arr_dla idx
  let retObjectRef t arr_dla f =
        retRef t $ DLE_ObjectRef at' arr_dla f
  let retVal idxi arrvs =
        case fromIntegerMay idxi >>= atMay arrvs of
          Nothing ->
            expect_ $ Err_Eval_RefOutOfBounds (length arrvs) idxi
          Just ansv -> return ansv
  case idxv of
    SLV_Int _ idxi ->
      case arrv of
        SLV_Array _ _ arrvs -> retVal idxi arrvs
        SLV_Tuple _ tupvs -> retVal idxi tupvs
        SLV_Struct _ kvs -> retVal idxi $ map snd kvs
        SLV_DLVar adv@(DLVar _ _ (T_Struct ts) _) ->
          case fromIntegerMay idxi >>= atMay ts of
            Nothing ->
              expect_ $ Err_Eval_RefOutOfBounds (length ts) idxi
            Just (k, t) -> retObjectRef t (DLA_Var adv) k
        SLV_DLVar adv@(DLVar _ _ (T_Tuple ts) _) ->
          case fromIntegerMay idxi >>= atMay ts of
            Nothing ->
              expect_ $ Err_Eval_RefOutOfBounds (length ts) idxi
            Just t -> retTupleRef t (DLA_Var adv) idxi
        SLV_DLVar adv@(DLVar _ _ (T_Array t sz) _) ->
          case idxi < sz of
            False ->
              expect_ $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
            True -> do
              idx_dla <- withAt $ \at -> DLA_Literal (DLL_Int at idxi)
              retArrayRef t sz (DLA_Var adv) idx_dla
        _ -> expect_t arrv $ Err_Eval_RefNotRefable
    SLV_DLVar idxdv@(DLVar _ _ T_UInt _) -> do
      (arr_ty, arr_dla) <- compileTypeOf arrv
      case arr_ty of
        T_Array elem_ty sz ->
          retArrayRef elem_ty sz arr_dla $ DLA_Var idxdv
        _ -> expect_t arrv $ Err_Eval_IndirectRefNotArray
    _ -> expect_t idxv $ Err_Eval_RefNotInt

doArrRef :: SLSVal -> JSAnnot -> JSExpression -> App SLSVal
doArrRef (arr_lvl, arrv) a idxe = locAtf (srcloc_jsa "array ref" a) $ do
  (idx_lvl, idxv) <- evalExpr idxe
  elemv <- doArrRef_ arrv idxv
  let lvl = arr_lvl <> idx_lvl
  return (lvl, elemv)

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

evalDeclLHSArray :: Bool -> Maybe EvalError -> SecurityLevel -> SLEnv -> [SLVal] -> [JSExpression] -> App SLEnv
evalDeclLHSArray trackVars merr rhs_lvl lhs_env vs es =
  case (vs, es) of
    ([], []) ->
      return $ lhs_env
    (_, (JSSpreadExpression a e) : es') -> do
      locAtf (srcloc_jsa "array spread" a) $ do
        v <- withAt $ \at -> SLV_Tuple at vs
        case es' of
          [] -> evalDeclLHS trackVars merr rhs_lvl lhs_env v e
          _ -> expect_ $ Err_Decl_ArraySpreadNotLast
    (v : vs', e : es') -> do
      lhs_env' <- evalDeclLHS trackVars merr rhs_lvl lhs_env v e
      evalDeclLHSArray trackVars merr rhs_lvl lhs_env' vs' es'
    -- Use default parameter if no args specified
    ([], JSAssignExpression lhs (JSAssign _) rhs : es') -> do
      rhs' <- sco_update lhs_env >>= flip locSco (evalExpr rhs)
      lhs_env' <- evalDeclLHS trackVars merr rhs_lvl lhs_env (snd rhs') lhs
      evalDeclLHSArray trackVars merr rhs_lvl lhs_env' [] es'
    (_, _) -> do
      expect_ $ fromMaybe (Err_Decl_WrongArrayLength (length es) (length vs)) merr

evalDeclLHSObject :: Bool -> Maybe EvalError -> SecurityLevel -> SLEnv -> SLVal -> SLObjEnv -> [JSObjectProperty] -> App SLEnv
evalDeclLHSObject trackVars merr rhs_lvl lhs_env orig_v vm = \case
  [] -> return $ lhs_env
  (JSObjectSpread a e) : os' -> do
    locAtf (srcloc_jsa "object spread" a) $
      case os' of
        [] -> do
          vom <- evalObjEnv vm
          vo <- withAt $ \at_ -> SLV_Object at_ Nothing vom
          evalDeclLHS trackVars merr rhs_lvl lhs_env vo e
        _ -> expect_ $ Err_Decl_ObjectSpreadNotLast
  o : os' -> do
    let go x e = do
          (v_lvl, v) <- evalDot_ orig_v vm x
          let lvl' = rhs_lvl <> v_lvl
          lhs_env' <- evalDeclLHS trackVars merr lvl' lhs_env v e
          let vm' = M.delete x vm
          evalDeclLHSObject trackVars merr rhs_lvl lhs_env' orig_v vm' os'
    case o of
      JSPropertyIdentRef a x -> do
        go x $ JSIdentifier a x
      JSPropertyNameandValue pn _ [e] ->
        flip go e =<< (snd <$> evalPropertyName pn)
      _ ->
        expect_ $ Err_Parse_ExpectIdentifierProp o

evalDeclLHS :: Bool -> Maybe EvalError -> SecurityLevel -> SLEnv -> SLVal -> JSExpression -> App SLEnv
evalDeclLHS trackVars merr rhs_lvl lhs_env v = \case
  JSIdentifier a x -> do
    locAtf (srcloc_jsa "id" a) $ do
      at_ <- withAt id
      let v' = infectWithId_sv at_ x v
      when trackVars $ trackVariable (at_, x)
      env_insert x (SLSSVal at_ rhs_lvl v') lhs_env
  JSArrayLiteral a xs _ -> do
    locAtf (srcloc_jsa "array" a) $ do
      vs <- explodeTupleLike "lhs array" v
      evalDeclLHSArray trackVars merr rhs_lvl lhs_env vs (jsa_flatten xs)
  JSObjectLiteral a props _ -> do
    locAtf (srcloc_jsa "object" a) $ do
      vm <- evalAsEnv v
      evalDeclLHSObject trackVars merr rhs_lvl lhs_env v vm (jso_flatten props)
  -- Ignore default argument since assigned a value
  JSAssignExpression lhs (JSAssign a) _ -> do
    locAtf (srcloc_jsa "default" a) $ do
      evalDeclLHS trackVars merr rhs_lvl lhs_env v lhs
  e -> expect_ $ Err_DeclLHS_IllegalJS e

evalDeclLHSs :: Bool -> SLEnv -> [(JSExpression, SLSVal)] -> App SLEnv
evalDeclLHSs trackVars lhs_env = \case
  [] -> return $ lhs_env
  (e, (rhs_lvl, v)) : more ->
    flip (evalDeclLHSs trackVars) more =<< evalDeclLHS trackVars Nothing rhs_lvl lhs_env v e

evalDecl :: Bool -> JSExpression -> JSExpression -> App SLEnv
evalDecl trackVars lhs rhs = do
  (rhs_lvl, rhs_v) <- evalExpr rhs
  evalDeclLHS trackVars Nothing rhs_lvl mempty rhs_v lhs

destructDecls :: (JSCommaList JSExpression) -> App (JSExpression, JSExpression)
destructDecls = \case
  (JSLOne (JSVarInitExpression lhs (JSVarInit _ rhs))) -> return (lhs, rhs)
  es -> expect_ $ Err_Decls_IllegalJS es

-- | Make sure all bindings in this SLEnv respect the rule that
-- private vars must be named with a leading underscore.
enforcePrivateUnderscore :: SLEnv -> App ()
enforcePrivateUnderscore = mapM_ enf . M.toList
  where
    enf (k, SLSSVal at secLev _) = case secLev of
      Secret
        | not (isSpecialIdent k)
            && not (isSecretIdent k) ->
          locAt at $ expect_ $ Err_Eval_NotSecretIdent k
      _ -> return ()

doOnlyExpr :: ((SLPart, Maybe SLVar), SrcLoc, SLCloEnv, JSExpression) -> App (SLEnv, DLType, SLVal)
doOnlyExpr ((who, vas), only_at, only_cloenv, only_synarg) = do
  st <- readSt id
  sco <- e_sco <$> ask
  let SLCloEnv only_penvs only_cenv only_strict = only_cloenv
  let st_localstep = st {st_mode = SLM_LocalStep}
  let sco_only_pre =
        sco
          { sco_penvs = only_penvs
          , sco_cenv = only_cenv
          , sco_use_strict = only_strict
          }
  locWho who $
    locSco sco_only_pre $
      locSt st_localstep $ do
        penv__ <- sco_lookup_penv who
        ios <- ae_ios <$> aisd
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
              only_clo@(SLV_Clo _ Nothing (SLClo _ [] _ _)) -> do
                SLAppRes sco' (_, only_v) <- evalApplyVals only_clo []
                let penv' = (sco_penvs sco') M.! who
                --- TODO: check less things
                enforcePrivateUnderscore penv'
                only_ty <- fst <$> typeOf only_v
                return (penv', only_ty, only_v)
              _ -> expect_t only_arg $ Err_Only_NotOneClosure

doOnly :: SLScope -> ((SLPart, Maybe SLVar), SrcLoc, SLCloEnv, JSExpression) -> App SLScope
doOnly sco ((who, vas), only_at, only_cloenv, only_synarg) = locAt only_at $ do
  (alifts, (penv', only_ty, only_v)) <-
    captureLifts $
      doOnlyExpr ((who, vas), only_at, only_cloenv, only_synarg)
  case only_ty of
    T_Null -> do
      saveLift $ DLS_Only only_at who alifts
      return $ sco {sco_penvs = M.insert who penv' $ sco_penvs sco}
    ty -> locAt (srclocOf only_v) $ expect_ $ Err_Block_NotNull ty

doGetSelfAddress :: SLPart -> App DLVar
doGetSelfAddress who = do
  isClass <- is_class who
  at <- withAt id
  nonce <- ctxt_alloc
  ctxt_lift_expr
    (DLVar at Nothing T_Address)
    (DLE_PrimOp
       at
       SELF_ADDRESS
       [ DLA_Literal (DLL_Bytes who)
       , DLA_Literal (DLL_Bool isClass)
       , DLA_Literal (DLL_Int at $ fromIntegral nonce)
       ])

all_just :: [Maybe a] -> Maybe [a]
all_just = \case
  [] -> Just []
  Nothing : _ -> Nothing
  Just x : xs ->
    case all_just xs of
      Just xs' -> Just $ x : xs'
      Nothing -> Nothing

getBindingOrigin :: [DLArg] -> Maybe (SrcLoc, SLVar)
getBindingOrigin [DLA_Var (DLVar _ b _ _)] = b
getBindingOrigin _ = Nothing

compilePayAmt :: TransferType -> SLVal -> App DLPayAmt
compilePayAmt tt v = do
  at <- withAt id
  let v_at = srclocOf_ at v
  (t, ae) <- typeOf v
  case (t, ae) of
    (T_UInt, _) -> do
      a <- compileArgExpr ae
      return $ DLPayAmt a []
    (T_Tuple ts, DLAE_Tuple aes) -> do
      let go sa = \case
            (T_UInt, gae) -> do
              let ((seenNet, sks), DLPayAmt _ tks) = sa
              when seenNet $
                locAt v_at $ expect_ $ Err_Transfer_DoubleNetworkToken tt
              a <- compileArgExpr gae
              return $ ((True, sks), DLPayAmt a tks)
            (T_Tuple [T_UInt, T_Token], DLAE_Tuple [amt_ae, token_ae]) -> do
              let ((seenNet, sks), DLPayAmt nts tks) = sa
              amt_a <- compileArgExpr amt_ae
              token_a <- compileArgExpr token_ae
              when (token_a `elem` sks) $ do
                locAt v_at $ expect_ $ Err_Transfer_DoubleToken tt
              let sks' = token_a : sks
              return $ ((seenNet, sks'), (DLPayAmt nts $ ((amt_a, token_a) : tks)))
            _ -> locAt v_at $ expect_t v $ Err_Transfer_Type tt
      snd <$> (foldM go ((False, []), DLPayAmt (DLA_Literal $ DLL_Int at 0) []) $ zip ts aes)
    _ -> locAt v_at $ expect_t v $ Err_Transfer_Type tt

doToConsensus :: [JSStatement] -> S.Set SLPart -> Maybe SLVar -> [SLVar] -> JSExpression -> JSExpression -> Maybe (SrcLoc, JSExpression, Maybe JSBlock) -> App SLStmtRes
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
  let compilePayAmt_ e = compilePayAmt TT_Pay =<< ensure_public =<< evalExpr e
  let tc_send1 who = do
        let st_lpure = st {st_mode = SLM_LocalPure}
        (only_lifts, res) <-
          locWho who $
            locSt st_lpure $
              captureLifts $ do
                let repeat_dv = M.lookup who pdvs
                ds_isClass <- is_class who
                ds_msg <- mapM (\v -> snd <$> (compileTypeOf =<< ensure_public =<< evalId "publish msg" v)) msg
                ds_pay <- compilePayAmt_ amt_e
                ds_when <- ctepee T_Bool when_e
                return (repeat_dv, DLSend {..})
        saveLift $ DLS_Only at who only_lifts
        return $ res
  tc_send'0 <- sequence $ M.fromSet tc_send1 whos
  let tc_send' = tc_send'0
  let tc_send = fmap snd tc_send'
  let msg_dass = fmap ds_msg tc_send
  let msg_dass_t = transpose $ M.elems $ msg_dass
  let get_msg_t = typeEqs . map argTypeOf
  msg_ts <- mapM get_msg_t msg_dass_t
  let mrepeat_dvs = all_just $ M.elems $ M.map fst tc_send'
  -- Handle receiving / consensus
  dr_from <- ctxt_mkvar $ DLVar at Nothing T_Address
  let recv_imode = AllowShadowingRace whos (S.fromList msg)
  whosc <- mapM (\w -> (,) w <$> is_class w) $ S.toList whos
  (who_env_mod, pdvs_recv) <-
    case whosc of
      [(who, False)] -> do
        let who_dv = fromMaybe dr_from (M.lookup who pdvs)
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
  let mkmsg v t = ctxt_mkvar $ DLVar at (getBindingOrigin v) t
  dr_msg <- zipWithM mkmsg msg_dass_t msg_ts
  let toks = filter ((==) T_Token . varType) dr_msg
  unless (null toks) $ do
    sco <- e_sco <$> ask
    when (isJust $ sco_while_vars sco) $
      expect_ $ Err_Token_InWhile
  let st_recv =
        st
          { st_mode = SLM_ConsensusStep
          , st_pdvs = pdvs_recv
          , st_toks = st_toks st <> toks
          , st_after_first = True
          }
  msg_env <- foldlM env_insertp mempty $ zip msg $ map (sls_sss at . public . SLV_DLVar) $ dr_msg
  let recv_env_mod = who_env_mod . (M.insert "this" (SLSSVal at Public $ SLV_DLVar dr_from))
  let recv_env = msg_env
  (tc_recv, k_st, k_cr) <- do
    SLRes conlifts k_st (mktc_recv, k_cr) <- captureRes $ do
      setSt st_recv
      sco_recv <- sco_update_and_mod recv_imode recv_env recv_env_mod
      locSco sco_recv $ do
        let req_rator = SLV_Prim $ SLPrim_claim CT_Require
        bv <- evalAllDistinct $ map SLV_DLVar toks
        _ <- locSt st_pure $ evalApplyVals' req_rator [public bv]
        DLPayAmt {..} <- compilePayAmt_ amt_e
        unless (null pa_ks) $
          unless (st_after_ctor st) $
            expect_ $ Err_Token_OnCtor
        fs <- e_stack <$> ask
        let checkPayAmt1 mtok pa = do
              sv <- argToSV pa
              doBalanceUpdate mtok ADD sv
              ctxt_lift_eff $ DLE_CheckPay at fs pa mtok
        -- We ensure that the network payment is first
        checkPayAmt1 Nothing pa_net
        -- because it may need to pay the fee to initialize these tokens
        -- (on Algorand)
        forM_ (map DLA_Var toks) $ \tok -> do
          doBalanceInit $ Just tok
          ctxt_lift_eff $ DLE_TokenInit at tok
        forM_ pa_ks $ uncurry $ flip $ checkPayAmt1 . Just
        let check_repeat whoc_v repeat_dv = do
              repeat_cmp_v <-
                evalPrimOp ADDRESS_EQ $
                  map (public . SLV_DLVar) [repeat_dv, dr_from]
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
        let go fv = do
              v <- ctxt_mkvar $ DLVar at Nothing T_UInt
              doFluidSet fv $ public $ SLV_DLVar v
              return v
        dr_time <- go FV_thisConsensusTime
        dr_secs <- go FV_thisConsensusSecs
        k_cr <- evalStmt ks
        let mktc_recv dr_k = DLRecv {..}
        return $ (mktc_recv, k_cr)
    return $ (mktc_recv conlifts, k_st, k_cr)
  -- Handle timeout
  let not_true_send = \case
        DLA_Literal (DLL_Bool True) -> False
        _ -> True
  let not_all_true_send =
        getAny $ mconcat $ map (Any . not_true_send) (M.elems $ M.map ds_when tc_send)
  let mustHaveTimeout = S.null whos || not_all_true_send
  let timeout_ignore_okay1 (DLSend {..}) =
        case (ds_isClass, ds_when) of
          (_, DLA_Literal (DLL_Bool True)) -> True
          (True, when_da) ->
            case when_da of
              DLA_Literal (DLL_Bool False) -> False
              _ -> True
          _ -> False
  let timeout_ignore_okay =
        getAny $ mconcat $ map (Any . timeout_ignore_okay1) (M.elems tc_send)
  let mustHaveTimeoutNoMatterWhat = not $ timeout_ignore_okay
  (tc_mtime, fcr) <-
    case mtime of
      Nothing -> do
        when mustHaveTimeout $ expect_ $ Err_ToConsensus_WhenNoTimeout False
        setSt k_st
        return $ (Nothing, k_cr)
      Just (time_at, delay_e, mtimeb) -> locAt time_at $ do
        ensure_can_wait
        delay_sv <- locSt st_pure $ evalExpr delay_e >>= ensure_public
        case delay_sv of
          SLV_Bool _ False -> do
            when mustHaveTimeoutNoMatterWhat $
              expect_ $ Err_ToConsensus_WhenNoTimeout True
            setSt k_st
            return $ (Nothing, k_cr)
          _ -> do
            delay_ta <- compileTimeArg delay_sv
            doBaseWaitUpdate delay_ta
            case mtimeb of
              Nothing ->
                expect_ $ Err_ToConsensus_NoTimeoutBlock
              Just (JSBlock _ time_ss _) -> do
                SLRes time_lifts time_st time_cr <-
                  captureRes $ evalStmt time_ss
                setSt k_st
                mergeSt time_st
                fcr <- combineStmtRes True Public k_cr time_st time_cr
                return $ (Just (delay_ta, time_lifts), fcr)
  -- Prepare final result
  saveLift $ DLS_ToConsensus at tc_send tc_recv tc_mtime
  return $ fcr

srcloc2annot :: SrcLoc -> JSAnnot
srcloc2annot = \case
  SrcLoc _ (Just x) _ -> JSAnnot x []
  _ -> JSNoAnnot

at2a :: SrcLoc -> JSAnnot
at2a = srcloc2annot

typeToExpr :: DLType -> JSExpression
typeToExpr = \case
  T_Null -> var "Null"
  T_Bool -> var "Bool"
  T_UInt -> var "UInt"
  T_Bytes i -> call "Bytes" [ie i]
  T_Digest -> var "Digest"
  T_Address -> var "Address"
  T_Token -> var "Token"
  T_Array t i -> call "Array" [r t, ie i]
  T_Tuple ts -> call "Tuple" $ map r ts
  T_Object m -> call "Object" [rm m]
  T_Data m -> call "Data" [rm m]
  T_Struct ts -> call "Struct" $ [arr $ map sg ts]
  where
    str x = JSStringLiteral a x
    arr l = JSArrayLiteral a (map JSArrayElement l) a
    sg (k, t) = arr [(str k), r t]
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
  , cfc_msg_type_def :: [JSStatement]
  }

forkCaseSameParticipant :: ForkCase -> ForkCase -> Bool
forkCaseSameParticipant l r = getWho l == getWho r
  where
    getWho e = jse_expect_id (fc_at e) (fc_who e)

doFork :: [JSStatement] -> [ForkCase] -> Maybe (SrcLoc, [JSExpression]) -> Maybe JSExpression -> App SLStmtRes
doFork ks cases mtime mnntpay = do
  idx <- ctxt_alloc
  let fid x = ".fork" <> (show idx) <> "." <> x
  at <- withAt id
  let a = srcloc2annot at
  let jid = JSIdentifier a
  let sp = JSSemi a
  let fd_e = jid (fid "data")
  let res_e = jid (fid "res")
  let msg_e = jid (fid "msg")
  let when_e = jid (fid "when")
  let tv = jid $ ".t" <> show idx
  let mkobj l = JSObjectLiteral a (JSCTLNone $ toJSCL l) a
  let makeOnly who_e only_body = JSMethodCall (JSMemberDot who_e a (jid "only")) a (JSLOne $ jsThunkStmts a only_body) a sp
  let defcon l r = JSConstant a (JSLOne $ JSVarInitExpression l $ JSVarInit a r) sp
  let indexed = zip [0 ..] :: [a] -> [(Int, a)]
  let forkOnlyHelp who_e e_at before_e msg_id when_id = locAt e_at $ do
        let only_before_call_e = JSCallExpression (JSMemberDot who_e a (jid "only")) a (JSLOne before_e) a
        (_, (_, res_sv)) <- captureLifts $ evalExpr only_before_call_e
        (_, (_, res_ty, only_sv)) <-
          case res_sv of
            SLV_Form (SLForm_EachAns [(who_, vas)] only_at only_cloenv only_synarg) ->
              captureLifts $
                doOnlyExpr ((who_, vas), only_at, only_cloenv, only_synarg)
            _ -> impossible "not each"
        res_ty_m <- locAt (srclocOf only_sv) $ mustBeObjectTy Err_Fork_ResultNotObject res_ty
        let resHas = flip M.member res_ty_m
        let tDot field def =
              bool def (JSMemberDot tv a $ jid field) $ resHas field
        let defWhen = defcon when_id $ tDot "when" $ JSLiteral a "true"
        let defMsg = defcon msg_id $ tDot "msg" $ JSLiteral a "null"
        return $
          (,) res_ty_m $
            [ defcon tv $ jsCallThunk a before_e
            , defWhen
            , defMsg
            ]
  let lookupMsgTy t = fromMaybe T_Null $ M.lookup "msg" t
  let getAfter (a_at, after_e) = locAt a_at $
        case after_e of
          JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ s -> return [s]
          JSArrowExpression (JSParenthesizedArrowParameterList _ (JSLOne ae) _) _ s -> return [defcon ae msg_e, s]
          JSExpressionParen _ e _ -> getAfter (a_at, e)
          ow -> expect_ $ Err_Fork_ConsensusBadArrow ow
  let go pcases = do
        let (ats, who_es, before_es, pay_es, after_es) =
              unzip5 $ map (\ForkCase {..} -> (fc_at, fc_who, fc_before, fc_pay, fc_after)) pcases
        let who_e = hdDie who_es
        let c_at = hdDie ats
        let cfc_part = who_e
        let who = jse_expect_id c_at who_e
        let partCase n = who <> show n
        let partMsgType = fid $ who <> "MsgType"
        let ifOneCase t e = if length before_es == 1 then t else e
        -- Generate:
        -- const runAlice<N> = () => {
        --   const res = before<N>();
        --   const when = res.when || true;
        --   const msg = res.msg || null;
        --   return { ... res, when, msg: Alice<N>(msg) };
        -- }
        let makeRuns (n, (e_at, before_e)) = do
              (res_ty_m, only_body) <-
                forkOnlyHelp who_e e_at before_e (jid "msg") (jid "when")
              -- If there's one case, simply return message.
              --  Otherwise, inject msg into variant representing which case executed
              let msgExpr = ifOneCase (jid "msg") $ JSCallExpression (JSMemberDot (jid partMsgType) a (jid $ partCase n)) a (JSLOne (jid "msg")) a
              let returnExpr = JSObjectLiteral a (mkCommaTrailingList $ [JSObjectSpread a tv, JSPropertyIdentRef a "when", JSPropertyNameandValue (JSPropertyIdent a "msg") a [msgExpr]]) a
              let stmts =
                    only_body
                      <> [JSReturn a (Just returnExpr) sp]
              let cloName = "run" <> partCase n
              return (res_ty_m, cloName, jsThunkStmts a stmts)

        (res_ty_ms, beforeNames, beforeClosures) <- unzip3 <$> mapM makeRuns (indexed $ zip ats before_es)
        -- Msg is either the type for a single case or variant of all possible case msgs
        let msg_ty =
              case res_ty_ms of
                [t] -> lookupMsgTy t
                _ -> T_Data $ M.fromList $ map (bimap partCase lookupMsgTy) $ indexed res_ty_ms
        -- Generate:
        --    const case_res0 = runAlice0(); ...
        --    const res = case_res<N - 1>.when ? case_res<N - 1> : runAliceN();
        let genCaseResStmts = aux 0
              where
                aux :: Int -> [String] -> [JSStatement]
                aux 0 [h] = [defcon res_e $ jsCallThunk a $ jid h]
                aux 0 (h : t) = defcon (mkCaseRes 0) (jsCallThunk a $ jid h) : aux 1 t
                aux n (h : t) = defcon thisRes e : aux (n + 1) t
                  where
                    prevRes = mkCaseRes $ n - 1
                    thisRes = if null t then res_e else mkCaseRes n
                    cnd = JSMemberDot prevRes a (jid "when")
                    e = JSExpressionTernary cnd a prevRes a $ jsCallThunk a $ jid h
                aux _ [] = []
                mkCaseRes :: Int -> JSExpression
                mkCaseRes n = jid $ "case_res" <> show n

        let cr_ss = genCaseResStmts beforeNames
        let this_eq_who = JSExpressionBinary (jid "this") (JSBinOpEq a) who_e
        who_s <-
          (snd <$> evalExpr who_e) >>= \case
            SLV_Participant _ x _ _ -> return $ x
            v -> expect_t v $ Err_Expected "participant"
        isBound <- readSt $ M.member who_s . st_pdvs
        let req_e = bool (JSLiteral a "true") this_eq_who isBound
        let mkobjp i x = JSPropertyNameandValue (JSPropertyIdent a i) a [x]
        let pay_go (i, e) = mkobjp (partCase i) e
        let pay_obj_props = map pay_go $ indexed pay_es
        let pay_obj = JSObjectLiteral a (mkCommaTrailingList pay_obj_props) a
        let pay_cases = jsArrowExpr a [tv] $ JSCallExpression (JSMemberDot tv a (jid "match")) a (JSLOne pay_obj) a
        let cfc_req_prop = mkobjp who $ jsArrowExpr a [(jid "_")] req_e
        let cfc_pay_prop = mkobjp who $ ifOneCase (hdDie pay_es) pay_cases
        let (cfc_msg_type_def, tyExpr) =
              case msg_ty of
                T_Data {} -> let i = jid partMsgType in ([defcon i $ typeToExpr msg_ty], i)
                ow -> ([], typeToExpr ow)
        let cfc_data_def = mkobjp who tyExpr
        let var_e = jid who
        let res_msg = JSMemberDot res_e a (jid "msg")
        let msg_vde = JSCallExpression (JSMemberDot fd_e a var_e) a (JSLOne res_msg) a
        let res_when = JSMemberDot res_e a (jid "when")
        let run_ss = zipWith (defcon . jid) beforeNames beforeClosures
        let only_body = run_ss <> cr_ss <> [defcon msg_e msg_vde, defcon when_e res_when]
        isClass <- is_class who_s
        let who_is_this_ss =
              case (isBound, isClass) of
                (True, _) ->
                  [JSMethodCall (jid "assert") a (JSLOne this_eq_who) a sp]
                (False, False) ->
                  [JSMethodCall (JSMemberDot who_e a (jid "set")) a (JSLOne (jid "this")) a sp]
                (False, True) -> []
        all_afters <- mapM getAfter $ zip ats after_es
        let cases_switch_cases =
              map
                (\(i, as) ->
                   JSCase a (jid $ partCase i) a as)
                $ indexed all_afters
        let after_ss =
              ifOneCase (concat all_afters) [JSSwitch a a msg_e a a cases_switch_cases a sp]
        let cfc_switch_case = JSCase a var_e a $ who_is_this_ss <> after_ss
        let cfc_only = makeOnly who_e only_body
        locAt c_at $ return CompiledForkCase {..}
  (before_tc_ss, pay_e, tc_head_e, after_tc_ss) <-
    case cases of
      [ForkCase {..}] -> do
        (_, only_body) <-
          forkOnlyHelp fc_who fc_at fc_before msg_e when_e
        let tc_head_e = fc_who
        let before_tc_ss = [makeOnly fc_who only_body]
        let pay_e = JSCallExpression fc_pay a (JSLOne msg_e) a
        after_tc_ss <- getAfter (fc_at, fc_after)
        return $ (before_tc_ss, pay_e, tc_head_e, after_tc_ss)
      _ -> do
        casel <- mapM go $ groupBy forkCaseSameParticipant cases
        let cases_msg_type_def = concatMap cfc_msg_type_def casel
        let cases_data_def = map cfc_data_def casel
        let cases_parts = map cfc_part casel
        let cases_pay_props = map cfc_pay_prop casel
        let cases_req_props = map cfc_req_prop casel
        let cases_switch_cases = map cfc_switch_case casel
        let cases_onlys = map cfc_only casel
        let fd_def = JSCallExpression (jid "Data") a (JSLOne $ mkobj cases_data_def) a
        let data_decls = JSLOne $ JSVarInitExpression fd_e $ JSVarInit a fd_def
        let data_ss = [JSConstant a data_decls sp]
        let tc_head_e = JSCallExpression (jid "race") a (toJSCL cases_parts) a
        let pay_e = JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_pay_props) a
        let req_arg = JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_req_props) a
        let req_e = JSCallExpression (jid "require") a (JSLOne $ req_arg) a
        let req_ss = [JSExpressionStatement req_e sp]
        let switch_ss = [JSSwitch a a msg_e a a cases_switch_cases a sp]
        let before_tc_ss = cases_msg_type_def <> data_ss <> cases_onlys
        let after_tc_ss = req_ss <> switch_ss
        return $ (before_tc_ss, pay_e, tc_head_e, after_tc_ss)
  let tc_pub_e = JSCallExpression (JSMemberDot tc_head_e a (jid "publish")) a (JSLOne msg_e) a
  let tc_when_e = JSCallExpression (JSMemberDot tc_pub_e a (jid "when")) a (JSLOne when_e) a
  -- START: Non-network token pay
  pay_expr <-
    case mnntpay of
      Just (JSArrayLiteral aa ts _) -> do
        let network_pay_var = jid "networkTokenPay"
        let nnts = map (jse_expect_id at) $ jsa_flatten ts
        let nnts_js =
              map
                (\i ->
                   JSArrayLiteral aa [JSArrayElement (jid $ "amt" <> show i), JSArrayElement (jid $ "nntok" <> show i)] aa)
                [0 .. length nnts - 1]
        let nnts_ret =
              toList $
                mapWithIndex
                  (\i nnt ->
                     JSArrayLiteral
                       aa
                       [ JSArrayElement (jid $ "amt" <> show i)
                       , JSArrayElement (jid $ show $ pretty nnt)
                       ]
                       aa)
                  (Seq.fromList nnts)
        let verifyPaySpec =
              toList $
                mapWithIndex
                  (\i nnt ->
                     JSExpressionStatement
                       (JSCallExpression
                          (jid "assert")
                          aa
                          (toJSCL
                             [ JSExpressionBinary (jid ("nntok" <> show i)) (JSBinOpEq aa) (jid nnt)
                             , JSStringLiteral aa ("'Expected the non-network token at position " <> show (i + 1) <> " in `case` payment to be equal to " <> show (pretty nnt) <> " as specified in `.paySpec`'")
                             ])
                          aa)
                       sp)
                  (Seq.fromList nnts)
        let pay_var tl = JSArrayLiteral aa (intercalate [JSArrayComma aa] $ map ((: []) . JSArrayElement) $ network_pay_var : tl) aa
        let pay_ss = [JSConstant aa (JSLOne $ JSVarInitExpression (pay_var nnts_js) $ JSVarInit aa pay_e) sp] <> verifyPaySpec <> [JSReturn aa (Just (pay_var nnts_ret)) sp]
        let pay_call = jsCallThunk aa $ jsThunkStmts aa pay_ss
        return pay_call
      _ -> return pay_e
  let tc_pay_e = JSCallExpression (JSMemberDot tc_when_e a (jid "pay")) a (JSLOne pay_expr) a
  -- END: Non-network token pay
  let tc_time_e =
        case mtime of
          Nothing -> tc_pay_e
          Just (_, targs) ->
            JSCallExpression (JSMemberDot tc_pay_e a (jid "timeout")) a (toJSCL targs) a
  let tc_e = tc_time_e
  let tc_ss = [JSExpressionStatement tc_e sp]
  let exp_ss = before_tc_ss <> tc_ss <> after_tc_ss
  -- liftIO $ putStrLn $ "Fork Output"
  -- liftIO $ putStrLn $ show $ pretty exp_ss
  evalStmt $ exp_ss <> ks

doParallelReduce :: JSExpression -> SrcLoc -> Maybe ParallelReduceMode -> JSExpression -> Maybe JSExpression -> Maybe JSExpression -> [(SrcLoc, [JSExpression])] -> Maybe (ParallelReduceMode, SrcLoc, [JSExpression]) -> Maybe JSExpression -> Maybe JSExpression -> App [JSStatement]
doParallelReduce lhs pr_at pr_mode init_e pr_minv pr_mwhile pr_cases pr_mtime pr_mpay pr_mdef = locAt pr_at $ do
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
  let fork_e0 = jsCall a (jid "fork") []
  let makeContinue ss = do
        let def_e = jid $ prid "res"
        let call_og = jsCallThunk a (jsThunkStmts a [ss])
        let def_s = JSConstant a (JSLOne $ JSVarInitExpression def_e $ JSVarInit a call_og) sp
        let asn_s = JSAssignStatement lhs (JSAssign a) def_e sp
        let continue_s = JSContinue a JSIdentNone sp
        [def_s, asn_s, continue_s]
  let injectContinueIntoBody = \case
        JSExpressionParen _ e _ -> injectContinueIntoBody e
        JSArrowExpression args annt ss -> do
          return $ JSExpressionParen a (JSArrowExpression args annt $ JSStatementBlock annt (makeContinue ss) annt (a2sp annt)) a
        ow -> evalExpr ow >>= \v -> (expect_t (snd v) $ Err_Expected "function")
  fork_e1 <-
    case pr_mtime of
      Nothing -> return fork_e0
      Just (mode, t_at, args) ->
        case (mode, args) of
          (PRM_ThrowTimeout, [t_e]) ->
            callTimeout [t_e, jsThunkStmts ta [JSThrow ta lhs semi]]
          (PRM_TimeRemaining, [t_e]) -> do
            let dot o f = JSCallExpressionDot o ta f
            let publish = dot (jid "Anybody") $ jid "publish"
            let pubApp = jsCallThunk ta publish
            let bodyStmts =
                  JSStatementBlock
                    ta
                    [ JSExpressionStatement pubApp semi
                    , JSReturn ta (Just lhs) semi
                    ]
                    ta
                    sp
            callTimeout [t_e, jsThunkStmts ta $ makeContinue bodyStmts]
          (PRM_Timeout, t_d : t_fn : _) -> do
            locAt t_at $ do
              t_fn' <- injectContinueIntoBody t_fn
              callTimeout [t_d, t_fn']
          (PRM_Timeout, [t_e]) -> callTimeout [t_e]
          (PRM_Timeout, _) -> locAt t_at $ expect_ $ Err_ParallelReduceBranchArgs "timeout" 2 args
          (PRM_ThrowTimeout, _) -> locAt t_at $ expect_ $ Err_ParallelReduceBranchArgs "throwTimeout" 1 args
          (PRM_TimeRemaining, _) -> locAt t_at $ expect_ $ Err_ParallelReduceBranchArgs "timeRemaining" 1 args
          _ -> impossible "pr_mtime must be PRM_TimeRemaining or PRM_Timeout"
        where
          semi = JSSemiAuto
          timeOutId = jid "timeout"
          callTimeout = return . jsCall ta (JSMemberDot fork_e0 ta timeOutId)
          ta = ao t_at
  fork_e2 <-
    case pr_mpay of
      Nothing -> return fork_e1
      Just toks ->
        return $ jsCall a (JSMemberDot fork_e1 a $ jid "paySpec") [toks]
  let forkcase fork_eN (case_at, case_es) = do
        let aux ccomps cbody = locAt case_at $ do
              cbody' <- injectContinueIntoBody cbody
              return $ ccomps <> [cbody']
        let injectContinue cs = case reverse cs of
              [] -> return []
              cbody : rst -> aux (reverse rst) cbody
        cases' <- injectContinue case_es
        return $ JSCallExpression (JSMemberDot fork_eN ca (jid "case")) ca (toJSCL cases') ca
        where
          ca = ao case_at
  fork_e <- foldM forkcase fork_e2 pr_cases
  let fork_s = JSExpressionStatement fork_e sp
  let commit_s = JSMethodCall (jid "commit") a JSLNil a sp
  let while_body = [commit_s, fork_s]
  let while_s = JSWhile a a while_e a $ JSStatementBlock a while_body a sp
  block_s <- case pr_mdef of
                  Just (JSArrowExpression _ _ sb@JSStatementBlock {}) -> return sb
                  Just ow -> locAtf (srcloc_jsa "define" $ jsa ow) $ expect_ Err_ParallelReduce_DefineBlock
                  Nothing -> return $ JSStatementBlock a [] a sp
  let pr_ss = [var_s, block_s, inv_s, while_s]
  -- liftIO $ putStrLn $ "ParallelReduce"
  -- liftIO $ putStrLn $ show $ pretty pr_ss
  return $ pr_ss

jsArrow :: JSAnnot -> [JSExpression] -> JSStatement -> JSExpression
jsArrow a args s = JSArrowExpression args' a s
  where
    args' = JSParenthesizedArrowParameterList a (toJSCL args) a

jsArrowExpr :: JSAnnot -> [JSExpression] -> JSExpression -> JSExpression
jsArrowExpr a args e = jsArrow a args $ JSExpressionStatement e (a2sp a)

jsArrowStmts :: JSAnnot -> [JSExpression] -> [JSStatement] -> JSExpression
jsArrowStmts a args s = jsArrow a args $ JSStatementBlock a s a (a2sp a)

jsThunkStmts :: JSAnnot -> [JSStatement] -> JSExpression
jsThunkStmts a = jsArrowStmts a []

jsCallThunk :: JSAnnot -> JSExpression -> JSExpression
jsCallThunk a e = jsCall a e []

jsCall :: JSAnnot -> JSExpression -> [JSExpression] -> JSExpression
jsCall a f as = JSCallExpression f a (toJSCL as) a

evalStmtTrampoline :: JSSemi -> [JSStatement] -> SLVal -> App SLStmtRes
evalStmtTrampoline sp ks ev =
  case findStmtTrampoline ev of
    Just st -> st sp ks
    Nothing ->
      typeOf ev >>= \case
        (T_Null, _) -> evalStmt ks
        (ty, _) -> locAt (srclocOf ev) $ expect_ $ Err_Block_NotNull ty

findStmtTrampoline :: SLVal -> Maybe (JSSemi -> [JSStatement] -> App SLStmtRes)
findStmtTrampoline = \case
  SLV_Prim (SLPrim_part_setted at' who addr_da) -> Just $ \_ ks -> locAt at' $ do
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
        whodv <- ctxt_lift_expr (DLVar at' Nothing T_Address) (DLE_PartSet at' who addr_da)
        let pdvs' = M.insert who whodv pdvs
        let st' = st {st_pdvs = pdvs'}
        setSt st'
        evalStmt ks
  SLV_Prim SLPrim_exitted -> Just $ \sp ks -> do
    at <- withAt id
    expect_empty_tail "exit" (srcloc2annot at) sp ks
    sco <- e_sco <$> ask
    return $ SLStmtRes sco []
  SLV_Form (SLForm_EachAns parts only_at only_cloenv only_synarg) -> Just $ \_ ks -> do
    ensure_modes [SLM_Step, SLM_ConsensusStep] "local action (only or each)"
    sco <- e_sco <$> ask
    sco' <-
      foldM doOnly sco $
        map (\who -> (who, only_at, only_cloenv, only_synarg)) parts
    locSco sco' $ evalStmt ks
  SLV_Form (SLForm_Part_ToConsensus to_at whos vas Nothing mmsg mamt mwhen mtime) -> Just $ \_ ks -> locAt to_at $ do
    let msg = fromMaybe [] mmsg
    let amt = fromMaybe (JSDecimal JSNoAnnot "0") mamt
    let whene = fromMaybe (JSLiteral JSNoAnnot "true") mwhen
    doToConsensus ks whos vas msg amt whene mtime
  SLV_Form (SLForm_fork_partial f_at Nothing cases mtime mnntpay) -> Just $ \_ ks ->
    locAt f_at $ doFork ks cases mtime mnntpay
  SLV_Prim SLPrim_committed -> Just $ \_ ks -> do
    ensure_mode SLM_ConsensusStep "commit"
    sco <- e_sco <$> ask
    at <- withAt id
    st <- readSt id
    setSt $
      st
        { st_mode = SLM_Step
        , st_after_ctor = True
        }
    let sco' = sco_set "this" (SLSSVal at Public $ SLV_Kwd $ SLK_this) sco
    (steplifts, cr) <-
      captureLifts $
        locSco sco' $ evalStmt ks
    saveLift $ DLS_FromConsensus at steplifts
    return $ cr
  _ -> Nothing

doExit :: App ()
doExit = do
  ensure_modes [SLM_Step, SLM_AppInit] "exit"
  ensure_live "exit"
  at <- withAt id
  let zero = SLV_Int at 0
  let lab = "application exit"
  let one mtok = doBalanceAssert mtok zero PEQ $ "balance zero at " <> lab
  one Nothing
  mapM_ (one . Just . DLA_Var) =<< readSt st_toks
  let mintedEnsureDestroyed tokv = doBalanceAssert_ FV_destroyed (Just $ DLA_Var tokv) (SLV_Bool at True) PEQ $ "token destroyed at " <> lab
  mapM_ mintedEnsureDestroyed =<< readSt st_toks_c
  saveLift $ DLS_Stop at
  st <- readSt id
  setSt $ st {st_live = False}

doWhileLikeInitEval :: JSExpression -> JSExpression -> App (M.Map SLVar DLVar, DLAssignment, SLScope)
doWhileLikeInitEval lhs rhs = do
  vars_env <- unchangedSt $ evalDecl True lhs rhs
  let help v (SLSSVal at _ val) = do
        (t, da) <- typeOf val
        dv <- ctxt_mkvar $ DLVar at (Just (at, v)) t
        return $ (dv, da)
  helpm <- M.traverseWithKey help vars_env
  let unknown_var_env = M.map (\(dv, _) -> SLSSVal (srclocOf dv) Public (SLV_DLVar dv)) helpm
  sco_env' <- sco_update unknown_var_env
  let init_daem = M.fromList $ M.elems helpm
  let init_vars = M.map fst helpm
  init_dam <- compileArgExprMap init_daem
  let init_dl = DLAssignment init_dam
  return $ (init_vars, init_dl, sco_env')

doWhileLikeContinueEval :: JSExpression -> M.Map SLVar DLVar -> SLSVal -> App ()
doWhileLikeContinueEval lhs whilem (rhs_lvl, rhs_v) = do
  at <- withAt id
  decl_env <- evalDeclLHS False Nothing rhs_lvl mempty rhs_v lhs
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
        dae <- typeCheck_d et val
        return $ (dv, dae)
  cont_daem <- M.fromList <$> (mapM f $ M.toList whilem)
  cont_dam' <- compileArgExprMap cont_daem
  let cont_das = DLAssignment cont_dam'
  saveLift $ DLS_Continue at cont_das

evalModeToBlock :: SLMode -> DLType -> App (a, SLVal) -> App DLSBlock
evalModeToBlock mode rest e = do
  at <- withAt id
  st <- readSt id
  let pure_st = st {st_mode = mode}
  fs <- asks e_stack
  (e_lifts, e_da) <-
    captureLifts $
      locSt pure_st $
        compileCheckType rest . snd =<< e
  return $ DLSBlock at fs e_lifts e_da

evalExportClosureToBlock :: SLClo -> [SLSVal] -> Maybe SLTypeFun -> DLType -> App DLSBlock
evalExportClosureToBlock clo sargs mtf rest = do
  at <- withAt id
  let sv = SLV_Clo at mtf clo
  evalModeToBlock SLM_Module rest $ do
    SLAppRes _ val <- evalApplyValsAux True sv sargs
    return val

evalPureExprToBlock :: JSExpression -> DLType -> App DLSBlock
evalPureExprToBlock e rest = do
  evalModeToBlock SLM_ConsensusPure rest $ evalExpr e

evalLValue :: JSExpression -> App SLLValue
evalLValue = \case
  JSMemberSquare ce a fe _ -> locAtf (srcloc_jsa "ref" a) $ do
    at <- withAt id
    cv <- snd <$> evalExpr ce
    fv <- snd <$> evalExpr fe
    case cv of
      SLV_Map mv -> do
        fa <- compileCheckType T_Address fv
        return $ SLLV_MapRef at mv fa
      _ ->
        expect_t cv $ Err_Eval_RefNotRefable
  e -> expect_ $ Err_LValue_IllegalJS e

evalAssign :: SLSVal -> SLLValue -> App ()
evalAssign rhs = \case
  SLLV_MapRef _at mv mc -> do
    ensure_mode SLM_ConsensusStep "Map.set"
    mapSet mv mc =<< ensure_public rhs

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
        -- Rather than erroring now, we error later, because there might be an
        -- an exit or continue prior to this, so the linearizer will remove
        -- this code
        fs <- e_stack <$> ask
        saveLift $ DLS_Unreachable at fs $ "Scope requires explicit return, but none given; typically this is a branch of a `while` body without a `continue` or `exit`"
        ret []
      RS_MayBeEmpty -> ret []
  ((JSStatementBlock a ss' _ sp) : ks) -> do
    br <- locAtf (srcloc_jsa "block" a) $ evalStmt ss'
    locAtf (srcloc_after_semi "block" a sp) $ retSeqn br ks
  (s@(JSBreak a _ _) : _) -> illegal a s "break"
  (s@(JSLet a _ _) : _) -> illegal a s "let"
  (s@(JSClass a _ _ _ _ _ _) : _) -> illegal a s "class"
  (JSExpressionStatement (JSStringLiteral a hs) sp) : ks
    | trimQuotes hs == "use strict" ->
      locAtf (srcloc_after_semi "use strict" a sp) $
        locUseStrict True $ evalStmt ks
  ((JSConstant a decls sp) : ks) -> do
    let lab = "const"
    locAtf (srcloc_jsa lab a) $ do
      (lhs, rhs) <- destructDecls decls
      (rhs_lvl, rhs_v) <- evalExpr rhs
      case rhs_v of
        SLV_Form (SLForm_parallel_reduce_partial {..}) -> do
          pr_ss <- doParallelReduce lhs slpr_at slpr_mode slpr_init slpr_minv slpr_mwhile slpr_cases slpr_mtime slpr_mpay slpr_mdef
          evalStmt (pr_ss <> ks)
        _ -> do
          addl_env <- evalDeclLHS True Nothing rhs_lvl mempty rhs_v lhs
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
          om <- readSt st_mode
          saveLift =<< (checkCond om $ DLS_If at (DLA_Var cond_dv) sa tlifts flifts)
          let levelHelp = map (\(r_at, rmi, (r_lvl, r_v), _) -> (r_at, rmi, (clvl <> r_lvl, r_v), True))
          let trets' = levelHelp trets
          let frets' = levelHelp frets
          rets' <- brCombineRets trets' frets'
          setSt st_t
          mergeSt st_f
          brSeqn (SLStmtRes sco rets') ks_ne
        _ -> do
          (n_at', ns, os) <- case cv of
            SLV_Bool _ False -> return (f_at', fs, ts)
            SLV_Bool _ True -> return (t_at', ts, fs)
            _ -> do
              useStrict >>= \case
                True -> expect_ $ Err_Strict_Conditional cv
                False -> return (t_at', ts, fs)
          whenUsingStrict $ ignoreAll $ evalStmt [os]
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
  ((JSAssignStatement lhs op rhs asp) : ks) ->
    case (op, ks) of
      ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) -> do
        let lab = "continue"
        ensure_mode SLM_ConsensusStep lab
        let var_at = srcloc_jsa lab var_a
        rhs_sv <- locAtf var_at $ evalExpr rhs
        sco <- e_sco <$> ask
        locAtf var_at $ do
          whilem <-
            case sco_while_vars sco of
              Nothing -> locAtf (srcloc_jsa lab cont_a) $ expect_ $ Err_Eval_ContinueNotInWhile
              Just x -> return $ x
          doWhileLikeContinueEval lhs whilem rhs_sv
        -- NOTE We could/should look at sco_must_ret and see if it is
        -- RS_MayBeEmpty which means that the outside scope has an empty
        -- tail?
        expect_empty_tail lab cont_a cont_sp cont_ks
        return $ SLStmtRes sco []
      ((JSAssign var_a), _) -> do
        let lab = "assign"
        let var_at = srcloc_jsa lab var_a
        lhs' <- locAtf var_at $ evalLValue lhs
        rhs' <- evalExpr rhs
        locAtf var_at $ evalAssign rhs' lhs'
        locAtf (srcloc_after_semi lab var_a asp) $ evalStmt ks
      (jsop, _) ->
        locAtf (srcloc_jsa "assign" $ jsa op) $
          expect_ $ Err_Block_Assign jsop ks
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
      case findStmtTrampoline $ snd sev of
        Just st ->
          st sp $ JSReturn a Nothing sp : ks
        Nothing -> do
          ret <- case sco_ret sco of
            Just x ->
              case sco_must_ret sco of
                RS_CannotReturn -> expect_ $ Err_CannotReturn
                _ -> return $ x
            Nothing -> expect_ $ Err_Eval_NoReturn
          (mt, da) <- typeOfM (snd sev) >>= \case
            Just (t, dae) -> (,) (Just t) <$> compileArgExpr dae
            Nothing -> return $ (Nothing, DLA_Literal $ DLL_Null)
          saveLift $ DLS_Return at' ret da
          expect_empty_tail lab a sp ks
          return $ SLStmtRes sco [(at', mt, sev, False)]
  (JSSwitch a _ de _ _ cases _ sp : ks) -> do
    locAtf (srcloc_jsa "switch" a) $ do
      at' <- withAt id
      om <- readSt st_mode
      let de_v = jse_expect_id at' de
      (de_lvl, de_val) <- evalId "switch" de_v
      (de_ty, _) <- typeOf de_val
      varm <- mustBeDataTy Err_Switch_NotData de_ty
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
                locAt at1 $ expect_ $ Err_Switch_DoubleCase at0 at1 (Just k)
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
                  locAt at_c' $ expect_ $ Err_Switch_DoubleCase at_c at_c' Nothing
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
                      dv' <- ctxt_mkvar $ DLVar at_c (Just (at_c, de_v)) vt
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
                          rets'' <- brCombineRets rets' case_rets'
                          return $ (Just st'', rets'')
                        _ -> impossible $ "switch"
                    let casemm'' = M.insert vn (mdv', case_lifts) casemm'
                    return $ (mst'', sa'', Just rets'', casemm'')
            (mst', sa', mrets', casemm') <-
              foldM cmb (Nothing, mempty, Nothing, mempty) $ M.toList casemm
            let rets' = maybe mempty id mrets'
            maybe (return ()) setSt mst'
            saveLift =<< checkCond om
              =<< withAt (\at -> DLS_Switch at dv sa' casemm')
            return $ SLStmtRes sco rets'
      fr <-
        case de_val of
          SLV_Data _ t vn vv -> do
            let (at_c, shouldBind, body) = (casesm M.! vn)
            let mvv = if shouldBind then Just vv else Nothing
            whenUsingStrict $ ignoreAll $ select_all (SLV_DLVar $ DLVar srcloc_builtin Nothing (T_Data t) 0)
            select at_c body mvv
          SLV_DLVar {} -> select_all de_val
          _ -> impossible "switch mvar"
      locAtf (srcloc_after_semi "switch" a sp) $ brSeqn fr ks_ne
  ((JSThrow a e sp) : ks) -> do
    let lab = "throw"
    let throwAtf = srcloc_jsa lab a
    (dv_ty, dv) <- locAtf throwAtf (evalExpr e) >>= compileTypeOf . snd
    curSt <- readSt id
    exn_ref <- asks e_exn
    exn <- liftIO $ readIORef exn_ref
    -- Ensure we are throwing inside of a `try` block
    unless (e_exn_in_throw exn) $
      locAtf throwAtf $
        expect_ Err_Throw_No_Catch
    -- Set or ensure the type that this `try` block is expected to catch
    case e_exn_ty exn of
      Nothing ->
        liftIO $ modifyIORef exn_ref (\ex -> ex {e_exn_ty = Just dv_ty})
      Just ty ->
        case ty == dv_ty of
          True ->
            return ()
          False ->
            locAtf throwAtf $ expect_ $ Err_Try_Type_Mismatch ty dv_ty
    saveLift =<< withAt (\at -> DLS_Throw at dv $ not $ isConsensusStep $ e_exn_mode exn)
    liftIO $ modifyIORef exn_ref (\ex -> ex {e_exn_st = Just curSt})
    forM_ (e_exn_st exn) mergeSt
    st <- asks e_st
    liftIO $ modifyIORef st (\s -> s {st_live = False})
    sco <- asks e_sco
    expect_empty_tail lab a sp ks
    return $ SLStmtRes sco []
  ((JSTry try_a (JSBlock _ stmts _) [JSCatch _ _ ce _ (JSBlock _ handler _)] _) : ks) -> do
    locAtf (srcloc_jsa "try" try_a) $ do
      at <- withAt id
      mode <- readSt st_mode
      -- Create fresh exception environment
      exn_env_ref <-
        liftIO $
          newIORef $
            ExnEnv
              { e_exn_in_throw = True
              , e_exn_ty = Nothing
              , e_exn_st = Nothing
              , e_exn_mode = mode
              }
      let locTry = local (\e -> e {e_exn = exn_env_ref})
      -- Process try block
      SLRes try_stmts try_st try_ret <- captureRes $ locTry $ evalStmt stmts
      exn_env <- liftIO $ readIORef exn_env_ref
      case exn_env of
        -- Get the type of thrown/caught expression & the state during throw
        ExnEnv {e_exn_ty = Just arg_ty, e_exn_st = Just thrown_st} -> do
          -- Bind `catch` argument before evaluating handler
          sco <- asks e_sco
          handler_arg <- ctxt_mkvar (DLVar at Nothing arg_ty)
          handler_env <- evalDeclLHS True Nothing Public mempty (SLV_DLVar handler_arg) ce
          sco' <- sco_update handler_env
          -- Eval handler
          SLRes handler_stmts handler_st (SLStmtRes _ handler_res) <-
            locSt thrown_st $
              locSco sco' $
                captureRes $
                  evalStmt handler
          saveLift $ DLS_Try at try_stmts handler_arg handler_stmts
          -- Coalesce the try and handler returns
          let levelHelp = SLStmtRes sco . map (\(ra, rb, rc, _) -> (ra, rb, rc, True))
          let SLStmtRes _ try_res = try_ret
          ret <- combineStmtRes True Public (levelHelp handler_res) try_st (levelHelp try_res)
          -- Ensure the successful try block and catch block end with compatible states
          setSt try_st
          mergeSt handler_st
          retSeqn ret ks
        -- If there were no `throw`s in try block, ignore handler
        _ -> do
          saveLifts try_stmts
          setSt try_st
          retSeqn try_ret ks
  (s@(JSTry a _ _ _) : _) -> illegal a s "try"
  ((JSVariable var_a while_decls vsp) : var_ks) -> do
    locAtf (srcloc_jsa "var" var_a) $ do
      let var_ks' =
            case var_ks of
              (JSStatementBlock {}) : _ -> var_ks
              _ -> JSStatementBlock var_a [] var_a vsp : var_ks
      case var_ks' of
        ( (JSStatementBlock _ blk_ss _ _)
            -- XXX This is a bad macro, it duplicates blk_ss in the while and
            -- after, rather than expanding/evaluating once and incorporating
            -- things, which might be possible
            : (JSMethodCall (JSIdentifier inv_a "invariant") _ (JSLOne invariant_e) _ _isp)
            : (JSWhile while_a cond_a while_cond _ while_body)
            : ks
          ) -> locAtf (srcloc_jsa "while" while_a) $ do
            ensure_mode SLM_ConsensusStep "while"
            (while_lhs, while_rhs) <- destructDecls while_decls
            (init_vars, init_dl, sco_env') <- doWhileLikeInitEval while_lhs while_rhs
            let add_preamble a e = jsCallThunk a $ jsThunkStmts a $ blk_ss <> [JSReturn a (Just e) (a2sp a)]
            inv_b <-
              locAtf (srcloc_jsa "invariant" inv_a) $
                locSco sco_env' $
                  locWhileInvariant $
                    evalPureExprToBlock (add_preamble inv_a invariant_e) T_Bool
            cond_b <-
              locAtf (srcloc_jsa "cond" cond_a) $
                locSco sco_env' $ evalPureExprToBlock (add_preamble cond_a while_cond) T_Bool
            let while_sco =
                  sco_env'
                    { sco_while_vars = Just init_vars
                    , sco_must_ret = RS_NeedExplicit
                    }
            (body_lifts, (SLStmtRes _ body_rets)) <-
              unchangedSt $
                locSco while_sco $
                  captureLifts $ evalStmt $ blk_ss <> [while_body]
            saveLift
              =<< withAt
                (\at ->
                   DLS_While at init_dl inv_b cond_b body_lifts)
            SLStmtRes k_sco' k_rets <- locSco sco_env' $ evalStmt $ blk_ss <> ks
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

combineStmtRes :: Bool -> SecurityLevel -> SLStmtRes -> SLState -> SLStmtRes -> App SLStmtRes
combineStmtRes addNull lvl (SLStmtRes _ lrets) rst (SLStmtRes sco rrets) = do
  let ret = return . SLStmtRes sco
  at <- withAt id
  lst <- readSt id
  let yes lab st =
        case st_live st of
          True -> [(at, Just T_Null, (lvl, SLV_Null at $ "empty " <> lab), False)]
          False -> []
  let no _ _ = []
  let mnull = if addNull then yes else no
  case (lrets, rrets) of
    ([], []) -> ret []
    ([], _) -> ret $ mnull "left" lst <> rrets
    (_, []) -> ret $ lrets <> mnull "right" rst
    (_, _) -> ret $ lrets <> rrets

brSeqn :: SLStmtRes -> [JSStatement] -> App SLStmtRes
brSeqn sr@(SLStmtRes sco rets) ks = do
  case dropEmptyJSStmts ks of
    [] -> return $ sr
    ks' ->
      case rets of
        [] -> locSco sco $ evalStmt ks'
        _ -> expect_ $ Err_Return_MustBeTail

brCombineRets :: SLStmtRets -> SLStmtRets -> App SLStmtRets
brCombineRets lrets rrets =
  case (lrets, rrets) of
    ([], []) -> return $ []
    ((_ : _), (_ : _)) -> return $ lrets <> rrets
    _ -> expect_ $ Err_Return_BothSidesMust

expect_empty_tail :: String -> JSAnnot -> JSSemi -> [JSStatement] -> App ()
expect_empty_tail lab a sp = \case
  [] -> return ()
  ks -> locAtf (srcloc_after_semi lab a sp) $ expect_ $ Err_TailNotEmpty ks

-- Maps

data MapEnv = MapEnv
  { me_id :: Counter
  , me_ms :: IORef (M.Map DLMVar DLMapInfo)
  }

ignoreAll :: App a -> App ()
ignoreAll e =
  void $ captureRes $ locMap e

mapLookup :: DLMVar -> App DLMapInfo
mapLookup mv = do
  MapEnv {..} <- e_mape <$> ask
  msm <- liftIO $ readIORef me_ms
  case M.lookup mv msm of
    Just x -> return x
    Nothing -> impossible $ "mapLookup on unknown map"

mapNew :: DLType -> App DLMVar
mapNew dlmi_ty = do
  dlmi_at <- withAt id
  MapEnv {..} <- e_mape <$> ask
  mc <- DLMVar <$> (liftIO $ incCounter me_id)
  let mci = DLMapInfo {..}
  liftIO $ modifyIORef me_ms $ M.insert mc mci
  return $ mc

mapDel :: DLMVar -> DLArg -> App ()
mapDel mv mc = do
  at <- withAt id
  ctxt_lift_eff $ DLE_MapSet at mv mc Nothing

mapSet :: DLMVar -> DLArg -> SLVal -> App ()
mapSet mv mc nv = do
  at <- withAt id
  DLMapInfo {..} <- mapLookup mv
  na <- compileCheckType dlmi_ty nv
  ctxt_lift_eff $ DLE_MapSet at mv mc $ Just na

mapRef :: DLMVar -> SLVal -> App DLVar
mapRef mv mcv = do
  at <- withAt id
  mi <- mapLookup mv
  mc <- compileCheckType T_Address mcv
  let mt = dlmi_tym mi
  let mkvar = DLVar at Nothing mt
  ctxt_lift_expr mkvar $ DLE_MapRef at mv mc
