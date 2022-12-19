{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Eval.Core where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Data.Bits
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.Either
import Data.Foldable
import Data.Functor ((<&>))
import Data.IORef
import Data.List (groupBy, intercalate, intersperse, transpose, unzip6, (\\), zip4)
import qualified Data.List as L
import Data.List.Extra (mconcatMap, splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple.Extra (fst3, snd3, thd3)
import qualified Data.Vector as Vector
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase hiding (typeOf)
import Reach.AST.SL
import Reach.Connector
import Reach.Counter
import Reach.Eval.Error
import Reach.Eval.Types
import Reach.JSUtil
import Reach.Pandemic
import Reach.Parser
import Reach.Texty (pretty)
import Reach.Util
import Reach.Warning
import Safe (atMay, maximumMay, readMay)
import System.Directory
import System.FilePath
import Text.ParserCombinators.Parsec.Number (numberValue)
import Text.RE.TDFA (RE, compileRegex, matched, (?=~))
import Numeric (showFFloat, fromRat)
import qualified Data.ByteString.Base16 as B16

--- New Types

type CompileProg = SLVal -> App ConnectorObject

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
  , ae_apis :: S.Set SLPart
  }

data AppRes = AppRes
  { ar_pie :: M.Map SLPart InteractEnv
  , ar_isAPI :: S.Set SLPart
  , ar_views :: DLViews
  , ar_apis :: DLAPIs
  , ar_events :: DLEvents
  , -- All the bound Participants, Views, APIs
    ar_entities :: M.Map String SrcLoc
  , ar_api_alias :: M.Map SLVar (Maybe B.ByteString, [SLType])
  , ar_view_alias :: M.Map SLVar [B.ByteString]
  }

data AppInitSt
  = AIS_Init
      { aisi_env :: IORef AppEnv
      , aisi_res :: IORef AppRes
      }
  | AIS_Inited
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
  , e_vars_tracked :: IORef (S.Set (SrcLoc, SLVar))
  , e_vars_used :: IORef (S.Set (SrcLoc, SLVar))
  , e_while_invariant :: Bool
  , e_exn :: IORef ExnEnv
  , e_appr :: Either DLOpts (IORef AppInitSt)
  , e_droppedAsserts :: Counter
  , e_infections :: IORef Infections
  , e_compileProg :: CompileProg
  , e_universe :: Int
  }

instance HasCounter Env where
  getCounter = e_id

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
    Just (AIS_Inited d) -> return $ Just d
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

locUseUnstrict :: Bool -> App a -> App a
locUseUnstrict v m = do
  old_sco <- asks e_sco
  local (\e -> e {e_sco = old_sco {sco_use_unstrict = v}}) m

msdef :: MSrcLoc
msdef = Just $ SrcLoc (Just "definition") Nothing Nothing

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
        AIS_Inited d -> return $ f (ae_dlo d)

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

readUniverse :: App Int
readUniverse = asks e_universe

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
  , -- Deliberately ignore strict mode when evaluating code
    -- declared in strict mode
    sco_use_unstrict :: Bool
  } deriving Show

data EnvInsertMode
  = AllowShadowing
  | DisallowShadowing
  | AllowShadowingSome (S.Set SLVar)
  | AllowShadowingRace (S.Set SLPart) (S.Set SLVar)

data SLRes a = SLRes DLStmts SLState a

type SLLibs = (M.Map ReachSource SLEnv)

type SLMod = (ReachSource, [JSModuleItem])

mkDefaultApp :: IO SLVal
mkDefaultApp = do
  conR <- newIORef mempty
  return $ SLV_Prim $ SLPrim_App_Delay sb mt_sb (mempty, False) conR Nothing
  where mt_sb = JSStatementBlock JSNoAnnot [] JSNoAnnot JSSemiAuto

app_default_opts :: Counter -> Counter -> Connectors -> DLOpts
app_default_opts idxr dar cns =
  DLOpts
    { dlo_verifyArithmetic = False
    , dlo_verifyPerConnector = False
    , dlo_autoTrackPublishedTokens = True
    , dlo_connectors = cns
    , dlo_counter = idxr
    , dlo_bals = 1
    , dlo_droppedAsserts = dar
    , dlo_aem = defaultALGOExitMode
    }

app_options :: M.Map SLVar (SrcLoc -> DLOpts -> SLVal -> App (Either String DLOpts))
app_options =
  M.fromList
    [ ("verifyArithmetic", bland $ opt_bool (\opts b -> opts {dlo_verifyArithmetic = b}))
    , ("untrustworthyMaps", opt_deprecated D_UntrustworthyMaps)
    , ("verifyPerConnector", bland $ opt_bool (\opts b -> opts {dlo_verifyPerConnector = b}))
    , ("autoTrackPublishedTokens", bland $ opt_bool (\opts b -> opts {dlo_autoTrackPublishedTokens = b}))
    , ("connectors", bland $ opt_connectors)
    , ("ALGOExitMode", bland $ opt_read $ \opts v -> opts { dlo_aem = v })
    ]
  where
    bland f _at opts v = return $ f opts v
    opt_deprecated msg at opts _v = do
      liftIO $ emitWarning (Just at) $ W_Deprecated msg
      return $ Right opts
    opt_read :: forall a . (Show a, Read a, Enum a) => (DLOpts -> a -> DLOpts) -> DLOpts -> SLVal -> Either String DLOpts
    opt_read f opts v =
      case v of
        SLV_Bytes _ bs -> g $ bunpack bs
        SLV_BytesDyn _ bs -> g $ bunpack bs
        SLV_String _ bs -> g $ T.unpack bs
        _ -> Left $ "expected bytes or string"
      where
        g bs =
          case readMay bs of
            Just x -> Right $ f opts x
            Nothing -> Left $ "invalid option; valid are: " <> show vs
        vs :: [a]
        vs = enumFrom (toEnum 0)
    opt_bool f opts v =
      case v of
        SLV_Bool _ b -> Right $ f opts b
        _ -> Left $ "expected boolean"
    opt_connectors opts v =
      case v of
        SLV_Tuple _ vs ->
          case traverse f vs of
            Left x -> Left x
            Right y -> Right $ opts {dlo_connectors = M.filterWithKey g $ dlo_connectors opts }
              where
                g x _ = x `elem` y
          where
            f = \case
              SLV_Connector cn -> Right $ cn
              _ -> Left $ "expected connector"
        _ -> Left $ "expected tuple"

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

verifyName :: SrcLoc -> String -> [String] -> String -> App ()
verifyName at ty keys ns = do
  when (isSpecialBackendIdent ns) $
    expect_ $ Err_InvalidNameExport ty ns
  regex <- nameRegex
  unless (matched $ ns ?=~ regex) $
    expect_ $ Err_InvalidNameRegex ty ns
  usedNames <- ar_entities <$> aisiGet aisi_res
  let add n =
        case M.lookup n usedNames of
          Nothing ->
            aisiPut aisi_res $ \ar ->
              ar {ar_entities = M.insert n at (ar_entities ar)}
          Just bat -> expect_ $ Err_DuplicateName n bat
  add ns
  -- Add tagged view fields which can clash with untagged
  mapM_ (add . (<>) (ns <> "_")) keys

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
      (SLSSVal _ Public _ _)
        | not (isSpecialIdent k) && isSecretIdent k ->
          expect_ $ Err_Eval_NotPublicIdent k
      (SLSSVal _ _ _ ev) -> do
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
useStrict = do
  usingStrict <- asks (sco_use_strict . e_sco)
  usingUnstrict <- asks (sco_use_unstrict . e_sco)
  return $ usingStrict && not usingUnstrict

whenUsingStrict :: App () -> App ()
whenUsingStrict e = useStrict >>= flip when e

shouldNotTrackVariable :: (SrcLoc, SLVar) -> Bool
shouldNotTrackVariable = \case
  (_, "main") -> True
  (_, "_") -> True
  _ -> False

envSetInsert :: (Ord a) => (Env -> IORef (S.Set a)) -> a -> App ()
envSetInsert f v = do
  sr <- asks f
  liftIO $ modifyIORef sr $ S.insert v

trackVariable :: (SrcLoc, SLVar) -> App ()
trackVariable el =
  whenUsingStrict $
    unless (shouldNotTrackVariable el) $
      envSetInsert e_vars_tracked el

markVarUsed :: (SrcLoc, SLVar) -> App ()
markVarUsed = envSetInsert e_vars_used

env_lookup_ :: LookupCtx -> SLVar -> (a -> Bool) -> M.Map SLVar a -> App a
env_lookup_ _ "_" _ _ = expect_ $ Err_Eval_LookupUnderscore
env_lookup_ ctx x hide env =
  case M.lookup x env of
    Just v -> return v
    Nothing -> do
      expect_ $ Err_Eval_UnboundId ctx x $ M.keys $ M.filter (not . hide) env

isKwd :: SLSSVal -> Bool
isKwd = \case
  SLSSVal _ _ _ (SLV_Kwd _) -> True
  _ -> False

-- | The "_" ident may never be looked up.
env_lookup :: LookupCtx -> SLVar -> SLEnv -> App SLSSVal
env_lookup ctx x env = do
  sv <- env_lookup_ ctx x isKwd env
  uni <- readUniverse
  markVarUsed (sss_at sv, x)
  case sv of
    SLSSVal {sss_val = SLV_Deprecated d v} -> do
      at <- withAt id
      liftIO $ emitWarning (Just at) $ W_Deprecated d
      return $ sv {sss_val = v}
    SLSSVal { sss_universe, sss_val }
      | sss_universe /= uni && isDynamic sss_val -> do
        expect_ $ Err_Invalid_Universe x
    v -> return $ v

m_fromList_public_builtin :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public_builtin = m_fromList_public sb 0

base_env_slvals :: [(String, SLVal)]
base_env_slvals =
  [ ("makeEnum", SLV_Prim SLPrim_makeEnum)
  , ("declassify", SLV_Prim SLPrim_declassify)
  , ("commit", SLV_Prim SLPrim_commit)
  , ("digest", SLV_Prim SLPrim_digest)
  , ("transfer", SLV_Prim SLPrim_transfer)
  , ("assert", SLV_Prim $ SLPrim_claim CT_Assert)
  , ("assume", SLV_Prim $ SLPrim_claim $ CT_Assume)
  , ("enforce", SLV_Prim $ SLPrim_claim $ CT_Enforce)
  , ("require", SLV_Prim $ SLPrim_claim CT_Require)
  , ("possible", SLV_Prim $ SLPrim_claim CT_Possible)
  , ("currentMode", SLV_Prim SLPrim_currentMode)
  , ("unknowable", SLV_Form $ SLForm_unknowable)
  , ("balance", SLV_Prim $ SLPrim_balance)
  , ("lastConsensusTime", SLV_Prim $ SLPrim_fluid_read_canWait FV_lastConsensusTime)
  , ("thisConsensusTime", SLV_Prim $ SLPrim_fluid_read_didPublish FV_thisConsensusTime)
  , ("baseWaitTime", SLV_Prim $ SLPrim_fluid_read_canWait FV_baseWaitTime)
  , ("lastConsensusSecs", SLV_Prim $ SLPrim_fluid_read_canWait FV_lastConsensusSecs)
  , ("thisConsensusSecs", SLV_Prim $ SLPrim_fluid_read_didPublish FV_thisConsensusSecs)
  , ("baseWaitSecs", SLV_Prim $ SLPrim_fluid_read_canWait FV_baseWaitSecs)
  , ("didPublish", SLV_Prim $ SLPrim_didPublish)
  , ("Digest", SLV_Type ST_Digest)
  , ("Null", SLV_Type ST_Null)
  , ("Bool", SLV_Type ST_Bool)
  , ("UInt", SLV_Type $ ST_UInt UI_Word)
  , ("UInt256", SLV_Type $ ST_UInt UI_256)
  , ("Bytes", SLV_Prim SLPrim_Bytes)
  , ("BytesDyn", SLV_Type $ ST_BytesDyn)
  , ("StringDyn", SLV_Type $ ST_StringDyn)
  , ("Contract", SLV_Type ST_Contract)
  , ("ContractCode", SLV_Prim $ SLPrim_ContractCode)
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
  , ("intEq", SLV_Prim $ SLPrim_op S_PEQ)
  , ("polyEq", SLV_Prim $ SLPrim_op S_PEQ)
  , ("polyNeq", SLV_Prim $ SLPrim_polyNeq)
  , ("xor", SLV_Prim $ SLPrim_xor)
  , ("polyMod", SLV_Prim $ SLPrim_mod)
  , ("digestEq", SLV_Prim $ SLPrim_op S_DIGEST_EQ)
  , ("addressEq", SLV_Prim $ SLPrim_op S_ADDRESS_EQ)
  , ("sqrt", SLV_Prim $ SLPrim_op S_SQRT)
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
  , ("API", SLV_Prim SLPrim_API)
  , ("Events", SLV_Prim SLPrim_Event)
  , ("init", SLV_Prim SLPrim_init)
  , ("deploy", SLV_Deprecated (D_Replaced "deploy" "init") $ SLV_Prim SLPrim_init)
  , ("setOptions", SLV_Prim SLPrim_setOptions)
  , (".adaptReachAppTupleArgs", SLV_Prim SLPrim_adaptReachAppTupleArgs)
  , ("muldiv", SLV_Prim $ SLPrim_op $ S_MUL_DIV Nothing)
  , ("safeMuldiv", SLV_Prim $ SLPrim_op $ S_MUL_DIV $ Just PV_Safe)
  , ("veriMuldiv", SLV_Prim $ SLPrim_op $ S_MUL_DIV $ Just PV_Veri)
  , ("verifyMuldiv", SLV_Prim $ SLPrim_verifyMuldiv)
  , ("unstrict", SLV_Prim $ SLPrim_unstrict)
  , ("getContract", SLV_Prim $ SLPrim_getContract)
  , ("getAddress", SLV_Prim $ SLPrim_getAddress)
  , ("getCompanion", SLV_Prim $ SLPrim_getCompanion)
  , (".emitLog", SLV_Prim $ SLPrim_EmitLog)
  , ("call", SLV_Form $ SLForm_apiCall)
  , (".setApiDetails", SLV_Form $ SLForm_setApiDetails)
  , ("getUntrackedFunds", SLV_Prim $ SLPrim_getUntrackedFunds)
  , ("isDataVariant", SLV_Prim $ SLPrim_isDataVariant)
  , ("fromSome", SLV_Prim $ SLPrim_fromSome)
  , ("distinct", SLV_Prim $ SLPrim_distinct)
  , ("safeAdd", SLV_Prim $ SLPrim_op (S_ADD $ Just PV_Safe))
  , ("safeSub", SLV_Prim $ SLPrim_op (S_SUB $ Just PV_Safe))
  , ("safeMul", SLV_Prim $ SLPrim_op (S_MUL $ Just PV_Safe))
  , ("safeDiv", SLV_Prim $ SLPrim_op (S_DIV $ Just PV_Safe))
  , ("safeMod", SLV_Prim $ SLPrim_op (S_MOD $ Just PV_Safe))
  , ("veriAdd", SLV_Prim $ SLPrim_op (S_ADD $ Just PV_Veri))
  , ("veriSub", SLV_Prim $ SLPrim_op (S_SUB $ Just PV_Veri))
  , ("veriMul", SLV_Prim $ SLPrim_op (S_MUL $ Just PV_Veri))
  , ("veriDiv", SLV_Prim $ SLPrim_op (S_DIV $ Just PV_Veri))
  , ("veriMod", SLV_Prim $ SLPrim_op (S_MOD $ Just PV_Veri))
  , ( "Reach"
    , (SLV_Object sb (Just $ "Reach") $
         m_fromList_public_builtin
           [("App", SLV_Form SLForm_App)])
    )
  ]
  -- Add language keywords to env to prevent variables from using names.
  <> map (\t -> (show t, SLV_Kwd t)) allKeywords

base_env :: SLEnv
base_env = m_fromList_public_builtin base_env_slvals

jsClo_ :: HasCallStack => SrcLoc -> String -> String -> SLEnv -> SLVal
jsClo_ at name js env = SLV_Clo at Nothing $ SLClo (Just name) args body cloenv
  where
    -- Since we're generating closure, don't fuss about unused vars
    cloenv = SLCloEnv mempty env False
    (args, body) =
      case rjsa (srcloc2annot at) (readJsExpr js) of
        JSArrowExpression aformals _ bodys -> (a_, b_)
          where
            b_ = jsArrowBodyToRetBlock bodys
            a_ = parseJSArrowFormals at aformals
        _ -> impossible "not arrow"

jsClo :: HasCallStack => SrcLoc -> Universe -> String -> String -> (M.Map SLVar SLVal) -> SLVal
jsClo at uni name js env_ = jsClo_ at name js $ M.map (SLSSVal at Public uni) env_

jsCloExtendEnv :: HasCallStack => SrcLoc -> String -> String -> (M.Map SLVar SLVal) -> App SLVal
jsCloExtendEnv at name js env = do
  curEnv <- (sco_cenv . e_sco) <$> ask
  uni <- readUniverse
  let env' = M.map (SLSSVal at Public uni) env
  return $ jsClo_ at name js (M.union env' curEnv)

typeEqb :: DLType -> DLType -> Bool
typeEqb = (==)

typeEq :: DLType -> DLType -> MSrcLoc -> MSrcLoc -> App ()
typeEq e a mel mal = do
  unless (typeEqb e a) $
    expect_ $ Err_Type_Mismatch e a mel mal

typeEqs :: [(MSrcLoc, DLType)] -> App DLType
typeEqs = \case
  [] -> impossible $ "typeEqs none"
  [(_, xt)] -> return xt
  (xl, x) : (yl, y) : more -> typeEq x y xl yl >> typeEqs ((yl, y) : more)

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
  DLV_Bytes _ b -> return $ DLAE_Bytes b
  DLV_BytesDyn _ b -> return $ DLAE_BytesDyn b
  DLV_StringDyn _ s -> return $ DLAE_StringDyn s
  _ -> Nothing
  where
    recAssoc (k, v) = (,) k <$> dlvToDL v

slToDLExportVal :: SLVal -> App (Maybe DLSExportBlock)
slToDLExportVal v = slToDLV v >>= maybe (return Nothing) (dlvToEV >=> (return . Just))

dlvToEV :: DLValue -> App DLSExportBlock
dlvToEV = \case
  DLV_Fun at vs b ->
    return $ DLinExportBlock at (Just $ map v2vl vs) b
  ow ->
    case dlvToDL ow of
      Nothing -> impossible "dlvToEV"
      Just i -> do
        let at = srclocOf ow
        (stmts, da) <- captureLifts $ compileArgExpr_ i
        return $ DLinExportBlock at Nothing (DLSBlock at [] stmts da)

expectDLVar :: SLVal -> DLVar
expectDLVar = \case
  SLV_DLVar dv -> dv
  _ -> impossible "expectDLVar"

slToDLV :: SLVal -> App (Maybe DLValue)
slToDLV = \case
  SLV_Null at _ -> lit at DLL_Null
  SLV_Bool at b -> lit at $ DLL_Bool b
  SLV_Int at mt i -> lit at $ DLL_Int at (fromMaybe UI_Word mt) i
  SLV_Bytes at bs -> return $ Just $ DLV_Bytes at bs
  SLV_BytesDyn at bs -> return $ Just $ DLV_BytesDyn at bs
  SLV_String at t -> return $ Just $ DLV_StringDyn at t
  SLV_DLC c -> arg sb $ DLA_Constant c
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
    mds <- mapM (\(k, v) -> (,) k <$> slToDLV v) vs
    let mds' = all_just $ map (\(k, v) -> (,) k <$> v) mds
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
  SLV_Deprecated _ v -> slToDLV v
  SLV_ContractCode {} -> no
  where
    recs = mapM slToDLV
    lit at = arg at . DLA_Literal
    arg at = yes . DLV_Arg at
    yes = return . Just
    no = return Nothing

slToJSON :: SLVal -> App Aeson.Value
slToJSON v =
  case v of
    SLV_Null {} -> return $ Aeson.Null
    SLV_Bool _ b -> return $ Aeson.Bool b
    SLV_Int _ _ n -> return $ Aeson.Number $ fromIntegral n
    SLV_Bytes _ bs -> return $ Aeson.String $ b2t bs
    SLV_BytesDyn _ bs -> return $ Aeson.String $ b2t bs
    SLV_String _ t -> return $ Aeson.String $ t
    SLV_Array _ _ vs -> arr vs
    SLV_Tuple _ vs -> arr vs
    SLV_Object _ _ oe -> obj $ M.map sss_val oe
    SLV_Struct _ vs -> obj $ M.fromList vs
    SLV_Clo {} -> no
    SLV_Data {} -> no
    SLV_DLC {} -> no
    SLV_DLVar {} -> no
    SLV_Type {} -> no
    SLV_Connector {} -> no
    SLV_Participant {} -> no
    SLV_RaceParticipant {} -> no
    SLV_Anybody {} -> no
    SLV_Prim {} -> no
    SLV_Form {} -> no
    SLV_Kwd {} -> no
    SLV_Map {} -> no
    SLV_Deprecated _ x -> slToJSON x
    SLV_ContractCode {} -> no
  where
    no = expect_t v Err_JSON
    arr vs = (Aeson.Array . Vector.fromList) <$> mapM slToJSON vs
    obj m = Aeson.Object <$> (mToKM <$> mapM slToJSON (M.mapKeys s2t m))

getSecurityEnv :: SLVal -> M.Map SLVar SecurityLevel
getSecurityEnv = \case
  SLV_Object _ _ env -> M.map sss_level env
  _ -> mempty

typeOfM :: SLVal -> App (Maybe (DLType, DLArgExpr))
typeOfM v = do
  mdae <- slToDL v
  let m_senv = getSecurityEnv v
  return $ fmap (\x -> (argExprTypeOf m_senv x, x)) mdae

typeOf :: SLVal -> App (DLType, DLArgExpr)
typeOf v =
  typeOfM v >>= \case
    Just x -> return x
    Nothing -> expect_ $ Err_Type_None v

typeCheck_d :: MSrcLoc -> DLType -> SLVal -> App DLArgExpr
typeCheck_d tyLoc ty val = do
  (val_ty, res) <- typeOf val
  typeEq ty val_ty tyLoc $ Just $ srclocOf val
  return res

applyRefinement :: ClaimType -> SLVal -> [SLVal] -> Maybe SLVal -> App ()
applyRefinement ct p args mmsg = do
  pres <- evalApplyVals' p $ map public args
  void $ evalPrim (SLPrim_claim ct) $ pres : catMaybes [public <$> mmsg]

applyType :: ClaimType -> SLVal -> SLType -> App ()
applyType ct v = \case
  ST_Null -> meh
  ST_Bool -> meh
  ST_UInt {} -> meh
  ST_Bytes {} -> meh
  ST_BytesDyn -> meh
  ST_StringDyn -> meh
  ST_Digest -> meh
  ST_Address -> meh
  ST_Contract -> meh
  ST_Token -> meh
  ST_Array st _ -> mapM_ (rec st) =<< tups
  ST_Tuple sts -> zipWithM_ rec sts =<< tups
  ST_Object stm -> do
    -- This is like explodeObjectLike
    oe <- evalAsEnv (public v)
    se <- evalObjEnv oe
    forWithKeyM_ se $ \k kv -> do
      rec (stm M.! k) (sss_val kv)
  ST_Data _ stm ->
    -- This is like explodeDataLike
    case v of
      SLV_Data _ _ vn vv -> do
        rec (stm M.! vn) vv
      SLV_DLVar dv -> do
        -- XXX This code could be merged with select_all in JSSwitch case
        at <- withAt id
        let cmb (sa', casem') (vn, vt) = do
              vt' <- st2dte vt
              dv' <- ctxt_mkvar $ DLVar at Nothing vt'
              let vv = SLV_DLVar dv'
              SLRes case_lifts _ _ <- captureRes $ rec vt vv
              let sa'' = sa' <> mkAnnot case_lifts
              let casem'' = M.insert vn (SwitchCase (v2vl dv') case_lifts) casem'
              return (sa'', casem'')
        (sa', casem') <- foldM cmb (mempty, mempty) $ M.toList stm
        saveLift $ DLS_Switch at dv sa' $ SwitchCases casem'
      _ -> meh
  ST_Struct stfs -> zipWithM_ rec (map snd stfs) =<< tups
  ST_Fun {} -> meh
  ST_UDFun {} -> meh
  ST_Type {} -> meh
  ST_Refine _ p msg -> applyRefinement ct p [v] msg
  where
    meh = return ()
    rec = flip (applyType ct)
    tups = explodeTupleLike "applyType" v

typeCheck_s :: ClaimType -> MSrcLoc -> SLType -> SLVal -> App DLArgExpr
typeCheck_s ct ms st val = do
  dt <- st2dte st
  res <- typeCheck_d ms dt val
  applyType ct val st
  return $ res

is_class :: SLPart -> App Bool
is_class who = S.member who <$> (ae_classes <$> aisd)

is_api :: SLPart -> App Bool
is_api who = S.member who <$> (ae_apis <$> aisd)

ctxt_alloc :: App Int
ctxt_alloc = allocVarIdx

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

compileArgExpr :: Maybe DLType -> DLArgExpr -> App DLArg
compileArgExpr mt = \case
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
    a <- compileArgExpr_ ae
    mk $ DLLA_Data t v a
  DLAE_Struct kvs -> do
    let go (k, v) = (,) k <$> compileArgExpr_ v
    kvs' <- mapM go kvs
    mk $ DLLA_Struct kvs'
  DLAE_Bytes b -> do
    mk $ DLLA_Bytes b
  DLAE_BytesDyn b -> do
    mk $ DLLA_BytesDyn b
  DLAE_StringDyn t -> do
    mk $ DLLA_StringDyn t
  where
    mk la = do
      at <- withAt id
      let t = fromMaybe (largeArgTypeOf la) mt
      let mkvar = DLVar at Nothing t
      DLA_Var <$> ctxt_lift_expr mkvar (DLE_LArg at la)

compileArgExpr_ :: DLArgExpr -> App DLArg
compileArgExpr_ = compileArgExpr Nothing

compileArgExprs :: [DLArgExpr] -> App [DLArg]
compileArgExprs = mapM compileArgExpr_

compileArgExprMap :: M.Map a DLArgExpr -> App (M.Map a DLArg)
compileArgExprMap = mapM compileArgExpr_

slvParticipant_part :: SLVal -> App SLPart
slvParticipant_part = \case
  SLV_Participant _ x _ _ -> return x
  x -> expect_t x $ Err_NotParticipant

compileTypeOf :: SLVal -> App (DLType, DLArg)
compileTypeOf v = do
  (t, dae) <- typeOf v
  da <- compileArgExpr (Just t) dae
  return (t, da)

compileTypeOfs :: [SLVal] -> App ([DLType], [DLArg])
compileTypeOfs vs = unzip <$> mapM compileTypeOf vs

compileCheckType :: MSrcLoc -> DLType -> SLVal -> App DLArg
compileCheckType el et v = compileArgExpr_ =<< typeCheck_d el et v

compileToVar :: SLVal -> App DLVar
compileToVar v = do
  (t, a) <- compileTypeOf v
  case a of
    DLA_Var dv -> return dv
    _ -> do
      at <- withAt id
      ctxt_lift_expr (DLVar at Nothing t) (DLE_Arg at a)

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
  readSt st_after_ctor >>= \case
    True -> return ()
    False -> expect_ $ Err_Eval_IllegalWait

ensure_after_first :: App ()
ensure_after_first = do
  readSt st_after_first >>= \case
    True -> return ()
    False -> expect_ $ Err_NotAfterFirst

sco_to_cloenv :: SLScope -> App SLCloEnv
sco_to_cloenv SLScope {..} = do
  return $ SLCloEnv sco_penvs sco_cenv sco_use_strict

sco_lookup_unstrict :: App Bool
sco_lookup_unstrict = do
  Env {..} <- ask
  return $ sco_use_unstrict e_sco

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
      SLM_Export -> c_mod
      SLM_AppInit -> c_mod
      SLM_Step -> c_mod
      SLM_LocalStep -> return
      SLM_LocalPure -> return
      SLM_ConsensusStep -> c_mod
      SLM_ConsensusPure -> c_mod
    ps :: SLScope -> App SLScope
    ps = h $ \case
      SLM_Module -> return
      SLM_Export -> return
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

stMeet :: SLState -> SLState -> SLState
stMeet x y =
  x
    { st_live = f st_live
    , st_after_ctor = f st_after_ctor
    , st_after_first = f st_after_first
    }
  where
    f g = g x && g y

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
    (True, True) -> do
      -- Next, if they are both alive, then we take the "meet" of them and
      -- compare those. The idea of the meet is that the boolean flags get
      -- reduced to False if either side is false, so the continuation of this
      -- can't rely on it being set. This probably will not matter, but it
      -- might. This is mainly here so that
      --
      -- init();
      -- A.publish();
      -- while (c) {
      --  f();
      -- }
      -- g();
      --
      -- is allowed, but that g cannot rely on being after the constructor
      -- (because it could BE the constructor if c is false)
      --
      let old' = stMeet old new
      let new' = stMeet new old
      case old' == new' of
        True -> return new'
        False -> expect_ $ Err_Eval_IncompatibleStates new' old'

stEnsureMode :: SLMode -> App ()
stEnsureMode slm = do
  st <- readSt id
  mergeSt $ st {st_mode = slm}

binaryToPrim :: JSBinOp -> App SLVal
binaryToPrim = \case
  JSBinOpAnd _ -> impossible "and"
  JSBinOpDivide a -> prim a $ S_DIV Nothing
  JSBinOpEq a -> fun a "polyEq" "=="
  JSBinOpGe a -> prim a S_PGE
  JSBinOpGt a -> prim a S_PGT
  JSBinOpLe a -> prim a S_PLE
  JSBinOpLt a -> prim a S_PLT
  JSBinOpMinus a -> prim a $ S_SUB Nothing
  JSBinOpMod a -> fun a "polyMod" "%"
  JSBinOpNeq a -> fun a "polyNeq" "!="
  JSBinOpOr _ -> impossible "or"
  JSBinOpPlus a -> prim a $ S_ADD Nothing
  JSBinOpStrictEq a -> fun a "polyEq" "==="
  JSBinOpStrictNeq a -> fun a "polyNeq" "!=="
  JSBinOpTimes a -> prim a $ S_MUL Nothing
  JSBinOpLsh a -> prim a S_LSH
  JSBinOpRsh a -> prim a S_RSH
  JSBinOpBitAnd a -> prim a S_BAND
  JSBinOpBitOr a -> prim a S_BIOR
  JSBinOpBitXor a -> fun a "xor" "^"
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
      uni <- readUniverse
      return $ jsClo at uni s js mempty

infectWithId_clo :: SLVar -> SLClo -> SLClo
infectWithId_clo v (SLClo _ e b c) =
  SLClo (Just v) e b c

infectWithId_sv :: SrcLoc -> SLVar -> SLVal -> App SLVal
infectWithId_sv at v val = do
  case val of
    SLV_Participant a who _ mdv -> return $ SLV_Participant a who (Just v) mdv
    SLV_Prim (SLPrim_App_Delay a s e cr _) -> return $ SLV_Prim (SLPrim_App_Delay a s e cr $ Just v)
    SLV_Clo a mt c -> return $ SLV_Clo a mt $ infectWithId_clo v c
    SLV_DLVar (DLVar a _ t i) -> do
      infections <- asks e_infections
      liftIO $ modifyIORef infections $ M.insert i (at, v)
      return $ SLV_DLVar $ DLVar a (Just (at, v)) t i
    x -> return $ x

infectWithId_sss :: SLVar -> SLSSVal -> App SLSSVal
infectWithId_sss v (SLSSVal at lvl uni sv) = do
  SLSSVal at lvl uni <$> infectWithId_sv at v sv

infectWithId_sls :: SrcLoc -> SLVar -> SLSVal -> App SLSVal
infectWithId_sls at v (lvl, sv) = do
  r <- infectWithId_sv at v sv
  return $ (lvl, r)

evalObjEnv :: SLObjEnv -> App SLEnv
evalObjEnv = mapM go
  where
    go :: App SLSVal -> App SLSSVal
    go getv = do
      at <- withAt id
      uni <- readUniverse
      sls_sss at uni <$> getv

evalAsEnv :: SLSVal -> App SLObjEnv
evalAsEnv sv = do
  r <- evalAsEnvM sv
  case r of
    Just x -> return x
    Nothing -> expect_t (snd sv) $ Err_Eval_NotObject

evalAsEnvM :: SLSVal -> App (Maybe SLObjEnv)
evalAsEnvM sv@(lvl, obj) = case obj of
  SLV_Object _ _ env ->
    return $ Just $ M.map (retV . sss_sls) env
  SLV_DLVar obj_dv@(DLVar _ _ (T_Object tm) _) ->
    return $ Just $ retDLVar tm (DLA_Var obj_dv)
  SLV_Participant _ who vas _ -> do
    aisdM <- aisd_
    -- `<part>.interact.<field>(...)` is a shorthand for `<part>.only(() => interact.<field>(...))`
    -- Wrap each field of the participant's interface in a form, which will expand as described.
    let makeInteractField = do
          ios <- ae_ios <$> aisd
          return $
            case M.lookup who ios of
              Just isv@(SLSSVal _ _ _ (SLV_Object oa ol env)) ->
                secret $
                  SLV_Object oa ol $
                    flip M.mapWithKey env $ \k _ ->
                      isv {sss_val = SLV_Form $ SLForm_liftInteract who vas k}
              _ -> impossible "participant has no interact interface"
    return $ Just $
      M.fromList $
        [ ("only", retV $ public $ SLV_Form (SLForm_Part_Only who vas))
        , ("publish", go TCM_Publish)
        , ("pay", go TCM_Pay)
        , ("check", go TCM_Check)
        , ("set", delayCall SLPrim_part_set)
        ] <> case aisdM of
         Just _ -> [("interact", makeInteractField)]
         Nothing -> []
    where
      go m = withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus $ ToConsensusRec at whos vas (Just m) Nothing Nothing Nothing Nothing Nothing False False Nothing)
      whos = S.singleton who
  SLV_Anybody -> do
    at <- withAt id
    aisd_ >>= \case
      Nothing -> return Nothing
      Just a -> do
        let apis = ae_apis a
        whos <- S.fromList . M.keys <$> (ae_ios <$> aisd)
        let ps = S.difference whos apis
        case S.toList ps of
          [] -> expect_ $ Err_No_Participants
          [h] -> evalAsEnvM $ (lvl, SLV_Participant at h Nothing Nothing)
          _ -> evalAsEnvM $ (lvl, SLV_RaceParticipant sb ps)
  SLV_RaceParticipant _ whos ->
    return $ Just $
      M.fromList
        [ ("publish", go TCM_Publish)
        , ("pay", go TCM_Pay)
        , ("check", go TCM_Check)
        ]
    where
      go m = withAt $ \at -> public $ SLV_Form (SLForm_Part_ToConsensus $ ToConsensusRec at whos Nothing (Just m) Nothing Nothing Nothing Nothing Nothing False False Nothing)
  SLV_Form (SLForm_Part_ToConsensus p@(ToConsensusRec {..}))
    | slptc_mode == Nothing ->
      return $ Just $
        M.fromList $
          gom "publish" TCM_Publish slptc_msg
            <> gom "pay" TCM_Pay slptc_amte
            <> gom "when" TCM_When slptc_whene
            <> gom "timeout" TCM_Timeout slptc_timeout
            <> gom "throwTimeout" TCM_ThrowTimeout slptc_timeout
            <> gob ".fork" TCM_Fork slptc_fork
            <> gob ".api" TCM_Api slptc_api
            <> gom "check" TCM_Check slptc_check
    where
      gob key mode = \case
        False -> go key mode
        True -> []
      gom key mode = \case
        Nothing -> go key mode
        Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form $ SLForm_Part_ToConsensus $ p {slptc_mode = Just mode})]
  SLV_Form (SLForm_apiCall_partial p@(ApiCallRec {..}))
    | slac_mode == Nothing ->
      return $ Just $
        M.fromList $
          gom "throwTimeout" AC_ThrowTimeout slac_mtime
            <> gom "pay" AC_Pay slac_mpay
            <> gom "check" AC_Check slac_mcheck
            <> gom "assume" AC_Assume slac_massume
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form $ SLForm_apiCall_partial $ p {slac_mode = Just mode})]
  SLV_Form (SLForm_fork_partial p@(ForkRec {..}))
    | slf_mode == Nothing ->
      return $ Just $
        M.fromList $
          go "case" FM_Case
            <> go "api" FM_API
            <> go "api_" FM_API_
            <> gom "timeout" FM_Timeout slf_mtime
            <> gom "throwTimeout" FM_ThrowTimeout slf_mtime
            <> gom "paySpec" FM_PaySpec Nothing
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form $ SLForm_fork_partial $ p {slf_mode = Just mode})]
  SLV_Form (SLForm_parallel_reduce_partial p@(ParallelReduceRec {..}))
    | slpr_mode == Nothing ->
      return $ Just $
        M.fromList $
          go "invariant" PRM_Invariant
            <> gom "while" PRM_While slpr_mwhile
            <> go "case" PRM_Case
            <> go "api" PRM_API
            <> go "api_" PRM_API_
            <> gom "timeout" PRM_Timeout slpr_mtime
            <> gom "timeRemaining" PRM_TimeRemaining slpr_mtime
            <> gom "throwTimeout" PRM_ThrowTimeout slpr_mtime
            <> gom "paySpec" PRM_PaySpec Nothing
            <> go "define" PRM_Def
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial $ p {slpr_mode = Just mode})]
  --- FIXME rewrite the rest to look at the type and go from there
  SLV_Tuple _ _ -> return $ Just $ tupleValueEnv obj
  SLV_DLVar (DLVar _ _ (T_Tuple _) _) -> return $ Just $ tupleValueEnv obj
  SLV_DLVar (DLVar _ _ T_Token _) ->
    return $ Just $
      M.fromList $
        [ ("burn", delayCall SLPrim_Token_burn)
        , ("destroy", delayCall SLPrim_Token_destroy)
        , ("destroyed", delayCall SLPrim_Token_destroyed)
        , ("supply", delayCall SLPrim_Token_supply)
        , ("track", delayCall SLPrim_Token_track)
        , ("accepted", delayCall SLPrim_Token_accepted)
        ]
  SLV_Connector "ALGO" ->
    return $ Just $
      M.fromList $
        [ ("blockSeed", retV $ public $ SLV_Prim $ SLPrim_ALGOBlockField ABF_Seed)
        , ("blockSecs", retV $ public $ SLV_Prim $ SLPrim_ALGOBlockField ABF_Timestamp)
        ]
  SLV_Type ST_Token ->
    return $ Just $
      M.fromList $
        [ ("new", retV $ public $ SLV_Prim $ SLPrim_Token_new)
        , ("burn", retV $ public $ SLV_Prim $ SLPrim_Token_burn)
        , ("destroy", retV $ public $ SLV_Prim $ SLPrim_Token_destroy)
        , ("destroyed", retV $ public $ SLV_Prim $ SLPrim_Token_destroyed)
        , ("supply", retV $ public $ SLV_Prim $ SLPrim_Token_supply)
        , ("track", retV $ public $ SLV_Prim SLPrim_Token_track)
        , ("accepted", retV $ public $ SLV_Prim SLPrim_Token_accepted)
        ]
  SLV_Prim SLPrim_Struct -> return $ Just structValueEnv
  SLV_Type (ST_Struct ts) ->
    return $ Just $
      structValueEnv
        <> M.fromList
          [ ("fromTuple", retV $ public $ SLV_Prim $ SLPrim_Struct_fromTuple ts)
          , ("fromObject", retV $ public $ SLV_Prim $ SLPrim_Struct_fromObject ts)
          ]
  SLV_Struct _ kvs ->
    return $ Just $ M.map (retV . public) $ M.fromList kvs
  SLV_DLVar obj_dv@(DLVar _ _ (T_Struct tml) _) ->
    return $ Just $ retDLVarl tml (DLA_Var obj_dv)
  SLV_Prim SLPrim_Tuple ->
    return $ Just $
      M.fromList
        [ ("set", retV $ public $ SLV_Prim $ SLPrim_tuple_set)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_tuple_length)
        , ("includes", retV $ public $ SLV_Prim $ SLPrim_tuple_includes)
        ]
  SLV_Array {} -> return $ Just arrayValueEnv
  SLV_DLVar (DLVar _ _ (T_Array _ _) _) -> return $ Just arrayValueEnv
  SLV_Data {} ->
    return $ Just $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_DLVar (DLVar _ _ (T_Data _) _) ->
    return $ Just $
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
  SLV_Type (ST_Bytes len) -> do
    return $ Just $
      M.fromList
        [ ("pad", retV $ public $ SLV_Prim $ SLPrim_padTo len)]
  SLV_Prim SLPrim_Participant ->
    return $ Just $
      M.fromList
        [("set", retV $ public $ SLV_Prim SLPrim_part_set)]
  SLV_Prim SLPrim_Foldable ->
    return $ Just $
      M.fromList foldableValueEnv
  SLV_Prim SLPrim_Array ->
    return $ Just $
      M.fromList $
        [ ("empty", retStdLib "Array_empty")
        , ("findIndex", retStdLib "Array_findIndex")
        , ("find", retStdLib "Array_find")
        , ("withIndex", retStdLib "Array_withIndex")
        , ("forEachWithIndex", retStdLib "Array_forEachWithIndex")
        , ("indexOf", retStdLib "Array_indexOf")
        , ("replicate", retStdLib "Array_replicate")
        , ("slice", retStdLib "Array_slice")
        , ("elemType", retV $ public $ SLV_Prim $ SLPrim_array_elemType)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_array_length)
        , ("set", retV $ public $ SLV_Prim $ SLPrim_array_set)
        , ("iota", retV $ public $ SLV_Prim $ SLPrim_Array_iota)
        , ("concat", retV $ public $ SLV_Prim $ SLPrim_array_concat)
        , ("map", retV $ public $ SLV_Prim $ SLPrim_array_map False)
        , ("mapWithIndex", retV $ public $ SLV_Prim $ SLPrim_array_map True)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_array_reduce False)
        , ("reduceWithIndex", retV $ public $ SLV_Prim $ SLPrim_array_reduce True)
        , ("zip", retV $ public $ SLV_Prim $ SLPrim_array_zip)
        ]
          <> foldableValueEnv
  SLV_Prim SLPrim_Object ->
    return $ Just $
      M.fromList
        [ ("set", retV $ public $ SLV_Prim SLPrim_Object_set)
        , ("setIfUnset", retStdLib "Object_setIfUnset")
        , ("has", retV $ public $ SLV_Prim $ SLPrim_Object_has)
        , ("fields", retV $ public $ SLV_Prim $ SLPrim_Object_fields)
        ]
  SLV_Type ST_Contract ->
    return $ Just $
      M.fromList
        [ ("addressEq", retV $ public $ SLV_Prim $ SLPrim_op S_CTC_ADDR_EQ)
        , ("new", retV $ public $ SLV_Prim $ SLPrim_Contract_new)
        , ("fromAddress", retV $ public $ SLV_Prim $ SLPrim_Contract_fromAddress)
        ]
  SLV_Type (ST_UInt UI_Word) ->
    return $ Just $
      M.fromList
        [("max", retV $ public $ SLV_DLC DLC_UInt_max)]
  SLV_Type (ST_UInt UI_256) ->
    return $ Just $
      M.fromList
        [("max", retV $ public $ SLV_Int sb (Just UI_256) uint256_Max)]
  SLV_Type ST_StringDyn ->
    return $ Just $
      M.fromList
        [("concat", retV $ public $ SLV_Prim $ SLPrim_op S_STRINGDYN_CONCAT)]
  SLV_Type (ST_Data mAt varm) ->
    return $ Just $
      flip M.mapWithKey varm $ \k t ->
        retV $ public $ SLV_Prim $ SLPrim_Data_variant mAt varm k t
  SLV_Prim SLPrim_Map ->
    return $ Just $
      M.fromList $
        [ ("new", retV $ public $ SLV_Prim $ SLPrim_Map_new)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_Map_reduce False)
        , ("reduceWithKey", retV $ public $ SLV_Prim $ SLPrim_Map_reduce True)
        ]
          <> foldableValueEnv
  SLV_Map _ ->
    return $ Just $
      M.fromList $
        [ ("reduce", delayCall $ SLPrim_Map_reduce False)
        , ("reduceWithKey", delayCall $ SLPrim_Map_reduce True)
        ]
        <> foldableObjectEnv
  SLV_Prim SLPrim_Bytes ->
    return $ Just $
      M.fromList $
        [ ("fromHex", retV $ public $ SLV_Prim $ SLPrim_Bytes_fromHex)]
  SLV_Prim (SLPrim_remotef rat aa ma stf mpay mbill malgo Nothing) ->
    return $ Just $
      M.fromList $
        gom "pay" RFM_Pay mpay
          <> gom "bill" RFM_Bill mbill
          <> gom "withBill" RFM_WithBill mbill
          <> gom "ALGO" RFM_ALGO malgo
    where
      gom key mode me =
        case me of
          Nothing -> go key mode
          Just _ -> []
      go key mode =
        [(key, retV $ public $ SLV_Prim $ SLPrim_remotef rat aa ma stf mpay mbill malgo (Just mode))]
  _ -> return Nothing
  where
    foldableMethods = ["forEach", "min", "max", "imin", "imax", "all", "any", "or", "and", "sum", "average", "product", "includes", "size", "count"]
    foldableObjectEnv :: [(SLVar, App SLSVal)]
    foldableObjectEnv = map (\m -> (m, delayStdlib $ "Foldable_" <> m <> "1")) foldableMethods
    foldableValueEnv :: [(SLVar, App SLSVal)]
    foldableValueEnv = map (\m -> (m, retStdLib $ "Foldable_" <> m)) foldableMethods
    tupleValueEnv :: SLVal -> SLObjEnv
    tupleValueEnv tupSlv =
      M.fromList
        [ ("set", delayCall SLPrim_tuple_set)
        , ("length", doCall SLPrim_tuple_length)
        , ("includes",
           do
             -- I would just do `delayCall SLPrim_tuple_includes`, but it fails
             -- if tested by `SLPrim_is` as a function, which means that something
             -- like `mytuple.includes` can't be set as a view function.
             -- Fixing the `SLPrim_is` implementation seems a lot harder than this.
             clo <- jsCloExtendEnv (srclocOf tupSlv) "tuple_includes"
               "(v) => f(tup, v)"
               (M.fromList [ ("f", SLV_Prim SLPrim_tuple_includes)
                           , ("tup", tupSlv)
                           ])
             return $ (lvl, clo))
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
             , ("forEachWithIndex", delayStdlib "Array_forEachWithIndex1")
             , ("slice", delayStdlib "Array_slice1")
             , ("map", delayCall $ SLPrim_array_map False)
             , ("mapWithIndex", delayCall $ SLPrim_array_map True)
             , ("reduce", delayCall $ SLPrim_array_reduce False)
             , ("reduceWithIndex", delayCall $ SLPrim_array_reduce True)
             , ("zip", delayCall SLPrim_array_zip)
             ]
    structValueEnv :: SLObjEnv
    structValueEnv =
      M.fromList $
        [ ("toTuple", retV $ public $ SLV_Prim $ SLPrim_Struct_toTuple)
        , ("toObject", retV $ public $ SLV_Prim $ SLPrim_Struct_toObject)
        , ("fields", retV $ public $ SLV_Prim $ SLPrim_Struct_fields)
        ]
    delayCall :: SLPrimitive -> App SLSVal
    delayCall p = do
      at <- withAt id
      retV $ public $ SLV_Prim $ SLPrim_PrimDelay at p [sv] []
    delayStdlib :: SLVar -> App SLSVal
    delayStdlib = doApply <=< lookStdlib
    doCall :: SLPrimitive -> App SLSVal
    doCall p = doApply $ SLV_Prim p
    doApply :: SLVal -> App SLSVal
    doApply f = evalApplyVals' f [sv]
    retDLVarl tml obj_dla = do
      let retk (field, t) = (,) field $ do
            at <- withAt id
            let mkv = DLVar at Nothing t
            let e = DLE_ObjectRef at obj_dla field
            dv <- ctxt_lift_expr mkv e
            let ansv = SLV_DLVar dv
            let lvl' = lvl <> idLevel field
            return $ (lvl', ansv)
      M.fromList $ map retk tml
    retDLVar tm = retDLVarl $ M.toAscList tm
    retV = return
    retStdLib :: SLVar -> App SLSVal
    retStdLib n = (retV . public) =<< lookStdlib n

idLevel :: String -> SecurityLevel
idLevel = \case
  '_':_ -> Secret
  _ -> Public

lookStdlib :: SLVar -> App SLVal
lookStdlib n = sss_val <$> ((env_lookup (LC_RefFrom "stdlib") n) =<< (sco_cenv . e_sco) <$> ask)

evalDot_ :: SLVal -> SLObjEnv -> String -> App SLSVal
evalDot_ obj env field =
  case M.lookup field env of
    Just gv -> gv
    Nothing -> expect_t obj $ \o -> Err_Dot_InvalidField o (M.keys env) field

evalDot :: SLSVal -> String -> App SLSVal
evalDot obj s = flip (evalDot_ $ snd obj) s =<< evalAsEnv obj

st2dte :: HasCallStack => SLType -> App DLType
st2dte t =
  case st2dt t of
    Just x -> return $ x
    Nothing -> expect_ $ Err_Type_NotDT t

compileInteractResult :: ClaimType -> String -> SLType -> (DLType -> App DLExpr) -> App SLVal
compileInteractResult ct lab st de = do
  at <- withAt id
  dt <- st2dte st
  de' <- de dt
  isv <-
    case dt of
      T_Null -> do
        ctxt_lift_eff de'
        return $ SLV_Null at lab
      _ ->
        SLV_DLVar <$> ctxt_lift_expr (DLVar at Nothing dt) de'
  applyType ct isv st
  return isv

makeInteract :: SLPart -> SLInterface -> App (SLSSVal, InteractEnv)
makeInteract who (SLInterface spec) = do
  at <- withAt id
  uni <- readUniverse
  let lab = Just $ (bunpack who) <> "'s interaction interface"
  let wrap_ty :: SLVar -> (SrcLoc, SLType) -> App (SLSSVal, IType)
      wrap_ty k (i_at, ty) =
        case ty of
          ST_Fun stf@(SLTypeFun {..}) -> do
            dom' <- mapM st2dte stf_dom
            rng' <- st2dte stf_rng
            return $
              ( (sls_sss i_at uni $ secret $ SLV_Prim $ SLPrim_localf i_at who k $ Left stf)
              , IT_Fun dom' rng'
              )
          ST_UDFun rng -> do
            rng' <- st2dte rng
            return $
              ( (sls_sss i_at uni $ secret $ SLV_Prim $ SLPrim_localf i_at who k $ Right rng)
              , IT_UDFun rng'
              )
          t -> do
            t' <- st2dte t
            isv <- secret <$> compileInteractResult CT_Assume "interact" t (\dt -> return $ DLE_Arg i_at $ DLA_Interact who k dt)
            return $ (sls_sss i_at uni isv, IT_Val t')
  (lifts, spec') <- captureLifts $ mapWithKeyM wrap_ty spec
  let io = SLSSVal at Secret uni $ SLV_Object at lab $ M.map fst spec'
  let ienv = InteractEnv $ M.map (\(_, y) -> y) spec'
  saveLift $ DLS_Only at who lifts
  return $ (io, ienv)

convertTernaryReachApp :: SrcLoc -> JSAnnot -> JSExpression -> JSArrowParameterList -> JSExpression -> JSStatement -> JSStatement
convertTernaryReachApp at a opte top_formals partse top_s = top_s'
  where
    pis = parseJSArrowFormals at top_formals
    sp = JSSemi a
    lhs = jsArrayLiteral a pis
    rhs = jsCall a (JSIdentifier a ".adaptReachAppTupleArgs") [partse]
    top_ss =
      [ JSExpressionStatement (JSCallExpression (JSIdentifier a "setOptions") a (JSLOne opte) a) sp
      , JSConstant a (JSLOne $ JSVarInitExpression lhs $ JSVarInit a rhs) sp
      , JSExpressionStatement (JSCallExpression (JSIdentifier a "init") a JSLNil a) sp
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
  SLV_Data _ dm "Left" v
    | correctData dm ->
      Left <$> compileCheckType msdef t v
  SLV_Data at dm "Right" v
    | correctData dm -> do
      liftIO $ emitWarning (Just at) $ W_NetworkSeconds
      Right <$> compileCheckType msdef t v
  SLV_DLVar (DLVar _ _ (T_Data dm) _)
    | correctData dm ->
      expect_ Err_TimeArg_NotStatic
  v -> do
    f <- lookStdlib "relativeTime"
    c <- lookStdlib "require"
    at <- withAt id
    liftIO $ emitWarning (Just at) $ W_Deprecated D_UntypedTimeArg
    compileTimeArg =<< ensure_public =<< evalApplyVals' f [public v, public c]
  where
    correctData = (==) (dataTypeMap $ eitherT t t)
    t = T_UInt UI_Word

decodePay :: (JSExpression -> App JSExpression) -> JSExpression -> App JSExpression
decodePay cont = \case
  JSExpressionParen _ i _ -> rec i
  JSExpressionTernary c _ tt _ ff -> do
    (snd <$> evalExpr c) >>= \case
      SLV_Bool _ b -> rec (if b then tt else ff)
      _ -> expect_ $ Err_InvalidPaySpec
  x -> cont x
  where
    rec = decodePay cont

evalForm :: SLForm -> [JSExpression] -> App SLSVal
evalForm f args = do
  case f of
    SLForm_App -> do
      at <- withAt id
      SLScope {..} <- asks e_sco
      top_s <-
        case args of
          [JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ top_s] -> return $ jsArrowBodyToStmt top_s
          [opte, partse, JSArrowExpression top_formals a top_s] -> do
            -- liftIO $ emitWarning $ W_Deprecated at D_ReachAppArgs
            let at' = srcloc_jsa "app arrow" a at
            return $ convertTernaryReachApp at' a opte top_formals partse (jsArrowBodyToStmt top_s)
          _ -> expect_ $ Err_App_InvalidArgs args
      conInfoR <- liftIO $ newIORef mempty
      retV $ public $ SLV_Prim $ SLPrim_App_Delay at top_s (sco_cenv, sco_use_strict) conInfoR Nothing
    SLForm_Part_Only who mv -> do
      at <- withAt id
      x <- one_arg
      env <- ask >>= sco_to_cloenv . e_sco
      return $ public $ SLV_Form $ SLForm_EachAns [(who, mv)] at env x
    SLForm_liftInteract who mv k -> do
      at <- withAt id
      uni <- readUniverse
      let whoOnly = SLV_Form $ SLForm_Part_Only who mv
      -- Enclose the generated `only` with a closure that binds all the arguments to the interact call,
      -- which allows `this` to point to the originator of the consensus transfer instead of `who`.
      let rator' = jsClo at uni "liftedInteract" "(...args) => whoOnly(() => interact[k](...args))" $ M.fromList [("whoOnly", whoOnly), ("k", SLV_Bytes at $ bpack k)]
      evalApply rator' args
    SLForm_fork -> do
      zero_args
      slf_at <- withAt id
      let slf_mode = Nothing
      let slf_cases = []
      let slf_mtime = Nothing
      let slf_mnntpay = Nothing
      retV $ public $ SLV_Form $ SLForm_fork_partial $ ForkRec {..}
    SLForm_fork_partial p@(ForkRec {..}) ->
      case slf_mode of
        Just FM_Case -> doFM_Case_args args
        Just FM_API -> doForkAPI2Case False args >>= doFM_Case_args
        Just FM_API_ -> doForkAPI2Case True args >>= doFM_Case_args
        Just FM_Timeout -> do
          at <- withAt id
          go $ p {slf_mtime = Just (at, args)}
        Just FM_ThrowTimeout -> do
          at <- withAt id
          (d, arg) <- case args of
            [d] -> return (d, JSLiteral JSNoAnnot "null")
            [d, x] -> return (d, x)
            _ -> illegal_args 2
          let ta = srcloc2annot at
          let throwS = jsArrow ta [] $ JSThrow ta arg (a2sp ta)
          go $ p {slf_mtime = Just (at, [d, throwS])}
        Just FM_PaySpec -> do
          x <- one_arg
          go $ p {slf_mnntpay = Just x}
        Nothing -> expect_t rator $ Err_Eval_NotApplicable
      where
        go p' = retV $ public $ SLV_Form $ SLForm_fork_partial $ p' {slf_mode = Nothing}
        doFM_Case_args = \case
          [who, chk, loc, pay, con] -> doFM_Case who (Just chk) loc (Just pay) con
          [who, loc, pay, con]      -> doFM_Case who Nothing loc (Just pay) con
          [who, loc, con]           -> doFM_Case who Nothing loc Nothing con
          _ -> illegal_args 4
        doFM_Case who m_chk loc m_pay con = do
          at <- withAt id
          let a = srcloc2annot at
          let decodePay' = \case
                JSArrayLiteral aa ts ae -> do
                  let tok_ids = map (jse_expect_id at) $ jsa_flatten ts
                  let tok_pays =
                        JSDecimal a "0" :
                        map
                          (\i ->
                             JSArrayLiteral
                               aa
                               [ JSArrayElement (JSDecimal aa "0")
                               , JSArrayComma aa
                               , JSArrayElement (JSIdentifier aa i)
                               ]
                               ae)
                          tok_ids
                  return $ JSArrayLiteral aa (intersperse (JSArrayComma aa) $ map JSArrayElement tok_pays) ae
                _ -> expect_ $ Err_InvalidPaySpec
          def_pay <-
                case slf_mnntpay of
                  Just pp -> decodePay decodePay' pp
                  Nothing -> return $ JSDecimal a "0"
          let default_pay = jsArrowExpr a [JSIdentifier a "_"] def_pay
          let pay = fromMaybe default_pay m_pay
          let default_chk = jsArrowStmts a [] []
          let chk = fromMaybe default_chk m_chk
          w <- slvParticipant_part =<< (snd <$> evalExpr who)
          let fc = ForkCase at who (bunpack w) loc chk pay con
          go $ p {slf_cases = slf_cases <> [fc]}
    SLForm_parallel_reduce -> do
      slpr_at <- withAt id
      let slpr_mode = Nothing
      slpr_init <- one_arg
      let slpr_minv = []
      let slpr_mwhile = Nothing
      let slpr_cases = []
      let slpr_apis = []
      let slpr_api_s = []
      let slpr_mtime = Nothing
      let slpr_defs = []
      let slpr_mpay = Nothing
      retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial $ ParallelReduceRec {..}
    SLForm_parallel_reduce_partial (p@ParallelReduceRec {..}) -> do
      aa <- withAt $ \at -> (at, args)
      case slpr_mode of
        Just PRM_Invariant -> do
          (x, my) <- one_mtwo_args
          go $ p {slpr_minv = slpr_minv <> [(x, my)] }
        Just PRM_While -> do
          x <- one_arg
          go $ p {slpr_mwhile = Just x}
        Just PRM_Case ->
          go $ p {slpr_cases = slpr_cases <> [aa]}
        Just PRM_API ->
          go $ p {slpr_apis = slpr_apis <> [aa]}
        Just PRM_API_ ->
          go $ p {slpr_api_s = slpr_api_s <> [aa]}
        Just PRM_PaySpec -> do
          x <- one_arg
          go $ p {slpr_mpay = Just x}
        Just PRM_Def -> do
          x <- one_arg
          go $ p {slpr_defs = slpr_defs <> [x]}
        Just PRM_Timeout -> retTimeout PRM_Timeout aa
        Just PRM_TimeRemaining -> retTimeout PRM_TimeRemaining aa
        Just PRM_ThrowTimeout -> retTimeout PRM_ThrowTimeout aa
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
      where
        go p' = retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial $ p' {slpr_mode = Nothing}
        makeTimeoutArgs mode aa = Just (mode, fst aa, snd aa)
        retTimeout prm aa = go $ p {slpr_mtime = makeTimeoutArgs prm aa}
    SLForm_Part_ToConsensus p@(ToConsensusRec {..}) -> do
      let mustBeThunk fn = do
            fst <$> deconstructFun fn >>= \case
              [] -> return fn
              _  -> expect_ Err_ExpectedThunk
      case slptc_mode of
        Just TCM_Publish -> do
          at <- withAt id
          let msg = map (jse_expect_id at) args
          go $ p {slptc_msg = Just msg}
        Just TCM_Pay -> do
          (x, my) <- one_mtwo_args
          go $ p {slptc_amte = Just x, slptc_amt_req = my}
        Just TCM_When -> do
          x <- one_arg
          go $ p {slptc_whene = Just x}
        Just TCM_Fork -> do
          zero_args
          go $ p {slptc_fork = True}
        Just TCM_Api -> do
          zero_args
          go $ p {slptc_api = True}
        Just TCM_Check -> do
          x <- one_arg >>= mustBeThunk
          go $ p { slptc_check = Just x }
        Just TCM_Timeout -> do
          at <- withAt id
          let proc = \case
                JSExpressionParen _ e _ -> proc e
                JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s ->
                  return $ jsArrowBodyToBlock dt_s
                _ -> expect_ $ Err_ToConsensus_TimeoutArgs args
          x <-
            case args of
              [de] -> return $ (at, de, Nothing)
              [de, te] -> do
                te' <- proc te
                return $ (at, de, Just te')
              _ -> expect_ $ Err_ToConsensus_TimeoutArgs args
          go $ p {slptc_timeout = Just x}
        Just TCM_ThrowTimeout -> do
          at <- withAt id
          let ta = srcloc2annot at
          (de, x) <- one_two_args ta
          let throwS = JSThrow ta x JSSemiAuto
          go $ p {slptc_timeout = Just (at, de, Just (jsStmtToBlock throwS))}
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
      where
        go p' = retV $ public $ SLV_Form $ SLForm_Part_ToConsensus $ p' {slptc_mode = Nothing}
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
      ensure_mode SLM_Step "wait"
      amt_e <- one_arg
      amt_ta <-
        locStMode SLM_ConsensusPure $
          evalExpr amt_e >>= ensure_public >>= compileTimeArg
      doBaseWaitUpdate amt_ta
      at <- withAt id
      ctxt_lift_eff $ DLE_Wait at amt_ta
      return $ public $ SLV_Null at "wait"
    SLForm_apiCall -> do
      ensure_mode SLM_Step "call"
      slac_who <- one_arg
      who <- evalExpr slac_who
      case snd who of
        SLV_Participant _ ns _ _ ->
          is_api ns >>= \case
            True -> return ()
            False -> impossible "Must be API Participant"
        _ -> impossible "Must be API Participant"
      slac_at <- withAt id
      let slac_mode = Nothing
      let slac_mtime = Nothing
      let slac_mpay = Nothing
      let slac_massume = Nothing
      let slac_mcheck = Nothing
      retV $ public $ SLV_Form $ SLForm_apiCall_partial $ ApiCallRec {..}
    SLForm_apiCall_partial p@(ApiCallRec {..}) ->
      case slac_mode of
        Just AC_Pay -> do
          x <- one_arg
          go $ p {slac_mpay = Just x}
        Just AC_ThrowTimeout -> do
          at <- withAt id
          let ta = srcloc2annot at
          (de, x) <- one_two_args ta
          go $ p {slac_mtime = Just (de, x)}
        Just AC_Assume -> do
          x <- one_arg
          go $ p {slac_massume = Just x}
        Just AC_Check -> do
          x <- one_arg
          go $ p { slac_mcheck = Just x }
        Nothing ->
          expect_t rator $ Err_Eval_NotApplicable
      where
        go p' = retV $ public $ SLV_Form $ SLForm_apiCall_partial $ p' {slac_mode = Nothing}
    SLForm_setApiDetails -> do
      at <- withAt id
      (who_e, msg, mc_id) <- two_mthree_args
      who <- mustBeParticipant who_e
      let mCaseId = maybe Nothing (Just . jse_expect_id at) mc_id
      (ct, tys) <- case mCaseId of
        -- API Fork With Multiple Cases (msg is Data instance)
        Just _ -> do
          evalExpr msg >>= \case
            (_, SLV_Type s) -> do
              s' <- st2dte s
              return (AIC_Case, [s'])
            _ -> impossible "Expected data instance Type"
        -- API Call / Single case fork
        Nothing -> do
          e <- evalExpr msg
          (dt, _) <- compileTypeOf $ snd e
          return (AIC_SpreadArg, [dt])
      ctxt_lift_eff $ DLE_setApiDetails at who tys mCaseId ct
      return $ public $ SLV_Null at "setApiDetails"
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
    one_mtwo_args = case args of
      [x] -> return $ (x, Nothing)
      [x, y] -> return $ (x, Just y)
      _ -> illegal_args 1
    two_args = case args of
      [x, y] -> return $ (x, y)
      _ -> illegal_args 2
    _three_args = case args of
      [x, y, z] -> return $ (x, y, z)
      _ -> illegal_args 3
    two_mthree_args = case args of
      [w, x] -> return $ (w, x, Nothing)
      [w, x, y] -> return $ (w, x, Just y)
      _ -> illegal_args 2
    one_two_args ta = case args of
      [de] -> return (de, JSLiteral ta "null")
      [de, e] -> return (de, e)
      _ -> illegal_args 2

mustBeParticipant :: JSExpression -> App SLPart
mustBeParticipant who_e =
  (snd <$> evalExpr who_e) >>= \case
    SLV_Participant _ part _ _ -> return part
    v -> expect_t v $ Err_Expected "Participant"

evalPolyEq :: SecurityLevel -> SLVal -> SLVal -> App SLSVal
evalPolyEq lvl x y =
  case (x, y) of
    -- Both args static
    (SLV_Int _ t1 l, SLV_Int _ t2 r) | t1 == t2 -> retBool $ l == r
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
        (T_UInt lut, T_UInt rut) | lut == rut -> make_var $ PEQ lut
        (T_Digest, T_Digest) -> make_var DIGEST_EQ
        (T_Address, T_Address) -> make_var ADDRESS_EQ
        (T_Token, T_Token) -> make_var TOKEN_EQ
        (T_Bool, T_Bool) -> do
          notR <- evalPrimOp S_IF_THEN_ELSE [(lvl, r), public $ SLV_Bool sb False, public $ SLV_Bool sb True]
          evalPrimOp S_IF_THEN_ELSE [(lvl, l), (lvl, r), notR]
        _ ->
          case typeEqb lty rty of
            False -> retBool False
            True -> hashAndCmp (lvl, l) (lvl, r)
  where
    andMapEq ls rs = do
      xs <- zipWithM (\l r -> snd <$> evalPolyEq lvl l r) ls rs
      foldrM evalAnd (SLV_Bool sb True) xs
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

evalOr :: SLVal -> SLVal -> App SLVal
evalOr l r =
  case (l, r) of
    (SLV_Bool bAt lb, SLV_Bool _ rb) ->
      return $ SLV_Bool bAt $ lb || rb
    (SLV_DLVar {}, SLV_DLVar {}) ->
      snd <$> evalPrimOp S_IF_THEN_ELSE [(Public, l), public $ SLV_Bool sb True, (Public, r)]
    (SLV_Bool bAt True, _) -> return $ SLV_Bool bAt True
    (SLV_Bool _ False, SLV_DLVar {}) -> return $ r
    (SLV_DLVar _, SLV_Bool {}) -> evalOr r l
    _ -> impossible $ "evalOr expecting SLV_Bool or SLV_DLVar"

evalAnd :: SLVal -> SLVal -> App SLVal
evalAnd l r =
  case (l, r) of
    (SLV_Bool bAt lb, SLV_Bool _ rb) ->
      return $ SLV_Bool bAt $ lb && rb
    (SLV_DLVar {}, SLV_DLVar {}) ->
      snd <$> evalPrimOp S_IF_THEN_ELSE [(Public, l), (Public, r), public $ SLV_Bool sb False]
    (SLV_Bool bAt False, _) -> return $ SLV_Bool bAt False
    (SLV_Bool _ True, SLV_DLVar {}) -> return $ r
    (SLV_DLVar _, SLV_Bool {}) -> evalAnd r l
    _ -> impossible $ "evalAnd expecting SLV_Bool or SLV_DLVar"

evalOrMap :: (SLVal -> App SLVal) -> [SLVal] -> App SLVal
evalOrMap f = \case
  [] -> return $ SLV_Bool sb False
  h : t -> do
    h' <- f h
    t' <- evalOrMap f t
    evalOr h' t'

evalAndMap :: (SLVal -> App SLVal) -> [SLVal] -> App SLVal
evalAndMap f = \case
  [] -> return $ SLV_Bool sb True
  h : t -> do
    h' <- f h
    t' <- evalAndMap f t
    evalAnd h' t'

evalDistinctTokens :: [SLVal] -> [SLVal] -> App SLVal
evalDistinctTokens old = \case
  [] -> withAt $ flip SLV_Bool True
  (v : vs) -> do
    v_d <- evalAndMap (\v' -> snd <$> (evalNeg =<< (snd <$> evalPolyEq Public v v'))) $ old <> vs
    vs_d <- evalDistinctTokens old vs
    evalAnd v_d vs_d

evalITE :: SecurityLevel -> SLVal -> SLVal -> SLVal -> App SLSVal
evalITE lvl c t f = do
  at <- withAt id
  ca <- compileCheckType msdef T_Bool c
  (tt, ta) <- compileTypeOf t
  fa <- compileCheckType (Just $ srclocOf t) tt f
  dv <- ctxt_lift_expr (DLVar at Nothing tt) (DLE_PrimOp at IF_THEN_ELSE [ca, ta, fa])
  return $ (lvl, SLV_DLVar dv)

evalNeg :: SLVal -> App SLSVal
evalNeg v = evalITE Public v (b False) (b True)
  where
    b = SLV_Bool sb

evalPrimOp :: SPrimOp -> [SLSVal] -> App SLSVal
evalPrimOp sp sargs = do
  at <- withAt id
  let zero mt = SLV_Int at mt 0
  let goMul = do
        case args of
          [SLV_Int _ mt 1, rhs] | mtOkay mt rhs -> static rhs
          [lhs, SLV_Int _ mt 1] | mtOkay mt lhs -> static lhs
          [SLV_Int _ mt 0, rhs] | mtOkay mt rhs -> static $ zero $ uintTyM rhs
          [lhs, SLV_Int _ mt 0] | mtOkay mt lhs -> static $ zero $ uintTyM lhs
          _ -> nn2n (*)
  case sp of
    S_UCAST to trunc _ -> do
      case args of
        [SLV_Int lhs_at _ lhs_i] | not trunc -> static $ SLV_Int lhs_at (Just to) lhs_i
        _ -> do
          let from = if to == UI_Word then UI_256 else UI_Word
          let dom = T_UInt from
          make_var [dom] (T_UInt to) args
    S_BYTES_ZPAD xtra ->
      case args of
        [SLV_Bytes _ lhs] -> do
          let rhs = bytesZero xtra
          static $ SLV_Bytes at $ lhs <> rhs
        [lhs] -> do
          (lhs_l, lhs_ae) <- typeOfBytes lhs
          let rng = T_Bytes $ lhs_l + xtra
          make_var_ rng [lhs_ae]
        _ -> expect_ $ Err_Apply_ArgCount at 1 (length args)
    S_BTOI_LAST8 isDigest ->
      case args of
        [b] -> do
          ae <- case isDigest of
                  True  -> typeOfDigest b
                  False -> snd <$> typeOfBytes b
          make_var_ (T_UInt UI_Word) [ae]
        _ -> expect_ $ Err_Apply_ArgCount at 1 (length args)
    S_SQRT -> n2n isqrt
    S_ADD _ -> goAdd
    S_SUB _ -> goSub
    S_MUL _ -> goMul
    S_DIV _ -> goDiv
    S_MOD _ -> nn2n (mod)
    S_PLT -> nn2b (<)
    S_PLE -> nn2b (<=)
    S_PEQ ->
      case args of
        [x, y] -> do
          useStrict >>= \case
            True ->
              (,) <$> typeOfM x <*> typeOfM y >>= \case
                (Just (x_ty, _), Just (y_ty, _)) ->
                  typeEq x_ty y_ty (Just $ srclocOf x) (Just $ srclocOf y) >> chkEq
                _ -> return (lvl, SLV_Bool at $ x == y)
            False -> chkEq
          where
            chkEq = evalPolyEq lvl x y
        _ -> expect_ $ Err_Apply_ArgCount at 2 (length args)
    S_PGE -> nn2b (>=)
    S_PGT -> nn2b (>)
    S_IF_THEN_ELSE ->
      case args of
        [SLV_Bool _ b, t, f] ->
          static $ if b then t else f
        [b, t, f] -> evalITE lvl b t f
        _ -> expect_ $ Err_Apply_ArgCount at 3 (length args)
    S_DIGEST_EQ -> make_var [T_Digest, T_Digest] T_Bool args
    S_DIGEST_XOR -> make_var [T_Digest, T_Digest] T_Digest args
    S_BYTES_XOR ->
      case args of
        [lhs, rhs] -> do
          (lhs_l, lhs_ae) <- typeOfBytes lhs
          (_, rhs_ae) <- typeOfBytes rhs
          make_var_ (T_Bytes lhs_l) [lhs_ae, rhs_ae]
        _ -> expect_ $ Err_Apply_ArgCount at 2 (length args)
    S_ADDRESS_EQ -> make_var [T_Address, T_Address] T_Bool args
    S_TOKEN_EQ -> make_var [T_Token, T_Token] T_Bool args
    -- FIXME fromIntegral may overflow the Int
    S_LSH -> nn2n (\a b -> shift a (fromIntegral b))
    S_RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    S_BAND -> nn2n (.&.)
    S_BIOR -> nn2n (.|.)
    S_BXOR -> nn2n (xor)
    S_MUL_DIV _ -> case args of
      [SLV_Int _ mt 1, rhs, den] | mtOkay2 mt rhs den ->
        evalPrimOp (S_DIV Nothing) $ map (lvl,) [rhs, den]
      [lhs, SLV_Int _ mt 1, den] | mtOkay2 mt lhs den ->
        evalPrimOp (S_DIV Nothing) $ map (lvl,) [lhs, den]
      [SLV_Int _ mt 0, rhs, den] | mtOkay2 mt rhs den -> static $ zero $ uintTyM rhs
      [lhs, SLV_Int _ mt 0, den] | mtOkay2 mt lhs den -> static $ zero $ uintTyM den
      [x, y, z]
        | x == z -> static y
        | y == z -> static x
      _ -> nnn2n (\a b c -> (a * b) `div` c)
      where
        mtOkay2 x y z = mtOkay x y && mtOkay x z && mtOkay (uintTyM y) z
    S_CTC_ADDR_EQ -> make_var [T_Contract, T_Address] T_Bool args
    S_STRINGDYN_CONCAT -> make_var [T_StringDyn, T_StringDyn] T_StringDyn args
    S_UINT_TO_STRINGDYN ui -> make_var [T_UInt ui] T_StringDyn args
  where
    args = map snd sargs
    lvl = mconcat $ map fst sargs
    mtOkay' mt my =
      case mt of
        Nothing -> True
        Just t -> Just t == my
    mtOkay'3 m1 m2 m3 = mtOkay' m1 (mtJoin m2 m3) && mtOkay' m2 m3
    mtOkay mt = mtOkay' mt . uintTyM
    arg1ty n =
      case args of
        x : _ -> mustBeUIntTy msdef (Just $ srclocOf x) =<< fst <$> typeOf x
        _ -> do
          at <- withAt id
          expect_ $ Err_Apply_ArgCount at n (length args)
    goAdd = do
      case args of
        [SLV_Int _ mt 0, rhs] | mtOkay mt rhs -> static rhs
        [lhs, SLV_Int _ mt 0] | mtOkay mt lhs -> static lhs
        _ -> nn2n (+)
    goSub = do
      case args of
        [lhs, SLV_Int _ mt 0] | mtOkay mt lhs -> static lhs
        _ -> nn2n (-)
    goDiv = do
      case args of
        [lhs, SLV_Int _ mt 1] | mtOkay mt lhs -> static lhs
        _ -> nn2n (div)
    nn2b op =
      case args of
        [SLV_Int _ mt1 lhs, SLV_Int _ mt2 rhs] | mtOkay' mt1 mt2 -> do
          at <- withAt id
          static $ SLV_Bool at $ op lhs rhs
        _ -> do
          dom <- arg1ty 2
          make_var [dom, dom] T_Bool args
    n2n op =
      case args of
        [SLV_Int _ mt lhs] -> do
          at <- withAt id
          static $ SLV_Int at mt $ op lhs
        _ -> do
          dom <- arg1ty 1
          make_var [dom] dom args
    nn2n op =
      case args of
        [SLV_Int _ mt1 lhs, SLV_Int _ mt2 rhs] | mtOkay' mt1 mt2 -> do
          at <- withAt id
          static $ SLV_Int at (mtJoin mt1 mt2) $ op lhs rhs
        _ -> do
          dom <- arg1ty 2
          make_var [dom, dom] dom args
    nnn2n op =
      case args of
        [SLV_Int _ mt1 x, SLV_Int _ mt2 y, SLV_Int _ mt3 z] | mtOkay'3 mt1 mt2 mt3 -> do
          at <- withAt id
          static $ SLV_Int at (mtJoin mt1 $ mtJoin mt2 mt3) $ op x y z
        _ -> do
          dom <- arg1ty 3
          make_var [dom, dom, dom] dom args
    static v = return $ (lvl, v)
    make_var dom rng args' = do
      at <- withAt id
      args'e <-
        mapM (uncurry (typeCheck_d msdef))
          =<< zipEq (Err_Apply_ArgCount at) dom args'
      let uit_dom = case dom of
                      x:_ -> uintTyOf x
                      _ -> UI_Word
      make_var_' uit_dom rng args'e
    make_var_ = make_var_' UI_Word
    make_var_' uit_dom rng args'e = do
      at <- withAt id
      dargs <- compileArgExprs args'e
      let dopClaim ca msg = doClaim CT_Assert ca $ Just msg
      let mkvar = DLVar at Nothing
      let doOp t cp cargs = DLA_Var <$> (ctxt_lift_expr (mkvar t) $ DLE_PrimOp at cp cargs)
      let doCmp = doOp T_Bool
      let uit_rng = uintTyOf rng
      usesVerifyArithmetic <- readDlo dlo_verifyArithmetic
      let p = sprimToPrim uit_dom uit_rng usesVerifyArithmetic sp
      let lim_maxUInt_a = uintTyMax uit_rng
      let chkDiv t denom = do
            ca <- doCmp (PGT t) [denom, DLA_Literal $ DLL_Int sb t 0]
            dopClaim ca "div by zero"
      let verifyAdd t = do
            let (a, b) = case dargs of
                  [a_, b_] -> (a_, b_)
                  _ -> impossible "add args"
            ra <- doOp (T_UInt t) (SUB t PV_Safe) [lim_maxUInt_a, b]
            ca <- doCmp (PLE t) [a, ra]
            dopClaim ca "add overflow"
      let verifySub t = do
            ca <- doCmp (PGE t) dargs
            dopClaim ca "sub wraparound"
      let verifyDiv t = do
            chkDiv t $ case dargs of
              [_, b] -> b
              _ -> impossible "div args"
      let verifyMod t = do
            chkDiv t $ case dargs of
              [_, b] -> b
              _ -> impossible "mod args"
      let verifyUCast trunc = unless trunc $ do
            -- We add an assertion when casting from UInt256 to UInt, unless we want to truncate
            let a = case dargs of
                  [a_] -> a_
                  _ -> impossible "cast args"
            wordLimitAs256 <- doOp (T_UInt UI_256) (UCAST UI_Word UI_256 False PV_Veri) [ uintTyMax UI_Word ]
            ca <- doCmp (PLE UI_256) [a, wordLimitAs256]
            dopClaim ca "cast overflow"
      let verifyMul t pv = do
            let (a, b) = case dargs of
                  [a_, b_] -> (a_, b_)
                  _ -> impossible "mul args"
            ra <- doOp (T_UInt t) (DIV t pv) [lim_maxUInt_a, b]
            ca <- doCmp (PLE t) [a, ra]
            denomZero <- doCmp (PEQ t) [(DLA_Literal $ DLL_Int at t 0), b]
            orDenomZeroCa <- doOp (T_Bool) IF_THEN_ELSE [denomZero, (DLA_Literal $ DLL_Bool True), ca]
            dopClaim orDenomZeroCa "mul overflow"
      let shouldVerifyArith = \case
            PV_Veri -> True
            _ -> False
      let whenShouldVerifyArith pv = when $ shouldVerifyArith pv
      case p of
        ADD t pv -> whenShouldVerifyArith pv $ verifyAdd t
        SUB t pv -> whenShouldVerifyArith pv $ verifySub t
        DIV t pv -> whenShouldVerifyArith pv $ verifyDiv t
        MOD t pv -> whenShouldVerifyArith pv $ verifyMod t
        MUL t pv -> whenShouldVerifyArith pv $ verifyMul t pv
        MUL_DIV pv -> do
          whenShouldVerifyArith pv $
            chkDiv UI_Word $ case dargs of
              [_, _, b] -> b
              _ -> impossible "muldiv args"
        UCAST UI_256 UI_Word trunc pv -> whenShouldVerifyArith pv $ verifyUCast trunc
        _ -> return $ mempty
      dv <- ctxt_lift_expr (DLVar at Nothing rng) (DLE_PrimOp at p dargs)
      let da = DLA_Var dv
      let chkMul t = do
            ca <- doCmp (PLE t) [da, lim_maxUInt_a]
            dopClaim ca "mul overflow"
      case p of
        MUL t pv -> whenShouldVerifyArith pv $ chkMul t
        MUL_DIV pv -> whenShouldVerifyArith pv $ chkMul UI_Word
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
    let mkde _ da i = DLE_ArrayRef at da (DLA_Literal $ DLL_Int at UI_Word i)
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
  ensure_modes (all_slm_modes \\ [SLM_Module, SLM_Export]) "fluid ref"
  let fvt = fluidVarType fv
  dv <- ctxt_mkvar (DLVar at (Just (at, show $ pretty fv)) fvt)
  saveLift $ DLS_FluidRef at dv fv
  return dv

doFluidRef_da :: FluidVar -> App DLArg
doFluidRef_da fv = DLA_Var <$> doFluidRef_dv fv

doFluidRef :: FluidVar -> App SLSVal
doFluidRef fv = (public . SLV_DLVar) <$> doFluidRef_dv fv

doFluidSet_ :: FluidVar -> DLArg -> App ()
doFluidSet_ fv da = do
  at <- withAt id
  typeEq (fluidVarType fv) (argTypeOf da) msdef Nothing
  saveLift $ DLS_FluidSet at fv da

doFluidSet :: FluidVar -> SLSVal -> App ()
doFluidSet fv ssv = do
  sv <- ensure_public ssv
  da <- compileCheckType msdef (fluidVarType fv) sv
  doFluidSet_ fv da

doBaseWaitUpdate :: DLTimeArg -> App ()
doBaseWaitUpdate = \case
  Left x -> doFluidSet_ FV_baseWaitTime x
  Right x -> doFluidSet_ FV_baseWaitSecs x

ensureCreatedToken :: String -> DLArg -> App ()
ensureCreatedToken lab a = do
  toks_c <- readSt st_toks_c
  case a of
    DLA_Var v ->
      case S.member v toks_c of
        True -> return ()
        False -> expect_ $ Err_Token_NotCreated lab
    _ -> expect_ $ Err_Token_DynamicRef

tokenMetaSet :: TokenMeta -> DLArg -> DLArg -> Bool -> App ()
tokenMetaSet tm tok amta initTok = do
  at <- withAt id
  mp <- M.lookup tok <$> getTokenPosEnv
  saveLift $ DLS_TokenMetaSet tm at tok amta mp initTok

tokenMetaGet :: TokenMeta -> DLArg -> App DLVar
tokenMetaGet tm tok = do
  at <- withAt id
  dv <- ctxt_mkvar $ DLVar at Nothing $ tmTypeOf tm
  mi <- M.lookup tok <$> getTokenPosEnv
  saveLift $ DLS_TokenMetaGet tm at dv tok mi
  return dv

getBalanceOf :: Maybe DLArg -> App SLSVal
getBalanceOf mtok = do
  bv <- getBalanceOf' mtok
  return $ public $ SLV_DLVar bv

getBalanceOf' :: Maybe DLArg -> App DLVar
getBalanceOf' = \case
  Nothing -> do
    v <- doFluidRef FV_netBalance
    compileToVar $ snd v
  Just tok -> tokenMetaGet TM_Balance tok

getTokenPosEnv :: App (M.Map DLArg Int)
getTokenPosEnv = readSt st_tok_pos

setBalance :: TokenMeta -> Maybe DLArg -> DLArg -> App ()
setBalance tm mtok amta = setBalance' tm mtok amta False

setBalance' :: TokenMeta -> Maybe DLArg -> DLArg -> Bool -> App ()
setBalance' tm mtok amta initTok =
  case mtok of
    Nothing -> doFluidSet_ FV_netBalance amta
    Just tok -> do
      at <- withAt id
      mi <- M.lookup tok <$> getTokenPosEnv
      saveLift $ DLS_TokenMetaSet tm at tok amta mi initTok

doBalanceInit :: TokenMeta -> Maybe DLArg -> App ()
doBalanceInit tm mtok = do
  at <- withAt id
  let amt = DLA_Literal $ DLL_Int at UI_Word 0
  setBalance' tm mtok amt True

doBalanceInit' :: TokenMeta -> Maybe DLArg -> DLArg -> App ()
doBalanceInit' tm mtok amt = do
  setBalance' tm mtok amt True

doBalanceAssert :: Maybe DLArg -> SLVal -> SPrimOp -> B.ByteString -> App ()
doBalanceAssert mtok lhs op msg = do
  at <- withAt id
  let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [(Public, lhs)] []
  balance_v <- getBalanceOf mtok
  cmp_v <- evalApplyVals' cmp_rator [balance_v]
  let ass_rator = SLV_Prim $ SLPrim_claim CT_Assert
  void $
    evalApplyVals' ass_rator $
      [cmp_v, public $ SLV_Bytes at msg]

tokenMetaAssert :: TokenMeta -> DLArg -> SLVal -> SPrimOp -> B.ByteString -> App ()
tokenMetaAssert tm tok lhs op msg = do
  at <- withAt id
  let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [(Public, lhs)] []
  v <- public . SLV_DLVar <$> tokenMetaGet tm tok
  cmp_v <- evalApplyVals' cmp_rator [v]
  let ass_rator = SLV_Prim $ SLPrim_claim CT_Assert
  void $
    evalApplyVals' ass_rator $
      [cmp_v, public $ SLV_Bytes at msg]

assumeOp :: B.ByteString -> SPrimOp -> DLVar -> SLVal -> App ()
assumeOp lab op v bound = do
  cmp <- evalPrimOp op [public $ SLV_DLVar v, public bound]
  cmp_v <- compileCheckType msdef T_Bool $ snd cmp
  doClaim CT_Assume cmp_v $ Just ("assume " <> lab)

assumeGtZero :: DLVar -> App ()
assumeGtZero v = do
  at <- withAt id
  let zero = SLV_Int at (Just UI_Word) 0
  assumeOp ">= 0" S_PGE v zero

assumeLtUMax :: DLVar -> App ()
assumeLtUMax v = do
  assumeOp "<= UInt.max" S_PLE v $ SLV_DLC DLC_UInt_max

assumeBalanceUpdate :: SLVal -> SLVal -> (PrimOp, DLVar -> App b) -> App b
assumeBalanceUpdate balV amtV (op, assumeF) = do
  at <- withAt id
  let tInt = T_UInt UI_Word
  bal_dv <- compileCheckType msdef tInt balV
  amt_dv <- compileCheckType msdef tInt amtV
  -- Avoid using `evalPrimOp` to avoid generating claims for verify arithmetic
  bal' <- ctxt_lift_expr (DLVar at Nothing tInt) $ DLE_PrimOp at op [bal_dv, amt_dv]
  assumeF bal'

doBalanceUpdate :: Maybe DLArg -> SPrimOp -> SLVal -> App ()
doBalanceUpdate mtok op = \case
  SLV_Int _ _ 0 -> return ()
  rhs -> do
    at <- withAt id
    let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
    -- Find the current balance of mtok
    bsv <- getBalanceOf mtok
    -- Assume we can add/sub from balance
    let assumeOps = M.fromList [
          (S_ADD (Just PV_None), (ADD UI_Word PV_None, assumeLtUMax)),
          (S_SUB (Just PV_None), (SUB UI_Word PV_None, assumeGtZero)) ]
    whenVerifyArithmetic $ do
      forM_ (M.lookup op assumeOps) $
        assumeBalanceUpdate (snd bsv) rhs
    bv' <- evalApplyVals' up_rator [bsv]
    bva <- compileCheckType msdef (T_UInt UI_Word) $ snd bv'
    setBalance TM_Balance mtok bva

tokenMetaUpdate :: TokenMeta -> DLArg -> SPrimOp -> SLVal -> App ()
tokenMetaUpdate tm tok op rhs = do
  at <- withAt id
  let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
  v <- tokenMetaGet tm tok
  v' <- evalApplyVals' up_rator [public $ SLV_DLVar v]
  dv <- compileCheckType msdef (tmTypeOf tm) $ snd v'
  tokenMetaSet tm tok dv False

doArrayBoundsCheck :: Integer -> SLVal -> App ()
doArrayBoundsCheck sz idxv = do
  at <- withAt id
  cmp_v <- evalApplyVals' (SLV_Prim $ SLPrim_op S_PLT) [public idxv, public $ SLV_Int at nn sz]
  void $
    evalApplyVals' (SLV_Prim $ SLPrim_claim CT_Assert) $
      [cmp_v, public $ SLV_Bytes at "array bounds check"]

mustBeBytes :: SLVal -> App B.ByteString
mustBeBytes = \case
  SLV_Bytes _ x -> return $ x
  v@(SLV_DLVar (DLVar _ _ (T_Bytes _) _)) ->
    expect_t v $ Err_Obj_IllegalComputedField
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

mustBeType :: SLVal -> App SLType
mustBeType = \case
  SLV_Type t -> return t
  ow ->
    locAtf (flip srclocOf_ ow) $
      expect_t ow $ Err_Expected "type"

mustBeDataTy :: (DLType -> EvalError) -> DLType -> App (M.Map SLVar DLType)
mustBeDataTy err = \case
  T_Data x -> return $ x
  t -> expect_ $ err t

mustBeObjectTy :: (DLType -> EvalError) -> DLType -> App (M.Map SLVar (SecurityLevel, DLType))
mustBeObjectTy err = \case
  T_Object x -> return $ M.mapWithKey (curry $ bimap idLevel id) x
  t -> expect_ $ err t

mustBeUIntTy :: MSrcLoc -> MSrcLoc -> DLType -> App DLType
mustBeUIntTy atExpected atActual ty = case ty of
  T_UInt {} -> return ty
  _ -> expect_ $ Err_Type_Mismatch (T_UInt UI_Word) ty atExpected atActual

structKeyRegex :: App RE
structKeyRegex = liftIO $ compileRegex "^([_a-zA-Z][_a-zA-Z0-9]*)$"

nameRegex :: App RE
nameRegex = liftIO $ compileRegex "^([a-zA-Z][_a-zA-Z0-9]*)$"

tokenPay :: Maybe DLArg -> DLArg -> B.ByteString -> ReaderT Env IO ()
tokenPay mtok_a amt_a msg = do
  amt_sv <- argToSV amt_a
  doBalanceAssert mtok_a amt_sv S_PLE msg
  doBalanceUpdate mtok_a (S_SUB $ Just PV_None) amt_sv

getBillTokens :: Maybe (Either SLVal SLVal) -> DLPayAmt -> App (Bool, [DLArg])
getBillTokens mbill billAmt = do
  case mbill of
    Just (Left arg) -> do
      (ty, ae) <- typeOf arg
      case (ty, ae) of
        (T_Tuple ts, DLAE_Tuple tas)
          | all (== T_Token) ts -> do
            tas' <- mapM compileArgExpr_ tas
            return (True, tas')
        _ -> expect_ $ Err_WithBill_Type ty
    Just (Right _) -> do
      let nRecv = not $ staticZero $ pa_net billAmt
      let nnBilled = pa_ks billAmt
      return (nRecv, map snd nnBilled)
    Nothing ->
      return (False, [])

typeOfBytes :: SLVal -> App (Integer, DLArgExpr)
typeOfBytes v = do
  (t, ae) <- typeOf v
  case t of
    T_Bytes l -> return (l, ae)
    _ -> expect_t v $ Err_Expected "bytes"

typeOfDigest :: SLVal -> App DLArgExpr
typeOfDigest v = do
  (t, ae) <- typeOf v
  case t of
    T_Digest -> return ae
    _ -> expect_t v $ Err_Expected "Digest"

verifyNotReserved :: SrcLoc -> String -> App ()
verifyNotReserved at s = do
  connectors <- readDlo dlo_connectors
  forM_ connectors $ \cn -> do
    when (conReserved cn s) $ do
      expect_thrown at $ Err_Connector_Reserved (conName cn) s

warnInteractType :: SLType -> App ()
warnInteractType = \case
  ST_Object _ -> do
    at <- withAt id
    liftIO $ emitWarning (Just at) W_ExternalObject
  ST_Tuple ts -> mapM_ r ts
  ST_Data _ e -> mapM_ r e
  ST_Struct a -> mapM_ (r . snd) a
  ST_Fun (SLTypeFun {..}) -> mapM_ r (stf_rng : stf_dom)
  ST_Array t _ -> r t
  ST_UDFun t -> r t
  ST_Refine t _ _ -> r t
  _ -> return ()
  where
    r = warnInteractType

trackToken :: DLArg -> Maybe DLArg -> App ()
trackToken tok mBal = do
  at <- withAt id
  let bal = fromMaybe (DLA_Literal $ DLL_Int at UI_Word 0) mBal
  tokdv <- case tok of
            DLA_Var dv -> return dv
            _ -> impossible "Not token"
  st <- readSt id
  let existingToks = st_toks st
  setSt $
    st
      { st_toks = existingToks <> [tokdv]
      , st_tok_pos = M.insert tok (length existingToks) (st_tok_pos st)
      }
  doBalanceInit' TM_Balance (Just tok) bal
  let req_rator = SLV_Prim $ SLPrim_claim CT_Require
  -- Initialize and distinctize tokens
  bv <- let f = map SLV_DLVar in evalDistinctTokens (f existingToks) (f [tokdv])
  void $
    locSt (st {st_mode = SLM_ConsensusPure}) $
      evalApplyVals' req_rator [public bv, public $ SLV_Bytes at $ "non-network tokens distinct"]

evalPrim :: SLPrimitive -> [SLSVal] -> App SLSVal
evalPrim p sargs =
  case p of
    SLPrim_castOrTrunc to -> do
      (mTrunc, mShouldVerify, sargs') <-
        case sargs of
          [i, (_, SLV_Bool _ trunc), (_, SLV_Bool _ shouldVerify)] -> return (Just trunc, Just shouldVerify, [i])
          [i, (_, SLV_Bool _ trunc)] -> return (Just trunc, Nothing, [i])
          _ -> return (Nothing, Nothing, sargs)
      let mpv = maybe Nothing (Just . bool PV_Safe PV_Veri) mShouldVerify
      evalPrimOp (S_UCAST to (fromMaybe False mTrunc) mpv) sargs'
    SLPrim_Token_burn -> do
      (tokv, mamtv) <-
        case args of
          [x] -> return (x, Nothing)
          [x, y] -> return (x, Just y)
          _ -> illegal_args
      at <- withAt id
      toka <- compileCheckType msdef T_Token tokv
      let lab = "Token.burn"
      ensureCreatedToken lab toka
      amta <-
        case mamtv of
          Just v -> compileCheckType msdef (T_UInt UI_Word) v
          Nothing -> DLA_Var <$> getBalanceOf' (Just toka)
      ensure_mode SLM_ConsensusStep lab
      let mtok_a = Just toka
      amt_sv <- argToSV amta
      doBalanceAssert mtok_a amt_sv S_PLE (bpack lab)
      doBalanceUpdate mtok_a (S_SUB $ Just PV_None) amt_sv
      tokenMetaUpdate TM_Supply toka (S_SUB $ Just PV_None) amt_sv
      ctxt_lift_eff $ DLE_TokenBurn at toka amta
      return $ public $ SLV_Null at lab
    SLPrim_Token_destroy -> do
      at <- withAt id
      toka <- compileCheckType msdef T_Token =<< one_arg
      let lab = "Token.destroy"
      ensureCreatedToken lab toka
      ensure_mode SLM_ConsensusStep lab
      tokenMetaAssert TM_Destroyed toka (SLV_Bool at False) S_PEQ $ bpack $ "token not yet destroyed at " <> lab
      tokenMetaAssert TM_Supply toka (SLV_Int at nn 0) S_PEQ $ bpack $ "token supply zero at " <> lab
      ctxt_lift_eff $ DLE_TokenDestroy at toka
      tokenMetaSet TM_Destroyed toka (DLA_Literal $ DLL_Bool True) False
      return $ public $ SLV_Null at lab
    SLPrim_Token_new -> do
      at <- withAt id
      metam <- mustBeObject =<< one_arg
      metam' <- mapM (ensure_public . sss_sls) metam
      let metal f k = k (M.lookup f metam')
      let bytes f len = metal f $ \case
            Just v -> compileCheckType msdef (T_Bytes len) v
            Nothing -> compileArgExpr_ $ largeArgToArgExpr $ bytesZeroLit len
      dtn_name <- bytes "name" tokenNameLen
      dtn_sym <- bytes "symbol" tokenSymLen
      dtn_url <- bytes "url" tokenURLLen
      dtn_metadata <- bytes "metadata" tokenMetadataLen
      dtn_supply <- metal "supply" $ \case
        Nothing -> return $ DLA_Constant $ DLC_UInt_max
        Just v -> compileCheckType msdef (T_UInt UI_Word) v
      dtn_decimals <- metal "decimals" $ \case
        Nothing -> return $ Nothing
        Just v -> Just <$> compileCheckType msdef (T_UInt UI_Word) v
      let check = \case
            "name" -> ok
            "symbol" -> ok
            "url" -> ok
            "metadata" -> ok
            "decimals" -> ok
            "supply" -> ok
            k -> expect_ $ Err_TokenNew_InvalidKey k
            where
              ok = return ()
      mapM_ check (M.keys metam')
      let tns = DLTokenNew {..}
      let supplya = dtn_supply
      ensure_mode SLM_ConsensusStep "new Token"
      tokdv_ <-
        ctxt_lift_expr (DLVar at Nothing T_Token) $
          DLE_TokenNew at tns
      tokdv <- doInternalLog_ Nothing tokdv_
      st <- readSt id
      let existingToks = st_toks st
      tokIsUniq <- tokIsUnique (map DLA_Var existingToks) $ DLA_Var tokdv
      doClaim CT_Assume tokIsUniq $ Just "New token is unique"
      setSt $ st { st_toks_c = S.insert tokdv (st_toks_c st) }
      let toka = DLA_Var tokdv
      trackToken toka $ Just supplya
      tokenMetaSet TM_Supply toka supplya False
      tokenMetaSet TM_Destroyed toka (DLA_Literal $ DLL_Bool False) False
      return $ public $ SLV_DLVar tokdv
    SLPrim_Token_track -> do
      ensure_mode SLM_ConsensusStep "Token.track"
      at <- withAt id
      toka <- compileCheckType msdef T_Token =<< one_arg
      trackToken toka Nothing
      return $ public $ SLV_Null at "Token.track"
    SLPrim_padTo len -> do
      v <- one_arg
      (vl, _) <- typeOfBytes v
      let xtra = fromIntegral $ len - vl
      evalPrimOp (S_BYTES_ZPAD xtra) [public v]
    SLPrim_race -> do
      at <- withAt id
      ps <- mapM slvParticipant_part $ map snd sargs
      retV $ (lvl, SLV_RaceParticipant at (S.fromList ps))
    SLPrim_balance -> do
      case args of
        [] -> getBalanceOf Nothing
        [v] -> getBalanceOf . Just =<< compileCheckType msdef T_Token v
        _ -> illegal_args
    SLPrim_Token_accepted -> do
      at <- withAt id
      (addr_, tok_) <- two_args
      ensure_mode SLM_ConsensusStep "Token.accepted"
      addr <- compileCheckType msdef T_Address addr_
      tok <- compileCheckType msdef T_Token tok_
      dv_ <- ctxt_lift_expr (DLVar at Nothing T_Bool) $ DLE_TokenAccepted at addr tok
      dv <- doInternalLog_ Nothing dv_
      retV $ public $ SLV_DLVar dv
    SLPrim_Token_supply -> do
      v <- one_arg
      da <- compileCheckType msdef T_Token v
      ensureCreatedToken "Token.supply" da
      sr <- tokenMetaGet TM_Supply da
      return $ public $ SLV_DLVar sr
    SLPrim_Token_destroyed -> do
      v <- one_arg
      da <- compileCheckType msdef T_Token v
      ensureCreatedToken "Token.destroyed" da
      dr <- tokenMetaGet TM_Destroyed da
      return $ public $ SLV_DLVar dr
    SLPrim_fluid_read fv -> do
      zero_args
      doFluidRef fv
    SLPrim_fluid_read_didPublish fv -> do
      ensure_after_first
      zero_args
      evalPrim (SLPrim_fluid_read fv) []
    SLPrim_fluid_read_canWait fv -> do
      ensure_can_wait
      zero_args
      evalPrim (SLPrim_fluid_read fv) []
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
        [(SLV_Int _ _ sz)] -> retV $ (lvl, SLV_Type $ ST_Bytes sz)
        _ -> illegal_args
    SLPrim_Array ->
      case map snd sargs of
        [(SLV_Type ty), (SLV_Int _ _ sz)] ->
          retV $ (lvl, SLV_Type $ ST_Array ty sz)
        _ -> illegal_args
    SLPrim_Foldable -> expect_ Err_Prim_Foldable
    SLPrim_tuple_includes -> do
      (tup, needle) <- two_args
      vs <- explodeTupleLike "Tuple.includes" tup
      v_ret <- evalOrMap (\v -> snd <$> evalPolyEq Public v needle) vs
      retV $ (lvl, v_ret)
    SLPrim_tuple_length -> do
      at <- withAt id
      one_arg >>= \case
        SLV_Tuple _ vs ->
          retV $ public $ SLV_Int at nn $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Tuple ts) _) ->
          retV $ public $ SLV_Int at nn $ fromIntegral $ length ts
        _ -> illegal_args
    SLPrim_array_length -> do
      at <- withAt id
      one_arg >>= \case
        SLV_Array _ _ vs ->
          retV $ public $ SLV_Int at nn $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Array _ sz) _) ->
          retV $ public $ SLV_Int at nn $ fromIntegral $ sz
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
        [SLV_Int _ mt sz] -> do
          let t = fromMaybe UI_Word mt
          retV $ (lvl, SLV_Array at (T_UInt t) $ map (SLV_Int at (Just t)) [0 .. (sz -1)])
        [_] -> expect_ $ Err_Prim_InvalidArg_Dynamic p
        _ -> illegal_args
    SLPrim_array ->
      case map snd sargs of
        [(SLV_Type elem_sty), elems_v] ->
          case elems_v of
            SLV_Tuple _ elem_vs -> do
              at <- withAt id
              elem_ty <- st2dte elem_sty
              let check1 sv = typeCheck_d msdef elem_ty sv >> return sv
              elem_vs_checked <- mapM check1 elem_vs
              retV $ (lvl, SLV_Array at elem_ty elem_vs_checked)
            --- FIXME we could support turning a DL Tuple into an array.
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_concat -> do
      at <- withAt id
      case map snd sargs of
        [SLV_Array x_at x_ty x_vs, SLV_Array y_at y_ty y_vs] -> do
          typeEq x_ty y_ty (Just x_at) (Just y_at)
          retV $ (lvl, SLV_Array at x_ty $ x_vs ++ y_vs)
        [x, y] -> do
          (xt, xa) <- compileTypeOf x
          (yt, ya) <- compileTypeOf y
          case (xt, yt) of
            (T_Array x_ty x_sz, T_Array y_ty y_sz) -> do
              typeEq x_ty y_ty (Just $ srclocOf x) (Just $ srclocOf y)
              let t = T_Array x_ty (x_sz + y_sz)
              let mkdv = DLVar at Nothing t
              dv <- ctxt_lift_expr mkdv $ DLE_ArrayConcat at xa ya
              return $ (lvl, SLV_DLVar dv)
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_zip -> do
      at <- withAt id
      uni <- readUniverse
      let mapArgs = intercalate ", " $ map (\n -> "a" <> (show n)) [0..(length sargs -1)]
      let jsF = "(" <> mapArgs <> ") => [" <> mapArgs <> "]"
      let f = jsClo at uni "zipFunc" jsF mempty
      evalApplyVals' (SLV_Prim $ SLPrim_array_map False) $ sargs <> [public f]
    SLPrim_array_map withIndex ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        _ -> do
          let (f, xs) = case reverse args of
                f_ : rmore -> (f_, reverse rmore)
                _ -> impossible "array_map"
          at <- withAt id
          (xt, x_da) <- unzip <$> mapM compileTypeOf xs
          (x_ty, x_sz) <- unzip <$> mapM mustBeArray xt
          size <- case allEqual x_sz of
            Right s -> return s
            Left (Just (s1, s2)) -> expect_ $ Err_Zip_ArraysNotEqualLength s1 s2
            Left _ -> impossible "array_map"
          let f' as i = evalApplyVals' f $ (map (\a -> (lvl, a)) as) <> (if withIndex then [(lvl, i)] else [])
          (a_dv, a_dsv) <- unzip <$> mapM (make_dlvar at) x_ty
          (i_dv, i_dsv) <- make_dlvar at $ T_UInt UI_Word
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ (f_lvl, f_ty, f_da) <-
            captureRes $ do
              (f_lvl, f_v) <- f' a_dsv i_dsv
              (f_ty, f_da) <- compileTypeOf f_v
              return (f_lvl, f_ty, f_da)
          let shouldUnroll = not (isLocal f_lifts) || all isSmallLiteralArray xs
          case shouldUnroll of
            True -> do
              xs_vs <- transpose <$> mapM (explodeTupleLike "map") xs
              let evalem xvs i = snd <$> f' xvs (SLV_Int at nn i)
              vs' <- zipWithM evalem xs_vs [0..]
              return $ (f_lvl, SLV_Array at f_ty vs')
            False -> do
              let t = T_Array f_ty size
              (ans_dv, ans_dsv) <- make_dlvar at t
              let f_bl = DLSBlock at [] f_lifts f_da
              saveLift $ DLS_ArrayMap at (v2lv ans_dv) x_da (map v2vl a_dv) (v2vl i_dv) f_bl
              return $ (f_lvl, ans_dsv)
    SLPrim_array_reduce withIndex ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [_, _] -> illegal_args
        _ -> do
          let (f, z, xs) = case reverse args of
                f_ : z_ : rmore -> (f_, z_, reverse rmore)
                _ -> impossible "array_reduce"
          at <- withAt id
          (xt, x_da) <- unzip <$> mapM compileTypeOf xs
          (x_ty, x_sz) <- unzip <$> mapM mustBeArray xt
          _ <- case allEqual x_sz of
            Right s -> return s
            Left (Just (s1, s2)) -> expect_ $ Err_Zip_ArraysNotEqualLength s1 s2
            Left _ -> impossible "array_reduce"
          let f' b as i = evalApplyVals' f $ [(lvl, b)] <> (map (\a -> (lvl, a)) as) <> (if withIndex then [(lvl, i)] else [])
          (z_ty, z_da) <- compileTypeOf z
          (b_dv, b_dsv) <- make_dlvar at z_ty
          (a_dv, a_dsv) <- unzip <$> mapM (make_dlvar at) x_ty
          (i_dv, i_dsv) <- make_dlvar at $ T_UInt UI_Word
          -- We ignore the state because if it is impure, then we will unroll
          -- anyways
          SLRes f_lifts _ f_da <-
            captureRes $ do
              (f_lvl, f_v) <- f' b_dsv a_dsv i_dsv
              ensure_level lvl f_lvl
              (f_ty, f_da) <- compileTypeOf f_v
              typeEq z_ty f_ty (Just $ srclocOf z) (Just $ srclocOf f_v)
              return $ f_da
          let shouldUnroll = not (isLocal f_lifts) || all isSmallLiteralArray xs
          case shouldUnroll of
            True -> do
              xs_vs <- transpose <$> mapM (explodeTupleLike "reduce") xs
              let evalem :: SLSVal -> ([SLVal], Integer) -> App SLSVal
                  evalem prev_z (xvs, i) = do
                    let iv = SLV_Int at nn i
                    xv_v' <- f' (snd prev_z) xvs iv
                    --- Note: We are artificially restricting reduce
                    --- to be parameteric in the state. We also ensure
                    --- that they type is the same as the anonymous
                    --- version.
                    _ <- typeCheck_d (Just $ srclocOf z) z_ty (snd xv_v')
                    return $ xv_v'
              foldM evalem (lvl, z) $ zip xs_vs [0..]
            False -> do
              (ans_dv, ans_dsv) <- make_dlvar at z_ty
              let f_bl = DLSBlock at [] f_lifts f_da
              saveLift $ DLS_ArrayReduce at (v2lv ans_dv) x_da z_da (v2vl b_dv) (map v2vl a_dv) (v2vl i_dv) f_bl
              return $ (lvl, ans_dsv)
    SLPrim_array_set ->
      case map snd sargs of
        [arrv, idxv, valv] -> do
          (idxty, idxda) <- compileTypeOf idxv
          case (idxty, idxda) of
            (T_UInt UI_Word, (DLA_Literal (DLL_Int _ _ idxi))) ->
              case arrv of
                SLV_Array arrAt elem_ty arrvs ->
                  case idxi' < length arrvs of
                    True -> do
                      at <- withAt id
                      void $ typeCheck_d (Just arrAt) elem_ty valv
                      let arrvs' = arraySet idxi' valv arrvs
                      let arrv' = SLV_Array at elem_ty arrvs'
                      retV $ (lvl, arrv')
                    False ->
                      expect_ $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                SLV_DLVar arrdv@(DLVar arrAt _ arr_ty@(T_Array elem_ty sz) _) ->
                  case idxi < sz of
                    True -> do
                      valda <- compileCheckType (Just arrAt) elem_ty valv
                      doArrayBoundsCheck sz idxv
                      at <- withAt id
                      retArrDV arr_ty $ DLE_ArraySet at (DLA_Var arrdv) idxda valda
                    False ->
                      expect_ $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                _ -> illegal_args
              where
                idxi' = fromIntegral idxi
            (T_UInt UI_Word, _) -> do
              (arr_ty, arrda) <- compileTypeOf arrv
              case arr_ty of
                T_Array elem_ty sz -> do
                  valda <- compileCheckType (Just $ srclocOf arrv) elem_ty valv
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
          verifyNotReserved at s
          bool (expect_ $ Err_Struct_Key_Invalid s) (return s) $ matched $ s ?=~ r
    SLPrim_Struct_fromTuple ts -> do
      tv <- one_arg
      at <- withAt id
      let go (i, k) = (,) k <$> doArrRef_ tv (SLV_Int at nn i)
      kvs <- mapM go $ zip [0 ..] $ map fst ts
      return $ (lvl, SLV_Struct at kvs)
    SLPrim_Struct_fromObject ts -> do
      ov <- one_arg
      at <- withAt id
      let go k = (,) k <$> (snd <$> evalDot (lvl, ov) k)
      kvs <- mapM go $ map fst ts
      return $ (lvl, SLV_Struct at kvs)
    SLPrim_Struct_toTuple -> do
      at <- withAt id
      (,) lvl <$> (SLV_Tuple at <$> (explodeTupleLike "Struct.asTuple" =<< one_arg))
    SLPrim_Struct_toObject -> do
      sv <- one_arg
      at <- withAt id
      (,) lvl <$> (SLV_Object at Nothing <$> (evalObjEnv =<< evalAsEnv (lvl, sv)))
    SLPrim_Struct_fields -> do
      at <- withAt id
      a <- one_arg
      ty <- mustBeType a
      pairs <- case ty of
        ST_Struct pairs_ -> return pairs_
        _ -> expect_t a $ Err_Expected "struct type"
      let pairs' = map (\(n, t) -> SLV_Tuple at
                         [SLV_Bytes at $ bpack n, SLV_Type t]) pairs
      retV $ (lvl, SLV_Tuple at pairs')
    SLPrim_Tuple -> do
      vs <- mapM (expect_ty "Tuple argument") $ map snd sargs
      retV $ (lvl, SLV_Type $ ST_Tuple vs)
    SLPrim_tuple_set -> do
      at <- withAt id
      (tup_v, index_v, val_v) <- three_args
      (tup_t, tup_a) <- compileTypeOf tup_v
      (val_t, val_a) <- compileTypeOf val_v
      index <- case index_v of
        SLV_Int _ _ i -> return i
        v -> expect_t v $ Err_Expected "int"
      let tupTypes = tupleTypes tup_t
      let tupLen = length tupTypes
      when (index >= fromIntegral tupLen) $
        expect_ $ Err_Eval_RefOutOfBounds tupLen index
      let result_t = T_Tuple $ replace index val_t tupTypes
      case tup_t == result_t of
        True -> do
          let dle = DLE_TupleSet at tup_a index val_a
          dlv <- ctxt_lift_expr (DLVar at Nothing tup_t) dle
          return (lvl, SLV_DLVar dlv)
        False -> do
          tupvs <- explodeTupleLike "tuple_set" tup_v
          let go i v = if index == i then val_v else v
          let tupvs' = zipWith go [0 ..] tupvs
          return (lvl, SLV_Tuple at tupvs')
    SLPrim_Object -> do
      objm <- mustBeObject =<< one_arg
      vm <-
        mapWithKeyM
          (\k v -> do
             locAt (sss_at v) $
               expect_ty ("value of " <> k) (sss_val v))
          objm
      retV $ (lvl, SLV_Type $ ST_Object vm)
    SLPrim_Object_has -> do
      at <- withAt id
      (obj, bsv) <- two_args
      bs <- mustBeBytes bsv
      vm <- evalAsEnv (lvl, obj)
      retV $ (lvl, SLV_Bool at $ M.member (bunpack bs) vm)
    SLPrim_Object_fields -> do
      at <- withAt id
      uni <- readUniverse
      a <- one_arg
      ty <- mustBeType a
      tm <- case ty of
        ST_Object m -> return m
        _ -> expect_t a $ Err_Expected "object type"
      let tm' = M.map (\t -> SLSSVal at lvl uni (SLV_Type t)) tm
      retV $ (lvl, SLV_Object at Nothing tm')
    SLPrim_Object_set -> do
      at <- withAt id
      (obj_v, field_v, val_v) <- three_args
      (obj_t, obj_a) <- compileTypeOf obj_v
      case obj_t of
        T_Object _ -> return ()
        _ -> expect_t obj_v $ Err_Expected "object"
      (val_t, val_a) <- compileTypeOf val_v
      fieldName <- mustBeBytes field_v
      let fieldName' = bunpack fieldName
      let objFields = M.fromList $ objstrTypes obj_t
      let result_t = T_Object $ M.insert fieldName' val_t objFields
      -- DLE_ObjectSet only applies when updating an existing field,
      -- not when adding a new field or changing a field's type
      case obj_t == result_t of
        True -> do
          let dle = DLE_ObjectSet at obj_a fieldName' val_a
          dlv <- ctxt_lift_expr (DLVar at Nothing obj_t) dle
          return (lvl, SLV_DLVar dlv)
        False -> do
          explodingObject_set <- lookStdlib "explodingObject_set"
          evalApplyVals' explodingObject_set (map (lvl,) [obj_v, SLV_Bytes at fieldName, val_v])
    SLPrim_makeEnum -> do
      at' <- withAt $ srcloc_at "makeEnum" Nothing
      uni <- readUniverse
      case map snd sargs of
        [iv@(SLV_Int _ mt i)] ->
          retV $ (lvl, SLV_Tuple at' (enum_pred : map (SLV_Int at' mt) [0 .. (i -1)]))
          where
            enum_pred = jsClo at' uni "makeEnum" "(x) => ((0 <= x) && (x < M))" (M.fromList [("M", iv)])
        _ -> illegal_args
    SLPrim_App_Delay {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_localf iat who m stf ->
      secret <$> doInteractiveCall sargs iat stf SLM_LocalStep "interact" CT_Assume (\at fs drng dargs -> return $ DLE_Interact at fs who m drng dargs)
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
    SLPrim_currentMode -> do
      am <- readSt st_mode
      let vn = case am of
            SLM_Module -> "Module"
            SLM_AppInit -> "AppInit"
            SLM_Step -> "Step"
            SLM_LocalStep -> "Local"
            SLM_LocalPure -> "Local"
            SLM_ConsensusStep -> "Consensus"
            SLM_ConsensusPure -> "Consensus"
            SLM_Export -> "Export"
      at <- withAt id
      let dt = M.fromList $
            [ ("Module", T_Null)
            , ("AppInit", T_Null)
            , ("Step", T_Null)
            , ("Local", T_Null)
            , ("Consensus", T_Null)
            , ("Export", T_Null)
            ]
      let sv = SLV_Null at "currentMode"
      return $ (lvl, SLV_Data at dt vn sv)
    SLPrim_claim ct -> do
      at <- withAt id
      let barg = compileCheckType msdef T_Bool
      (dargm, mmsg) <- case map snd sargs of
        [arg] -> return $ (barg arg, Nothing)
        [arg, marg] -> do
          bs <- mustBeBytes marg
          return $ (barg arg, Just bs)
        _ -> illegal_args
      darg <- dargm
      doClaim ct darg mmsg
      let good = return $ public $ SLV_Null at "claim"
      let some_good ems = ensure_modes ems ("assert " <> show ct) >> good
      case ct of
        CT_Assume -> some_good [SLM_LocalStep, SLM_LocalPure, SLM_Export]
        CT_Enforce -> good
        CT_Require -> some_good [SLM_ConsensusStep, SLM_ConsensusPure, SLM_Export]
        CT_Assert -> good
        CT_Possible -> good
        CT_Unknowable {} -> impossible "unknowable"
    SLPrim_transfer -> do
      at <- withAt id
      uni <- readUniverse
      pay_sv <-
        case args of
          [a] -> return $ a
          [a, b] -> return $ SLV_Tuple at [SLV_Tuple at [a, b]]
          _ -> illegal_args
      let transferToPrim = SLV_Prim (SLPrim_transfer_amt_to pay_sv)
      return $
        public $
          SLV_Object at (Just "transfer") $
            M.fromList [("to", SLSSVal sb Public uni transferToPrim)]
    SLPrim_transfer_amt_to pay_sv -> do
      at <- withAt id
      ensure_mode SLM_ConsensusStep "transfer"
      part <- one_arg
      who_a <-
        typeOfM part >>= \case
          Just (ty, res) -> typeEq T_Address ty msdef (Just $ srclocOf part)
            >> compileArgExpr_ res
          Nothing ->
            case part of
              SLV_Participant _ who _ _ ->
                is_class who
                  >>= expect_
                    . bool (Err_Transfer_NotBound who) (Err_Transfer_Class who)
              _ -> expect_ $ Err_Type_None part
      DLPayAmt {..} <- compilePayAmt TT_Pay pay_sv
      let one amt_a mtok_a = unless (staticZero amt_a) $ do
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
          dv <- ctxt_lift_expr (DLVar at Nothing dt) (DLE_Impossible at tag $ Err_Impossible_Inspect "forall")
          return $ (olvl, SLV_DLVar dv)
        [one, (tlvl, two)] -> do
          one' <- evalPrim SLPrim_forall [one]
          lvlMeet tlvl <$> evalApplyVals' two [one']
        _ -> illegal_args
    SLPrim_PrimDelay _at dp bargs aargs ->
      evalPrim dp $ bargs <> sargs <> aargs
    SLPrim_part_set -> do
      at <- withAt id
      case map snd sargs of
        [(SLV_Participant _ who _ _), addr] -> do
          addr_da <- compileCheckType msdef T_Address addr
          return $ (lvl, (SLV_Prim $ SLPrim_part_setted at who addr_da))
        _ -> illegal_args
    SLPrim_part_setted {} -> expect_t rator $ Err_Eval_NotApplicable
    SLPrim_Data -> do
      at <- withAt id
      argm <- mustBeObject =<< one_arg
      varm <-
        mapWithKeyM
          (\k v -> do
             locAt (sss_at v) $
               expect_ty ("value of " <> k) (sss_val v))
          argm
      when (M.null varm) $
        expect_ $ Err_Eval_EmptyData
      retV $ (lvl, SLV_Type $ ST_Data (Just at) varm)
    SLPrim_Data_variant mAt t vn vt -> do
      at <- withAt id
      dt <- mapM st2dte t
      vv <- case (vt, args) of
        (ST_Null, []) -> return $ SLV_Null at "variant"
        _ -> one_arg
      void $ typeCheck_s CT_Assert mAt vt vv
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
             (sss_at v,) <$> case sss_val v of
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
        let go (tycon, (case_at, case_args)) = do
              let case_ann = at2a case_at
              case tycon == "default" of
                True -> JSDefault case_ann case_ann case_body
                False -> JSCase case_ann case_id case_ann case_body
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
        let switch_parts = map go $ L.sortBy (\(_, l) (_, r) -> fst l `compare` fst r) $ M.toAscList args_x_case
        let body = JSSwitch ann ann data_param ann ann switch_parts ann semi
        jsArrow ann [data_param, case_param] body
      -- Apply the object and cases to the newly created function
      let fn = snd fnv
      evalApplyVals' fn [(lvl, obj), (lvl, cases)]
    SLPrim_Participant -> makeParticipant False False
    SLPrim_ParticipantClass -> makeParticipant False True
    SLPrim_API -> do
      ensure_mode SLM_AppInit "API"
      uni <- readUniverse
      (nv, intv, alias) <- case args of
        [x] -> return (Nothing, x, Nothing)
        [x, y] ->
          typeOfM x >>= \case
            -- name and interact object
            Just (T_Bytes {}, _) -> return (Just x, y, Nothing)
            -- interact object and alias
            _ -> return (Nothing, x, Just y)
        [x, y, z] -> return (Just x, y, Just z)
        _ -> illegal_args
      n <- mapM mustBeBytes nv
      SLInterface im <- mustBeInterface intv
      aliasEnv <- mapM mustBeObject alias >>= return . fromMaybe mempty
      let mns = bunpack <$> n
      nAt <- withAt id
      let ns = fromMaybe "Untagged" mns
      forM_ mns $ verifyName nAt "API" []
      ix <- flip mapWithKeyM im $ \k -> \(at, ty) ->
        case ty of
          ST_Fun (SLTypeFun {..}) -> do
            let nk = maybe k (<> "_" <> k) mns
            verifyNotReserved nAt nk
            let nkb = bpack nk
            mapM_ warnInteractType $ stf_rng : stf_dom
            let nv' = SLV_Bytes at nkb
            let stf_dom' = ST_Tuple stf_dom
            let mkpre o = jsClo at uni (nk <> "pre") "(mt, dom) => o(dom)" $ M.fromList [("o", o)]
            let stf_pre' = fmap mkpre stf_pre
            let mkpost o = jsClo at uni (nk <> "post") "([dom,rng]) => o(dom,rng)" $ M.fromList [("o", o)]
            let stf_post' = fmap mkpost stf_post
            let in_t = SLTypeFun [] stf_dom' Nothing stf_pre' Nothing stf_pre_msg
            let out_t = SLTypeFun [stf_dom', stf_rng] ST_Null stf_post' Nothing stf_post_msg Nothing
            let fake tf = SLSSVal at Public uni $ SLV_Type $ ST_Fun tf
            let nkm = M.fromList $ [("in", fake in_t), ("out", fake out_t)]
            let intv' = SLV_Object at (Just $ nk <> " interact") nkm
            it <- IT_Fun <$> mapM st2dte stf_dom <*> st2dte stf_rng
            m_alias <- mapM (mustBeBytes . sss_val) $ M.lookup k aliasEnv
            (M.singleton nk (m_alias, stf_dom),,) (nkb, it) <$> (sls_sss at uni <$> makeParticipant_ True True nv' intv')
          t -> expect_ $ Err_API_NotFun k t
      let aliases = M.unions $ M.elems $ M.map fst3 ix
      let i' = M.map snd3 ix
      let io = M.map thd3 ix
      aisiPut aisi_res $ \ar ->
        ar { ar_apis = M.insertWith M.union n i' $ ar_apis ar
           , ar_api_alias = M.union aliases $ ar_api_alias ar }
      retV $ (lvl, SLV_Object nAt (Just $ ns <> " API") io)
    SLPrim_View -> do
      ensure_mode SLM_AppInit "View"
      (nv, intv, alias) <- case args of
        [x] -> return (Nothing, x, Nothing)
        [x, y] ->
          typeOfM x >>= \case
            Just (T_Bytes {}, _) -> return (Just x, y, Nothing)
            _ -> return (Nothing, x, Just y)
        [x, y, z] -> return (Just x, y, Just z)
        _ -> illegal_args
      n <- mapM mustBeBytes nv
      SLInterface im <- mustBeInterface intv
      aliasEnv <- mapM mustBeObject alias >>= return . fromMaybe mempty
      let mns = bunpack <$> n
      nAt <- withAt id
      uni <- readUniverse
      let mustBeTupleOfBytes ma = do
            a <- mustBeTuple $ sss_val ma
            mapM mustBeBytes a
      aliases <- concatMapM (fmap (fmap bunpack) . mustBeTupleOfBytes) $ M.elems aliasEnv
      mapM_ (verifyName nAt "View" $ M.keys im <> aliases) mns
      let ns = fromMaybe "Untagged" mns
      let go k (at, t) = do
            warnInteractType t
            m_alias <- fromMaybe [] <$> mapM mustBeTupleOfBytes (M.lookup k aliasEnv)
            when (isNothing nv) $ do
              let names = k : map bunpack m_alias
              mapM_ (verifyName at "View" []) names
              mapM_ (verifyNotReserved at) names
            let vv = SLV_Prim $ SLPrim_viewis at n k t
            let vom = M.singleton "set" $ SLSSVal at Public uni vv
            let vo = SLV_Object at (Just $ ns <> " View, " <> k) vom
            let io = SLSSVal at Public uni vo
            let dvw_at = at
            dvw_it <-
              case t of
                ST_Fun (SLTypeFun {..}) ->
                  IT_Fun <$> mapM st2dte stf_dom <*> st2dte stf_rng
                ST_UDFun {} ->
                  expect_ $ Err_View_UDFun
                _ -> IT_Val <$> st2dte t
            let dvw_as = m_alias
            return $ (DLView {..}, (m_alias, io))
      ix <- mapWithKeyM go im
      let i' = M.map fst ix
      let io = M.map (snd . snd) ix
      let va = M.map (fst . snd) ix
      -- Merge untagged views which have `Nothing` key
      aisiPut aisi_res $ \ar ->
        ar { ar_views = M.insertWith M.union n i' $ ar_views ar
           , ar_view_alias = M.union va $ ar_view_alias ar }
      retV $ (lvl, SLV_Object nAt (Just $ ns <> " View") io)
    SLPrim_Map -> illegal_args
    SLPrim_Map_new -> do
      (ktv, tv) <- case args of
        [x] -> return $ (SLV_Type ST_Address, x)
        [x, y] -> return $ (x, y)
        _ -> illegal_args
      kt <- expect_ty "Map.new" ktv
      t <- expect_ty "Map.new" tv
      ensure_mode SLM_ConsensusStep "Map.new"
      kt' <- st2dte kt
      t' <- st2dte t
      mv <- mapNew kt' t'
      retV $ public $ SLV_Map mv
    SLPrim_Map_reduce withKey -> do
      at <- withAt id
      uni <- readUniverse
      (m, z, f_) <- three_args
      mv <-
        case m of
          SLV_Map mv -> return $ mv
          _ -> expect_t m $ Err_Expected_Map
      mi <- mapLookup mv
      let f_' = if withKey then f_ else jsClo at uni "keyWrapper" ("(b, k, a) => f(b, a)") (M.fromList [("f", f_)])
      let f = jsClo at uni "reduceWrapper" ("(b, k, ma) => ma.match({None: (() => b), Some: (a => f(b, k, a))})") (M.fromList [("f", f_')])
      ensure_while_invariant $ "Map.reduce" <> (if withKey then "WithKey" else "")
      (z_ty, z_da) <- compileTypeOf z
      (b_dv, b_dsv) <- make_dlvar at z_ty
      (k_dv, k_dsv) <- make_dlvar at $ dlmi_kt mi
      (ma_dv, ma_dsv) <- make_dlvar at $ dlmi_tym mi
      SLRes f_lifts _ f_da <-
        captureRes $ do
          (f_lvl, f_v) <- evalApplyVals' f [(lvl, b_dsv), (lvl, k_dsv), (lvl, ma_dsv)]
          ensure_level lvl f_lvl
          (f_ty, f_da) <- compileTypeOf f_v
          typeEq z_ty f_ty (Just $ srclocOf z) (Just $ srclocOf f_v)
          return $ f_da
      (ans_dv, ans_dsv) <- make_dlvar at z_ty
      let f_bl = DLSBlock at [] f_lifts f_da
      mri <- ctxt_alloc
      saveLift $ DLS_MapReduce at mri (v2lv ans_dv) mv z_da (v2vl b_dv) (v2vl k_dv) (v2vl ma_dv) f_bl
      return $ (lvl, ans_dsv)
    SLPrim_Refine -> do
      at <- withAt id
      uni <- readUniverse
      let mkClo = jsClo at uni "refine"
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
          void $ typeCheck_s CT_Assert (Just $ srclocOf y) t x
          return $ (lvl, x)
    SLPrim_remote -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote"
      uni <- readUniverse
      (av, ri, ma) <- two_mthree_args
      at <- withAt id
      aa <- compileCheckType msdef T_Contract av
      rm_ <- mustBeObject ri
      ae <- fromMaybe mempty <$> mapM mustBeObject ma
      rm <-
        mapWithKeyM
          (\k v -> do
             locAt (sss_at v) $
               expect_ty ("value of " <> k) (sss_val v))
          rm_
      let go k = \case
            ST_Fun stf -> do
              m_alias <- mapM (return . bunpack <=< mustBeBytes . sss_val) $ M.lookup k ae
              -- XXX support raw calls
              let k' = Just $ fromMaybe k m_alias
              return $
                SLSSVal at Public uni $
                  SLV_Prim $
                    SLPrim_remotef at aa k' stf Nothing Nothing Nothing Nothing
            t -> expect_ $ Err_Remote_NotFun k t
      om <- mapWithKeyM go rm
      return $ (lvl, SLV_Object at Nothing om)
    SLPrim_remotef rat aa ma stf _ mbill malgo (Just RFM_Pay) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote pay"
      payv <- one_arg
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa ma stf (Just payv) mbill malgo Nothing)
    SLPrim_remotef rat aa ma stf mpay _ malgo (Just RFM_Bill) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote bill"
      billv <- one_arg
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa ma stf mpay (Just $ Right billv) malgo Nothing)
    SLPrim_remotef rat aa ma stf mpay _ malgo (Just RFM_WithBill) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote withBill"
      at <- withAt id
      nonNetToks <- zero_mone_arg $ SLV_Tuple at []
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa ma stf mpay (Just $ Left nonNetToks) malgo Nothing)
    SLPrim_remotef rat aa ma stf mpay mbill _ (Just RFM_ALGO) -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote ALGO"
      metam <- mustBeObject =<< one_arg
      metam' <- mapM (ensure_public . sss_sls) metam
      let keys = M.keysSet metam'
      let validKeys = S.fromList $ [ "fees", "accounts", "assets", "addressToAccount", "apps", "boxes", "onCompletion", "strictPay", "rawCall", "simNetRecv", "simTokensRecv", "simReturnVal", "txnOrderForward" ]
      unless (keys `S.isSubsetOf` validKeys) $ do
        expect_ $ Err_Remote_ALGO_extra $ S.toAscList $
          keys `S.difference` validKeys
      let metal f k = k (M.lookup f metam')
      at <- withAt id
      let expectBool lab = metal lab $ \case
            Nothing -> return $ False
            Just (SLV_Bool _ b) -> return b
            Just _ -> expect_ $ Err_Remote_ALGO_extra $ [ lab <> " with non-compile time value" ]
      ra_fees <- metal "fees" $ \case
        Nothing -> return $ DLA_Literal $ DLL_Int at UI_Word 0
        Just v -> compileCheckType msdef (T_UInt UI_Word) v
      ra_strictPay <- expectBool "strictPay"
      ra_rawCall <- expectBool "rawCall"
      ra_txnOrderForward <- expectBool "txnOrderForward"
      ra_accounts <- metal "accounts" $ \case
        Nothing -> return $ mempty
        Just v -> do
          vs <- explodeTupleLike "REMOTE_FUN.ALGO.accounts" v
          mapM (compileCheckType msdef T_Address) vs
      ra_assets <- metal "assets" $ \case
        Nothing -> return $ mempty
        Just v -> do
          vs <- explodeTupleLike "REMOTE_FUN.ALGO.assets" v
          mapM (compileCheckType msdef T_Token) vs
      ra_apps <- metal "apps" $ \case
        Nothing -> return $ mempty
        Just v -> do
          vs <- explodeTupleLike "REMOTE_FUN.ALGO.apps" v
          mapM (compileCheckType msdef T_Contract) vs
      ra_boxes <- metal "boxes" $ \case
        Nothing -> return $ mempty
        Just v -> do
          vs <- explodeTupleLike "REMOTE_FUN.ALGO.boxes" v
          forM vs $ \tv -> do
            (t, da) <- compileTypeOf tv
            case t of
              T_Tuple [ T_UInt UI_Word, T_Bytes n ] | n <= 64 -> do
                return da
              _ ->
                expect_ $ Err_Remote_ALGO_extra $ [ "values in boxes array must be a UInt and a Bytes(n) where N <= 64" ]
      ra_addr2acc <- expectBool "addressToAccount"
      ra_onCompletion <- metal "onCompletion" $ \case
        Nothing -> return $ RA_NoOp
        Just (SLV_Bytes _ "NoOp") -> return $ RA_NoOp
        Just (SLV_Bytes _ "OptIn") -> return $ RA_OptIn
        Just (SLV_Bytes _ "CloseOut") -> return $ RA_CloseOut
        Just (SLV_Bytes _ "ClearState") -> return $ RA_ClearState
        Just (SLV_Bytes _ "UpdateApplication") -> return $ RA_UpdateApplication
        Just (SLV_Bytes _ "DeleteApplication") -> return $ RA_DeleteApplication
        Just _ -> expect_ $ Err_Remote_ALGO_extra $ [ "illegal value for onCompletion" ]
      let locAtOf = locAt . srclocOf
      let compileUInt = compileCheckType msdef (T_UInt UI_Word)
      ra_simNetRecv <- metal "simNetRecv" $ maybe (pure argLitZero) (\x -> locAtOf x $ compileUInt x)
      ra_simTokensRecv <- metal "simTokensRecv" $ maybe (pure RA_Unset) $ \case
        SLV_Tuple tupAt amts -> locAt tupAt $ RA_List tupAt <$> mapM compileUInt amts
        _ -> expect_ $ Err_Remote_ALGO_extra ["simTokensRecv must be a Tuple of UInts"]
      rngTy <- st2dte $ stf_rng stf
      ra_simReturnVal <- metal "simReturnVal" $ \case
        Just x -> locAtOf x $ Just <$> compileCheckType msdef rngTy x
        Nothing -> return Nothing
      let malgo = Just $ DLRemoteALGO {..}
      return $ (lvl, SLV_Prim $ SLPrim_remotef rat aa ma stf mpay mbill malgo Nothing)
    SLPrim_remotef rat aa ma stf mpay mbill malgo Nothing -> do
      ensure_modes [SLM_ConsensusStep, SLM_ConsensusPure] "remote"
      at <- withAt id
      uni <- readUniverse
      let zero = SLV_Int at nn 0
      let amtv = fromMaybe zero mpay
      payAmt <- compilePayAmt TT_Pay amtv
      tokenPay Nothing (pa_net payAmt) "balance sufficient for payment to remote object"
      forM_ (pa_ks payAmt) $ \(a, t) -> do
        tokenPay (Just t) a "balance sufficient for payment to remote object"
      let dzero = argLitZero
      billAmt <- case fromMaybe (Left zero) mbill of
        Left _ -> return $ DLPayAmt dzero []
        Right v -> compilePayAmt TT_Bill v
      (nBilled, nntbRecv) <- getBillTokens mbill billAmt
      let nntbC = length nntbRecv
      let nntbT = ST_Tuple $ replicate nntbC $ ST_UInt UI_Word
      let nntbNo = nntbC == 0
      let nntbTL = if nntbNo then [] else [nntbT]
      let SLTypeFun dom rng pre post pre_msg post_msg = stf
      let rng' = ST_Tuple $ [ST_UInt UI_Word] <> nntbTL <> [rng]
      drng <- st2dte rng'
      rt <- st2dte rng
      let postArg = "(dom, [_," <> (if nntbNo then "" else " _,") <> " rng])"
      let post' = flip fmap post $ \postv ->
            jsClo at uni "post" (postArg <> " => post(dom, rng)") $
              M.fromList [("post", postv)]
      let stf' = SLTypeFun dom rng' pre post' pre_msg post_msg
      allTokens <- fmap DLA_Var <$> readSt st_toks
      let nnToksNotBilled = allTokens \\ nntbRecv
      let withBill = DLWithBill nBilled nntbRecv nnToksNotBilled
      let DLRemoteALGO {..} = fromMaybe zDLRemoteALGO malgo
      ra_simTokensRecv' <- fmap RA_Tuple . compileArgExpr_ =<< case ra_simTokensRecv of
        -- If user didn't give simTokensRecv, generate default where the ctc receives zero of every token
        RA_Unset -> return $ DLAE_Tuple $ replicate nntbC $ DLAE_Arg $ DLA_Literal $ DLL_Int at UI_Word 0
        RA_List _ amts | length amts == nntbC -> return $ DLAE_Tuple $ map DLAE_Arg amts
        RA_List at' _ -> locAt at' $ expect_ $ Err_Remote_ALGO_extra ["Length of simTokensRecv must match the number of tokens billed"]
        RA_Tuple _ -> impossible "RA_Tuple"
      let ralgo = DLRemoteALGO { ra_simTokensRecv = ra_simTokensRecv', .. }
      res' <-
        doInteractiveCall
          sargs
          rat
          (Left stf')
          SLM_ConsensusStep
          "remote"
          CT_Enforce
          (\_ fs _ dargs -> do
            let dr = DLRemote ma payAmt dargs withBill ralgo
            rr <- ctxt_lift_expr (DLVar at Nothing drng) $ DLE_Remote at fs aa rt dr
            el <- compileToVar =<< doInternalLog Nothing (SLV_DLVar rr)
            return $ DLE_Arg at $ DLA_Var el)
      apdvv <- doArrRef_ res' zero
      let getRemoteResults = do
            case nntbNo of
              True -> return (1, Nothing)
              False -> do
                nnTokAmts <- doArrRef_ res' $ SLV_Int at nn 1
                return (2, Just nnTokAmts)
      (resi, mNonNetToksRecv) <- getRemoteResults
      res <- doArrRef_ res' $ SLV_Int at nn resi
      doBalanceUpdate Nothing (S_ADD $ Just PV_None) apdvv
      case fromMaybe (Right zero) mbill of
        Left _ -> do
          forM_ (zip nntbRecv [0 .. length nntbRecv - 1]) $ \(t, i) ->
            case mNonNetToksRecv of
              Nothing -> return ()
              Just nnTokAmts -> do
                doBalanceUpdate (Just t) (S_ADD $ Just PV_None)
                  =<< doArrRef_ nnTokAmts (SLV_Int at nn $ fromIntegral i)
          return $ public res'
        Right _ -> do
          sv <- argToSV $ pa_net billAmt
          forM_ (pa_ks billAmt) $ \(a, t) -> do
            a' <- argToSV a
            doBalanceUpdate (Just t) (S_ADD $ Just PV_None) a'
          -- Ensure we're paid expected network tokens
          cmp_v <- evalApplyVals' (SLV_Prim $ SLPrim_op S_PEQ) [public sv, public apdvv]
          void $
            evalApplyVals' (SLV_Prim $ SLPrim_claim $ CT_Enforce) $
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
    SLPrim_init -> do
      zero_args
      retV $ public $ SLV_Prim SLPrim_inited
    SLPrim_inited -> illegal_args
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
                opt opt_at acc v >>= \case
                  Right x -> return x
                  Left x -> expect_thrown opt_at $ Err_App_InvalidOptionValue k x
      dlo <- ae_dlo <$> aisiGet aisi_env
      dlo' <- foldrWithKeyM use_opt dlo opts
      aisiPut aisi_env $ \ae -> ae { ae_dlo = dlo' }
      return $ public $ SLV_Null at "setOptions"
    SLPrim_adaptReachAppTupleArgs -> do
      tat <- withAt id
      tvs <- mustBeTuple =<< one_arg
      let adapt_tuple at p' who int = do
            liftIO $ emitWarning (Just at) $ W_Deprecated $ D_ParticipantTuples
            snd <$> evalPrim p' (map public $ [who, int])
      let go = \case
            SLV_Tuple at [who, int] ->
              adapt_tuple at SLPrim_Participant who int
            SLV_Tuple at [SLV_Bytes _ "class", who, int] ->
              adapt_tuple at SLPrim_ParticipantClass who int
            x -> return x
      tvs' <- mapM go tvs
      return (lvl, SLV_Tuple tat tvs')
    SLPrim_verifyMuldiv -> do
      at <- withAt id
      (x, y, z) <- three_args
      args' <- mapM (compileCheckType msdef $ T_UInt UI_Word) [x, y, z]
      m <- readSt st_mode
      cl <- case m of
        SLM_Export -> return $ CT_Enforce
        mode
          | isLocalStep mode -> return $ CT_Assume
          | isConsensusStep mode -> return $ CT_Require
          | otherwise -> return $ CT_Assert
      let err = Err_Impossible_Inspect "verifyMulDiv"
      fs <- asks e_stack
      ctxt_lift_eff $ DLE_VerifyMuldiv at fs cl args' err
      return (lvl, SLV_Null at "verifyMulDiv")
    SLPrim_didPublish -> do
      ensure_mode SLM_LocalStep "local"
      ensure_after_first
      zero_args
      evalPrim (SLPrim_fluid_read FV_didSend) []
    SLPrim_unstrict -> do
      x <- one_arg
      locUseUnstrict True $ evalApply x []
    SLPrim_polyNeq -> do
      (x, y) <- two_args
      notFn <- unaryToPrim (JSUnaryOpNot JSNoAnnot)
      eqFn <- evalPrimOp S_PEQ $ map (lvl,) [x, y]
      evalApplyVals' notFn [eqFn]
    SLPrim_getContract -> getContractInfo GET_CONTRACT
    SLPrim_getAddress -> getContractInfo GET_ADDRESS
    SLPrim_getCompanion -> getContractInfo GET_COMPANION
    SLPrim_ALGOBlockField bf -> do
      ensure_mode SLM_ConsensusStep "ALGO.Block"
      nv <- one_arg
      na <- compileCheckType msdef (T_UInt UI_Word) nv
      at <- withAt id
      let mdv = DLVar at Nothing $ maybeT $ abfType bf
      dv <- ctxt_lift_expr mdv $ DLE_PrimOp at (ALGO_BLOCK bf) [na]
      dv' <- doInternalLog_ Nothing dv
      return $ (lvl, SLV_DLVar dv')
    SLPrim_EmitLog -> do
      (x, y) <- two_args
      ma <- mustBeBytes y
      public <$> doInternalLog (Just ma) x
    SLPrim_Event -> do
      ensure_mode SLM_AppInit "Event"
      uni <- readUniverse
      (nv, intv) <- case args of
        [x] -> return (Nothing, x)
        [x, y] -> return (Just x, y)
        _ -> illegal_args
      at <- withAt id
      n <- mapM mustBeBytes nv
      im <- eventInterface intv
      let mns = bunpack <$> n
      mapM_ (verifyName at "Event" (M.keys im)) mns
      let ns = fromMaybe "Untagged" mns
      ix <- flip mapWithKeyM im $ \k (at', tys) -> do
        mapM_ warnInteractType tys
        when (isNothing nv) $ do
          verifyName at "Event" [] k
          verifyNotReserved at k
        let v = SLV_Prim $ SLPrim_event_is n k tys
        let io = SLSSVal at' Public uni v
        di <- flip mapM tys $ \ty ->
          case st2dt ty of
            Nothing -> expect_ $ Err_Type_NotDT ty
            Just dt -> return dt
        return $ (di, io)
      let i' = M.map fst ix
      let io = M.map snd ix
      aisiPut aisi_res $ \ar ->
        ar {ar_events = M.insert n i' $ ar_events ar}
      retV $ (lvl, SLV_Object at (Just $ ns <> " Event") io)
    SLPrim_event_is eventLabel which tys -> do
      at <- withAt id
      typedArgs <- zipEq (Err_Apply_ArgCount at) tys args
      vs <- flip mapM typedArgs $ \(ty, arg) ->
        (snd <$> evalPrim SLPrim_is [public arg, public $ SLV_Type ty])
      void $ doEmitLog False (Just $ (eventLabel, which)) Nothing vs
      return $ public $ SLV_Null at "event_is"
    SLPrim_getUntrackedFunds -> do
      marg <- mone_arg
      mtok <- flip mapM marg $ \arg -> do
        (ty, da) <- compileTypeOf arg
        void $ mustBeToken ty
        return da
      at <- withAt id
      let mdv = DLVar at Nothing $ T_UInt UI_Word
      fvBal <- getBalanceOf' mtok
      let trackedBal = DLA_Var fvBal
      untrackedFunds <- ctxt_lift_expr mdv $ DLE_GetUntrackedFunds at mtok trackedBal
      dv <- ctxt_lift_expr mdv $ DLE_PrimOp at (ADD UI_Word PV_Safe) [DLA_Var untrackedFunds, trackedBal]
      setBalance TM_Balance mtok (DLA_Var dv)
      return (lvl, SLV_DLVar untrackedFunds)
    SLPrim_isDataVariant -> do
      at <- withAt id
      (nameB, variantTuple, inst) <- three_args
      nameB' <- mustBeBytes nameB
      let name = bunpack nameB'
      variants <- mustBeTuple variantTuple
      variantNamesB <- mapM mustBeBytes variants
      let variantNames = map bunpack variantNamesB
      let givenTags = dataTagMap $ T_Data $ M.fromList $ zip variantNames
           $ map (const T_Bool) variantNames
      let tagNumM = givenTags M.!? name
      tagNum <- case tagNumM of
        Just x -> return x
        Nothing -> expect_ $ Err_Switch_MissingCases [name]
      let tagNumArg = DLA_Literal $ DLL_Int at UI_Word tagNum
      case inst of
        SLV_Data _ dmap vn _ -> do
          let tm = dataTagMap $ T_Data dmap
          _ <- tagMapsEqual tm givenTags
          let i = fromMaybe (impossible "SLV_Data missing key") $ tm M.!? vn
          return (lvl, SLV_Bool at (i == tagNum))
        _ -> do
          (t, a) <- compileTypeOf inst
          _ <- mustBeDataTy Err_Switch_NotData t
          let tm = dataTagMap $ t
          _ <- tagMapsEqual tm givenTags
          act <- ctxt_lift_expr (DLVar at Nothing (T_UInt UI_Word)) $ DLE_DataTag at a
          eq <- ctxt_lift_expr (DLVar at Nothing T_Bool)
            $ DLE_PrimOp at (PEQ UI_Word) [DLA_Var act, tagNumArg]
          return (lvl, SLV_DLVar eq)
      where
        tagMapsEqual actual given = case nameDiff actual given of
          [] -> case nameDiff given actual of
            [] -> return ()
            extras -> expect_ $ Err_Switch_ExtraCases extras
          missing -> expect_ $ Err_Switch_MissingCases missing
        nameDiff x y = map fst $ M.toAscList $ M.difference x y
    SLPrim_fromSome -> do
      (mv, dv) <- two_args
      at <- withAt id
      case mv of
        SLV_Data _ _ vn vv ->
          case vn of
            "Some" -> return (lvl, vv)
            -- XXX Maybe be better in strict mode?
            _ -> return (lvl, dv)
        _ -> do
          (mvt, ma) <- compileTypeOf mv
          mvt_tm <- mustBeDataTy Err_Switch_NotData mvt
          case M.toAscList mvt_tm of
            [("None", T_Null), ("Some", st)] -> do
              da <- compileCheckType (Just $ srclocOf mv) st dv
              let mkv = DLVar at Nothing st
              let e = DLE_FromSome at ma da
              fsv <- ctxt_lift_expr mkv e
              return (lvl, SLV_DLVar fsv)
            cs -> do
              expect_ $
                Err_Switch_MissingCases $
                  S.toList $
                    S.delete "Some" $
                      S.delete "None" $
                        S.fromList $ map fst cs
    SLPrim_distinct -> do
      (,) lvl <$> evalDistinctTokens [] args
    SLPrim_xor -> do
      (x, y) <- two_args
      (x_ty, _) <- compileTypeOf x
      (y_ty, _) <- compileTypeOf y
      case (x_ty, y_ty) of
        (T_UInt x_ui, T_UInt y_ui) | x_ui == y_ui -> prim S_BXOR
        (T_Digest, T_Digest) -> prim S_DIGEST_XOR
        (T_Bytes l, T_Bytes r) | l == r -> prim S_BYTES_XOR
        (T_Bool, T_Bool) -> do
          f <- lookStdlib "boolXor"
          evalApplyVals' f sargs
        (l, r) -> expect_ $ Err_xor_Types l r
      where
        prim = flip evalPrimOp sargs
    SLPrim_mod -> do
      (x, y) <- two_args
      (x_ty, _) <- compileTypeOf x
      (y_ty, _) <- compileTypeOf y
      let go isDigest = do
            bi <- evalPrimOp (S_BTOI_LAST8 isDigest) [public x]
            evalPrimOp (S_MOD Nothing) [bi, public y]
      case (x_ty, y_ty) of
        (T_UInt x_ui, T_UInt y_ui) | x_ui == y_ui -> evalPrimOp (S_MOD Nothing) sargs
        (T_Bytes {}, T_UInt UI_Word) -> go False
        (T_Digest, T_UInt UI_Word) -> go True
        (l, r) -> expect_ $ Err_mod_Types l r
    SLPrim_ContractCode -> do
      at <- withAt id
      ccv <- one_arg
      case ccv of
        SLV_Object _ _ o -> do
          cns <- readDlo dlo_connectors
          let f cn c = do
                let cnv = t2s cn
                co <- sss_val <$> env_lookup (LC_RefFrom "ContractCode") cnv o
                let SrcLoc _ _ msrc = srclocOf co
                dir <-
                  case msrc of
                    Just (ReachSourceFile fp) ->
                      return $ takeDirectory fp
                    _ -> liftIO $ getCurrentDirectory
                ov <- slToJSON co
                (liftIO $ withCurrentDirectory dir $ conCompileCode c ov) >>= \case
                  Right x -> return x
                  Left x -> expect_ $ Err_ContractCode cn x
          public <$> (SLV_ContractCode at <$> mapWithKeyM f cns)
        _ -> expect_t ccv $ Err_Expected "Object or Reach.App"
    SLPrim_Contract_new -> do
      let lab = "new Contract"
      let expectContractCode = \case
            SLV_ContractCode _ cc -> return cc
            ow -> expect_t ow $ Err_Expected "ContractCode or Reach.App"
      cns <- readDlo dlo_connectors
      (cc, con_x_opts) <-
        case args of
          [x@(SLV_Prim (SLPrim_App_Delay {}))] -> do
            compileProg <- asks e_compileProg
            co <- compileProg x

            let forEachConn f = forWithKeyM cns $ \cn c ->
                                  either impossible return $ f c $ M.lookup cn co
            co'  <- forEachConn conCompileConnectorInfo
            opts <- forEachConn conContractNewOpts
            return (co', opts)
          [x] -> (, mempty) <$> expectContractCode x
          [x, y] -> do
            x' <- expectContractCode x
            y' <- mustBeObject y
            opts <- forWithKeyM cns $ \cn c -> do
                let cnv = t2s cn
                let ctx = LC_RefFrom lab
                moptsv <-
                  case M.member cnv y' of
                    True -> do
                      sv <- sss_val <$> env_lookup ctx cnv y'
                      return $ Just sv
                    False -> return Nothing
                mopts <- traverse slToJSON moptsv
                opts' <- either (expect_ . Err_ContractCode cn) return $
                           conContractNewOpts c mopts
                return opts'
            return (x', opts)
          _ -> illegal_args
      let cc' = M.mapKeys t2s cc
      dcns <- forWithKeyM cns $ \cn _ -> do
        let cnv = t2s cn
        let ctx = LC_RefFrom lab
        dcn_code <- env_lookup_ ctx cnv (const False) cc'
        let dcn_opts = fromMaybe (impossible "No connector options") $ M.lookup cn con_x_opts
        return $ DLContractNew {..}
      return $ public $ SLV_Prim $ SLPrim_Contract_new_ctor dcns
    SLPrim_Contract_new_ctor dcns -> do
      at <- withAt id
      let lab = "new Contract"
      ensure_mode SLM_ConsensusStep lab
      -- XXX support all these options for real
      let payAmt = DLPayAmt argLitZero mempty
      dargs <- map snd <$> mapM compileTypeOf args
      let withBill = DLWithBill False mempty mempty
      let ralgo = zDLRemoteALGO
      let dr = DLRemote Nothing payAmt dargs withBill ralgo
      ctcdv_ <-
        ctxt_lift_expr (DLVar at Nothing T_Contract) $
          DLE_ContractNew at dcns dr
      ctcdv <- doInternalLog_ Nothing ctcdv_
      return $ public $ SLV_DLVar ctcdv
    SLPrim_BytesDynCast -> do
      case args of
        [SLV_Bytes at bs] -> return $ (lvl, SLV_BytesDyn at bs)
        [x] -> do
          at <- withAt id
          (ty, dla) <- compileTypeOf x
          case ty of
            T_Bytes _ -> return ()
            _ -> illegal_args
          dv <- ctxt_lift_expr (DLVar at Nothing T_BytesDyn) $ DLE_BytesDynCast at dla
          return $ (lvl, SLV_DLVar dv)
        _ -> illegal_args
    SLPrim_toStringDyn -> first_arg >>= \case
      SLV_Bytes at bs -> return $ (lvl, SLV_String at $ T.pack $ bunpack bs)
      v -> do
        (dt, _) <- compileTypeOf v
        case dt of
          T_UInt ui -> evalPrimOp (S_UINT_TO_STRINGDYN ui) [ (lvl, v) ]
          _ -> expect_t v $ Err_Expected "Bytes or UInt"
    SLPrim_Bytes_fromHex -> do
      at <- withAt id
      hs <- mustBeBytes =<< one_arg
      let hsNoPrefix = B.drop 2 hs
      bs <- either (const $ expect_ Err_BytesFromHex_Invalid) return $ B16.decodeBase16 hsNoPrefix
      return (lvl, SLV_Bytes at bs)
    SLPrim_Contract_fromAddress -> do
      at <- withAt id
      ensure_mode SLM_ConsensusStep "Contract.fromAddress"
      x <- one_arg
      xa <- compileCheckType msdef T_Address x
      let mkv = DLVar at Nothing $ maybeT T_Contract
      let e = DLE_ContractFromAddress at xa
      fsv <- ctxt_lift_expr mkv e
      fsv' <- doInternalLog_ Nothing fsv
      return (lvl, SLV_DLVar fsv')
    -- END OF evalPrim cases
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
    mone_arg = case args of
      [] -> return Nothing
      [x] -> return $ Just x
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
    mustBeToken = \case
      T_Token -> return $ T_Token
      _ -> illegal_args
    make_dlvar at' ty = do
      dv <- ctxt_mkvar $ DLVar at' Nothing ty
      return $ (dv, SLV_DLVar dv)
    eventInterface intv = do
      objEnv <- mustBeObject intv
      let checkInt = \case
            SLSSVal tAt _ _ (SLV_Tuple _ ts) ->
              (tAt,) <$> mapM (expect_ty "Event") ts
            SLSSVal idAt _ _ idV -> locAt idAt $ expect_t idV $ Err_App_InvalidInteract
      mapM checkInt objEnv
    mustBeInterface intv = do
      objEnv <- mustBeObject intv
      let checkint = \case
            SLSSVal t_at _ _ (SLV_Type t) -> return $ (t_at, t)
            SLSSVal idAt _ _ idV -> locAt idAt $ expect_t idV $ Err_App_InvalidInteract
      SLInterface <$> mapM checkint objEnv
    makeParticipant :: Bool -> Bool -> App SLSVal
    makeParticipant isAPI isClass = do
      (nv, intv) <- two_args
      makeParticipant_ isAPI isClass nv intv
    makeParticipant_ :: Bool -> Bool -> SLVal -> SLVal -> App SLSVal
    makeParticipant_ isAPI isClass nv intv = do
      ensure_mode SLM_AppInit "Participant Constructor"
      at <- withAt id
      n <- mustBeBytes nv
      let ns = bunpack n
      verifyName at "Participant" [] ns
      int <- mustBeInterface intv
      (io, ienv) <- makeInteract n int
      aisiPut aisi_env $ \ae ->
        ae {ae_ios = M.insert n io $ ae_ios ae}
      aisiPut aisi_res $ \ar ->
        ar {ar_pie = M.insert n ienv $ ar_pie ar}
      when isAPI $ do
        aisiPut aisi_res $ \ar ->
          ar {ar_isAPI = S.insert n $ ar_isAPI ar}
        aisiPut aisi_env $ \ae ->
          ae {ae_apis = S.insert n $ ae_apis ae}
      when isClass $ do
        aisiPut aisi_env $ \ae ->
          ae {ae_classes = S.insert n $ ae_classes ae}
      return (lvl, SLV_Participant at n Nothing Nothing)
    getContractInfo po = do
      zero_args
      let t = case po of
            GET_ADDRESS -> T_Address
            GET_CONTRACT -> T_Contract
            GET_COMPANION -> maybeT T_Contract
            _ -> impossible "getContractInfo"
      ensure_after_first
      at <- withAt id
      let mdv = DLVar at Nothing t
      dv <- ctxt_lift_expr mdv $ DLE_PrimOp at po []
      return $ (lvl, SLV_DLVar dv)

doInteractiveCall :: [SLSVal] -> SrcLoc -> Either SLTypeFun SLType -> SLMode -> String -> ClaimType -> (SrcLoc -> [SLCtxtFrame] -> DLType -> [DLArg] -> App DLExpr) -> App SLVal
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

doInternalLog :: Maybe SLPart -> SLVal -> App SLVal
doInternalLog ma x = doEmitLog True Nothing ma [x]

doInternalLog_ :: Maybe SLPart -> DLVar -> App DLVar
doInternalLog_ ma x = expectDLVar <$> doEmitLog_ True Nothing ma [x]

doEmitLog :: Bool -> Maybe (Maybe SLPart, String) -> Maybe SLPart -> [SLVal] -> App SLVal
doEmitLog isInternal ml ma vs =
  doEmitLog_ isInternal ml ma =<< mapM compileToVar vs

doEmitLog_ :: Bool -> Maybe (Maybe SLPart, String) -> Maybe SLPart -> [DLVar] -> App SLVal
doEmitLog_ isInternal ml ma dvs = do
  ensure_mode SLM_ConsensusStep "emitLog"
  at <- withAt id
  let tys = map varType dvs
  case (isInternal, ml, dvs, tys) of
    (True, Nothing, [_], [ty]) -> do
      let lk = maybe L_Internal L_Api ma
      dv <- ctxt_lift_expr (DLVar at Nothing ty) $ DLE_EmitLog at lk dvs
      return $ SLV_DLVar dv
    (False, Just (ml', l), vs, _) -> do
      ctxt_lift_eff $ DLE_EmitLog at (L_Event ml' l) vs
      return $ SLV_Null at "emitLog"
    _ -> impossible "doEmitLog_: Expected one arg for internal emitLog"

assertRefinedArgs :: ClaimType -> [SLSVal] -> SrcLoc -> SLTypeFun -> App (SLVal, [DLArgExpr])
assertRefinedArgs ct sargs iat (SLTypeFun {..}) = do
  let argvs = map snd sargs
  arges <-
    mapM (uncurry $ typeCheck_s ct (Just iat))
      =<< zipEq (Err_Apply_ArgCount iat) stf_dom argvs
  at <- withAt id
  let dom_tupv = SLV_Tuple at argvs
  forM_ stf_pre $ \domp ->
    applyRefinement ct domp [dom_tupv] stf_pre_msg
  return (dom_tupv, arges)

evalApplyVals' :: SLVal -> [SLSVal] -> App SLSVal
evalApplyVals' rator randvs = do
  SLAppRes _ val <- evalApplyVals rator randvs
  return val

litToSV :: DLLiteral -> App SLVal
litToSV = \case
  DLL_Null -> withAt $ flip SLV_Null "litToSV"
  DLL_Bool b -> withAt $ flip SLV_Bool b
  DLL_Int a t i -> return $ SLV_Int a (Just t) i
  DLL_TokenZero -> return $ SLV_DLC DLC_Token_zero

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
  evalApplyVals' rator =<< mapM (fmap public . argToSV) randas

evalApplyClosureVals :: SrcLoc -> SLClo -> [SLSVal] -> App SLAppRes
evalApplyClosureVals clo_at (SLClo mname formals (JSBlock body_a body _) SLCloEnv {..}) randvs = do
  ret <- ctxt_alloc
  let body_at = srcloc_jsa "block app" body_a clo_at
  let err = Err_Apply_ArgCount clo_at (length formals) (length randvs)
  using_unstrict <- sco_lookup_unstrict
  let clo_sco =
        (SLScope
           { sco_ret = Just ret
           , sco_must_ret = RS_MayBeEmpty
           , sco_while_vars = Nothing
           , sco_penvs = clo_penvs
           , sco_cenv = clo_cenv
           , sco_use_strict = clo_use_strict
           , sco_use_unstrict = using_unstrict
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
      case not (containsVarNewerThan ret xv) && all_same of
        -- only remove the prompt if there was a single return
        True -> do
          promptAndStatic mt (lvl, xv)
        False -> do
          let go (r_at, mrty, (_, v), _) =
                case mrty of
                  Nothing -> locAt r_at $ expect_ $ Err_Type_None v
                  Just t -> return $ (Just r_at, t)
          locAndTys <- mapM go rs
          r_ty <- locAt body_at $ typeEqs locAndTys
          let retv = DLVar body_at Nothing r_ty ret
          saveLift $ DLS_Prompt body_at retv body_sa body_lifts
          return $ SLAppRes clo_sco'' (lvl, (SLV_DLVar retv))

evalApplyVals :: SLVal -> [SLSVal] -> App SLAppRes
evalApplyVals = evalApplyValsAux False

evalApplyValsAux :: Bool -> SLVal -> [SLSVal] -> App SLAppRes
evalApplyValsAux assumePrecondition rator randvs =
  case rator of
    SLV_Type (ST_UInt to) ->
      evalApplyValsAux assumePrecondition (SLV_Prim $ SLPrim_castOrTrunc to) randvs
    SLV_Type ST_StringDyn ->
      evalApplyValsAux assumePrecondition (SLV_Prim $ SLPrim_toStringDyn) randvs
    SLV_Type ST_BytesDyn ->
      evalApplyValsAux assumePrecondition (SLV_Prim $ SLPrim_BytesDynCast) randvs
    SLV_Prim p -> do
      sco <- e_sco <$> ask
      SLAppRes sco <$> evalPrim p randvs
    SLV_Clo clo_at Nothing sc ->
      evalApplyClosureVals clo_at sc randvs
    SLV_Clo clo_at (Just tf) sc -> do
      at <- withAt id
      let ct = if assumePrecondition then CT_Assume else CT_Assert
      (dom_tupv, _) <- assertRefinedArgs ct randvs at tf
      res@(SLAppRes _ (_, ret_v)) <- evalApplyClosureVals clo_at sc randvs
      forM_ (stf_post tf) $ \rngp ->
        applyRefinement CT_Assert rngp [dom_tupv, ret_v] (stf_post_msg tf)
      return res
    v -> expect_t v $ Err_Eval_NotApplicable

evalApply :: SLVal -> [JSExpression] -> App SLSVal
evalApply rator rands =
  case rator of
    SLV_Form f -> evalForm f rands
    _ -> evalApplyVals' rator =<< evalExprs rands

evalPropertyName :: JSPropertyName -> App (SecurityLevel, String)
evalPropertyName = \case
  JSPropertyIdent _ s -> return $ public s
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
      uni <- readUniverse
      case vs of
        [e] -> do
          at' <- withAt id
          sv' <- sls_sss at' uni <$> evalExpr e
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
              uni <- readUniverse
              let lvl = idLevel k
              let de = DLE_ObjectRef at' (DLA_Var dlv) k
              let mdv = DLVar at' Nothing t
              dv <- ctxt_lift_expr mdv de
              return $ M.singleton k $ SLSSVal at' lvl uni $ SLV_DLVar dv
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
        SLM_Export -> s
        SLM_AppInit -> s
        SLM_Step -> s
        SLM_LocalStep -> l
        SLM_LocalPure -> l
        SLM_ConsensusStep -> c
        SLM_ConsensusPure -> c
  let _lab' = lab <> " in " <> elab
  infectWithId_sss x =<< env_lookup (LC_RefFrom lab) x =<< env


evalId :: String -> SLVar -> App SLSVal
evalId lab x = sss_sls <$> evalId_ lab x

parseCoefficient :: String -> App (Rational, Maybe Int)
parseCoefficient b = do
  let parseInt i = do
        let miInt = readMay i :: Maybe Integer
        iInt <- maybe (expect_ err) return miInt
        return (fromIntegral iInt, precision)
        where precision = Nothing
  case splitOn "." b of
    [i, f]
      | f == "" -> parseInt i
      | otherwise -> do
        let mPrec = Just $ length f
        bd <- maybe (expect_ err) return (readMay b :: Maybe Double)
        return $ (toRational bd, mPrec)
    [i] -> parseInt i
    _ -> expect_ err
  where err = Err_Invalid_Exponential_Form "coefficient"

parseExponent :: String -> App (Integer, Bool)
parseExponent x = do
  let mxInt :: Maybe Integer = readMay $ filter (/= '+') x
  xInt <- maybe (expect_ err) (return . abs) mxInt
  let isNeg = startsWith '-' x
  return (xInt, isNeg)
  where err = Err_Invalid_Exponential_Form "exponent"

padToPrecision :: String -> Maybe Int -> App String
padToPrecision rs mPrec = do
  case splitOn "." rs of
    wDigits:fDigits ->
      case mPrec of
        Nothing -> return $ wDigits
        Just pr -> return $ wDigits <> "." <> take pr (rightPad pr '0' fDigits')
          where fDigits' = concat fDigits
    [] -> return rs

evalExpr :: JSExpression -> App SLSVal
evalExpr e = case e of
  JSIdentifier a x ->
    locAtf (srcloc_jsa "id ref" a) $
      evalId "expression" x
  JSDecimal a ns -> do
    uni <- readUniverse
    let handleE chr = do
          case splitOn chr ns of
            [b, x] -> do
              (bRat, mCoefPrec) <- parseCoefficient b
              (posExpo, isNegExpo) <- parseExponent x
              let r = (bool (*) (/) isNegExpo) bRat $ 10 ^ posExpo
              let mPrec = maximumMay $ catMaybes [mCoefPrec, mExpPrec]
                          where mExpPrec = bool Nothing mExpInt isNegExpo
                                mExpInt  = Just $ fromInteger posExpo
              let rs = showFFloat Nothing (fromRat r :: Double) ""
              padToPrecision rs mPrec
            _ -> expect_ $ Err_Eval_IllegalJS e
    ns' <- case ns of
              _ | 'e' `elem` ns -> handleE "e"
                | 'E' `elem` ns -> handleE "E"
              _ -> return ns
    case splitOn "." ns' of
      [iDigits, fDigits] -> do
        let i = iDigits <> fDigits
        let scale = '1' : replicate (length fDigits) '0'
        let signV = \at -> SLSSVal at Public uni $ SLV_Bool at True
        let signedInt = \at ->
              SLV_Object at Nothing $
                M.fromList
                  [ ("scale", SLSSVal at Public uni $ SLV_Int at nn $ numberValue 10 scale)
                  , ("i", SLSSVal at Public uni $ SLV_Int at nn $ numberValue 10 i)
                  ]
        let iV = \at -> SLSSVal at Public uni $ signedInt at
        locAtf (srcloc_jsa "decimal" a) $
            withAt $ \at ->
              public $
                SLV_Object at Nothing $
                  M.fromList [("sign", signV at), ("i", iV at)]
      [_] -> locAtf (srcloc_jsa "decimal" a) $
        withAt $ \at -> public $ SLV_Int at nn $ numberValue 10 ns'
      _ -> impossible "Number must have 0 or 1 decimal points."
  JSLiteral a l -> locAtf (srcloc_jsa "literal" a) $ do
    at' <- withAt id
    case l of
      "null" -> return $ public $ SLV_Null at' "null"
      -- Used by`parallelReduce`/`fork` when a case has no `local`
      ".null" -> return $ secret $ SLV_Null at' ".null"
      "true" -> return $ public $ SLV_Bool at' True
      "false" -> return $ public $ SLV_Bool at' False
      "this" -> evalExpr $ JSIdentifier a l
      _ -> expect_ $ Err_Parse_IllegalLiteral l
  JSHexInteger a ns -> locAtf (srcloc_jsa "hex" a) $
    withAt $ \at -> public $ SLV_Int at nn $ numberValue 16 (drop 2 ns {- trim '0x' prefix -})
  JSOctal a ns -> locAtf (srcloc_jsa "octal" a) $
    withAt $ \at -> public $ SLV_Int at nn $ numberValue 8 ns
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
    let body = jsArrowBodyToRetBlock bodys
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
          (T_UInt UI_Word, _) ->
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
            <$> evalPrim (SLPrim_op $ S_IF_THEN_ELSE) [csv, tsv, fsv]
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
          typeEq t_ty f_ty (Just $ srclocOf tv) (Just $ srclocOf fv)
          at' <- withAt id
          let ans_dv = DLVar at' Nothing t_ty ret
          theIf <- checkCond om $ DLS_If at' (Just ans_dv) (DLA_Var cond_dv) sa tlifts' flifts'
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
  lvlMeet obj_lvl <$> (locAt fieldAt $ evalDot (obj_lvl, objv) fields)

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
    SLV_Int _ _ idxi ->
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
                idx_dla <- withAt $ \at -> DLA_Literal (DLL_Int at UI_Word idxi)
                retArrayRef t sz (DLA_Var adv) idx_dla
        _ -> expect_t arrv $ Err_Eval_RefNotRefable
    SLV_DLVar idxdv@(DLVar _ _ (T_UInt UI_Word) _) -> do
      (arr_ty, arr_dla) <- compileTypeOf arrv
      case arr_ty of
        T_Array elem_ty sz ->
          retArrayRef elem_ty sz arr_dla $ DLA_Var idxdv
        _ -> expect_t arrv $ Err_Eval_IndirectRefNotArray
    SLV_Bytes _ bs ->
      snd <$> (evalDot (public arrv) $ bunpack bs)
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
      uni <- readUniverse
      v' <- infectWithId_sv at_ x v
      when trackVars $ trackVariable (at_, x)
      env_insert x (SLSSVal at_ rhs_lvl uni v') lhs_env
  JSArrayLiteral a xs _ -> do
    locAtf (srcloc_jsa "array" a) $ do
      vs <- explodeTupleLike "lhs array" v
      evalDeclLHSArray trackVars merr rhs_lvl lhs_env vs (jsa_flatten xs)
  JSObjectLiteral a props _ -> do
    locAtf (srcloc_jsa "object" a) $ do
      vm <- evalAsEnv (rhs_lvl, v)
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
    enf (k, SLSSVal at secLev _ _) = case secLev of
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
        uni <- readUniverse
        let penv_ =
              -- Ensure that only has access to "interact" if it didn't before,
              -- such as when an only occurs inside of a closure in a module body
              case M.member "interact" penv__ of
                True -> penv__
                False -> M.insert "interact" (ios M.! who) penv__
        me_dv <- doGetSelfAddress who
        let me_v = SLV_DLVar me_dv
        let ssv_here = SLSSVal only_at Public uni
        let add_this v env = M.insert "this" (ssv_here v) env
        isClass <- is_class who
        penv <-
          case vas of
            Nothing -> return $ add_this me_v penv_
            Just v ->
              case M.lookup v penv_ of
                Just (SLSSVal pv_at pv_lvl pv_uni pv@(SLV_Participant at_ who_ mv_ mdv)) ->
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
                          M.insert v (SLSSVal pv_at pv_lvl pv_uni pv') penv_
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
  (alifts, (penv', only_ty, _)) <-
    captureLifts $
      doOnlyExpr ((who, vas), only_at, only_cloenv, only_synarg)
  case only_ty of
    T_Null -> do
      saveLift $ DLS_Only only_at who alifts
      return $ sco {sco_penvs = M.insert who penv' $ sco_penvs sco}
    ty -> locAt only_at $ expect_ $ Err_Block_NotNull ty

doGetSelfAddress :: SLPart -> App DLVar
doGetSelfAddress who = do
  isClass <- is_class who
  at <- withAt id
  nonce <- ctxt_alloc
  ctxt_lift_expr
    (DLVar at Nothing T_Address)
    (DLE_PrimOp
       at
       (SELF_ADDRESS who isClass nonce)
       [])

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
  let mtPay = DLPayAmt (DLA_Literal $ DLL_Int at UI_Word 0) []
  let v_at = srclocOf_ at v
  (t, ae) <- typeOf v
  case t of
    T_UInt UI_Word -> do
      a <- compileArgExpr_ ae
      return $ DLPayAmt a []
    T_Tuple ts -> do
      case ae of
        DLAE_Tuple aes -> do
          let go sa (gt, gae) =
                case gt of
                  T_UInt UI_Word -> do
                    let ((seenNet, sks), DLPayAmt _ tks) = sa
                    when seenNet $
                      locAt v_at $ expect_ $ Err_Transfer_DoubleNetworkToken tt
                    a <- compileArgExpr_ gae
                    return $ ((True, sks), DLPayAmt a tks)
                  T_Tuple [T_UInt UI_Word, T_Token] ->
                    case gae of
                      DLAE_Tuple [amt_ae, token_ae] -> do
                        let ((seenNet, sks), DLPayAmt nts tks) = sa
                        amt_a <- compileArgExpr_ amt_ae
                        token_a <- compileArgExpr_ token_ae
                        verifyTokenUnique sks token_a
                        return $ ((seenNet, token_a : sks), (DLPayAmt nts $ tks <> [(amt_a, token_a)]))
                      _ -> locAt v_at $ expect_ $ Err_Token_DynamicRef
                  _ -> locAt v_at $ expect_t v $ Err_Transfer_Type tt
          snd <$> (foldlM go ((False, []), mtPay) $ zip ts aes)
        DLAE_Arg (DLA_Var dv) -> do
          let go sa (ty, idx) = do
                let getPayAmt = DLE_TupleRef at $ DLA_Var dv
                let mkVar vt e = DLA_Var <$> (ctxt_lift_expr (DLVar at Nothing vt) e)
                case ty of
                  T_UInt UI_Word -> do
                    let ((seenNet, sks), DLPayAmt _ tks) = sa
                    when seenNet $ do
                      locAt v_at $ expect_ $ Err_Transfer_DoubleNetworkToken tt
                    a <- mkVar (T_UInt UI_Word) $ getPayAmt idx
                    return $ ((True, sks), DLPayAmt a tks)
                  tupTy@(T_Tuple [T_UInt UI_Word, T_Token]) -> do
                    let ((seenNet, sks), DLPayAmt nts tks) = sa
                    tup   <- mkVar tupTy $ getPayAmt idx
                    amt_a <- mkVar (T_UInt UI_Word)  $ DLE_TupleRef at tup 0
                    tok_a <- mkVar T_Token $ DLE_TupleRef at tup 1
                    verifyTokenUnique sks tok_a
                    return $ ((seenNet, tok_a : sks), (DLPayAmt nts $ tks <> [(amt_a, tok_a)]))
                  _ -> locAt v_at $ expect_t v $ Err_Transfer_Type tt
          snd <$> (foldlM go ((False, []), mtPay) $ zip ts [0 ..])
        _ -> locAt v_at $ expect_ $ Err_Token_DynamicRef
    _ -> locAt v_at $ expect_t v $ Err_Transfer_Type tt
  where
    verifyTokenUnique sks tok = do
      unless (null sks) $ do
        tokUniq <- tokIsUnique sks tok
        doClaim CT_Assert tokUniq $ Just "Token in pay amount is unique"

-- XXX This should be converted to take DLVars and use evalDistinctTokens
tokIsUnique :: [DLArg] -> DLArg -> App DLArg
tokIsUnique sks tok = do
  at <- withAt id
  let arrTy = T_Array T_Token $ fromIntegral $ length sks
  tokens <- ctxt_lift_expr (DLVar at Nothing $ arrTy) $ DLE_LArg at $ DLLA_Array T_Token sks
  let arrayIncludes args = do
        f <- lookStdlib "Foldable_includes"
        evalApplyArgs' f args
  let notF x = do
        f <- lookStdlib "not"
        evalApplyVals' f [x]
  tokUniqSv <- notF =<< arrayIncludes [DLA_Var tokens, tok]
  compileCheckType msdef T_Bool $ snd tokUniqSv

doToConsensus :: [JSStatement] -> ToConsensusRec -> App SLStmtRes
doToConsensus ks (ToConsensusRec {..}) = locAt slptc_at $ do
  ensure_mode SLM_Step "to consensus"
  ensure_live "to consensus"
  uni <- readUniverse
  let whos = slptc_whos
  who_apis <- S.intersection whos <$> (ae_apis <$> aisd)
  unless (S.null who_apis || slptc_api) $
    expect_ $ Err_Api_Publish who_apis
  let vas = slptc_mv
  let msg = fromMaybe [] slptc_msg
  let ann = at2a slptc_at
  let amt_e = fromMaybe (JSDecimal ann "0") slptc_amte
  let amt_req = maybe (JSDecimal ann "0") (\f -> jsCall ann f []) slptc_amt_req
  let when_e = fromMaybe (JSLiteral ann "true") slptc_whene
  let mtime = slptc_timeout
  let evalChecks = do
        (_, chk_ss) <- deconstructFunStmts $ fromMaybe (noop JSNoAnnot 0) slptc_check
        sco <- asks e_sco
        void $ locSco (sco { sco_must_ret = RS_ImplicitNull }) $ evalStmt chk_ss
  at <- withAt id
  st <- readSt id
  let st_pure = st {st_mode = SLM_ConsensusPure}
  let pdvs = st_pdvs st
  isSoloSend <- case S.toList whos of
    [solo] -> not <$> is_class solo
    _ -> return False
  unless (st_after_first st || isSoloSend) $ do
    expect_thrown at Err_UniqueFirstPublish
  let ctepee t e = compileCheckType msdef t =<< ensure_public =<< evalExpr e
  -- We go back to the original env from before the to-consensus step
  -- Handle sending
  let compilePayAmt_ e = compilePayAmt TT_Pay =<< ensure_public =<< evalExpr e
  let tc_send1 who = do
        let st_lpure = st {st_mode = SLM_LocalPure}
        (only_lifts, (res, res_ats)) <-
          locWho who $
            locSt st_lpure $
              captureLifts $ do
                evalChecks
                let repeat_dv = M.lookup who pdvs
                ds_isClass <- is_class who
                (ds_msg_at, ds_msg) <- unzip <$> mapM (\v -> do
                  sv <- evalId "publish msg" v
                  va <- snd <$> (compileTypeOf =<< ensure_public sv)
                  return (srclocOf (snd sv), va)) msg
                ds_pay <- compilePayAmt_ amt_e
                ds_when <- ctepee T_Bool when_e
                return ((repeat_dv, DLSend {..}), ds_msg_at)
        saveLift $ DLS_Only at who (only_lifts)
        return $ (res, res_ats)
  tc_send'0 <- sequence $ M.fromSet tc_send1 whos
  let tc_send' = tc_send'0
  let tc_send = fmap (snd . fst) tc_send'
  let msg_ats_map = fmap snd tc_send'
  let msg_dass = fmap ds_msg tc_send
  let msg_dass_t = transpose $ M.elems $ msg_dass
  let msg_ats = transpose $ M.elems $ msg_ats_map
  let msg_dass_t' = zipWith zip msg_dass_t msg_ats
  let get_msg_t = typeEqs . map (\(x, x_at) -> (Just x_at, argTypeOf x))
  msg_ts <- mapM get_msg_t msg_dass_t'
  let mrepeat_dvs = all_just $ M.elems $ M.map (fst . fst) tc_send'
  -- Handle receiving / consensus
  dr_from_ <- ctxt_mkvar $ DLVar at Nothing T_Address
  let recv_imode = AllowShadowingRace whos (S.fromList msg)
  whosc <- mapM (\w -> (,) w <$> is_class w) $ S.toList whos
  let dv_reorigin (DLVar a _b c d) b = DLVar a b c d
  (who_env_mod, dr_from_lab, pdvs_recv) <-
    case whosc of
      [(who, False)] -> do
        let who_dv_ = fromMaybe dr_from_ (M.lookup who pdvs)
        let who_lab = Just (at, bunpack who)
        let who_dv = dv_reorigin who_dv_ who_lab
        let pdvs' = M.insert who who_dv pdvs
        let add_who_env env =
              case vas of
                Nothing -> return $ env
                Just whov ->
                  evalId_ "publish who binding" whov >>= \case
                    (SLSSVal idAt lvl_ uni_ (SLV_Participant at_ who_ as_ _)) ->
                      return $ M.insert whov (SLSSVal idAt lvl_ uni_ (SLV_Participant at_ who_ as_ (Just who_dv))) env
                    _ ->
                      impossible $ "participant is not participant"
        return $ (add_who_env, who_lab, pdvs')
      _ -> do
        return $ (return, Nothing, pdvs)
  let dr_from = dv_reorigin dr_from_ dr_from_lab
  let mkmsg v t = ctxt_mkvar $ DLVar at (getBindingOrigin v) t
  dr_msg <- zipWithM mkmsg msg_dass_t msg_ts
  let toks = filter ((==) T_Token . varType) dr_msg
  unless (null toks) $ do
    sco <- e_sco <$> ask
    when (isJust $ sco_while_vars sco) $
      expect_ $ Err_Token_InWhile
  msg_env <- foldlM env_insertp mempty $ zip msg $ map (sls_sss at uni . public . SLV_DLVar) $ dr_msg
  let recv_env_mod = who_env_mod . (M.insert "this" (SLSSVal at Public uni $ SLV_DLVar dr_from))
  let recv_env = msg_env
  (tc_recv, k_st, k_cr) <- do
    SLRes conlifts k_st (mktc_recv, k_cr) <- captureRes $ do
      let st_recv =
            st
              { st_mode = SLM_ConsensusStep
              , st_pdvs = pdvs_recv
              , st_after_first = True
              }
      setSt st_recv
      track <- readDlo dlo_autoTrackPublishedTokens
      when track $
        mapM_ (flip trackToken Nothing) $ map DLA_Var toks
      sco_recv <- sco_update_and_mod recv_imode recv_env recv_env_mod
      locSco sco_recv $ do
        evalChecks
        let req_rator = SLV_Prim $ SLPrim_claim CT_Require
        void $ locSt st_pure $ evalExpr $ amt_req
        forM_ (map DLA_Var toks) $ \tok -> do
          ctxt_lift_eff $ DLE_TokenInit at tok
        -- Check payments
        DLPayAmt {..} <- compilePayAmt_ amt_e
        unless (null pa_ks) $
          unless (st_after_ctor st) $
            expect_ $ Err_Token_OnCtor
        fs <- e_stack <$> ask
        let checkPayAmt1 mtok pa = do
              sv <- argToSV pa
              doBalanceUpdate mtok (S_ADD $ Just PV_None) sv
              ctxt_lift_eff $ DLE_CheckPay at fs pa mtok
        checkPayAmt1 Nothing pa_net
        forM_ pa_ks $ uncurry $ flip $ checkPayAmt1 . Just
        -- Fork is trusted and doesn't check sender, because the macro does
        unless slptc_fork $ do
          -- Check sender
          let check_repeat whoc_v repeat_dv = do
                repeat_cmp_v <-
                  evalPrimOp S_ADDRESS_EQ $
                    map (public . SLV_DLVar) [repeat_dv, dr_from]
                evalPrimOp S_IF_THEN_ELSE $
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
          return ()
        let go fv = do
              v <- ctxt_mkvar $ DLVar at (Just (at, show $ pretty fv)) $ fluidVarType fv
              doFluidSet fv $ public $ SLV_DLVar v
              return v
        dr_time <- go FV_thisConsensusTime
        dr_secs <- go FV_thisConsensusSecs
        dr_didSend <- go FV_didSend
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
        delay_sv <- locSt st_pure $ evalExpr delay_e >>= ensure_public
        case delay_sv of
          SLV_Bool _ False -> do
            when mustHaveTimeoutNoMatterWhat $
              expect_ $ Err_ToConsensus_WhenNoTimeout True
            setSt k_st
            return $ (Nothing, k_cr)
          _ -> do
            delay_ta <- locSt st_pure $ compileTimeArg delay_sv
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
  T_UInt UI_Word -> var "UInt"
  T_UInt UI_256 -> var "UInt256"
  T_Bytes i -> call "Bytes" [ie i]
  T_BytesDyn -> var "BytesDyn"
  T_StringDyn -> var "StringDyn"
  T_Digest -> var "Digest"
  T_Address -> var "Address"
  T_Contract -> var "Contract"
  T_Token -> var "Token"
  T_Array t i -> call "Array" [r t, ie i]
  T_Tuple ts -> call "Tuple" $ map r ts
  T_Object m -> call "Object" [rm m]
  T_Data m -> call "Data" [rm m]
  T_Struct ts -> call "Struct" $ [arr $ map sg ts]
  where
    str x = JSStringLiteral a $ "'" <> x <> "'"
    arr = jsArrayLiteral a
    sg (k, t) = arr [(str k), r t]
    call f = jsCall a (var f)
    var = JSIdentifier a
    r = typeToExpr
    a = JSNoAnnot
    ie = JSDecimal a . show
    rm = jsObjectLiteral a . M.map r

data CompiledForkCase = CompiledForkCase
  { cfc_part_e :: JSExpression
  , cfc_data_def :: [JSObjectProperty]
  , cfc_pay_prop :: [JSObjectProperty]
  , cfc_pay_req_prop :: [JSObjectProperty]
  , cfc_switch_case :: [JSSwitchParts]
  , cfc_only :: JSStatement
  , cfc_local_data_def :: [JSObjectProperty]
  , cfc_local_penvs :: SLPartEnvs
  }

forkCaseSameParticipant :: ForkCase -> ForkCase -> Bool
forkCaseSameParticipant l r = fc_who l == fc_who r

mkDot :: JSAnnot -> [JSExpression] -> JSExpression
mkDot a = \case
  [] -> impossible "mkDot: empty list"
  h : t -> foldl' (flip JSMemberDot a) h t

mkIdDot :: JSAnnot -> [String] -> JSExpression
mkIdDot a = mkDot a . map (JSIdentifier a)

noop :: JSAnnot -> Int -> JSExpression
noop a numOfArgs = jsArrowStmts a (take numOfArgs $ repeat $ JSIdentifier a "_") []

doApiCall :: JSExpression -> ApiCallRec -> App [JSStatement]
doApiCall lhs (ApiCallRec {..}) = do
  at <- withAt id
  let a = at2a at
  let sa = JSSemiAuto
  let es = flip JSExpressionStatement sa
  let jid = JSIdentifier a
  let spread = JSSpreadExpression a
  idx <- ctxt_alloc
  let jidg xi = jid $ ".api" <> show idx <> "." <> xi
  let whoOnly = mkDot a [slac_who, jid "only"]
  let e2s = flip JSExpressionStatement sa
  who_str <- jsString a . bunpack <$> mustBeParticipant slac_who
  let callOnly s = es $ jsCall a whoOnly s
  -- Deconstruct args
  let dom = jidg "dom"
  -- Construct `only`
  let interactIn = jsCall a (jid "declassify") [jsCall a (mkIdDot a ["interact", "in"]) []]
  let assumeStmts = case slac_massume of
        Nothing -> []
        Just as -> [es $ jsCall a as [spread dom]]
  let onlyThunk = jsThunkStmts a $ jsConst a dom interactIn : assumeStmts
  -- Construct publish
  let pub1 = jsCall a (mkDot a [slac_who, jid "publish"]) [dom]
  let pub2 =
        case slac_mpay of
          Nothing -> pub1
          Just pe -> jsCall a (mkDot a [pub1, jid "pay"]) [jsCall a pe [spread dom]]
  let pub3 =
        case slac_mtime of
          Nothing -> pub2
          Just (to, e) -> jsCall a (mkDot a [pub2, jid "throwTimeout"]) [to, e]
  let pub4 = jsCall a (mkDot a [pub3, jid ".api"]) []
  let pub5 =
        case slac_mcheck of
          Nothing -> pub4
          Just chk -> jsCall a (mkDot a [pub4, jid "check"]) [jsArrowStmts a [] $ [e2s $ jsCall a chk [spread dom]] ]
  -- Construct `k = interact.out()`
  let returnVal = jidg "rng"
  let returnLVal = jidg "rngl"
  let interactOut = jsCall a (mkIdDot a ["interact", "out"]) [dom, returnLVal]
  let doLog = jsCall a (jid ".emitLog") [jidg "rng", who_str]
  let apiReturn =
        jsArrowStmts
          a
          [returnVal]
          [jsConst a returnLVal doLog, callOnly [jsThunkStmts a [es interactOut]]]
  let assignLhs = jsConst a lhs $ jsArrayLiteral a [dom, apiReturn]
  let setDetails = jsCall a (jid ".setApiDetails") [slac_who, dom]
  let ss = [callOnly [onlyThunk], es pub5, assignLhs, es setDetails]
  return ss

prependToFun :: [JSExpression] -> [JSStatement] -> JSExpression -> App JSExpression
prependToFun xtraArgs xtraStmts f = do
  let a = jsa f
  (fargs, fs) <- deconstructFunStmts f
  return $ jsArrowStmts a (xtraArgs <> fargs) $ (xtraStmts <> fs)

prependFunStmts :: [JSStatement] -> JSExpression -> App JSExpression
prependFunStmts = prependToFun mempty

prependFunArgs :: [JSExpression] -> JSExpression -> App JSExpression
prependFunArgs = flip prependToFun mempty

doForkAPI2Case :: Bool -> [JSExpression] -> App [JSExpression]
doForkAPI2Case isSingleFun args = do
  at <- withAt id
  let a = at2a at
  let jid = JSIdentifier a
  idx <- ctxt_alloc
  let jidg xi = jid $ ".api" <> show idx <> "." <> xi
  let dom = jidg "dom"
  let dotdom = JSSpreadExpression a dom
  let dom2 = jidg "dom2"
  let dotdom2 = JSSpreadExpression a $ dom2
  let sp = a2sp a
  let e2s = flip JSExpressionStatement sp
  let jsInlineCall f fargs = do
        let a' = jsa f
        (pargs, ss) <- deconstructFunStmts f
        let ss' = [jsConst a' (jsArrayLiteral a' pargs) (jsArrayLiteral a fargs)]
        return $ ss' <> ss
  let mkAssume chk_ss xp = do
        jsThunkStmts xpa $
                    [jsConst xpa dom (readJsExpr "declassify(interact.in())")] <>
                    chk_ss <>
                    [ e2s (jsCall xpa xp [dotdom]), JSReturn xpa (Just $ jsObjectLiteral xpa $ M.fromList [("msg", dom)]) sp]
        where
          xpa = jsa xp
  let locd = mkAssume [] $ jsArrowStmts a [dotdom2] [e2s $ JSUnaryExpression (JSUnaryOpVoid a) dom2]
  -- Check if tuple
  let callWithDom a' f = jsArrowExpr a' [dom] $ jsCall a' f [dotdom]
  let mkPay chks y = do
        let (pay, mreq) = splitPayExpr' y
        let req = maybe (noop ya 1) (callWithDom ya) mreq
        pay' <- callWithDom ya <$> injectChecks pay
        return $ jsArrayLiteral ya [ pay', req ]
        where
          ya = jsa y
          injectChecks = prependFunStmts chks
  let doLog w = do
        who_str <- jsString a . bunpack <$> mustBeParticipant w
        return $ jsCall a (jid ".emitLog") [jidg "rng", who_str]
  let mkConsensusOnly w = jsCall wa (JSMemberDot w wa (jid "only")) [jsThunkStmts wa [JSIf wa wa (jsCall wa (jid "didPublish") []) wa $ JSExpressionStatement (jsCall wa (JSMemberDot (jid "interact") wa (jid "out")) [dom, jidg "rngl"]) sp]]
        where
          wa = jsa w
  let mkConsensus w chks z = do
        log' <- doLog w
        let za = jsa z
        z' <- locAtf (srcloc_jsa "consensus block" za) $ jsInlineCall z [dotdom, jsArrowStmts za [jidg "rng"] [jsConst za (jidg "rngl") log', e2s $ mkConsensusOnly w]]
        return $ jsArrowStmts za [dom] $ chks <> z'
  let mkConsensus' w l mc z = do
        z' <- mkConsensus w mc z
        return $ [w] <> l <> [z']
  let splitApiConsensus stmts = do
        case reverse stmts of
          JSReturn _ (Just (JSArrayLiteral _ els _)) _ : rst
              | [pay, con] <- jsa_flatten els -> return (assumes, Just pay, con)
              | [con]      <- jsa_flatten els -> return (assumes, Nothing, con)
            where assumes = reverse rst
          _ -> expect_ $ Err_Api_Return_Type
  -- Splits the `api_` function into distinct assume, pay, and consensus expressions,
  -- each of which have checks injected
  let ignore = jid "_"
  let splitSingleApiBody w f = do
        let fa = jsa f
        (domain, body) <- deconstructFunStmts f
        (chks, mpay, con) <- splitApiConsensus body
        chk_ss <- flip jsInlineCall [dotdom] $ jsArrowStmts fa domain chks
        let iargs = ignore <$ domain
        let no_op = jsArrowStmts fa iargs []
        let assume = mkAssume chk_ss no_op
        con_e <- mkConsensus w chk_ss =<< prependFunArgs iargs con
        mpay_e <- forM mpay $ mkPay chks . jsArrayLiteral fa . (: []) . jsArrowExpr fa domain
        return (assume, mpay_e, con_e)
  let goSingle who f = do
        (assumes, mpay, con) <- splitSingleApiBody who f
        return $ [who, assumes] <> catMaybes [mpay] <>  [con]
  case (isSingleFun, args) of
    (True, [ who, b ]) -> goSingle who b
    (False, [who, locp, pay, con]) -> do
      let loc' = mkAssume [] locp
      pay' <- mkPay [] pay
      mkConsensus' who [loc', pay'] [] con
    (False, [who, pay, con]) -> do
      pay' <- mkPay [] pay
      mkConsensus' who [locd, pay'] [] con
    (False, [who, con]) -> mkConsensus' who [locd] [] con
    --- Delay error to next level
    (_, ow) -> return ow

splitPayExpr' :: JSExpression -> (JSExpression, Maybe JSExpression)
splitPayExpr' = \case
  JSArrayLiteral _ xs _ | x : y : _ <- jsa_flatten xs -> (x, Just y)
  JSArrayLiteral _ xs _ | [x] <- jsa_flatten xs -> (x, Nothing)
  ow -> (ow, Nothing)

splitPayExpr :: JSAnnot -> JSExpression -> (JSExpression, JSExpression)
splitPayExpr a v = (p, fromMaybe (noop a 1) mr)
  where (p, mr) = splitPayExpr' v

doFork :: [JSStatement] -> ForkRec -> App SLStmtRes
doFork ks (ForkRec {..}) = locAt slf_at $ do
  let cases = slf_cases
  when (null cases) $
    expect_ $ Err_ForkNoCases
  let mtime = slf_mtime
  idx <- ctxt_alloc
  idx2 <- ctxt_alloc
  let fid x = ".fork" <> (show idx) <> "." <> x
  at <- withAt id
  let a = srcloc2annot at
  let jid = JSIdentifier a
  let sp = JSSemi a
  let fd_e = jid (fid "data")
  let fld_e = jid (fid "localData")
  let res_e = jid (fid "res")
  let msg_e = jid (fid "msg")
  let when_e = jid (fid "when")
  let loc_id = "_" <> fid "local"
  let local_e = jid loc_id
  let tid = ".t" <> show idx
  let tv = jid tid
  let mkobj l =
        let la = jsaList a l
         in JSObjectLiteral la (JSCTLNone $ toJSCL l) la
  let makeOnly who_e only_body =
        let oa = jsaList (jsa who_e) only_body
         in JSMethodCall (JSMemberDot who_e oa (jid "only")) oa (JSLOne $ jsThunkStmts oa only_body) oa sp
  let defcon e = jsConst (jsa e) e
  let indexed = zip [0 ..] :: [a] -> [(Int, a)]
  let forkOnlyHelp who_e e_at before_e msg_id when_id local_label = locAt e_at $ do
        let ba = jsa before_e
        let only_before_call_e = JSCallExpression (JSMemberDot who_e ba (jid "only")) ba (JSLOne before_e) ba
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
              bool def (JSMemberDot tv (jsa tv) $ jid field) $ resHas field
        let defWhen = defcon when_id $ tDot "when" $ JSLiteral (jsa when_id) "true"
        let defMsg = defcon msg_id $ tDot "msg" $ JSLiteral (jsa msg_id) "null"
        let defLocal = bool [] [defcon (jid $ local_label) $ tDot "_local" $ JSLiteral ba ".null"] $ resHas "_local"
        return $
          (,) res_ty_m $
            [ defcon (JSIdentifier ba tid) $ jsCallThunk (jsa before_e) before_e
            , defWhen
            , defMsg
            ] <> defLocal
  let lookupMsgTy t = fromMaybe T_Null $ M.lookup "msg" t
  let lookupLocalTy t = fromMaybe T_Null $ M.lookup "_local" t
  let mkPartCase who n = who <> show n <> "_" <> show idx
  let mkLocalCase who n = who <> show n <> "_" <> show idx2
  let getAfter who_e isApi who usesData (a_at, after_e, case_n, case_ty) = locAtf afterAt $
        case after_e of
          JSArrowExpression args _ s -> do
            let JSBlock _ ss _ = jsArrowBodyToRetBlock s
            let awrap x = jsArrayLiteral (jsa x) [x]
            let def_only lhs rhs = case usesData of
                  False -> do
                    let only_body = [jsConst a lhs rhs]
                    return $ (mempty, JSMethodCall (JSMemberDot who_e a (jid "only")) a (JSLOne $ jsThunkStmts a only_body) a sp)
                  True -> do
                    -- Deconstruct cases and assign local variables
                    let arg = jid $ "_deconstr"
                    let case_id = mkLocalCase who case_n
                    var_idx <- ctxt_alloc
                    uni <- readUniverse
                    dv <- ctxt_lift_expr (DLVar at Nothing case_ty) $ DLE_Impossible at var_idx $ Err_Impossible_Case ("fork/local/" <> who <> "/" <> show case_n)
                    let varId = "_.v" <> show var_idx
                    let sco' = M.singleton varId $ SLSSVal at Secret uni $ SLV_DLVar dv
                    let props = mkCommaTrailingList [
                          JSPropertyNameandValue (JSPropertyIdent a case_id) a [jsArrowExpr a [arg] arg],
                          JSPropertyNameandValue (JSPropertyIdent a "default") a [jsArrowExpr a [arg] $ jid varId]
                          ]
                    let only_body = [jsConst a lhs $ JSCallExpression (JSMemberDot rhs a (jid "match")) a (JSLOne $ JSObjectLiteral a props a) a ]
                    return $ (sco', JSMethodCall (JSMemberDot who_e a (jid "only")) a (JSLOne $ jsThunkStmts a only_body) a sp)
            (sco', mdefmsg) <-
              case parseJSArrowFormals a_at args of
                [] -> return (mempty, [])
                [ae] -> return (mempty, [defcon (awrap ae) (awrap msg_e)])
                [ae, le] -> do
                  (sco', only) <- def_only le local_e
                  return (sco', [ defcon (awrap ae) (awrap msg_e), only ])
                _ -> expect_ $ Err_Fork_ConsensusBadArrow after_e
            let sps = case isApi of
                  False -> []
                  True -> do
                    let msg = if usesData then fd_e else msg_e
                    let mce = if usesData then [jid $ mkPartCase who case_n] else []
                    let e = jsCall a (jid ".setApiDetails") $ [who_e, msg] <> mce
                    [JSExpressionStatement e sp]
            return $ (sco', sps <> mdefmsg <> ss)
          JSExpressionParen _ e _ -> getAfter who_e isApi who usesData (a_at, e, case_n, case_ty)
          _ -> expect_ $ Err_Fork_ConsensusBadArrow after_e
          where
            afterAt = srcloc_jsa "consensus block" (jsa after_e)
  let augWithChecks chk e = do
        (_, chk_ss) <- deconstructFunStmts chk
        prependFunStmts chk_ss e
  let augPayWithChecks chk p = do
        let (pay, req) = splitPayExpr an p
        pay' <- augWithChecks chk pay
        req' <- augWithChecks chk req
        return $ jsArrayLiteral an [ pay', req' ]
        where
          an = jsa p
  let go pcases = do
        (ats, whos, who_es, before_es, paytup_es, after_es) <-
              unzip6 <$> mapM (\ForkCase {..} -> do
                before_e' <- augWithChecks fc_check fc_before
                after_e'  <- augWithChecks fc_check fc_after
                pay_e' <- augPayWithChecks fc_check fc_pay
                return $ (fc_at, fc_who, fc_who_e, before_e', pay_e', after_e')) pcases
        let (pay_es, pay_reqs) = unzip $ map (splitPayExpr a) paytup_es
        let who = hdDie whos
        let who_e = hdDie who_es
        let c_at = hdDie ats
        let cfc_part_e = who_e
        let partCase = mkPartCase who
        let localCase = mkLocalCase who
        -- Generate:
        -- const runAlice<N> = () => {
        --   const res = before<N>();
        --   const when = res.when || true;
        --   const msg = res.msg || null;
        --   return { ... res, when, msg: Alice<N>(msg) };
        -- }
        let makeRuns (n, (e_at, before_e)) = do
              let ea = at2a e_at
              let msg_id = JSIdentifier ea "msg"
              let when_id = JSIdentifier ea "when"
              let local_id = "_local"
              (res_ty_m, only_body) <-
                forkOnlyHelp who_e e_at before_e msg_id when_id local_id
              let msgExpr = JSCallExpression (JSMemberDot fd_e ea (jid $ partCase n)) ea (JSLOne msg_id) ea
              let loc_e = if M.member "_local" res_ty_m then jid local_id else JSLiteral ea ".null"
              let loc_de = JSCallExpression (JSMemberDot fld_e ea (jid $ localCase n)) ea (JSLOne $ loc_e) ea
              let returnExpr = JSObjectLiteral ea (mkCommaTrailingList $ [
                    JSObjectSpread ea tv,
                    JSPropertyIdentRef ea "when",
                    JSPropertyNameandValue (JSPropertyIdent ea "_local") ea [loc_de],
                    JSPropertyNameandValue (JSPropertyIdent ea "msg") ea [msgExpr]
                    ]) ea
              let stmts =
                    only_body
                      <> [JSReturn ea (Just returnExpr) sp]
              let cloName = "run" <> partCase n
              return (M.map snd res_ty_m, cloName, jsThunkStmts ea stmts)
        (res_ty_ms, beforeNames, beforeClosures) <- unzip3 <$> mapM makeRuns (indexed $ zip ats before_es)
        -- Generate:
        --    const case_res0 = runAlice0(); ...
        --    const res = case_res<N - 1>.when ? case_res<N - 1> : runAliceN();
        let genCaseResStmts = aux 0
              where
                aux :: Int -> [String] -> [JSStatement]
                aux 0 [h] = [defcon res_e $ jsCallThunk (jsa res_e) $ jid h]
                aux 0 (h : t) = defcon case0 (jsCallThunk (jsa case0) $ jid h) : aux 1 t
                  where
                    case0 = mkCaseRes 0
                aux n (h : t) = defcon thisRes e : aux (n + 1) t
                  where
                    prevRes = mkCaseRes $ n - 1
                    thisRes = if null t then res_e else mkCaseRes n
                    pra = jsa prevRes
                    cnd = JSMemberDot prevRes pra (jid "when")
                    e = JSExpressionTernary cnd pra prevRes pra $ jsCallThunk pra $ jid h
                aux _ [] = []
                mkCaseRes :: Int -> JSExpression
                mkCaseRes n = jid $ "case_res" <> show n <> "_" <> show idx

        let cr_ss = genCaseResStmts beforeNames
        let check_who = JSExpressionBinary (jid "this") (JSBinOpEq a) who_e
        who_s <-
          (snd <$> evalExpr who_e) >>= \case
            SLV_Participant _ x _ _ -> return $ x
            v -> expect_t v $ Err_Expected "participant"
        isBound <- readSt $ M.member who_s . st_pdvs
        let mkobjp i x = JSPropertyNameandValue (JSPropertyIdent xa i) xa [x]
              where
                xa = jsa x
        let pay_go (i, e) = mkobjp (partCase i) e
        let cfc_pay_prop = map pay_go $ indexed pay_es
        let cfc_pay_req_prop = map pay_go $ indexed pay_reqs
        let data_go (i, m) = mkobjp (partCase i) (typeToExpr $ lookupMsgTy m)
        let local_data_go (i, m) = mkobjp (localCase i) (typeToExpr $ lookupLocalTy m)
        let cfc_data_def = map data_go $ indexed res_ty_ms
        let cfc_local_data_def = map local_data_go $ indexed res_ty_ms
        let local_data_tys = map lookupLocalTy res_ty_ms
        let ra = jsa res_e
        let res_msg = JSMemberDot res_e ra (jid "msg")
        let res_when = JSMemberDot res_e ra (jid "when")
        let res_local = JSMemberDot res_e ra (jid "_local")
        let run_ss = zipWith (defcon . jid) beforeNames beforeClosures
        let def_local = case all (== T_Null) local_data_tys of
                        True  -> [] -- No cases use `local`, don't create variable
                        False -> [defcon local_e res_local]
        let only_body = run_ss <> cr_ss <> [defcon msg_e res_msg, defcon when_e res_when] <> def_local
        isClass <- is_class who_s
        let who_is_this_ss =
              case (isBound, isClass) of
                (True, _) ->
                  [JSMethodCall (jid "require") a (JSLOne check_who) a sp]
                (False, False) ->
                  [JSMethodCall (JSMemberDot who_e a (jid "set")) a (JSLOne (jid "this")) a sp]
                (False, True) -> []
        isApi <- is_api $ bpack who
        (penvs, all_afters) <- fmap unzip $ mapM (getAfter who_e isApi who True) $ zip4 ats after_es ([0 ..] :: [Int]) local_data_tys
        let cfc_switch_case =
              map
                (\(i, as) ->
                   let ca = jsaList a as
                    in JSCase a (jid $ partCase i) ca $ who_is_this_ss <> as)
                $ indexed all_afters
        let cfc_only = makeOnly who_e only_body
        let cfc_local_penvs = M.singleton (bpack who) $ M.unions penvs
        locAt c_at $ return CompiledForkCase {..}
  (isFork, before_tc_ss, pay_e, pay_req, tc_head_e, after_tc_ss, local_penvs) <-
    case cases of
      [ForkCase {..}] -> do
        before_e' <- augWithChecks fc_check fc_before
        after_e'  <- augWithChecks fc_check fc_after
        (_, only_body) <- forkOnlyHelp fc_who_e fc_at before_e' msg_e when_e loc_id
        let tc_head_e = fc_who_e
        let before_tc_ss = [makeOnly fc_who_e only_body]
        let pa = jsa fc_pay
        let (fc_pay_e, fc_pay_req) = splitPayExpr a fc_pay
        fc_pay_e'   <- augWithChecks fc_check fc_pay_e
        let pay_e' = JSCallExpression fc_pay_e' pa (JSLOne msg_e) pa
        fc_pay_req' <- augWithChecks fc_check fc_pay_req
        let pay_req' = jsArrowExpr pa [] $ jsCall pa fc_pay_req' [msg_e]
        isApi <- is_api $ bpack fc_who
        (_, after_tc_ss) <- getAfter fc_who_e isApi fc_who False (fc_at, after_e', 0 :: Int, T_Null)
        return $ (False, before_tc_ss, pay_e', pay_req', tc_head_e, after_tc_ss, mempty)
      _ -> do
        casel <- mapM go $ groupBy forkCaseSameParticipant cases
        let cases_data_def = concatMap cfc_data_def casel
        let cases_local_data_def = concatMap cfc_local_data_def casel
        let cases_part_es = map cfc_part_e casel
        let cases_pay_props = concatMap cfc_pay_prop casel
        let cases_pay_req_props = concatMap cfc_pay_req_prop casel
        let cases_switch_cases = concatMap cfc_switch_case casel
        let cases_onlys = map cfc_only casel
        let cases_local_penvs = map cfc_local_penvs casel
        let fd_def = JSCallExpression (jid "Data") a (JSLOne $ mkobj cases_data_def) a
        let fld_def = JSCallExpression (jid "Data") a (JSLOne $ mkobj cases_local_data_def) a
        let data_decls = JSLOne $ JSVarInitExpression fd_e $ JSVarInit a fd_def
        let local_data_decls = JSLOne $ JSVarInitExpression fld_e $ JSVarInit a fld_def
        let data_ss = [JSConstant a data_decls sp, JSConstant a local_data_decls sp]
        let tc_head_e = JSCallExpression (jid "race") a (toJSCL cases_part_es) a
        let pay_e = JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_pay_props) a
        let pay_req = jsArrowExpr a [] $ JSCallExpression (JSMemberDot msg_e a (jid "match")) a (JSLOne $ mkobj cases_pay_req_props) a
        let switch_ss = [JSSwitch a a msg_e a a cases_switch_cases a sp]
        let before_tc_ss = data_ss <> cases_onlys
        return $ (True, before_tc_ss, pay_e, pay_req, tc_head_e, switch_ss, cases_local_penvs)
  let tc_pub_e = JSCallExpression (JSMemberDot tc_head_e a (jid "publish")) a (JSLOne msg_e) a
  let tc_api_e = JSCallExpression (JSMemberDot tc_pub_e a (jid ".api")) a JSLNil a
  let tc_when_e = JSCallExpression (JSMemberDot tc_api_e a (jid "when")) a (JSLOne when_e) a
  -- START: Non-network token pay
  let decodePay' = \case
        JSArrayLiteral aa ts _ -> do
          let network_pay_var = jid "networkTokenPay"
          let nnts = map (jse_expect_id at) $ jsa_flatten ts
          let mkint :: SLVar -> Integer -> (JSExpression, JSExpression, JSStatement)
              mkint nnt i = (js, ret, vps)
                where
                  amt = jid $ "amt" <> show i
                  nntok = jid $ "nntok" <> show i
                  nnt' = jid nnt
                  js = jsArrayLiteral aa [amt, nntok]
                  ret = jsArrayLiteral aa [amt, nnt']
                  vps =
                    JSExpressionStatement
                      (jsCall
                         aa
                         (jid "assert")
                         [ JSExpressionBinary nntok (JSBinOpEq aa) (jid nnt)
                         , JSStringLiteral aa ("'Expected the non-network token at position " <> show (i + 1) <> " in `case` payment to be equal to " <> show (pretty nnt) <> " as specified in `.paySpec`'")
                         ])
                      sp
          let nnts_int = zipWith mkint nnts [0 ..]
          let (nnts_js, nnts_ret, verifyPaySpec) = unzip3 nnts_int
          let pay_var tl = JSArrayLiteral aa (intercalate [JSArrayComma aa] $ map ((: []) . JSArrayElement) $ network_pay_var : tl) aa
          let pay_ss = [JSConstant aa (JSLOne $ JSVarInitExpression (pay_var nnts_js) $ JSVarInit aa pay_e) sp] <> verifyPaySpec <> [JSReturn aa (Just (pay_var nnts_ret)) sp]
          let pay_call = jsCallThunk aa $ jsThunkStmts aa pay_ss
          return pay_call
        _ -> expect_ $ Err_InvalidPaySpec
  pay_expr <-
    case slf_mnntpay of
      Just pp -> decodePay decodePay' pp
      Nothing -> return pay_e
  let tc_pay_e = JSCallExpression (JSMemberDot tc_when_e a (jid "pay")) a (toJSCL [pay_expr, pay_req]) a
  -- END: Non-network token pay
  let tc_time_e =
        case mtime of
          Nothing -> tc_pay_e
          Just (at', targs) ->
            let ta = at2a at'
             in JSCallExpression (JSMemberDot tc_pay_e ta (jid "timeout")) ta (toJSCL targs) ta
  let tc_fork_e =
        case isFork of
          True -> JSCallExpression (JSMemberDot tc_time_e a (jid ".fork")) a (toJSCL []) a
          False -> tc_time_e
  let tc_e = tc_fork_e
  let tc_ss = [JSExpressionStatement tc_e sp]
  let exp_ss = before_tc_ss <> tc_ss <> after_tc_ss
  -- liftIO $ putStrLn $ "Fork Output"
  -- liftIO $ putStrLn $ show $ pretty exp_ss
  sco <- asks e_sco
  let sco_penvs' = M.unionWith M.union (sco_penvs sco) $ M.unions local_penvs
  let sco' = sco { sco_penvs = sco_penvs' }
  locSco sco' $ evalStmt $ exp_ss <> ks

modifyLastM :: Monad m => (a -> m a) -> [a] -> m [a]
modifyLastM f l =
  case reverse l of
    [] -> return []
    x : l' -> do
      x' <- f x
      return $ reverse (x' : l')

getReturnAnnot :: JSExpression -> Maybe JSAnnot
getReturnAnnot = \case
  JSArrowExpression _ _ b -> cb b
  _ -> Nothing
  where
    cb = \case
      JSConciseExprBody e -> Just $ jsa e
      JSConciseFunBody (JSBlock _ stmts _) ->
        case reverse stmts of
          JSReturn a _ _:_ -> Just a
          _ -> Nothing

deconstructFun' :: (SLValTy -> EvalError) -> JSExpression -> App ([JSExpression], JSBlock)
deconstructFun' mkErr = \case
  a@(JSArrowExpression _ _ cb) -> return $ (getFormals a, jsArrowBodyToRetBlock cb)
  JSExpressionParen _ e _ -> deconstructFun' mkErr e
  e -> do
    e' <- evalExpr e
    case snd e' of
      SLV_Clo _ _ (SLClo _ args bl _) -> return $ (args, bl)
      ow -> expect_ . mkErr =<< mkValType ow

deconstructFunStmts' :: (SLValTy -> EvalError) -> JSExpression -> App ([JSExpression], [JSStatement])
deconstructFunStmts' mkErr f = do
  (args, bl) <- deconstructFun' mkErr f
  return $ (args, jsBlockToStmts bl)

deconstructFun :: JSExpression -> App ([JSExpression], JSBlock)
deconstructFun = deconstructFun' Err_Eval_NotApplicable

deconstructFunStmts :: JSExpression -> App ([JSExpression], [JSStatement])
deconstructFunStmts = deconstructFunStmts' Err_Eval_NotApplicable

doParallelReduce :: JSExpression -> ParallelReduceRec -> App [JSStatement]
doParallelReduce lhs (ParallelReduceRec {..}) = locAt slpr_at $ do
  let pr_at = slpr_at
  let pr_mode = slpr_mode
  let init_e = slpr_init
  let pr_invs = slpr_minv
  let pr_mwhile = slpr_mwhile
  let pr_cases = slpr_cases
  let pr_apis = slpr_apis
  let pr_api_s = slpr_api_s
  let pr_mtime = slpr_mtime
  let pr_defs = slpr_defs
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
  while_e <- want "while" pr_mwhile
  let var_decls = JSLOne (JSVarInitExpression lhs (JSVarInit a init_e))
  let var_s = JSVariable a var_decls sp
  let inv_s = flip map pr_invs $ \ (inv_e, minv_lab) ->
                JSMethodCall (JSIdentifier a "invariant") a (toJSCL $ inv_e : catMaybes [minv_lab]) a sp
  let fork_e0 = jsCall a (jid "fork") []
  let injectContinueIntoBody addArgs isTimeout e = do
        let ea = jsa e
        let ra = fromMaybe ea $ getReturnAnnot e
        let esp = JSSemi ea
        let def_e = JSIdentifier ra $ prid "res"
        let dotargs = JSSpreadExpression a (JSIdentifier ea $ prid "args")
        let argsl = if addArgs then [dotargs] else []
        (params, body) <- deconstructFun e
        let (e', loc_args) = case (isTimeout, params) of
              (False, msg:loc:_) -> (jsArrowBlock ea [msg] body, [loc])
              _ -> (e, [])
        let call_og = jsCall ea e' argsl
        let def_s = JSConstant ea (JSLOne $ JSVarInitExpression def_e $ JSVarInit ea call_og) esp
        let asn_s = JSAssignStatement lhs (JSAssign ra) def_e esp
        let continue_s = JSContinue ea JSIdentNone esp
        return $ jsArrowStmts ea (argsl <> loc_args) [def_s, asn_s, continue_s]
  fork_e1 <-
    case pr_mtime of
      Nothing -> return fork_e0
      Just (mode, t_at, args) -> locAt t_at $
        case (mode, args) of
          (PRM_ThrowTimeout, [t_e]) ->
            callTimeout [t_e, jsThunkStmts ta [JSThrow ta lhs semi]]
          (PRM_TimeRemaining, [t_e]) -> do
            let dot o f = JSCallExpressionDot o ta f
            let publish = dot (jid "Anybody") $ jid "publish"
            doTimeout t_e $
              jsThunkStmts ta $
                [ JSExpressionStatement (jsCallThunk ta publish) semi
                , JSReturn ta (Just lhs) semi
                ]
          (PRM_Timeout, [t_e, t_fn]) -> doTimeout t_e t_fn
          (PRM_Timeout, [t_e]) -> callTimeout [t_e]
          (PRM_Timeout, _) ->
            expect_ $ Err_ParallelReduceBranchArgs "timeout" 2 args
          (PRM_ThrowTimeout, _) ->
            expect_ $ Err_ParallelReduceBranchArgs "throwTimeout" 1 args
          (PRM_TimeRemaining, _) ->
            expect_ $ Err_ParallelReduceBranchArgs "timeRemaining" 1 args
          _ -> impossible "pr_mtime must be PRM_TimeRemaining or PRM_Timeout"
        where
          semi = JSSemiAuto
          timeOutId = jid "timeout"
          doTimeout t_e t_fn = do
            t_fn' <- injectContinueIntoBody False True t_fn
            callTimeout [t_e, t_fn']
          callTimeout = return . jsCall ta (JSMemberDot fork_e0 ta timeOutId)
          ta = ao t_at
  fork_e2 <-
    case slpr_mpay of
      Nothing -> return fork_e1
      Just toks ->
        return $ jsCall a (JSMemberDot fork_e1 a $ jid "paySpec") [toks]
  let api2case isSingleFun (api_at, api_es) = locAt api_at $ do
        (api_at,) <$> doForkAPI2Case isSingleFun api_es
  apis_cs  <- mapM (api2case False) pr_apis
  api_s_cs <- mapM (api2case True) pr_api_s
  let pr_cases' = pr_cases <> apis_cs <> api_s_cs
  let forkcase fork_eN (case_at, case_es) = do
        let ca = ao case_at
        jsCall ca (JSMemberDot fork_eN ca (jid "case"))
          <$> modifyLastM (injectContinueIntoBody True False) case_es
  fork_e3 <- foldM forkcase fork_e2 pr_cases'
  let fork_e = fork_e3
  let fork_s = JSExpressionStatement fork_e sp
  let commit_s = JSMethodCall (jid "commit") a JSLNil a sp
  let while_body = [commit_s, fork_s]
  let while_s = JSWhile a a while_e a $ JSStatementBlock a while_body a sp
  block_ss <- flip concatMapM pr_defs $ \def ->
                locAtf (srcloc_jsa "define" $ jsa def) $
                  snd <$> deconstructFunStmts' (const Err_ParallelReduce_DefineBlock) def
  let block_sb = JSStatementBlock a block_ss a sp
  let pr_ss = [var_s, block_sb] <> inv_s <> [while_s]
  -- liftIO $ putStrLn $ "ParallelReduce"
  -- liftIO $ putStrLn $ show $ pretty pr_ss
  return $ pr_ss

jsArrow :: JSAnnot -> [JSExpression] -> JSStatement -> JSExpression
jsArrow a args s = JSArrowExpression args' a (jsStmtToConciseBody s)
  where
    args' = JSParenthesizedArrowParameterList a (toJSCL args) a

jsArrowExpr :: JSAnnot -> [JSExpression] -> JSExpression -> JSExpression
jsArrowExpr a args e = jsArrow a args $ JSExpressionStatement e (a2sp a)

jsArrowStmts :: JSAnnot -> [JSExpression] -> [JSStatement] -> JSExpression
jsArrowStmts a args s = jsArrow a args $ JSStatementBlock a s a (a2sp a)

jsArrowBlock :: JSAnnot -> [JSExpression] -> JSBlock -> JSExpression
jsArrowBlock a args (JSBlock _ s _) = jsArrow a args $ JSStatementBlock a s a (a2sp a)

jsThunkStmts :: JSAnnot -> [JSStatement] -> JSExpression
jsThunkStmts a = jsArrowStmts a []

jsCallThunk :: JSAnnot -> JSExpression -> JSExpression
jsCallThunk a e = jsCall a e []

jsCall :: JSAnnot -> JSExpression -> [JSExpression] -> JSExpression
jsCall a f as = JSCallExpression f a (toJSCL as) a

jsConst :: JSAnnot -> JSExpression -> JSExpression -> JSStatement
jsConst a l r = JSConstant a (JSLOne $ JSVarInitExpression l $ JSVarInit a r) (a2sp a)

jsObjectLiteral :: JSAnnot -> M.Map String JSExpression -> JSExpression
jsObjectLiteral a m =
  JSObjectLiteral a (JSCTLNone $ toJSCL $ map rm1 $ M.toAscList m) a
  where
    rm1 (k, t) = JSPropertyNameandValue (JSPropertyIdent a k) a [t]

jsArrayLiteral :: JSAnnot -> [JSExpression] -> JSExpression
jsArrayLiteral a es = JSArrayLiteral a (map JSArrayElement es) a

evalStmtTrampoline :: JSSemi -> [JSStatement] -> SLVal -> App SLStmtRes
evalStmtTrampoline sp ks ev =
  case findStmtTrampoline ev of
    Just st -> st sp ks
    Nothing ->
      typeOf ev >>= \case
        (T_Null, _) -> evalStmt ks
        (ty, _) -> locAtf (srcloc_mtake_lab $ srclocOf ev) $
                    expect_ $ Err_Block_NotNull ty

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
        whodv <- ctxt_lift_expr (DLVar at' (Just (at', bunpack who)) T_Address) (DLE_PartSet at' who addr_da)
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
  SLV_Form (SLForm_Part_ToConsensus tcr) | slptc_mode tcr == Nothing -> Just $ \_ ks -> doToConsensus ks tcr
  SLV_Form (SLForm_fork_partial fr) | slf_mode fr == Nothing -> Just $ \_ ks -> doFork ks fr
  SLV_Prim SLPrim_committed -> Just $ \_ ks -> do
    ensure_mode SLM_ConsensusStep "commit"
    sco <- e_sco <$> ask
    fs <- e_stack <$> ask
    at <- withAt id
    st <- readSt id
    uni <- readUniverse
    setSt $
      st
        { st_mode = SLM_Step
        , st_after_ctor = True
        }
    let sco' = sco_set "this" (SLSSVal at Public uni $ SLV_Kwd $ SLK_this) sco
    (steplifts, cr) <-
      captureLifts $
        locSco sco' $ evalStmt ks
    saveLift $ DLS_FromConsensus at fs steplifts
    return $ cr
  SLV_Prim SLPrim_inited -> Just $ \_ ks -> do
    ensure_mode SLM_AppInit "init"
    e_appR <- fromRight (impossible "init") . e_appr <$> ask
    env <-
      (liftIO $ readIORef e_appR) >>= \case
        AIS_Init {..} -> do
          liftIO $ readIORef aisi_env
        _ -> impossible "init"
    liftIO $ writeIORef e_appR $ AIS_Inited env
    st <- readSt id
    setSt $
      st
        { st_mode = SLM_Step
        , st_live = True
        , st_after_ctor = False
        }
    doBalanceInit TM_Balance Nothing
    evalStmt ks
  _ -> Nothing

doExit :: App ()
doExit = do
  ensure_modes [SLM_Step, SLM_AppInit] "exit"
  ensure_live "exit"
  at <- withAt id
  let zero = SLV_Int at nn 0
  let lab = "application exit"
  let one mtok = doBalanceAssert mtok zero S_PEQ $ "balance zero at " <> lab
  one Nothing
  mapM_ (one . Just . DLA_Var) =<< readSt st_toks
  let mintedEnsureDestroyed tokv = tokenMetaAssert TM_Destroyed (DLA_Var tokv) (SLV_Bool at True) S_PEQ $ "token destroyed at " <> lab
  mapM_ mintedEnsureDestroyed =<< readSt st_toks_c
  saveLift $ DLS_Stop at
  st <- readSt id
  setSt $ st {st_live = False}

doWhileLikeInitEval :: JSExpression -> JSExpression -> App (M.Map SLVar DLVar, DLAssignment, SLScope)
doWhileLikeInitEval lhs rhs = do
  vars_env <- unchangedSt $ evalDecl True lhs rhs
  let help v (SLSSVal at _ _ val) = do
        (t, da) <- typeOf val
        dv <- ctxt_mkvar $ DLVar at (Just (at, v)) t
        return $ (dv, da)
  helpm <- M.traverseWithKey help vars_env
  uni <- readUniverse
  let unknown_var_env = M.map (\(dv, _) -> SLSSVal (srclocOf dv) Public uni (SLV_DLVar dv)) helpm
  -- We remove "this" afterwards because it is dangerous
  let rm_this e = return $ M.delete "this" e
  sco_env' <- sco_update_and_mod DisallowShadowing unknown_var_env rm_this
  let init_daem = M.fromList $ M.elems helpm
  let init_vars = M.map fst helpm
  init_dam <- compileArgExprMap init_daem
  let init_dl = DLAssignment init_dam
  return $ (init_vars, init_dl, sco_env')

doWhileLikeContinueEval :: JSExpression -> M.Map SLVar DLVar -> SLSVal -> App ()
doWhileLikeContinueEval lhs whilem (rhs_lvl, rhs_v) = do
  at <- withAt id
  uni <- readUniverse
  let merr = Just $ Err_LoopVariableLength $ srclocOf rhs_v
  decl_env <- evalDeclLHS False merr rhs_lvl mempty rhs_v lhs
  stEnsureMode SLM_ConsensusStep
  forM_
    (M.keys decl_env)
    (\v ->
       case M.lookup v whilem of
         Nothing -> expect_ $ Err_Eval_ContinueNotLoopVariable v
         Just _ -> return ())
  let f (v, dv) = do
        let sv = case M.lookup v decl_env of
              Nothing -> SLSSVal at Public uni $ SLV_DLVar dv
              Just x -> x
        let DLVar eloc _ et _ = dv
        val <- ensure_public $ sss_sls sv
        dae <- typeCheck_d (Just eloc) et val
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
        compileCheckType Nothing rest . snd =<< e
  return $ DLSBlock at fs e_lifts e_da

evalExportClosureToBlock :: SLClo -> [SLSVal] -> Maybe SLTypeFun -> DLType -> App DLSBlock
evalExportClosureToBlock clo sargs mtf rest = do
  at <- withAt id
  let sv = SLV_Clo at mtf clo
  evalModeToBlock SLM_Export rest $ do
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
        mi <- mapLookup mv
        fa <- compileCheckType (Just $ dlmi_at mi) (dlmi_kt mi) fv
        return $ SLLV_MapRef at mv fa
      _ ->
        expect_t cv $ Err_Eval_RefNotRefable
  e -> expect_ $ Err_LValue_IllegalJS e

evalAssign :: SLSVal -> SLLValue -> App ()
evalAssign rhs = \case
  SLLV_MapRef _at mv mc -> do
    ensure_mode SLM_ConsensusStep "Map.set"
    mapSet mv mc =<< ensure_public rhs

getInvariant :: JSStatement -> Maybe (JSCommaList JSExpression)
getInvariant = \case
  JSMethodCall (JSIdentifier _ "invariant") _ inv _ _ -> Just inv
  _ -> Nothing

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
    br <- locAtf (srcloc_jsa "block pre" a) $ evalStmt ss'
    locAtf (srcloc_after_semi "block post" a sp) $ retSeqn br ks
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
        SLV_Form (SLForm_parallel_reduce_partial prr) -> do
          pr_ss <- doParallelReduce lhs prr
          evalStmt (pr_ss <> ks)
        SLV_Form (SLForm_apiCall_partial acr) -> do
          ac_ss <- doApiCall lhs acr
          evalStmt (ac_ss <> ks)
        _ -> do
          addl_env <- evalDeclLHS True Nothing rhs_lvl mempty rhs_v lhs
          sco' <- sco_update addl_env
          locAtf (srcloc_after_semi lab a sp) $ locSco sco' $ evalStmt ks
  (cont@(JSContinue a _ sp) : cont_ks) ->
    evalStmt $ assign : cont : cont_ks
    where
      assign = JSAssignStatement lhs op rhs sp
      lhs = jsArrayLiteral a []
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
          saveLift =<< (checkCond om $ DLS_If at Nothing (DLA_Var cond_dv) sa tlifts flifts)
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
    locAtf (srcloc_after_semi "expr stmt" (jsa e) sp) $
      evalStmtTrampoline sp ks sev
  ((JSAssignStatement lhs op rhs asp) : ks) ->
    case (op, ks) of
      ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) -> do
        let lab = "continue"
        let var_at = srcloc_jsa lab var_a
        rhs_sv <- locAtf var_at $ evalExpr rhs
        ensure_mode SLM_ConsensusStep lab
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
          (mt, da) <-
            typeOfM (snd sev) >>= \case
              Just (t, dae) -> (,) (Just t) <$> compileArgExpr (Just t) dae
              Nothing -> return $ (Nothing, DLA_Literal $ DLL_Null)
          saveLift $ DLS_Return at' ret da
          expect_empty_tail lab a sp ks
          return $ SLStmtRes sco [(at', mt, sev, False)]
  (JSSwitch a _ de _ _ cases _ sp : ks) -> do
    locAtf (srcloc_jsa "switch" a) $ do
      at' <- withAt id
      uni <- readUniverse
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
      let case_insert k v@(at1, _, _) seenDefault m =
            case M.lookup k m of
              Nothing -> return $ M.insert k v m
              Just (at0, _, _) ->
                case seenDefault of
                  Just def_at ->
                    locAt at1 $ expect_ $ Err_Switch_UnreachableCase at1 k def_at
                  Nothing ->
                    locAt at1 $ expect_ $ Err_Switch_DoubleCase at0 at1 (Just k)
      let case_minserts cs v m = M.unions $ m : map (flip M.singleton v) cs
      let add_case (seenDefault, casem0) = \case
            JSCase ca ve _ body ->
              (,) seenDefault <$> case_insert vn (at_c, True, body) seenDefault casem0
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
      let select at_c shouldBind body vv = locAt at_c $ do
            let addl_env =
                  case shouldBind of
                    True -> M.singleton de_v (sls_sss at_c uni (de_lvl, vv))
                    False -> mempty
            sco'' <- locSco sco' $ sco_update_ AllowShadowing addl_env
            locSco sco'' $ evalStmt body
      let select_one vn (at_c, shouldBind, body) = do
            let vt = varm M.! vn
            dv' <- ctxt_mkvar $ DLVar at_c (Just (at_c, de_v)) vt
            vv <- case vt of
                  T_Null -> return $ SLV_Null at_c "case"
                  _ -> return $ SLV_DLVar dv'
            return $ (dv', at_c, select at_c shouldBind body vv)
      let select_all sv = do
            dv <- case sv of
              SLV_DLVar dv -> return dv
              _ -> impossible "select_all: not dlvar or interact field"
            let casemm = M.mapWithKey select_one casesm
            let cmb :: (Maybe SLState, StmtAnnot, Maybe SLStmtRets, SwitchCasesM DLStmts) -> (SLVar, App (DLVar, SrcLoc, App SLStmtRes)) -> App (Maybe SLState, StmtAnnot, Maybe SLStmtRets, SwitchCasesM DLStmts)
                cmb (mst', sa', mrets', casemm') (vn, casem) = do
                  (dv', at_c, casem') <- casem
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
                    let casemm'' = M.insert vn (SwitchCase (v2vl dv') case_lifts) casemm'
                    return $ (mst'', sa'', Just rets'', casemm'')
            (mst', sa', mrets', casemm') <-
              foldM cmb (Nothing, mempty, Nothing, mempty) $ M.toList casemm
            let rets' = maybe mempty id mrets'
            maybe (return ()) setSt mst'
            saveLift =<< checkCond om
              =<< withAt (\at -> DLS_Switch at dv sa' (SwitchCases casemm'))
            return $ SLStmtRes sco rets'
      fr <-
        case de_val of
          SLV_Data _ t vn vv -> do
            let (at_c, shouldBind, body) = (casesm M.! vn)
            whenUsingStrict $ ignoreAll $ select_all (SLV_DLVar $ DLVar sb Nothing (T_Data t) 0)
            select at_c shouldBind body vv
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
            : rst
          ) -> do
            let (inv_ss, rst') = span (isJust . getInvariant) rst
            let invs = mapMaybe getInvariant inv_ss
            when (null invs) $ do
              expect_ $ Err_Block_Variable
            (while_a, cond_a, while_cond, while_body, ks) <-
                  case rst' of
                    JSWhile wa ca wc _ wb : ks -> return (wa, ca, wc, wb, ks)
                    _ -> expect_ Err_Block_Variable
            locAtf (srcloc_jsa "while" while_a) $ do
              at <- withAt id
              ensure_mode SLM_ConsensusStep "while"
              invs' <- forM invs $ \ inv ->
                    case parseJSFormals at inv of
                      [x, y] -> return (x, Just y)
                      [x]    -> return (x, Nothing)
                      _ -> expect_ Err_Block_Variable
              (while_lhs, while_rhs) <- destructDecls while_decls
              (init_vars, init_dl, sco_env') <- doWhileLikeInitEval while_lhs while_rhs
              let add_preamble a e = jsCallThunk a $ jsThunkStmts a $ blk_ss <> [JSReturn a (Just e) (a2sp a)]
              invs'' <- forM invs' $ \(invariant_e, inv_lab_e) -> do
                    let inv_a = jsa invariant_e
                    inv_lab <- forM inv_lab_e $ mustBeBytes . snd <=< evalExpr
                    inv_b <-
                      locAtf (srcloc_jsa "invariant" inv_a) $
                        locSco sco_env' $
                          locWhileInvariant $
                            evalPureExprToBlock (add_preamble inv_a invariant_e) T_Bool
                    return $ DLInvariant inv_b inv_lab
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
                  (\at' ->
                    DLS_While at' init_dl invs'' cond_b body_lifts)
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

mapNew :: DLType -> DLType -> App DLMVar
mapNew dlmi_kt dlmi_ty = do
  dlmi_at <- withAt id
  MapEnv {..} <- e_mape <$> ask
  mc <- DLMVar <$> (liftIO $ incCounter me_id)
  let mci = DLMapInfo {..}
  liftIO $ modifyIORef me_ms $ M.insert mc mci
  return $ mc

mapDel :: DLMVar -> DLArg -> App ()
mapDel mv mc = do
  at <- withAt id
  DLMapInfo {..} <- mapLookup mv
  ctxt_lift_eff $ DLE_MapSet at mv mc dlmi_ty Nothing

mapSet :: DLMVar -> DLArg -> SLVal -> App ()
mapSet mv mc nv = do
  at <- withAt id
  DLMapInfo {..} <- mapLookup mv
  na <- compileCheckType (Just dlmi_at) dlmi_ty nv
  ctxt_lift_eff $ DLE_MapSet at mv mc dlmi_ty $ Just na

mapRef :: DLMVar -> SLVal -> App DLVar
mapRef mv mcv = do
  at <- withAt id
  mi <- mapLookup mv
  mc <- compileCheckType (Just $ dlmi_at mi) (dlmi_kt mi) mcv
  let vt = dlmi_ty mi
  let mt = dlmi_tym mi
  let mkvar = DLVar at Nothing mt
  ctxt_lift_expr mkvar $ DLE_MapRef at mv mc vt
