module Reach.Eval (evalBundle, prepareDAppCompiles) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.JavaScript.Parser
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.SL
import Reach.Connector
import Reach.Counter
import Reach.Eval.Core
import Reach.Eval.Error
import Reach.Eval.Module
import Reach.Eval.Types
import Reach.JSUtil
import Reach.Pandemic
import Reach.Parser
import Reach.Util
import Reach.Warning
import qualified Data.ByteString as B
import Data.List.Extra (groupSort)
import Reach.UnsafeUtil (unsafeNub)

compileDApp :: DLStmts -> DLSExports -> SLVal -> App DLProg
compileDApp shared_lifts exports (SLV_Prim (SLPrim_App_Delay at top_s (top_env, top_use_strict))) = locAt (srcloc_lab "compileDApp" at) $ do
  let (JSBlock _ top_ss _) = jsStmtToBlock top_s
  setSt $
    SLState
      { st_mode = SLM_AppInit
      , st_live = False
      , st_pdvs = mempty
      , st_after_ctor = True
      , st_after_first = False
      , st_toks = mempty
      , st_toks_c = mempty
      , st_tok_pos = mempty
      }
  let sco =
        SLScope
          { sco_ret = Nothing
          , sco_must_ret = RS_CannotReturn
          , sco_while_vars = Nothing
          , sco_penvs = mempty
          , sco_cenv = top_env
          , sco_use_strict = top_use_strict
          , sco_use_unstrict = False
          }
  init_dlo <- readDlo id
  envr <- liftIO $ newIORef $ AppEnv mempty init_dlo mempty mempty
  resr <- liftIO $ newIORef $ AppRes mempty mempty mempty mempty mempty mempty mempty mempty
  appr <- liftIO $ newIORef $ AIS_Init envr resr
  mape <- liftIO $ makeMapEnv
  e_droppedAsserts' <- (liftIO . dupeCounter) =<< (e_droppedAsserts <$> ask)
  (these_lifts, final_dlo) <- captureLifts $
    locSco sco $
      local
        (\e ->
           e
             { e_appr = Right appr
             , e_mape = mape
             , e_droppedAsserts = e_droppedAsserts'
             })
        $ do
          void $ evalStmt top_ss
          flip when doExit =<< readSt st_live
          readDlo id
  fin_toks <- readSt st_toks
  didPublish <- readSt st_after_first
  unless (didPublish || null top_ss) $
    liftIO . emitWarning (Just at) $ W_NoPublish
  let final = shared_lifts <> these_lifts
  let final_dlo' =
        final_dlo
          { dlo_bals = 1 + length fin_toks
          , dlo_droppedAsserts = e_droppedAsserts'
          }
  AppRes {..} <- liftIO $ readIORef resr
  aliases <- verifyApiAliases ar_api_alias
  dli_maps <- liftIO $ readIORef $ me_ms mape
  let dli = DLInit {..}
  let sps_ies = ar_pie
  let sps_apis = ar_isAPI
  let sps = SLParts {..}
  infectsR <- asks e_infections
  final' <- liftIO $ flip pan_ final =<< readIORef infectsR
  let dlp_at = at
  let dlp_opts = final_dlo'
  let dlp_parts = sps
  let dlp_init = dli
  let dlp_exports = exports
  let dlp_views = ar_views
  let dlp_apis = ar_apis
  let dlp_aliases = aliases
  let dlp_events = ar_events
  let dlp_stmts = final'
  return $ DLProg {..}
compileDApp _ _ _ = impossible "compileDApp called without a Reach.App"

verifyApiAliases :: M.Map SLVar (Maybe B.ByteString, [SLType]) -> App Aliases
verifyApiAliases m = do
  forM_ (groupSort $ M.elems m) $ \case
    (Just k, doms) -> do
      unless (length doms == length (unsafeNub doms)) $ do
        expect_ $ Err_Alias_Type_Clash $ bunpack k
    (Nothing, _) -> return ()
  return $ M.map fst m

mmapMaybeM :: Monad m => (a -> m (Maybe b)) -> M.Map k a -> m (M.Map k b)
mmapMaybeM f m = M.mapMaybe id <$> mapM f m

getExports :: SLEnv -> App DLSExports
getExports = mmapMaybeM (slToDLExportVal . sss_val)

makeMapEnv :: IO MapEnv
makeMapEnv = do
  me_id <- newCounter 0
  me_ms <- newIORef mempty
  return $ MapEnv {..}

makeEnv :: Connectors -> IO Env
makeEnv cns = do
  e_id <- newCounter 0
  let e_who = Nothing
  let e_stack = []
  let e_stv =
        SLState
          { st_mode = SLM_Module
          , st_live = False
          , st_pdvs = mempty
          , st_after_first = False
          , st_after_ctor = False
          , st_toks = mempty
          , st_toks_c = mempty
          , st_tok_pos = mempty
          }
  let e_sco =
        SLScope
          { sco_ret = Nothing
          , sco_must_ret = RS_CannotReturn
          , sco_while_vars = Nothing
          , -- FIXME change this type to (Either SLEnv (M.Map SLPart SLEnv) and use the left case here so we can remove base_penvs
            sco_penvs = mempty
          , sco_cenv = mempty
          , sco_use_strict = False
          , sco_use_unstrict = False
          }
  let e_depth = recursionDepthLimit
  let e_while_invariant = False
  e_st <- newIORef e_stv
  let e_at = srcloc_top
  e_lifts <- newIORef mempty
  e_vars_tracked <- newIORef mempty
  e_vars_used <- newIORef mempty
  e_infections <- newIORef mempty
  -- XXX revise
  e_exn <- newIORef $ ExnEnv False Nothing Nothing SLM_Module
  e_mape <- makeMapEnv
  e_droppedAsserts <- newCounter 0
  let e_appr = Left $ app_default_opts e_id e_droppedAsserts cns
  return (Env {..})

checkUnusedVars :: App a -> App a
checkUnusedVars m = do
  vt <- liftIO $ newIORef mempty
  vu <- liftIO $ newIORef mempty
  a <- local (\e -> e { e_vars_tracked = vt, e_vars_used = vu }) m
  tracked <- liftIO $ readIORef vt
  used <- liftIO $ readIORef vu
  let unused = S.difference tracked used
  let l = S.toList unused
  case l of
    [] -> return ()
    (at, _) : _ ->
      expect_throw Nothing at $ Err_Unused_Variables l
  return a

evalBundle :: Connectors -> JSBundle -> Bool -> IO (ReaderT Env m a -> m a, DLStmts, M.Map SLVar SLSSVal)
evalBundle cns (JSBundle mods) addToEnvForEditorInfo = do
  evalEnv <- makeEnv cns
  let run = flip runReaderT evalEnv
  let exe = fst $ hdDie mods
  let evalPlus = do
        (shared_lifts, libm) <- captureLifts $ evalLibs cns mods
        let exe_ex = libm M.! exe
        (,,) shared_lifts libm <$>
          case addToEnvForEditorInfo of
            False -> return exe_ex
            True -> do
              let stdlibEnv = libm M.! ReachStdLib
              local (\e -> e { e_sco = ((e_sco e) { sco_cenv = stdlibEnv }) }) $ do
                let envWithBase = M.union exe_ex base_env
                let innerEnv n v = evalAsEnvM v >>= \case
                      Nothing -> mempty
                      Just e -> M.mapKeys (\k -> n <> "." <> k) <$> evalObjEnv e
                innerEnvs <- mapWithKeyM innerEnv $ M.map sss_sls envWithBase
                return $ M.union envWithBase $ M.unions innerEnvs
  (shared_lifts, _, exe_ex) <- run $ evalPlus
  return (run, shared_lifts, exe_ex)

prepareDAppCompiles :: Monad m => (App DLProg -> a) -> DLStmts -> M.Map SLVar SLSSVal -> m (S.Set SLVar, SLVar -> a)
prepareDAppCompiles run shared_lifts exe_ex = do
  let tops =
        M.keysSet $
          flip M.filter exe_ex $
            \v ->
              case sss_val v of
                SLV_Prim SLPrim_App_Delay {} -> True
                _ -> False
  let go getdapp = run $ checkUnusedVars $ do
        exports <- getExports exe_ex
        topv <- ensure_public . sss_sls =<< getdapp
        compileDApp shared_lifts exports topv
  case S.null tops of
    True -> do
      return (S.singleton "default", const $ go $ return defaultApp)
    False -> do
      let go' which = go $ env_lookup LC_CompilerRequired which exe_ex
      return (tops, go')
