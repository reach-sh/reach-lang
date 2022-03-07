module Reach.Eval (evalBundle) where

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
import Reach.Parser
import Reach.Util
import Reach.Warning

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
  resr <- liftIO $ newIORef $ AppRes mempty mempty mempty mempty mempty mempty
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
  dli_maps <- liftIO $ readIORef $ me_ms mape
  let dli = DLInit {..}
  let sps_ies = ar_pie
  let sps_apis = ar_isAPI
  let sps = SLParts {..}
  final' <- pandemic' final
  return $ DLProg at final_dlo' sps dli exports ar_views ar_apis ar_events final'
compileDApp _ _ _ = impossible "compileDApp called without a Reach.App"

pandemic' :: DLStmts -> App DLStmts
pandemic' = mapM pandemic

pandemicLV :: DLLetVar -> App DLLetVar
pandemicLV = undefined

pandemicE :: DLExpr -> App DLExpr
pandemicE = undefined

pandemicArg :: DLArg -> App DLArg
pandemicArg = undefined

pandemicVar :: DLVar -> App DLVar
pandemicVar = undefined

pandemicBl :: DLSBlock -> App DLSBlock
pandemicBl = undefined

pandemicSW :: SwitchCases DLStmts -> App (SwitchCases DLStmts)
pandemicSW = undefined

pandemicMT :: Maybe (DLTimeArg, DLStmts) -> App (Maybe (DLTimeArg, DLStmts))
pandemicMT = undefined

pandemicAgn :: DLAssignment -> App DLAssignment
pandemicAgn = undefined

pandemicSend :: M.Map SLPart DLSend -> ReaderT Env IO (M.Map SLPart DLSend)
pandemicSend = undefined

pandemicRecv :: DLRecv DLStmts -> App (DLRecv DLStmts)
pandemicRecv = undefined

pandemic :: DLSStmt -> App DLSStmt
pandemic = \case
  DLS_Let at letv expr -> do
    r1 <- pandemicLV letv
    r2 <- pandemicE expr
    return $ DLS_Let at r1 r2
  DLS_ArrayMap at v1 a1 v2 v3 bl -> do
    r1 <- pandemicVar v1
    r2 <- pandemicArg a1
    r3 <- pandemicVar v2
    r4 <- pandemicVar v3
    r5 <- pandemicBl bl
    return $ DLS_ArrayMap at r1 r2 r3 r4 r5
  DLS_ArrayReduce at v1 a1 a2 v2 v3 v4 bl -> do
    r1 <- pandemicVar v1
    r2 <- pandemicArg a1
    r3 <- pandemicArg a2
    r4 <- pandemicVar v2
    r5 <- pandemicVar v3
    r6 <- pandemicVar v4
    r7 <- pandemicBl bl
    return $ DLS_ArrayReduce at r1 r2 r3 r4 r5 r6 r7
  DLS_If at arg ann sts1 sts2 -> do
    r1 <- pandemicArg arg
    r2 <- pandemic' sts1
    r3 <- pandemic' sts2
    return $ DLS_If at r1 ann r2 r3
  DLS_Switch at v sa sw -> do
    r1 <- pandemicVar v
    DLS_Switch at r1 sa <$> pandemicSW sw
  DLS_Return at i arg -> do
    DLS_Return at i <$> pandemicArg arg
  DLS_Prompt at v ann sts -> do
    DLS_Prompt at v ann <$> pandemic' sts
  DLS_Stop at -> return $ DLS_Stop at
  DLS_Unreachable at ctx s -> return $ DLS_Unreachable at ctx s
  DLS_Only at sl sts -> DLS_Only at sl <$> pandemic' sts
  DLS_ToConsensus at send recv mtime -> do
    r1 <- pandemicSend send
    r2 <- pandemicRecv recv
    r3 <- pandemicMT mtime
    return $ DLS_ToConsensus at r1 r2 r3
  DLS_FromConsensus at cxt sts -> DLS_FromConsensus at cxt <$> pandemic' sts
  DLS_While at agn bl1 bl2 sts -> do
    r1 <- pandemicAgn agn
    r2 <- pandemicBl bl1
    r3 <- pandemicBl bl2
    r4 <- pandemic' sts
    return $ DLS_While at r1 r2 r3 r4
  DLS_Continue at agn -> DLS_Continue at <$> pandemicAgn agn
  DLS_FluidSet at flv arg -> DLS_FluidSet at flv <$> pandemicArg arg
  DLS_FluidRef at v flv -> do
    r1 <- pandemicVar v
    return $ DLS_FluidRef at r1 flv
  DLS_MapReduce at i v1 dlmv arg v2 v3 bl -> do
    r1 <- pandemicVar v1
    r2 <- pandemicArg arg
    r3 <- pandemicVar v2
    r4 <- pandemicVar v3
    r5 <- pandemicBl bl
    return $ DLS_MapReduce at i r1 dlmv r2 r3 r4 r5
  DLS_Throw at arg b -> do
    r1 <- pandemicArg arg
    return $ DLS_Throw at r1 b
  DLS_Try at sts1 v sts2 -> do
    r1 <- pandemic' sts1
    r2 <- pandemicVar v
    r3 <- pandemic' sts2
    return $ DLS_Try at r1 r2 r3
  DLS_ViewIs at sl1 sl2 expo -> return $ DLS_ViewIs at sl1 sl2 expo
  DLS_TokenMetaGet tm at v a i -> do
    r1 <- pandemicVar v
    r2 <- pandemicArg a
    return $ DLS_TokenMetaGet tm at r1 r2 i
  DLS_TokenMetaSet tm at a1 a2 i b -> do
    r1 <- pandemicArg a1
    r2 <- pandemicArg a2
    return $ DLS_TokenMetaSet tm at r1 r2 i b

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
  let e_appr = Left $ app_default_opts e_id e_droppedAsserts $ M.keys cns
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

evalBundle :: Connectors -> JSBundle -> IO (S.Set SLVar, (SLVar -> IO DLProg))
evalBundle cns (JSBundle mods) = do
  evalEnv <- makeEnv cns
  let run = flip runReaderT evalEnv
  let exe = fst $ hdDie mods
  (shared_lifts, libm) <-
    run $
      captureLifts $
        evalLibs cns mods
  let exe_ex = libm M.! exe
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
