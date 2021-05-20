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

compileDApp :: DLStmts -> DLSExports -> SLVal -> App DLProg
compileDApp shared_lifts exports (SLV_Prim (SLPrim_App_Delay at top_s (top_env, top_use_strict))) = locAt (srcloc_at "compileDApp" Nothing at) $ do
  let (JSBlock _ top_ss _) = jsStmtToBlock top_s
  setSt $
    SLState
      { st_mode = SLM_AppInit
      , st_live = False
      , st_pdvs = mempty
      , st_after_ctor = True
      , st_after_first = False
      , st_toks = mempty
      }
  let sco =
        SLScope
          { sco_ret = Nothing
          , sco_must_ret = RS_CannotReturn
          , sco_while_vars = Nothing
          , sco_penvs = mempty
          , sco_cenv = top_env
          , sco_use_strict = top_use_strict
          }
  init_dlo <- readDlo id
  envr <- liftIO $ newIORef $ AppEnv mempty init_dlo mempty
  resr <- liftIO $ newIORef $ AppRes mempty mempty Nothing
  appr <- liftIO $ newIORef $ AIS_Init envr resr
  mape <- liftIO $ makeMapEnv
  (these_lifts, final_dlo) <- captureLifts $ locSco sco $
    local (\e -> e { e_appr = Right appr
                   , e_mape = mape }) $ do
      void $ evalStmt top_ss
      flip when doExit =<< readSt st_live
      readDlo id
  fin_toks <- readSt st_toks
  let final = shared_lifts <> these_lifts
  let final_dlo' = final_dlo {dlo_bals = 1 + length fin_toks}
  AppRes {..} <- liftIO $ readIORef resr
  dli_maps <- liftIO $ readIORef $ me_ms mape
  let dli_ctimem = ar_ctimem
  let dli = DLInit {..}
  return $ DLProg at final_dlo' (SLParts ar_pie) dli exports ar_views final
compileDApp _ _ _ = impossible "compileDApp called without a Reach.App"

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
            }
    let e_depth = recursionDepthLimit
    let e_while_invariant = False
    e_st <- newIORef e_stv
    let e_at = srcloc_top
    e_lifts <- newIORef mempty
    e_unused_variables <- newIORef mempty
    -- XXX revise
    e_exn <- newIORef $ ExnEnv False Nothing Nothing SLM_Module
    e_mape <- makeMapEnv
    let e_appr = Left $ app_default_opts e_id $ M.keys cns
    return (Env {..})

withUnusedVars :: App a -> App a
withUnusedVars m = do
  uvr <- liftIO $ newIORef mempty
  a <- local (\e -> e { e_unused_variables = uvr }) m
  uvs <- liftIO $ readIORef uvr
  let reportUnusedVars = \case
        [] -> return ()
        l@(h : _) ->
          expect_throw Nothing (fst h) $ Err_Unused_Variables l
  reportUnusedVars $ S.toList uvs
  return a

evalBundle :: Connectors -> JSBundle -> IO (S.Set SLVar, (SLVar -> IO DLProg))
evalBundle cns (JSBundle mods) = do
  evalEnv <- makeEnv cns
  let run = flip runReaderT evalEnv . withUnusedVars
  let exe = fst $ hdDie mods
  (shared_lifts, libm) <- run $ captureLifts $
    evalLibs cns mods
  let exe_ex = libm M.! exe
  let tops =
        M.keysSet $
          flip M.filter exe_ex $
            \v ->
              case sss_val v of
                SLV_Prim SLPrim_App_Delay {} -> True
                _ -> False
  let go getdapp = run $ do
        exports <- getExports exe_ex
        topv <- ensure_public . sss_sls =<< getdapp
        compileDApp shared_lifts exports topv
  case S.null tops of
    True -> do
      return (S.singleton "default", const $ go $ return defaultApp)
    False -> do
      let go' which = go $ env_lookup LC_CompilerRequired which exe_ex
      return (tops, go')
