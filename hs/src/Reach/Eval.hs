module Reach.Eval (compileBundle, defaultEnv) where

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
import Reach.Eval.Types
import Reach.JSUtil
import Reach.Parser
import Reach.Util


type CompiledDApp = M.Map DLMVar DLMapInfo -> DLStmts -> DLProg

compileDApp :: Connectors -> DLSExports -> SLVal -> App CompiledDApp
compileDApp cns exports (SLV_Prim (SLPrim_App_Delay at top_s (top_env, top_use_strict))) = locAt (srcloc_at "compileDApp" Nothing at) $ do
  idr <- asks e_id
  let dlo = app_default_opts idr $ M.keys cns
  let (JSBlock _ top_ss _) = jsStmtToBlock top_s
  let st_after_ctor0 =
        case dlo_deployMode dlo of
          DM_constructor -> True
          DM_firstMsg -> False
  let st_step =
        SLState
          { st_mode = SLM_AppInit
          , st_live = True
          , st_pdvs = mempty
          , st_after_ctor = st_after_ctor0
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
  setSt st_step
  doBalanceInit Nothing
  dli_ctimem <- do
    let no = return $ Nothing
    let yes = do
          time_dv <- ctxt_mkvar (DLVar at Nothing T_UInt)
          doFluidSet FV_lastConsensusTime $ public $ SLV_DLVar time_dv
          return $ Just time_dv
    case dlo_deployMode dlo of
      DM_constructor -> yes
      DM_firstMsg -> no
  _ <- locSco sco $ evalStmt top_ss
  dloSet <- readDlo id
  flip when doExit =<< readSt st_live
  sps <- SLParts <$> (asks e_pie >>= liftIO . readIORef)
  fin_toks <- readSt st_toks
  let dlo' = dloSet {dlo_bals = 1 + length fin_toks}
  dviews <- asks e_views >>= liftIO . readIORef
  return $ \dli_maps final ->
    let dli = DLInit {..}
     in DLProg at dlo' sps dli exports dviews final
compileDApp _ _ _ = impossible "compileDApp called without a Reach.App"

getExports :: SLLibs -> App DLSExports
getExports libs = do
  let getLibExports lib = justValues . M.toList <$> mapM (slToDLExportVal . sss_val) lib
  M.fromList
    <$> concatMapM
      (getLibExports . snd)
      (filter ((/= ReachStdLib) . fst) $ M.toList libs)

compileBundle_ :: Connectors -> JSBundle -> SLLibs -> SLVar -> App CompiledDApp
compileBundle_ cns (JSBundle mods) libm main = do
  exports <- getExports libm
  let exe = getLibExe mods
  let exe_ex = libm M.! exe
  topv <- ensure_public . sss_sls =<< env_lookup LC_CompilerRequired main exe_ex
  compileDApp cns exports topv

defaultEnv :: Connectors -> IO Env
defaultEnv cns = do
    e_id <- newCounter 0
    e_ios <- newIORef mempty
    e_views <- newIORef mempty
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
    e_dlo <- newIORef $ app_default_opts e_id $ M.keys cns
    e_classes <- newIORef mempty
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
    me_id <- newCounter 0
    me_ms <- newIORef mempty
    e_unused_variables <- newIORef mempty
    e_pie <- newIORef mempty
    e_exn <- newIORef $ ExnEnv False Nothing Nothing SLM_Module
    let e_mape = MapEnv {..}
    return (Env {..})

compileBundle :: Env -> Connectors -> JSBundle -> SLLibs -> SLVar -> IO DLProg
compileBundle env cns jsb libm main = do
  mkprog <-
    flip runReaderT env $
        compileBundle_ cns jsb libm main
  ms' <- readIORef $ me_ms $ e_mape env
  final <- readIORef $ e_lifts env
  unused_vars <- readIORef $ e_unused_variables env
  reportUnusedVars $ S.toList unused_vars
  return $ mkprog ms' final
  where
    reportUnusedVars [] = return ()
    reportUnusedVars l@(h : _) =
      expect_throw Nothing (fst h) $ Err_Unused_Variables l
