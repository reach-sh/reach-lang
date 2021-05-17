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

getLibExe :: [(p, b)] -> p
getLibExe = \case
  [] -> impossible "getLibExe: no files"
  ((x, _) : _) -> x

findTops :: JSBundle -> SLLibs -> S.Set SLVar
findTops (JSBundle mods) libm = do
  let exe = getLibExe mods
  let exe_ex = libm M.! exe
  M.keysSet $
    flip M.filter exe_ex $
      \v ->
        case sss_val v of
          SLV_Prim SLPrim_App_Delay {} -> True
          _ -> False

dropOtherTops :: SLVar -> SLLibs -> SLLibs
dropOtherTops which =
  M.map (M.filterWithKey (\ k v ->
      case sss_val v of
        SLV_Prim SLPrim_App_Delay {} -> k == which
        _ -> True ))

resetEnvRefs :: Env -> DLStmts -> IO ()
resetEnvRefs env lifts = do
  writeIORef (e_lifts env) lifts
  writeIORef (e_pie env) mempty
  writeIORef (e_ios env) mempty
  writeIORef (e_views env) mempty

evalBundle :: Connectors -> JSBundle -> IO (M.Map SLVar (IO DLProg))
evalBundle cns djp = do
  -- Either compile all the Reach.Apps or those specified by user
  evalEnv <- defaultEnv cns
  let JSBundle mods = djp
  libm <- flip runReaderT evalEnv $ evalLibs cns mods
  lifts <- readIORef $ e_lifts evalEnv
  let tops = findTops djp libm
  let (libm', tops') =
        case S.null tops of
          True -> do
            let name = "default"
            -- `default` will never be accepted by the parser as an identifier
            let exe = getLibExe mods
            (M.insert exe (M.insert name defaultApp $ libm M.! exe) libm, S.singleton name)
          False ->
            (libm, tops)
  let go which = do
        let libm'' = dropOtherTops which libm'
        -- Reutilize env from parsing module, but remove any mutable state
        -- from processing previous top
        resetEnvRefs evalEnv lifts
        compileBundle evalEnv cns djp libm'' which
  return $ M.fromSet go tops'

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
  progDlo <- readDlo id
  flip when doExit =<< readSt st_live
  sps <- fmap SLParts $ asks e_pie >>= liftIO . readIORef
  fin_toks <- readSt st_toks
  let dlo' = progDlo {dlo_bals = 1 + length fin_toks}
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
