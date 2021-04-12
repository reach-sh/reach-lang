module Reach.Eval (compileBundle) where

import Control.Arrow (second)
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
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

app_default_opts :: Counter -> [T.Text] -> DLOpts
app_default_opts idxr cns =
  DLOpts
    { dlo_deployMode = DM_constructor
    , dlo_verifyArithmetic = False
    , dlo_verifyPerConnector = False
    , dlo_connectors = cns
    , dlo_counter = idxr
    , dlo_bals = 1
    }

app_options :: M.Map SLVar (DLOpts -> SLVal -> Either String DLOpts)
app_options =
  M.fromList
    [ ("deployMode", opt_deployMode)
    , ("verifyArithmetic", opt_verifyArithmetic)
    , ("verifyPerConnector", opt_verifyPerConnector)
    , ("connectors", opt_connectors)
    ]
  where
    opt_verifyPerConnector opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyPerConnector = b}
        _ -> Left $ "expected boolean"
    opt_verifyArithmetic opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyArithmetic = b}
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

type CompiledDApp = M.Map DLMVar DLMapInfo -> DLStmts -> DLProg

compileDApp :: Connectors -> DLExports -> SLVal -> App CompiledDApp
compileDApp cns exports (SLV_Prim (SLPrim_App_Delay at opts part_ios top_formals top_s (top_env, top_use_strict))) = locAt (srcloc_at "compileDApp" Nothing at) $ do
  at' <- withAt id
  idr <- e_id <$> ask
  let use_opt k SLSSVal {sss_val = v, sss_at = opt_at} acc =
        case M.lookup k app_options of
          Nothing ->
            expect_thrown opt_at $
              Err_App_InvalidOption k (S.toList $ M.keysSet app_options)
          Just opt ->
            case opt acc v of
              Right x -> x
              Left x -> expect_thrown opt_at $ Err_App_InvalidOptionValue k x
  let dlo = M.foldrWithKey use_opt (app_default_opts idr $ M.keys cns) opts
  let make_part (SLCompiledPartInfo {..}) =
        public $ SLV_Participant slcpi_at slcpi_who Nothing Nothing
  let partvs = map make_part part_ios
  let top_args = map (jse_expect_id at) top_formals
  top_vargs <- zipEq (Err_Apply_ArgCount at) top_args partvs
  let top_viargs = map (\(i, pv) -> (i, infectWithId_sls at' i pv)) top_vargs
  let top_rvargs = map (second $ (sls_sss at)) top_viargs
  let (JSBlock _ top_ss _) = (jsStmtToBlock top_s)
  let st_after_ctor0 =
        case dlo_deployMode dlo of
          DM_constructor -> True
          DM_firstMsg -> False
  let st_step =
        SLState
          { st_mode = SLM_Step
          , st_live = True
          , st_pdvs = mempty
          , st_after_ctor = st_after_ctor0
          , st_after_first = False
          , st_toks = mempty
          }
  let classes = S.fromList $ [slcpi_who | SLCompiledPartInfo {..} <- part_ios, slcpi_isClass]
  let ios = M.fromList $ [(slcpi_who, slcpi_io) | SLCompiledPartInfo {..} <- part_ios]
  top_env_wps <- foldlM env_insertp top_env top_rvargs
  let make_penvp (SLCompiledPartInfo {..}) = do
        let iov = ios M.! slcpi_who
        saveLifts slcpi_lifts
        env0 <- locAt slcpi_at $ env_insert "interact" iov top_env_wps
        return $ (slcpi_who, env0)
  penvs <- M.fromList <$> mapM make_penvp part_ios
  let sco =
        SLScope
          { sco_ret = Nothing
          , sco_must_ret = RS_CannotReturn
          , sco_while_vars = Nothing
          , sco_penvs = penvs
          , sco_cenv = top_env_wps
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
  _ <-
    locIOs ios $
      locSco sco $
        locDLO dlo $
          locClasses classes $
            evalStmt top_ss
  flip when doExit =<< readSt st_live
  let sps = SLParts $ M.fromList $ [(slcpi_who, slcpi_ienv) | SLCompiledPartInfo {..} <- part_ios]
  fin_toks <- readSt st_toks
  let dlo' = dlo { dlo_bals = 1 + length fin_toks }
  return $ \dli_maps final ->
    let dli = DLInit {..}
     in DLProg at dlo' sps dli exports final
compileDApp _ _ topv =
  expect_t topv $ Err_Top_NotApp

getExports :: SLLibs -> App DLExports
getExports libs = do
  let getLibExports lib = justValues . M.toList <$> mapM (slToDLExportVal . sss_val) lib
  M.fromList <$> concatMapM (getLibExports . snd)
    (filter ((/= ReachStdLib) . fst) $ M.toList libs)

compileBundle_ :: Connectors -> JSBundle -> SLVar -> App CompiledDApp
compileBundle_ cns (JSBundle mods) main = do
  libm <- evalLibs cns mods
  exports <- getExports libm
  let exe = case mods of
        [] -> impossible $ "compileBundle: no files"
        ((x, _) : _) -> x
  let exe_ex = libm M.! exe
  topv <- ensure_public . sss_sls =<< env_lookup LC_CompilerRequired main exe_ex
  compileDApp cns exports topv

compileBundle :: Connectors -> JSBundle -> SLVar -> IO DLProg
compileBundle cns jsb main = do
  e_id <- newCounter 0
  let e_ios = mempty
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
  let e_dlo = app_default_opts e_id $ M.keys cns
  let e_classes = mempty
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
  e_exn <- newIORef $ ExnEnv False Nothing Nothing SLM_Module
  let e_mape = MapEnv {..}
  mkprog <-
    flip runReaderT (Env {..}) $
      compileBundle_ cns jsb main
  ms' <- readIORef me_ms
  final <- readIORef e_lifts
  unused_vars <- readIORef e_unused_variables
  reportUnusedVars $ S.toList unused_vars
  return $ mkprog ms' final
  where
    reportUnusedVars [] = return ()
    reportUnusedVars l@(h:_) =
      expect_throw Nothing (fst h) $ Err_Unused_Variables l
