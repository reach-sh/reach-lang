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
import Reach.Eval.Error
import Reach.Eval.Types
import Reach.Eval.Core
import Reach.Eval.Module
import Reach.Connector
import Reach.JSUtil
import Reach.Parser
import Reach.Counter
import Reach.Util

app_default_opts :: Counter -> [T.Text] -> DLOpts
app_default_opts idxr cns =
  DLOpts
    { dlo_deployMode = DM_constructor
    , dlo_verifyOverflow = False
    , dlo_verifyPerConnector = False
    , dlo_connectors = cns
    , dlo_counter = idxr
    }

app_options :: M.Map SLVar (DLOpts -> SLVal -> Either String DLOpts)
app_options =
  M.fromList
    [ ("deployMode", opt_deployMode)
    , ("verifyOverflow", opt_verifyOverflow)
    , ("verifyPerConnector", opt_verifyPerConnector)
    , ("connectors", opt_connectors)
    ]
  where
    opt_verifyPerConnector opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyPerConnector = b}
        _ -> Left $ "expected boolean"
    opt_verifyOverflow opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyOverflow = b}
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

compileDApp :: Connectors -> SLVal -> App (DLStmts -> DLProg)
compileDApp cns (SLV_Prim (SLPrim_App_Delay at opts part_ios top_formals top_s top_env)) = locAt (srcloc_at "compileDApp" Nothing at) $ do
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
  let top_viargs = map (\(i, pv) -> (i, infectWithId_sls i pv)) top_vargs
  let top_rvargs = map (second $ (sls_sss at)) top_viargs
  let (JSBlock _ top_ss _) = (jsStmtToBlock top_s)
  let st_after_first0 =
        case dlo_deployMode dlo of
          DM_constructor -> True
          DM_firstMsg -> False
  let st_step =
        SLState
          { st_mode = SLM_Step
          , st_live = True
          , st_pdvs = mempty
          , st_after_first = st_after_first0
          }
  let classes = S.fromList $ [slcpi_who | SLCompiledPartInfo {..} <- part_ios, slcpi_isClass ]
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
          }
  setSt st_step
  doFluidSet FV_balance $ public $ SLV_Int at' 0
  ctimem <- do
    let no = return $ Nothing
    let yes = do
          time_dv <- ctxt_mkvar $ DLVar at "ctime" T_UInt
          doFluidSet FV_lastConsensusTime $ public $ SLV_DLVar time_dv
          return $ Just time_dv
    case dlo_deployMode dlo of
      DM_constructor -> yes
      DM_firstMsg -> no
  _ <- locIOs ios $ locSco sco $ locDLO dlo $ locClasses classes $
    evalStmt top_ss
  flip when doExit =<< readSt st_live
  let sps = SLParts $ M.fromList $ [ (slcpi_who, slcpi_ienv) | SLCompiledPartInfo {..} <- part_ios ]
  let dli = DLInit ctimem
  return $ DLProg at dlo sps dli
compileDApp _ topv =
  expect_t topv $ Err_Top_NotApp

compileBundle_ :: Connectors -> JSBundle -> SLVar -> App (DLStmts -> DLProg)
compileBundle_ cns (JSBundle mods) main = do
  libm <- evalLibs cns mods
  let exe = case mods of
        [] -> impossible $ "compileBundle: no files"
        ((x, _) : _) -> x
  let exe_ex = libm M.! exe
  topv <- ensure_public . sss_sls =<< env_lookup LC_CompilerRequired main exe_ex
  compileDApp cns topv

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
           }
  let e_depth = recursionDepthLimit
  e_st <- newIORef e_stv
  let e_at = srcloc_top
  e_lifts <- newIORef mempty
  mkprog <-
    flip runReaderT (Env {..}) $
      compileBundle_ cns jsb main
  final <- readIORef e_lifts
  return $ mkprog final
