module Reach.Eval (evalBundle, prepareDAppCompiles) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.IORef
import qualified Data.Aeson as AS
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
  final' <- pan final
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
  return DLProg {..}
compileDApp _ _ _ = impossible "compileDApp called without a Reach.App"

verifyApiAliases :: M.Map SLVar (Maybe B.ByteString, [SLType]) -> App Aliases
verifyApiAliases m = do
  forM_ (groupSort $ M.elems m) $ \case
    (Just k, doms) -> do
      unless (length doms == length (unsafeNub doms)) $ do
        expect_ $ Err_Alias_Type_Clash $ bunpack k
    (Nothing, _) -> return ()
  return $ M.map fst m

class Pandemic a where
  pan :: a -> App a

instance Pandemic Char where
  pan = return

instance Pandemic DLType where
  pan = return

instance Pandemic a => Pandemic [a] where
  pan = mapM pan

instance Pandemic a => Pandemic (Maybe a) where
  pan = mapM pan

instance (Pandemic b) => Pandemic (Either a b) where
  pan = mapM pan

instance Pandemic DLStmts where
  pan = mapM pan

instance Pandemic AS.Value where
  pan = return

instance Pandemic DLContractNew where
  pan (DLContractNew {..}) =
    DLContractNew <$> pan dcn_code <*> pan dcn_opts

instance Pandemic DLRemote where
  pan (DLRemote s amt as bill malgo) =
    DLRemote <$> pan s <*> pan amt <*> pan as <*> pan bill <*> pan malgo

instance Pandemic DLExpr where
  pan = \case
    DLE_Arg at a -> DLE_Arg at <$> pan a
    DLE_LArg at a  -> DLE_LArg at <$> pan a
    DLE_Impossible at i err -> return $ DLE_Impossible at i err
    DLE_VerifyMuldiv at cxt ct as err -> DLE_VerifyMuldiv at cxt ct <$> pan as <*> pure err
    DLE_PrimOp at primop as -> DLE_PrimOp at primop <$> pan as
    DLE_ArrayRef at a i -> DLE_ArrayRef at <$> pan a <*> pan i
    DLE_ArraySet at a i v -> DLE_ArraySet at <$> pan a <*> pan i <*> pan v
    DLE_ArrayConcat at x y -> DLE_ArrayConcat at <$> pan x <*> pan y
    DLE_TupleRef at a i -> DLE_TupleRef at <$> pan a <*> pure i
    DLE_ObjectRef at a f -> DLE_ObjectRef at <$> pan a <*> pure f
    DLE_Interact at cxt slp s ty as -> DLE_Interact at cxt slp s ty <$> pan as
    DLE_Digest at as -> DLE_Digest at <$> pan as
    DLE_Claim at cxt ct a mbbs -> DLE_Claim at cxt ct <$> pan a <*> pure mbbs
    DLE_Transfer at who da mtok -> DLE_Transfer at <$> pan who <*> pan da <*> pan mtok
    DLE_TokenInit at a -> DLE_TokenInit at <$> pan a
    DLE_TokenAccepted at addr tok -> DLE_TokenAccepted at <$> pan addr <*> pan tok
    DLE_CheckPay at cxt a mtok -> DLE_CheckPay at cxt <$> pan a <*> pan mtok
    DLE_Wait at a -> DLE_Wait at <$> pan a
    DLE_PartSet at slp a -> DLE_PartSet at slp <$> pan a
    DLE_MapRef at mv a -> DLE_MapRef at mv <$> pan a
    DLE_MapSet at mv a marg -> DLE_MapSet at mv <$> pan a <*> pan marg
    DLE_Remote at cxt a ty dr -> DLE_Remote at cxt <$> pan a <*> pan ty <*> pan dr
    DLE_TokenNew at tns -> DLE_TokenNew at <$> pan tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> pan tok <*> pan amt
    DLE_TokenDestroy at a -> DLE_TokenDestroy at <$> pan a
    DLE_TimeOrder at op a b -> DLE_TimeOrder at op <$> pan a <*> pan b
    DLE_EmitLog at lk vars -> DLE_EmitLog at lk <$> pan vars
    DLE_setApiDetails at who dom mc info -> return $ DLE_setApiDetails at who dom mc info
    DLE_GetUntrackedFunds at marg a -> DLE_GetUntrackedFunds at <$> pan marg <*> pan a
    DLE_DataTag at d -> DLE_DataTag at <$> pan d
    DLE_FromSome at mo da -> DLE_FromSome at <$> pan mo <*> pan da
    DLE_ContractNew at cns dr -> DLE_ContractNew at <$> pan cns <*> pan dr
    DLE_ContractFromAddress at a -> DLE_ContractFromAddress at <$> pan a
    DLE_ObjectSet at o k v -> DLE_ObjectSet at <$> pan o <*> pan k <*> pan v
    DLE_TupleSet at t i v -> DLE_TupleSet at <$> pan t <*> pure i <*> pan v

instance Pandemic DLVar where
  pan (DLVar at m_locvar t i) = do
    case m_locvar of
      Nothing -> do
        infections <- asks e_infections
        r <- liftIO $ readIORef infections
        return $ DLVar at (M.lookup i r) t i
      Just _ -> return $ DLVar at m_locvar t i

instance Pandemic DLLetVar where
  pan = \case
    DLV_Eff -> return DLV_Eff
    DLV_Let vc v -> DLV_Let vc <$> pan v

instance Pandemic DLSBlock where
  pan (DLSBlock at cxt sts a) = DLSBlock at cxt <$> pan sts <*> pan a

instance Pandemic DLWithBill where
  pan (DLWithBill nr b nb) = DLWithBill nr <$> pan b <*> pan nb

instance Pandemic Bool where
  pan = return

instance Pandemic DLRemoteALGOOC where
  pan = return

instance Pandemic DLRemoteALGO where
  pan (DLRemoteALGO a b c d e f g h i) = DLRemoteALGO <$> pan a <*> pan b <*> pan c <*> pan d <*> pan e <*> pan f <*> pan g <*> pan h <*> pan i

instance Pandemic DLPayAmt where
  pan (DLPayAmt net ks) = do
    let f (a,b) = (,) <$> pan a <*> pan b
    DLPayAmt <$> pan net <*> mapM f ks

instance Pandemic DLTokenNew where
  pan (DLTokenNew r s t u v w) = do
    DLTokenNew <$> pan r <*> pan s <*> pan t <*> pan u <*> pan v <*> pan w

instance Pandemic DLArg where
  pan = \case
    DLA_Var v -> DLA_Var <$> pan v
    DLA_Constant c -> return $ DLA_Constant c
    DLA_Literal l -> return $ DLA_Literal l
    DLA_Interact sl s t -> return $ DLA_Interact sl s t

instance Pandemic b => Pandemic (a, b) where
  pan (s,a) = (,) s <$> pan a

instance (Pandemic a, Pandemic b, Pandemic c) => Pandemic (a, b, c) where
  pan (x, y, z) = (,,) <$> pan x <*> pan y <*> pan z

instance Pandemic DLLargeArg where
  pan = \case
    DLLA_Array t as -> DLLA_Array t <$> pan as
    DLLA_Tuple as -> DLLA_Tuple <$> pan as
    DLLA_Obj strs_args -> DLLA_Obj <$> pan strs_args
    DLLA_Data m s a -> DLLA_Data m s <$> pan a
    DLLA_Struct vars_args -> DLLA_Struct <$> pan vars_args
    DLLA_Bytes s -> return $ DLLA_Bytes s
    DLLA_StringDyn s -> return $ DLLA_StringDyn s

instance Pandemic DLSend where
  pan (DLSend b r s t) =
    DLSend b <$> pan r <*> pan s <*> pan t

instance Pandemic DLAssignment where
  pan (DLAssignment mvargs) = do
    let f (a,b) = (,) <$> pan a <*> pan b
    r <- mapM f $ M.toList mvargs
    return $ DLAssignment $ M.fromList r

instance Pandemic b => Pandemic (M.Map a b) where
  pan = mapM pan

instance Pandemic a => Pandemic (DLRecv a) where
  pan (DLRecv r s t u v w) =
    DLRecv <$> pan r <*> pan s <*> pan t <*> pan u <*> pan v <*> pan w

instance Pandemic a => Pandemic (DLInvariant a) where
  pan (DLInvariant inv lab) = DLInvariant <$> pan inv <*> pure lab

instance Pandemic DLSStmt where
  pan = \case
    DLS_Let at v e -> do
      DLS_Let at <$> pan v <*> pan e
    DLS_ArrayMap at v1 a1 v2 v3 bl -> do
      DLS_ArrayMap at <$> pan v1 <*> pan a1 <*> pan v2 <*> pan v3 <*> pan bl
    DLS_ArrayReduce at v1 a1 a2 v2 v3 v4 bl -> do
      DLS_ArrayReduce at <$> pan v1 <*> pan a1 <*> pan a2 <*> pan v2 <*> pan v3 <*> pan v4 <*> pan bl
    DLS_If at a ann sts1 sts2 -> DLS_If at <$> pan a <*> pure ann <*> pan sts1 <*> pan sts2
    DLS_Switch at v sa sw -> DLS_Switch at <$> pan v <*> pure sa <*> pan sw
    DLS_Return at i a -> DLS_Return at i <$> pan a
    DLS_Prompt at v ann sts -> DLS_Prompt at <$> pan v <*> pure ann <*> pan sts
    DLS_Stop at -> return $ DLS_Stop at
    DLS_Unreachable at ctx s -> return $ DLS_Unreachable at ctx s
    DLS_Only at sl sts -> DLS_Only at sl <$> pan sts
    DLS_ToConsensus at s r m -> DLS_ToConsensus at <$> pan s <*> pan r <*> pan m
    DLS_FromConsensus at cxt sts -> DLS_FromConsensus at cxt <$> pan sts
    DLS_While at agn bl1 bl2 sts -> do
      DLS_While at <$> pan agn <*> pan bl1 <*> pan bl2 <*> pan sts
    DLS_Continue at agn -> DLS_Continue at <$> pan agn
    DLS_FluidSet at flv a -> DLS_FluidSet at flv <$> pan a
    DLS_FluidRef at v flv -> DLS_FluidRef at <$> pan v <*> pure flv
    DLS_MapReduce at i v1 mv a v2 v3 bl -> do
      DLS_MapReduce at i <$> pan v1 <*> pure mv <*> pan a <*> pan v2 <*> pan v3 <*> pan bl
    DLS_Throw at a b -> DLS_Throw at <$> pan a <*> pure b
    DLS_Try at sts1 v sts2 -> DLS_Try at <$> pan sts1 <*> pan v <*> pan sts2
    DLS_ViewIs at sl1 sl2 expo -> return $ DLS_ViewIs at sl1 sl2 expo
    DLS_TokenMetaGet tm at v a i -> DLS_TokenMetaGet tm at <$> pan v <*> pan a <*> pure i
    DLS_TokenMetaSet tm at a1 a2 i b -> DLS_TokenMetaSet tm at <$> pan a1 <*> pan a2 <*> pure i <*> pure b

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
