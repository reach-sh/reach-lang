module Reach.EPP (epp, EPPError (..)) where

import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List.Extra (mconcatMap, groupOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Debug.Trace
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.AST.CP
import Reach.AST.EP
import Reach.CollectCounts
import Reach.Counter
import Reach.FixedPoint
import Reach.Optimize
import Reach.Texty
import Reach.Util
import Safe (headMay)
import Reach.CollectSvs

shouldTrace :: Bool
shouldTrace = False

-- Helpers
default_interval :: CInterval a
default_interval = CBetween Nothing Nothing

interval_from :: CInterval a -> Maybe a
interval_from (CBetween froml _) = froml

interval_add_from :: CInterval a -> a -> CInterval a
interval_add_from (CBetween _ tol) x =
  CBetween (Just x) tol

interval_add_to :: CInterval a -> a -> CInterval a
interval_add_to (CBetween froml _) x =
  CBetween froml (Just x)

interval_no_to :: CInterval a -> CInterval a
interval_no_to (CBetween froml _) =
  CBetween froml Nothing

-- Flow
type DLVarS = S.Set DLVar

type FlowInput = M.Map Int FlowInputData

data FlowInputData = FlowInputData
  { fid_uses :: DLVarS
  , fid_defns :: DLVarS
  , fid_edges :: M.Map DLVar DLVarS
  , fid_saves :: S.Set Int
  }

instance Semigroup FlowInputData where
  (FlowInputData xa xb xc xd) <> (FlowInputData ya yb yc yd) =
    FlowInputData (xa <> ya) (xb <> yb) (xc <> yc) (xd <> yd)

instance Monoid FlowInputData where
  mempty = FlowInputData mempty mempty mempty mempty

instance Pretty FlowInputData where
  pretty (FlowInputData {..}) =
    render_obj $
      M.fromList
        [ ("uses" :: String, pretty fid_uses)
        , ("defns", pretty fid_defns)
        , ("edges", pretty fid_edges)
        , ("saves", pretty fid_saves)
        ]

type FlowOutput = M.Map Int FlowOutputData

data FlowOutputData = FlowOutputData
  { fod_save :: DLVarS
  }
  deriving (Eq)

instance Semigroup FlowOutputData where
  (FlowOutputData xa) <> (FlowOutputData ya) =
    FlowOutputData (xa <> ya)

instance Monoid FlowOutputData where
  mempty = FlowOutputData mempty

instance Pretty FlowOutputData where
  pretty (FlowOutputData {..}) =
    render_obj $
      M.fromList
        [ ("save" :: String, fod_save)
        ]

fod_savel :: FlowOutputData -> [DLVar]
fod_savel = S.toAscList . fod_save

solve :: FlowInput -> IO FlowOutput
solve fi = do
  let mtrace m = when shouldTrace $ putStrLn m
  mtrace $ "\nflow input:" <> show (pretty fi) <> "\n"
  fixedPoint $ \i fo -> do
    mtrace $ "\nflow " <> show i <> "output:" <> show (pretty fo) <> "\n"
    flip mapWithKeyM fi $ \me (FlowInputData {..}) -> do
      let foread n = fromMaybe mempty $ M.lookup n fo
      let unionmap f s = S.unions $ map f $ S.toList s
      -- We need to save the saves we create
      let saves = unionmap (fod_save . foread) fid_saves
      -- We use what our saves need and what we actually use
      let use_ = S.union fid_uses saves
      (use, defs) <- fixedPoint_ (use_, fid_defns) $ \j in0 -> do
        let (use0, defs0) = in0
        let used_edge = flip $ const $ flip S.member use0
        let edges = M.filterWithKey used_edge fid_edges
        let edge_use = S.unions $ M.elems edges
        -- We define the edges that remain
        let edge_defs = M.keysSet edges
        let use1 = S.union use0 edge_use
        let defs1 = S.union defs0 edge_defs
        let out1 = (use1, defs1)
        mtrace $ "\ncloseEdges " <> show j <> " " <> show me <> ":\n" <> show (pretty in0) <> "\n->:\n" <> show (pretty out1) <> "\n"
        return out1
      -- We need what we save & use, minus what we define
      let need = S.difference use defs
      let fod_save = need
      return $ FlowOutputData {..}

-- Build flow
data EPPError
  = Err_ContinueDomination
  | Err_ViewSetDomination (Maybe SLPart) SLVar
  | Err_API_Twice SLPart
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode EPPError where
  errPrefix = const "REP"

  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If a constructor is obsolete, do NOT delete it nor re-allocate its number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_ContinueDomination {} -> 0
    Err_ViewSetDomination {} -> 1
    Err_API_Twice {} -> 2

instance Show EPPError where
  show = \case
    Err_ContinueDomination ->
      "`continue` must be dominated by communication"
    Err_ViewSetDomination v f ->
      let mvn = maybe "" (\v' -> bunpack v' <> ".") v
       in "The value that the view `" <> mvn <> show (pretty f) <> "` is set to will never be observable"
    Err_API_Twice who ->
      "The API `" <> bunpack who <> "` is called many times in the same consensus step."

type ViewSet = M.Map (SrcLoc, Maybe SLPart, SLVar) (IORef Bool)

data BEnv = BEnv
  { be_prev :: Int
  , be_prevs :: S.Set Int
  , be_which_prev :: M.Map Int Int
  , be_savec :: Counter
  , be_handlerc :: Counter
  , be_interval :: CInterval DLTimeArg
  , be_handlers :: IORef (M.Map Int (CApp CHandler))
  , be_flowr :: IORef FlowInput
  , be_more :: IORef Bool
  , be_stateToSrcMap :: IORef StateSrcMap
  , be_loop :: Maybe (Int, Int)
  , be_output_vs :: IORef [DLVar]
  , be_toks :: [DLArg]
  , be_viewr :: IORef (M.Map Int ([DLVar] -> ViewInfo))
  , be_views :: ViewsInfo
  , be_view_sets :: ViewSet
  , be_view_setsr :: IORef ViewSet
  , be_inConsensus :: Bool
  , be_counter :: Counter
  , be_which :: Int
  , be_apis :: S.Set SLPart
  , be_api_info :: IORef (M.Map SLPart (M.Map Int ApiInfo))
  , be_alias :: Aliases
  , be_api_rets :: IORef (M.Map SLPart DLType)
  , be_ms :: Seq.Seq DLStmt
  , be_api_steps :: IORef (M.Map SLPart [(Int, SrcLoc)])
  }

type BApp = ReaderT BEnv IO

type BAppT2 a = a -> BApp (CApp a, EApp a)

withConsensus :: Bool -> BApp a -> BApp a
withConsensus b = local (\e -> e {be_inConsensus = b})

signalMore :: BApp ()
signalMore = liftIO . flip writeIORef True =<< (be_more <$> ask)

captureMore :: BApp a -> BApp (Bool, a)
captureMore m = do
  mr <- liftIO $ newIORef False
  x <- local (\e -> e {be_more = mr}) m
  a <- liftIO $ readIORef mr
  return (a, x)

newCounterThing :: String -> (BEnv -> Counter) -> String -> BApp Int
newCounterThing lab0 be_f lab1 = do
  hc <- be_f <$> ask
  n <- liftIO $ incCounter hc
  when shouldTrace $ do
    traceM $ lab0 <> " " <> lab1 <> " => " <> show n
  return $ n

newHandler :: String -> BApp Int
newHandler = newCounterThing "newHandler" be_handlerc

setHandler :: Int -> CApp CHandler -> BApp ()
setHandler hn m = do
  hsr <- be_handlers <$> ask
  hs <- liftIO $ readIORef hsr
  case M.lookup hn hs of
    Just _ -> impossible "epp double set handler"
    Nothing -> do
      liftIO $ modifyIORef hsr $ M.insert hn m

newSavePoint :: String -> BApp Int
newSavePoint lab = do
  sp <- newCounterThing "newSavePoint" be_savec lab
  return sp

recordOutputVar :: DLLetVar -> BApp ()
recordOutputVar = \case
  DLV_Eff -> return ()
  DLV_Let _ dv -> do
    vsr <- be_output_vs <$> ask
    liftIO $ modifyIORef vsr $ (:) dv

captureOutputVars :: BApp a -> BApp ([DLVar], a)
captureOutputVars m = do
  vsr <- liftIO $ newIORef mempty
  x <- local (\e -> e {be_output_vs = vsr}) m
  a <- liftIO $ readIORef vsr
  return (a, x)

fg_record :: (FlowInputData -> FlowInputData) -> BApp ()
fg_record fidm = do
  prev <- be_prev <$> ask
  fr <- be_flowr <$> ask
  liftIO $
    modifyIORef fr $ \f ->
      M.insert prev (fidm $ fromMaybe mempty $ M.lookup prev f) f

fg_use :: Countable a => a -> BApp ()
fg_use x = fg_record $ \f -> f {fid_uses = S.union (countsS x) (fid_uses f)}

fg_defn :: Countable a => a -> BApp ()
fg_defn x = fg_record $ \f -> f {fid_defns = S.union (countsS x) (fid_defns f)}

fg_edge :: (Countable a) => DLLetVar -> a -> BApp ()
fg_edge DLV_Eff x = fg_use x
fg_edge (DLV_Let _ v) use = fg_record $ \f -> f {fid_edges = M.singleton v (countsS use) <> (fid_edges f)}

fg_saves :: Int -> BApp ()
fg_saves sp = do
  fg_record $ \f -> f {fid_saves = S.insert sp $ fid_saves f}

-- Read flow
data CEnv = CEnv
  { ce_flow :: FlowOutput
  , ce_vars :: IORef DLVarS
  }

type CApp = ReaderT CEnv IO

data EEnv = EEnv
  { ee_flow :: FlowOutput
  , ee_who :: SLPart
  , ee_m_api_step :: Maybe Int
  }

type EApp = ReaderT EEnv IO

ce_vsm :: (DLVarS -> DLVarS) -> CApp ()
ce_vsm f = do
  cstr <- ce_vars <$> ask
  liftIO $ modifyIORef cstr f

ce_vuse :: DLVar -> CApp ()
ce_vuse x = ce_vsm $ S.insert x

ce_vdef :: DLVar -> CApp ()
ce_vdef x = ce_vsm $ S.delete x

readFlow :: Monad m => (a -> FlowOutput) -> Int -> ReaderT a m FlowOutputData
readFlow get_flow n = do
  fo <- get_flow <$> ask
  case M.lookup n fo of
    Just x -> return $ x
    Nothing -> impossible $ "no flow for " <> show n

ee_readFlow :: Int -> EApp FlowOutputData
ee_readFlow = readFlow ee_flow

ee_readSave :: Int -> EApp [DLVar]
ee_readSave x = fod_savel <$> ee_readFlow x

ce_readFlow :: Int -> CApp FlowOutputData
ce_readFlow = readFlow ce_flow

ce_readSave :: Int -> CApp [DLVar]
ce_readSave x = fod_savel <$> ce_readFlow x

readVars :: CApp [DLVar]
readVars = do
  vsr <- ce_vars <$> ask
  S.toAscList <$> (liftIO $ readIORef vsr)

addVars :: SrcLoc -> CTail -> CApp CTail
addVars at k = do
  udvs <- readVars
  let add_udv_def uk udv = CT_Com (DL_Var at udv) uk
  return $ foldl' add_udv_def k udvs

-- End-point Construction

itsame :: SLPart -> Maybe Int -> EApp Bool
itsame who mApiStep = do
  e_who <- asks ee_who
  e_m_api_step <- asks ee_m_api_step
  return $ mApiStep == e_m_api_step && e_who == who

eeIze :: (a -> BApp (b, c)) -> a -> BApp c
eeIze f x = do
  be_flowr' <- (liftIO . dupeIORef) =<< (be_flowr <$> ask)
  local (\e -> e {be_flowr = be_flowr'}) $
    snd <$> f x

ee_m :: DLStmt -> BApp (EApp DLStmt)
ee_m = eeIze be_m

ee_t :: DLTail -> BApp (EApp DLTail)
ee_t = eeIze be_t

be_m :: BAppT2 DLStmt
be_m = \case
  DL_Nop at -> nop at
  DL_Let at mdv de -> do
    case de of
      DLE_Remote {} -> recordOutputVar mdv
      DLE_EmitLog _ l vs -> do
        fg_use de
        case l of
          L_Api p -> do
            BEnv {..} <- ask
            case vs of
              [v] -> do
                let ty = varType v
                liftIO $
                  modifyIORef be_api_rets $
                    M.insert p ty
              _ -> impossible "api"
          _ -> return ()
      DLE_setApiDetails apiAt p tys mc isf -> do
        BEnv {..} <- ask
        rets <- liftIO $ readIORef be_api_rets
        let ret = fromMaybe T_Null $ M.lookup p rets
        let alias = join $ M.lookup (bunpack p) be_alias
        let prev = be_prev
        let as = be_api_steps
        let ai' = ApiInfo apiAt tys mc be_which isf ret alias
        liftIO $ modifyIORef as $ M.insertWith (<>) p [(prev, apiAt)]
        liftIO $ modifyIORef be_api_info $ flip M.alter p $
          Just . M.insert prev ai' . fromMaybe mempty
      _ -> return ()
    fg_edge mdv de
    retb0 $ const $ return $ DL_Let at mdv de
  DL_ArrayMap at ans xs as i f -> do
    fg_defn $ [ans] <> as <> [i]
    fg_use xs
    be_bl f
      >>= retb
        (\f' ->
           return $ DL_ArrayMap at ans xs as i f')
  DL_ArrayReduce at ans xs z b as i f -> do
    fg_defn $ [ans] <> as <> [b, i]
    fg_use $ xs <> [z]
    be_bl f
      >>= retb
        (\f' ->
           return $ DL_ArrayReduce at ans xs z b as i f')
  DL_Var at v -> do
    fg_defn $ v
    let mkt _ = return $ DL_Var at v
    return $ (,) (ce_vdef v >> (mkt ())) (mkt ())
  DL_Set at v a -> do
    fg_defn $ v
    fg_use $ a
    let mkt _ = return $ DL_Set at v a
    return $ (,) (ce_vuse v >> (mkt ())) (mkt ())
  DL_LocalIf at mans c t f -> do
    fg_use $ c
    t'p <- be_t t
    f'p <- be_t f
    retb2
      t'p
      f'p
      (\t' f' ->
         return $ DL_LocalIf at mans c t' f')
  DL_LocalSwitch at ov csm -> do
    fg_use $ ov
    let go (v, vu, k) = do
          when vu $ fg_defn $ v
          k'p <- be_t k
          return $ (,,) v vu k'p
    csm' <- mapM go csm
    let mkt f = (DL_LocalSwitch at ov <$> mapM f' csm')
          where
            f' (v, vu, k'p) = (,,) v vu <$> (f k'p)
    return $ (,) (mkt fst) (mkt snd)
  DL_MapReduce at mri ans x z b a f -> do
    fg_defn $ [ans, b, a]
    fg_use $ z
    be_bl f
      >>= retb
        (\f' ->
           return $ DL_MapReduce at mri ans x z b a f')
  DL_Only at (Left who) l -> do
    ic <- be_inConsensus <$> ask
    l'l <- ee_t l
    mprev <- isApi who >>= \case
      False -> return $ Nothing
      True  -> do
          case ic of
            True -> do
              which <- asks be_which
              wps <- asks be_which_prev
              case M.lookup which wps of
                Just p' -> return $ Just p'
                _ -> impossible "which has no prev"
            False -> Just <$> asks be_prev
    let t'c = return $ DL_Nop at
    let t'l = do
          itsame who mprev >>= \case
            False -> return $ DL_Nop at
            True -> DL_Only at (Right ic) <$> l'l
    return $ (,) t'c t'l
  DL_Only {} -> impossible $ "right only before EPP"
  DL_LocalDo at mans t -> do
    (t'c, t'l) <- be_t t
    let mk = DL_LocalDo at mans
    return $ (,) (mk <$> t'c) (mk <$> t'l)
  where
    nop at = retb0 $ const $ return $ DL_Nop at

isApi :: SLPart -> BApp Bool
isApi who = S.member who <$> asks be_apis

be_t :: BAppT2 DLTail
be_t = \case
  DT_Return at -> retb0 $ const $ return $ DT_Return at
  DT_Com m k -> do
    k'p <- be_t k
    m'p <- be_m m
    retb2
      m'p
      k'p
      (\m' k' ->
         return $ mkCom DT_Com m' k')

retb :: (Monad m, Monad n, Monad p) => (forall o. Monad o => a -> o b) -> (m a, n a) -> p (m b, n b)
retb f (mx, my) = return $ (,) (mx >>= f) (my >>= f)

retb0 :: (Monad m, Monad n, Monad p) => (forall o. Monad o => () -> o b) -> p (m b, n b)
retb0 f = retb f (return (), return ())

retb2 :: (Monad m, Monad n, Monad p) => (m a1, n a1) -> (m a2, n a2) -> (forall o. Monad o => a1 -> a2 -> o b) -> p (m b, n b)
retb2 (mx1, my1) (mx2, my2) f = retb f' ((p mx1 mx2), (p my1 my2))
  where
    f' (x1, x2) = f x1 x2
    p mx my = (,) <$> mx <*> my

be_bl :: BAppT2 DLBlock
be_bl (DLBlock at fs t a) = do
  fg_use $ a
  be_t t
    >>= retb
      (\t' ->
         return $ DLBlock at fs t' a)

check_view_sets :: ViewSet -> IO ()
check_view_sets vs = forM_ (M.toAscList vs) $ \((at, v, f), obvr) -> do
  readIORef obvr >>= \case
    True -> return ()
    False -> do
      expect_thrown at $ Err_ViewSetDomination v f

mark_view_sets :: BApp ()
mark_view_sets = do
  vs <- asks be_view_sets
  forM_ (M.toAscList vs) $ \((_, _, _), obvr) -> do
    liftIO $ writeIORef obvr True

be_c_top :: DLStmt -> LLConsensus -> BApp (CApp CTail, EApp ETail)
be_c_top m c = do
  (cc, lc) <- be_c c
  (cm, _) <- withConsensus True $ be_m m
  let cc' = backwards (mkCom CT_Com) cm cc
  return (cc', lc)

backwards :: Monad m => (a -> b -> c) -> m a -> m b -> m c
backwards f xm ym = do
  y <- ym
  x <- xm
  return $ f x y

be_c :: LLConsensus -> BApp (CApp CTail, EApp ETail)
be_c = \case
  LLC_ViewIs at v f ma k -> do
    vr <- asks be_view_setsr
    vs <- liftIO $ readIORef vr
    let vk = (at, v, f)
    obvr <- case M.lookup vk vs of
              Nothing -> liftIO $ newIORef False
              Just o -> return o
    let add_vs1 = M.insert vk obvr
    liftIO $ modifyIORef vr $ add_vs1
    local (\e -> e { be_views = modv $ be_views e
                   , be_view_sets = add_vs1 $ be_view_sets e }) $
      be_c k
    where
      modv = mAdjust mempty v modf
      modf = case ma of
        Just a -> M.insert f a
        Nothing -> M.delete f
      mAdjust d mk m = flip M.alter mk $ Just . m . fromMaybe d
  LLC_Com c k -> do
    let toks =
          case c of
            DL_Let _ _ (DLE_TokenInit _ toka) -> [toka]
            _ -> []
    let remember_toks = local (\e -> e {be_toks = toks <> be_toks e})
    (k'c, k'l) <- remember_toks $ be_c k
    (c'c, c'l) <- withConsensus True $ be_m c
    return $ (,) (backwards (mkCom CT_Com) c'c k'c) (backwards (mkCom ET_Com) c'l k'l)
  LLC_If at c t f -> do
    (t'c, t'l) <- be_c t
    (f'c, f'l) <- be_c f
    fg_use $ c
    let go mk t' f' = mk at c <$> t' <*> f'
    return $ (,) (go CT_If t'c f'c) (go ET_If t'l f'l)
  LLC_Switch at ov csm -> do
    fg_use $ ov
    let go (v, vu, k) = do
          when vu $ fg_defn v
          (k'c, k'l) <- be_c k
          let wrap k' = (,,) v vu <$> k'
          return (wrap k'c, wrap k'l)
    csm' <- mapM go csm
    return $ (,) (CT_Switch at ov <$> mapM fst csm') (ET_Switch at ov <$> mapM snd csm')
  LLC_FromConsensus at1 _at2 fs s -> do
    this <- newSavePoint "fromConsensus"
    views <- asks be_views
    which <- asks be_which
    (more, s'l) <-
      captureMore $
        local
          (\e ->
             e
               { be_interval = default_interval
               , be_prev = this
               , be_prevs = S.singleton this
               , be_which_prev = M.insert which this (be_which_prev e)
               , be_view_sets = mempty
               })
          $ do
            fg_use views
            be_s s
    toks <- asks be_toks
    case more of
      True -> do
        viewr <- asks be_viewr
        liftIO $ modifyIORef viewr $ M.insert this $ flip ViewInfo views
        mark_view_sets
      False -> do
        fg_use toks
    let mkfrom_info do_read = do
          svs <- do_read this
          return $ case more of
            True -> FI_Continue $ asnLike svs
            False -> FI_Halt toks
    fg_saves this
    let cm = CT_From at1 this <$> mkfrom_info ce_readSave
    let lm = ET_FromConsensus at1 this <$> mkfrom_info ee_readSave <*> s'l
    stateToSrcMap <- asks be_stateToSrcMap
    liftIO $ modifyIORef stateToSrcMap $ M.insert this (at1, fs)
    return $ (,) cm lm
  LLC_While at asn _inv cond body k -> do
    let DLBlock cond_at cond_fs cond_l cond_a = cond
    this_loopj <- newHandler "While"
    this_loopsp <- newSavePoint "While"
    let inBlock the_prev the_prevs =
          local
            (\e ->
               e
                 { be_prev = the_prev
                 , be_prevs = S.union (be_prevs e) the_prevs
                 })
    let inLoop = inBlock this_loopsp (S.singleton this_loopsp)
    (goto_kont, k'l) <-
      inLoop $ be_c k
    fg_use $ asn
    let loop_vars = assignment_vars asn
    fg_defn $ loop_vars
    (cond_l'c, cond_l'l) <- inLoop $ do
      fg_defn $ loop_vars
      fg_use cond_a
      be_t cond_l
    (body'c, body'l) <-
      inLoop $
        local (\e -> e {be_loop = Just (this_loopj, this_loopsp)}) $
          be_c body
    let loop_if = CT_If cond_at cond_a <$> body'c <*> goto_kont
    let loop_top = dtReplace CT_Com <$> loop_if <*> cond_l'c
    cnt <- asks be_counter
    setHandler this_loopj $ do
      loop_svs <- ce_readSave this_loopsp
      looptop' <- loop_top
      let svs = collectSvs looptop'
      loopc <- addVars at =<< loop_top
      loopc_opt <- liftIO $ optimize_ cnt False svs loopc
      return $ C_Loop at (map v2vl loop_svs) (map v2vl loop_vars) loopc_opt
    fg_saves $ this_loopsp
    let cm = CT_Jump at this_loopj <$> ce_readSave this_loopsp <*> pure asn
    let cond'l = DLBlock cond_at cond_fs <$> cond_l'l <*> pure cond_a
    let lm = ET_While at asn <$> cond'l <*> body'l <*> k'l
    return $ (,) cm lm
  LLC_Continue at asn -> do
    fg_use $ asn
    (this_loopj, this_loopsp) <-
      fromMaybe (impossible "no loop") . be_loop <$> ask
    prevs <- be_prevs <$> ask
    when (S.member this_loopsp prevs) $
      expect_thrown at Err_ContinueDomination
    fg_saves $ this_loopsp
    let cm = CT_Jump at this_loopj <$> ce_readSave this_loopsp <*> pure asn
    let lm = return $ ET_Continue at asn
    return $ (,) cm lm

be_s :: LLStep -> BApp (EApp ETail)
be_s s = local (\e -> e { be_ms = mempty }) $ be_s_ s

be_s_ :: LLStep -> BApp (EApp ETail)
be_s_ = \case
  LLS_Com c k -> do
    int <- be_interval <$> ask
    let int' =
          case c of
            (DL_Let _ _ (DLE_Wait _ ta)) -> interval_add_from int ta
            _ -> int
    k' <- local (\e -> e
      { be_interval = int'
      , be_ms = (be_ms e) Seq.|> c }) $ rec k
    c'e <- withConsensus False $ ee_m c
    return $ mkCom ET_Com <$> c'e <*> k'
  LLS_Stop at -> do
    return $ (return $ ET_Stop at)
  LLS_ToConsensus at lct_v send recv mtime -> do
    let DLRecv from_v msg_vs time_v secs_v didSend_v ok_c = recv
    prev <- be_prev <$> ask
    signalMore
    this_h <- newHandler "ToConsensus"
    int <- be_interval <$> ask
    (int_ok, mtime'm) <-
      case mtime of
        Nothing -> do
          let int_ok = interval_no_to int
          return $ (int_ok, return $ Nothing)
        Just (ta, to_s) -> do
          let int_ok = interval_add_to int ta
          let int_to = interval_add_from int ta
          let delay_as = interval_from int_to
          to_s'm <-
            local (\e -> e {be_interval = int_to}) $
              rec to_s
          let mtime'm = Just . (,) delay_as <$> to_s'm
          return $ (int_ok, mtime'm)
    (out_vs, (ok_c'm, ok_l'm)) <-
      captureOutputVars $
        local
          (\e ->
             e
               { be_interval = int_ok
               , be_which = this_h
               , be_which_prev = M.insert this_h prev (be_which_prev e)
               })
          $ do
            ms <- asks be_ms
            let mst = dtList at (toList ms)
            let mss = DL_LocalDo at Nothing mst
            fg_use $ int_ok
            fg_defn $ from_v : time_v : secs_v : msg_vs
            be_c_top mss $ ok_c
    setHandler this_h $ do
      svs <- ce_readSave prev
      ok_c'' <- addVars at =<< ok_c'm
      return $ C_Handler at int_ok from_v prev (map v2vl svs) (map v2vl msg_vs) time_v secs_v ok_c''
    -- It is only a solo send if we are the only sender AND we are not a
    -- class
    let soloSend0 = (M.size send) == 1
    let soloSend1 = not $ getAll $ mconcatMap (All . ds_isClass) $ M.elems send
    let soloSend = soloSend0 && soloSend1
    let ok_l''m = do
          ok_l' <- ok_l'm
          who <- ee_who <$> ask
          m_api_step <- ee_m_api_step <$> ask
          let same_which = maybe True (== prev) m_api_step
          mfrom <- case (M.lookup who send, same_which) of
            (Just (DLSend {..}), True) -> do
              svs <- ee_readSave prev
              return $ Just (ds_msg, ds_pay, ds_when, svs, soloSend)
            (_, _) -> return $ Nothing
          mtime' <- mtime'm
          return $ ET_ToConsensus at from_v prev (Just lct_v) this_h mfrom msg_vs out_vs time_v secs_v didSend_v mtime' ok_l'
    return $ ok_l''m
  where
    rec = be_s_

mk_eb :: DLExportBlock -> BApp DLExportBlock
mk_eb (DLinExportBlock at vs (DLBlock bat sf ll a)) = do
  let body' = dtReplace DT_Com (DT_Return bat) ll
  return $ DLinExportBlock at vs (DLBlock bat sf body' a)

epp :: LLProg -> IO (PLProg EPProg CPProg)
epp (LLProg {..}) = do
  let LLOpts {..} = llp_opts
  -- Step 1: Analyze the program to compute basic blocks
  let be_counter = llo_counter
  be_savec <- newCounter 1
  be_handlerc <- newCounter 0
  be_handlers <- newIORef mempty
  be_flowr <- newIORef mempty
  be_more <- newIORef False
  be_stateToSrcMap <- newIORef mempty
  let be_loop = Nothing
  let be_prev = 0
  let be_which = 0
  let be_prevs = mempty
  let SLParts {..} = llp_parts
  let be_apis = sps_apis
  be_api_info <- newIORef mempty
  be_api_rets <- newIORef mempty
  let be_interval = default_interval
  be_output_vs <- newIORef mempty
  let be_toks = mempty
  be_viewr <- newIORef mempty
  let be_views = mempty
  be_view_setsr <- newIORef mempty
  let be_view_sets = mempty
  let be_inConsensus = False
  let be_ms = mempty
  let be_alias = llp_aliases
  be_api_steps <- newIORef mempty
  let be_which_prev = mempty
  mkep_ <- flip runReaderT (BEnv {..}) $ be_s llp_step
  check_view_sets =<< readIORef be_view_setsr
  api_info <- liftIO $ readIORef be_api_info
  hs <- readIORef be_handlers
  mkvm <- readIORef be_viewr
  -- Step 2: Solve the flow graph
  flowi <- readIORef be_flowr
  last_save <- readCounter be_savec
  let flowi' = M.fromList $ zip [0 .. last_save] $ repeat mempty
  flow <- solve $ flowi <> flowi'
  -- Step 3: Turn the blocks into handlers
  let mkh m = do
        let ce_flow = flow
        ce_vars <- newIORef mempty
        flip runReaderT (CEnv {..}) m
  dex' <-
    flip runReaderT (BEnv {..}) $
      mapM mk_eb llp_exports
  vm <- flip mapWithKeyM mkvm $ \which mk ->
    mkh $ mk <$> ce_readSave which
  let cpp_at = llp_at
  let cpp_init = llp_init
  let cpp_views = DLViewsX llp_views vm
  let cpp_apis = api_info
  let cpp_events = llp_events
  cpp_handlers <- CHandlers <$> mapM mkh hs
  let cpo_untrustworthyMaps = llo_untrustworthyMaps
  let cpo_counter = llo_counter
  let cpp_opts = CPOpts {..}
  let plp_cpp = CPProg {..}
  stateToSrcMap <- readIORef be_stateToSrcMap
  -- Step 4: Generate the end-points
  as <- readIORef be_api_steps
  -- Ensure an API is called at most once in a given consensus step
  forM_ (M.toAscList as) $ \ (k, v) -> do
      forM_ (groupOn fst v) $ \ vs -> do
        when (length vs /= 1) $ do
          let apiAt = snd $ fromMaybe (impossible "api empty") $ headMay vs
          expect_thrown apiAt $ Err_API_Twice k
  -- Make a separate `EPProg` for each `API x Step`,
  -- where step is the one in which `interact.in` gets called.
  let genSepApis k v acc =
        case M.lookup k as of
          Just ns -> foldr (\ (x,_) acc' -> M.insert (k, Just x) v acc') acc ns
          _ -> M.insert (k, Nothing) v acc
  let sps_ies' = M.foldrWithKey genSepApis mempty sps_ies
  let mkep (who, step) ep_interactEnv = do
        let ep_at = llp_at
        let ep_isApi = S.member who sps_apis
        let ee_who = who
        let ee_m_api_step = step
        let ee_flow = flow
        ep_tail <- flip runReaderT (EEnv {..}) $ mkep_
        return $ EPart {..}
  let epo_untrustworthyMaps = llo_untrustworthyMaps
  let epo_counter = llo_counter
  let epp_opts = EPOpts {..}
  let epp_init = llp_init
  let epp_exports = dex'
  let epp_views = cpp_views
  let epp_apis = llp_apis
  let epp_events = llp_events
  let epp_stateSrcMap = stateToSrcMap
  epp_m <- mapWithKeyM mkep sps_ies'
  let plp_at = llp_at
  let plp_epp = EPProg {..}
  -- Step 4: Generate the final PLProg
  return $ PLProg {..}
