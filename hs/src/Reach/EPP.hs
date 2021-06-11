module Reach.EPP (epp) where

import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Debug.Trace
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.CollectCounts
import Reach.Counter
import Reach.Optimize
import Reach.Texty
import Reach.Util

shouldTrace :: Bool
shouldTrace = False

-- Helpers
default_interval :: CInterval a
default_interval = CBetween [] []

interval_from :: CInterval a -> [a]
interval_from (CBetween froml _) = froml

interval_add_from :: CInterval a -> a -> CInterval a
interval_add_from (CBetween froml tol) x =
  CBetween (x : froml) (x : tol)

interval_add_to :: CInterval a -> a -> CInterval a
interval_add_to (CBetween froml tol) x =
  CBetween froml (x : tol)

interval_no_to :: CInterval a -> CInterval a
interval_no_to (CBetween froml _) =
  CBetween froml []

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

fixedPoint_ :: forall a. Eq a => a -> (a -> a) -> a
fixedPoint_ x0 f = h x0
  where
    h :: a -> a
    h x =
      let x' = f x
       in case x == x' of
            True -> x
            False -> h x'

fixedPoint :: (Eq a, Monoid a) => (a -> a) -> a
fixedPoint = fixedPoint_ mempty

solve :: FlowInput -> FlowOutput
solve fi' = fixedPoint go
  where
    mtrace m v =
      case shouldTrace of
        True -> trace m v
        False -> v
    fi = mtrace imsg fi'
    imsg = "\nflow input:" <> show (pretty fi') <> "\n"
    omsg fo = "\nflow output:" <> show (pretty fo) <> "\n"
    go fo = mtrace (omsg fo) $ go' fo
    go' fo = M.mapWithKey (go1 fo) fi
    go1 fo me (FlowInputData {..}) = fod
      where
        fod =
          FlowOutputData
            { fod_save = need
            }
        foread n = fromMaybe mempty $ M.lookup n fo
        unionmap f s = S.unions $ map f $ S.toList s
        saves =
          -- We need to save the saves we create
          unionmap (fod_save . foread) fid_saves
        (use, defs) =
          -- We use what our saves need and what we actually use
          closeEdges saves
        need =
          -- We need what we save & use, minus what we define
          S.difference use defs
        closeEdges extra =
          fixedPoint_ (S.union fid_uses extra, fid_defns) closeEdges1
        closeEdges1 in0 = mtrace msg $ out1
          where
            msg = "\ncloseEdges" <> show me <> ":\n" <> show (pretty in0) <> "\n->:\n" <> show (pretty out1) <> "\n"
            (use0, defs0) = in0
            out1 = (use1, defs1)
            used_edge =
              flip $ const $ flip S.member use0
            edges =
              M.filterWithKey used_edge fid_edges
            edge_use =
              S.unions $ M.elems edges
            edge_defs =
              -- We define the edges that remain
              M.keysSet edges
            use1 =
              S.union use0 edge_use
            defs1 =
              S.union defs0 edge_defs

-- Build flow
data EPPError
  = Err_ContinueDomination
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance Show EPPError where
  show Err_ContinueDomination =
    "Continue must be dominated by communication"

data BEnv = BEnv
  { be_prev :: Int
  , be_savec :: Counter
  , be_handlerc :: Counter
  , be_interval :: CInterval DLArg
  , be_handlers :: IORef (M.Map Int (CApp CHandler))
  , be_flowr :: IORef FlowInput
  , be_more :: IORef Bool
  , be_loop :: Maybe (Int, Int)
  , be_output_vs :: IORef [DLVar]
  , be_toks :: [DLArg]
  , be_viewmc :: Maybe Counter
  , be_viewr :: IORef ViewInfos
  , be_views :: ViewsInfo
  , be_inConsensus :: Bool
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

-- Views
asnLike :: [DLVar] -> [(DLVar, DLArg)]
asnLike = map (\x -> (x, DLA_Var x))

assignView :: BApp ViewSave
assignView = do
  BEnv {..} <- ask
  let vs = countsl be_views
  let vi = ViewInfo vs be_views
  i <- case be_viewmc of
    Nothing -> return $ 0
    Just c -> liftIO $ incCounter c
  let vis = ViewSave i $ asnLike vs
  liftIO $ modifyIORef be_viewr $ M.insert i vi
  return $ vis

-- Read flow
data CEnv = CEnv
  { ce_flow :: FlowOutput
  , ce_vars :: IORef DLVarS
  }

type CApp = ReaderT CEnv IO

data EEnv = EEnv
  { ee_flow :: FlowOutput
  , ee_who :: SLPart
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

-- End-point Construction

itsame :: SLPart -> EApp Bool
itsame who = ((who ==) . ee_who) <$> ask

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
      _ -> return ()
    fg_edge mdv de
    retb0 $ const $ return $ DL_Let at mdv de
  DL_ArrayMap at ans x a f -> do
    fg_defn $ [ans, a]
    fg_use $ x
    be_bl f
      >>= retb
        (\f' ->
           return $ DL_ArrayMap at ans x a f')
  DL_ArrayReduce at ans x z b a f -> do
    fg_defn $ [ans, b, a]
    fg_use $ [x, z]
    be_bl f
      >>= retb
        (\f' ->
           return $ DL_ArrayReduce at ans x z b a f')
  DL_Var at v -> do
    fg_defn $ v
    let mkt _ = return $ DL_Var at v
    return $ (,) (ce_vdef v >> (mkt ())) (mkt ())
  DL_Set at v a -> do
    fg_defn $ v
    fg_use $ a
    let mkt _ = return $ DL_Set at v a
    return $ (,) (ce_vuse v >> (mkt ())) (mkt ())
  DL_LocalIf at c t f -> do
    fg_use $ c
    t'p <- be_t t
    f'p <- be_t f
    retb2
      t'p
      f'p
      (\t' f' ->
         return $ DL_LocalIf at c t' f')
  DL_LocalSwitch at ov csm -> do
    fg_use $ ov
    let go (mv, k) = do
          fg_defn $ mv
          k'p <- be_t k
          return $ (,) mv k'p
    csm' <- mapM go csm
    let mkt f = (DL_LocalSwitch at ov <$> mapM f' csm')
          where
            f' (mv, k'p) = (,) mv <$> (f k'p)
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
    let t'c = return $ DL_Nop at
    let t'l = do
          itsame who >>= \case
            False -> return $ DL_Nop at
            True -> DL_Only at (Right ic) <$> l'l
    return $ (,) t'c t'l
  DL_Only {} -> impossible $ "right only before EPP"
  DL_LocalDo at t -> do
    (t'c, t'l) <- be_t t
    let mk = DL_LocalDo at
    return $ (,) (mk <$> t'c) (mk <$> t'l)
  where
    nop at = retb0 $ const $ return $ DL_Nop at

be_t :: BAppT2 DLTail
be_t = \case
  DT_Return at -> retb0 $ const $ return $ DT_Return at
  DT_Com m k -> do
    m'p <- be_m m
    k'p <- be_t k
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

class OnlyHalts a where
  onlyHalts :: a -> Bool

instance OnlyHalts LLConsensus where
  onlyHalts = \case
    LLC_Com {} -> False
    LLC_If {} -> False
    LLC_Switch {} -> False
    LLC_FromConsensus _ _ s -> onlyHalts s
    LLC_While {} -> False
    LLC_Continue {} -> False
    LLC_ViewIs {} -> False

instance OnlyHalts LLStep where
  onlyHalts = \case
    LLS_Com _ k -> onlyHalts k
    LLS_Stop _ -> True
    LLS_ToConsensus {} -> False

be_c :: LLConsensus -> BApp (CApp CTail, EApp ETail)
be_c = \case
  LLC_ViewIs _ v f ma k ->
    local (\e -> e {be_views = modv $ be_views e}) $
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
    return $ (,) (mkCom CT_Com <$> c'c <*> k'c) (mkCom ET_Com <$> c'l <*> k'l)
  LLC_If at c t f -> do
    (t'c, t'l) <- be_c t
    (f'c, f'l) <- be_c f
    fg_use $ c
    let go mk t' f' = mk at c <$> t' <*> f'
    return $ (,) (go CT_If t'c f'c) (go ET_If t'l f'l)
  LLC_Switch at ov csm -> do
    fg_use $ ov
    let go (mv, k) = do
          fg_defn $ mv
          (k'c, k'l) <- be_c k
          let wrap k' = (,) mv <$> k'
          return (wrap k'c, wrap k'l)
    csm' <- mapM go csm
    return $ (,) (CT_Switch at ov <$> mapM fst csm') (ET_Switch at ov <$> mapM snd csm')
  LLC_FromConsensus at1 _at2 s -> do
    this <- newSavePoint "fromConsensus"
    (more, s'l) <-
      captureMore $
        local (\e -> e { be_interval = default_interval
                       , be_prev = this }) $ do
          be_s s
    toks <- be_toks <$> ask
    mvis <-
      case more of
        True -> do
          vis <- assignView
          fg_use vis
          return $ Just vis
        False -> do
          fg_use toks
          return $ Nothing
    let mkfrom_info do_read = do
          svs <- do_read this
          return $ case mvis of
            Just vis -> FI_Continue vis $ asnLike svs
            Nothing -> FI_Halt toks
    fg_saves this
    let cm = CT_From at1 this <$> mkfrom_info ce_readSave
    let lm = ET_FromConsensus at1 this <$> mkfrom_info ee_readSave <*> s'l
    return $ (,) cm lm
  LLC_While at asn _inv cond body k -> do
    let DLBlock cond_at cond_fs cond_l cond_a = cond
    this_loopj <- newHandler "While"
    this_loopsp <- newSavePoint "While"
    let inBlock prev = local (\e -> e {be_prev = prev})
    let inLoop = inBlock this_loopsp
    -- <Kont>
    (goto_kont, k'l) <-
      -- XXX This is a convoluted hack because Solidity does not allow empty
      -- structs and if the computation immediately halts, then we won't have
      -- any saved variables and therefore we'll crash solc. Even this isn't
      -- enough though, because what if we don't immediately halt, but instead
      -- transfer 0 ETH to the sender... there will be no SVS. So, that's why
      -- this is a bad hack.
      case onlyHalts k of
        True -> inLoop $ be_c k
        False -> do
          kontsp <- newSavePoint "While Kont"
          kontj <- newHandler "While Kont"
          let inKont = inBlock kontsp
          (k'c, k'l) <- inKont $ be_c k
          setHandler kontj $ do
            kont_svs <- ce_readSave kontsp
            C_Loop at kont_svs [] <$> k'c
          inLoop $ fg_saves $ kontsp
          let gk = CT_Jump at kontj <$> ce_readSave kontsp <*> pure mempty
          return (gk, k'l)
    -- </Kont>
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
    setHandler this_loopj $ do
      loop_svs <- ce_readSave this_loopsp
      loopc <- (liftIO . optimize) =<< loop_top
      return $ C_Loop at loop_svs loop_vars loopc
    fg_saves $ this_loopsp
    let cm = CT_Jump at this_loopj <$> ce_readSave this_loopsp <*> pure asn
    let cond'l = DLBlock cond_at cond_fs <$> cond_l'l <*> pure cond_a
    let lm = ET_While at asn <$> cond'l <*> body'l <*> k'l
    return $ (,) cm lm
  LLC_Continue at asn -> do
    fg_use $ asn
    (this_loopj, this_loopsp) <-
      fromMaybe (impossible "no loop") . be_loop <$> ask
    prev <- be_prev <$> ask
    when (this_loopsp == prev) $
      expect_thrown at Err_ContinueDomination
    fg_saves $ this_loopsp
    let cm = CT_Jump at this_loopj <$> ce_readSave this_loopsp <*> pure asn
    let lm = return $ ET_Continue at asn
    return $ (,) cm lm

be_s :: LLStep -> BApp (EApp ETail)
be_s = \case
  LLS_Com c k -> do
    int <- be_interval <$> ask
    let int' =
          case c of
            (DL_Let _ _ (DLE_Wait _ amt)) -> interval_add_from int amt
            _ -> int
    k' <- local (\e -> e {be_interval = int'}) $ be_s k
    c'e <- withConsensus False $ ee_m c
    return $ mkCom ET_Com <$> c'e <*> k'
  LLS_Stop at ->
    return $ (return $ ET_Stop at)
  LLS_ToConsensus at send recv mtime -> do
    let DLRecv from_v msg_vs time_v (last_time_mv, ok_c) = recv
    prev <- be_prev <$> ask
    signalMore
    this_h <- newHandler "ToConsensus"
    int <- be_interval <$> ask
    (int_ok, mtime'm) <-
      case mtime of
        Nothing -> do
          let int_ok = interval_no_to int
          return $ (int_ok, return $ Nothing)
        Just (delay_a, to_s) -> do
          let int_ok = interval_add_to int delay_a
          let int_to = interval_add_from int delay_a
          let delay_as = interval_from int_to
          to_s'm <-
            local (\e -> e {be_interval = int_to}) $
              be_s to_s
          let mtime'm = Just . (,) delay_as <$> to_s'm
          return $ (int_ok, mtime'm)
    (out_vs, (ok_c'm, ok_l'm)) <-
      captureOutputVars $
        local
          (\e ->
             e
               { be_interval = int_ok
               })
          $ do
            fg_use $ int_ok
            fg_use $ last_time_mv
            fg_defn $ from_v : time_v : msg_vs
            be_c ok_c
    setHandler this_h $ do
      svs <- ce_readSave prev
      ok_c' <- ok_c'm
      udvs <- readVars
      let add_udv_def uk udv = CT_Com (DL_Var at udv) uk
      let ok_c'' = foldl' add_udv_def ok_c' udvs
      return $ C_Handler at int_ok last_time_mv from_v prev svs msg_vs time_v ok_c''
    -- It is only a solo send if we are the only sender AND we are not a
    -- class
    let soloSend0 = (M.size send) == 1
    let soloSend1 = not $ getAll $ mconcatMap (All . ds_isClass) $ M.elems send
    let soloSend = soloSend0 && soloSend1
    let ok_l''m = do
          ok_l' <- ok_l'm
          who <- ee_who <$> ask
          mfrom <- case M.lookup who send of
            Nothing -> return $ Nothing
            Just (DLSend {..}) -> do
              svs <- ee_readSave prev
              return $ Just (ds_msg, ds_pay, ds_when, svs, soloSend)
          mtime' <- mtime'm
          return $ ET_ToConsensus at from_v prev last_time_mv this_h mfrom msg_vs out_vs time_v mtime' ok_l'
    return $ ok_l''m

mk_eb :: DLExportBlock -> BApp DLExportBlock
mk_eb (DLinExportBlock at vs (DLBlock bat sf ll a)) = do
  let body' = dtReplace DT_Com (DT_Return bat) ll
  return $ DLinExportBlock at vs (DLBlock bat sf body' a)

epp :: LLProg -> IO PLProg
epp (LLProg at (LLOpts {..}) ps dli dex dvs s) = do
  -- Step 1: Analyze the program to compute basic blocks
  be_savec <- newCounter 1
  be_handlerc <- newCounter 1
  be_handlers <- newIORef mempty
  be_flowr <- newIORef mempty
  be_more <- newIORef False
  let be_loop = Nothing
  let be_prev = 0
  let be_interval = default_interval
  be_output_vs <- newIORef mempty
  let be_toks = mempty
  be_viewc <- newCounter 1
  let be_viewmc = if M.null dvs then Nothing else Just be_viewc
  be_viewr <- newIORef mempty
  let be_views = mempty
  let be_inConsensus = False
  mkep_ <- flip runReaderT (BEnv {..}) $ be_s s
  vm <- readIORef be_viewr
  hs <- readIORef be_handlers
  let mvm = if M.null dvs then Nothing else Just (dvs, vm)
  -- Step 2: Solve the flow graph
  flowi <- readIORef be_flowr
  last_save <- readCounter be_savec
  let flowi' = M.fromList $ zip [0..last_save] $ repeat mempty
  let flow = solve $ flowi <> flowi'
  -- Step 3: Turn the blocks into handlers
  let mkh m = do
        let ce_flow = flow
        ce_vars <- newIORef mempty
        flip runReaderT (CEnv {..}) m
  dex' <-
    flip runReaderT (BEnv {..}) $
      mapM mk_eb dex
  cp <- (CPProg at mvm . CHandlers) <$> mapM mkh hs
  -- Step 4: Generate the end-points
  let SLParts p_to_ie = ps
  let mkep ee_who ie = do
        let ee_flow = flow
        et <-
          flip runReaderT (EEnv {..}) $
            mkep_
        return $ EPProg at ie et
  pps <- EPPs <$> mapWithKeyM mkep p_to_ie
  -- Step 4: Generate the final PLProg
  let plo_deployMode = llo_deployMode
  let plo_verifyArithmetic = llo_verifyArithmetic
  let plo_counter = llo_counter
  return $ PLProg at (PLOpts {..}) dli dex' pps cp
