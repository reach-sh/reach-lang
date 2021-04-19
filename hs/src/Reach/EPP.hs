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
  , fid_parents :: S.Set Int
  , fid_children :: S.Set Int
  , fid_jumps :: S.Set Int
  }

instance Semigroup FlowInputData where
  (FlowInputData xa xb xc xd xe) <> (FlowInputData ya yb yc yd ye) =
    FlowInputData (xa <> ya) (xb <> yb) (xc <> yc) (xd <> yd) (xe <> ye)

instance Monoid FlowInputData where
  mempty = FlowInputData mempty mempty mempty mempty mempty

instance Pretty FlowInputData where
  pretty (FlowInputData {..}) =
    render_obj $
      M.fromList
        [ ("uses" :: String, pretty fid_uses)
        , ("defns", pretty fid_defns)
        , ("parents", pretty fid_parents)
        , ("children", pretty fid_children)
        , ("jumps", pretty fid_jumps)
        ]

type FlowOutput = M.Map Int FlowOutputData

data FlowOutputData = FlowOutputData
  { fod_save :: DLVarS
  , fod_recv :: DLVarS
  }
  deriving (Eq)

instance Semigroup FlowOutputData where
  (FlowOutputData xa xb) <> (FlowOutputData ya yb) =
    FlowOutputData (xa <> ya) (xb <> yb)

instance Monoid FlowOutputData where
  mempty = FlowOutputData mempty mempty

instance Pretty FlowOutputData where
  pretty (FlowOutputData {..}) =
    render_obj $
      M.fromList
        [ ("save" :: String, fod_save)
        , ("recv", fod_recv)
        ]

fod_savel :: FlowOutputData -> [DLVar]
fod_savel = S.toAscList . fod_save

fod_recvl :: FlowOutputData -> [DLVar]
fod_recvl = S.toAscList . fod_recv

fixedPoint :: forall a. Eq a => Monoid a => (a -> a) -> a
fixedPoint f = h mempty
  where
    h :: a -> a
    h x =
      let x' = f x
       in case x == x' of
            True -> x
            False -> h x'

solve :: FlowInput -> FlowOutput
solve fi' = fixedPoint go
  where
    mtrace m v =
      case shouldTrace of
        True -> trace m v
        False -> v
    fi = mtrace imsg fi'
    imsg = "\nflow input:" <> show (pretty fi') <> "\n"
    omsg fo = "\nflow ouput:" <> show (pretty fo) <> "\n"
    go fo = mtrace (omsg fo) $ go' fo
    go' fo = M.map (go1 fo) fi
    go1 fo (FlowInputData {..}) = fod
      where
        fod =
          FlowOutputData
            { fod_save = save
            , fod_recv = recv
            }
        foread which = fromMaybe mempty $ M.lookup which fo
        unionmap f s = S.unions $ map f $ S.toList s
        save =
          -- We must save what our children recv
          unionmap (fod_recv . foread) fid_children
        recv =
          -- We must recv what our parents sends plus what we need
          S.union (unionmap (fod_save . foread) fid_parents) need
        use =
          -- We use what our jumps recv and what we actually use
          S.union fid_uses (unionmap (fod_recv . foread) fid_jumps)
        need =
          -- We need what we save & use, minus what we define
          S.difference (S.union use save) fid_defns

-- Build flow
data EPPError
  = Err_ContinueDomination
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance Show EPPError where
  show Err_ContinueDomination =
    "Continue must be dominated by communication"

data BEnv = BEnv
  { be_which :: Int
  , be_handlerc :: Counter
  , be_interval :: CInterval DLArg
  , be_handlers :: IORef (M.Map Int (CApp CIHandler))
  , be_flowr :: IORef FlowInput
  , be_more :: IORef Bool
  , be_loop :: Maybe Int
  , be_output_vs :: IORef [DLVar]
  , be_toks :: [DLArg]
  }

type BApp = ReaderT BEnv IO

signalMore :: BApp ()
signalMore = liftIO . flip writeIORef True =<< (be_more <$> ask)

captureMore :: BApp a -> BApp (Bool, a)
captureMore m = do
  mr <- liftIO $ newIORef False
  x <- local (\e -> e {be_more = mr}) m
  a <- liftIO $ readIORef mr
  return (a, x)

newHandler :: String -> BApp Int
newHandler lab = do
  hc <- be_handlerc <$> ask
  n <- liftIO $ incCounter hc
  when shouldTrace $ do
    traceM $ "newHandler " <> show lab <> " => " <> show n
  return $ n

setHandler :: Int -> CApp CIHandler -> BApp ()
setHandler which m = do
  hsr <- be_handlers <$> ask
  hs <- liftIO $ readIORef hsr
  case M.lookup which hs of
    Just _ -> impossible "epp double set handler"
    Nothing -> do
      liftIO $ modifyIORef hsr $ M.insert which m

recordOutputVar :: LLVar -> BApp ()
recordOutputVar = \case
  Nothing -> return ()
  Just dv -> do
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
  which <- be_which <$> ask
  fr <- be_flowr <$> ask
  liftIO $
    modifyIORef fr $ \f ->
      M.insert which (fidm $ fromMaybe mempty $ M.lookup which f) f

fg_use :: Countable a => a -> BApp ()
fg_use x = fg_record $ \f -> f {fid_uses = S.union (countsS x) (fid_uses f)}

fg_defn :: Countable a => a -> BApp ()
fg_defn x = fg_record $ \f -> f {fid_defns = S.union (countsS x) (fid_defns f)}

fg_child :: Int -> BApp ()
fg_child child = do
  parent <- be_which <$> ask
  fg_record $ \f -> f {fid_children = S.insert child $ fid_children f}
  local (\e -> e {be_which = child}) $ do
    fg_record $ \f -> f {fid_parents = S.insert parent $ fid_parents f}

fg_jump :: Int -> BApp ()
fg_jump dst = do
  fg_record $ \f -> f {fid_jumps = S.insert dst $ fid_jumps f}

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
readFlow get_flow which = do
  fo <- get_flow <$> ask
  case M.lookup which fo of
    Just x -> return $ x
    Nothing -> impossible $ "no flow for " <> show which

ee_readFlow :: Int -> EApp FlowOutputData
ee_readFlow = readFlow ee_flow

ee_readMustSave :: Int -> EApp [DLVar]
ee_readMustSave x = fod_savel <$> ee_readFlow x

ee_readMustReceive :: Int -> EApp [DLVar]
ee_readMustReceive x = fod_recvl <$> ee_readFlow x

ce_readFlow :: Int -> CApp FlowOutputData
ce_readFlow = readFlow ce_flow

ce_readMustSave :: Int -> CApp [DLVar]
ce_readMustSave x = fod_savel <$> ce_readFlow x

ce_readMustReceive :: Int -> CApp [DLVar]
ce_readMustReceive x = fod_recvl <$> ce_readFlow x

readVars :: CApp [DLVar]
readVars = do
  vsr <- ce_vars <$> ask
  S.toAscList <$> (liftIO $ readIORef vsr)

-- End-point Construction

itsame :: SLPart -> EApp Bool
itsame who = ((who ==) . ee_who) <$> ask

ee_only :: SrcLoc -> SLPart -> LLTail -> EITail -> EApp EITail
ee_only _at who l k' =
  itsame who >>= \case
    False -> return k'
    True -> return $ dtReplace ET_Com k' l

be_m :: LLCommon -> BApp (CApp PILCommon)
be_m = \case
  DL_Nop at -> return $ return $ DL_Nop at
  DL_Let at mdv de -> do
    fg_use $ de
    case de of
      DLE_Remote {} -> recordOutputVar mdv
      _ -> return ()
    fg_defn $ mdv
    return $ return $ DL_Let at mdv de
  DL_ArrayMap at ans x a f -> do
    fg_defn $ [ans, a]
    fg_use $ x
    f' <- be_bl f
    return $ DL_ArrayMap at ans x a <$> f'
  DL_ArrayReduce at ans x z b a f -> do
    fg_defn $ [ans, b, a]
    fg_use $ [x, z]
    f' <- be_bl f
    return $ DL_ArrayReduce at ans x z b a <$> f'
  DL_Var at v -> do
    fg_defn $ v
    return $ do
      ce_vdef v
      return $ DL_Var at v
  DL_Set at v a -> do
    fg_defn $ v
    fg_use $ a
    return $ do
      ce_vuse v
      return $ DL_Set at v a
  DL_LocalIf at c t f -> do
    fg_use $ c
    t' <- be_t t
    f' <- be_t f
    return $ DL_LocalIf at c <$> t' <*> f'
  DL_LocalSwitch at ov csm -> do
    fg_use $ ov
    let go (mv, k) = do
          fg_defn $ mv
          k' <- be_t k
          return $ (,) mv <$> k'
    csm' <- mapM go csm
    return $ (DL_LocalSwitch at ov <$> mapM id csm')
  DL_MapReduce at mri ans x z b a f -> do
    fg_defn $ [ans, b, a]
    fg_use $ z
    f' <- be_bl f
    return $ DL_MapReduce at mri ans x z b a <$> f'

be_t :: LLTail -> BApp (CApp PILTail)
be_t = \case
  DT_Return at -> return $ return $ DT_Return at
  DT_Com m k -> do
    m' <- be_m m
    k' <- be_t k
    return $ DT_Com <$> m' <*> k'

be_bl :: LLBlock -> BApp (CApp PILBlock)
be_bl (DLinBlock at fs t a) = do
  fg_use $ a
  t' <- be_t t
  return $ DLinBlock at fs <$> t' <*> pure a

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
    LLC_Only _ _ _ k -> onlyHalts k

instance OnlyHalts LLStep where
  onlyHalts = \case
    LLS_Com _ k -> onlyHalts k
    LLS_Stop _ -> True
    LLS_Only _ _ _ s -> onlyHalts s
    LLS_ToConsensus {} -> False

be_c :: LLConsensus -> BApp (CApp CITail, EApp EITail)
be_c = \case
  LLC_Com c k -> do
    let toks =
          case c of
            DL_Let _ _ (DLE_TokenInit _ toka) -> [toka]
            _ -> []
    let remember_toks = local (\e -> e {be_toks = toks <> be_toks e})
    (k'c, k'l) <- remember_toks $ be_c k
    c'c <- be_m c
    return $ (,) (CT_Com <$> c'c <*> k'c) (ET_Com c <$> k'l)
  LLC_Only at who l k -> do
    (k'c, k'l) <- be_c k
    let lm =
          itsame who >>= \case
            False -> k'l
            True -> ET_ConsensusOnly at l <$> k'l
    return $ (,) (k'c) lm
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
    which <- be_which <$> ask
    (more, s'l) <-
      captureMore $
        local (\e -> e {be_interval = default_interval}) $
          be_s s
    toks <- be_toks <$> ask
    let mkfrom_info do_readMustSave = do
          svs <- do_readMustSave which
          return $ case more of
            True -> FI_Continue $ map (\x -> (x, DLA_Var x)) svs
            False -> FI_Halt toks
    let cm = CT_From at1 which <$> mkfrom_info ce_readMustSave
    let lm = ET_FromConsensus at1 which <$> mkfrom_info ee_readMustSave <*> s'l
    return $ (,) cm lm
  LLC_While at asn _inv cond body k -> do
    let DLinBlock cond_at _ cond_l cond_a = cond
    this_loop <- newHandler "While"
    let inBlock which = local (\e -> e {be_which = which})
    let inLoop = inBlock this_loop
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
          kont_block <- newHandler "While Kont"
          let inKont = inBlock kont_block
          (k'c, k'l) <- inKont $ be_c k
          setHandler kont_block $ do
            kont_svs <- ce_readMustReceive kont_block
            C_Loop at kont_svs [] <$> k'c
          inLoop $ fg_jump $ kont_block
          let gk = CT_Jump at kont_block <$> ce_readMustReceive kont_block <*> pure mempty
          return (gk, k'l)
    -- </Kont>
    fg_use $ asn
    let loop_vars = assignment_vars asn
    fg_defn $ loop_vars
    cond_l' <- inLoop $ do
      fg_defn $ loop_vars
      be_t cond_l
    (body'c, body'l) <-
      inLoop $
        local (\e -> e {be_loop = Just this_loop}) $
          be_c body
    let loop_if = CT_If cond_at cond_a <$> body'c <*> goto_kont
    let loop_top = dtReplace CT_Com <$> loop_if <*> cond_l'
    setHandler this_loop $ do
      loop_svs <- ce_readMustReceive this_loop
      C_Loop at loop_svs loop_vars <$> loop_top
    fg_jump $ this_loop
    let cm = CT_Jump at this_loop <$> ce_readMustReceive this_loop <*> pure asn
    let lm = ET_While at asn cond <$> body'l <*> k'l
    return $ (,) cm lm
  LLC_Continue at asn -> do
    fg_use $ asn
    this_loop <- fromMaybe (impossible "no loop") . be_loop <$> ask
    which <- be_which <$> ask
    when (this_loop == which) $
      expect_thrown at Err_ContinueDomination
    fg_jump $ this_loop
    let cm = CT_Jump at this_loop <$> ce_readMustReceive this_loop <*> pure asn
    let lm = return $ ET_Continue at asn
    return $ (,) cm lm

be_s :: LLStep -> BApp (EApp EITail)
be_s = \case
  LLS_Com c k -> do
    int <- be_interval <$> ask
    let int' =
          case c of
            (DL_Let _ _ (DLE_Wait _ amt)) -> interval_add_from int amt
            _ -> int
    k' <- local (\e -> e {be_interval = int'}) $ be_s k
    return $ ET_Com c <$> k'
  LLS_Stop at ->
    return $ (return $ ET_Stop at)
  LLS_Only at who l k -> do
    k' <- be_s k
    return $ ee_only at who l =<< k'
  LLS_ToConsensus at send recv mtime -> do
    let DLRecv from_v msg_vs time_v (last_time_mv, ok_c) = recv
    prev <- be_which <$> ask
    signalMore
    which <- newHandler "ToConsensus"
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
               , be_which = which
               })
          $ do
            fg_use $ int_ok
            fg_use $ last_time_mv
            fg_defn $ from_v : time_v : msg_vs
            be_c ok_c
    fg_child which
    setHandler which $ do
      svs <- ce_readMustReceive which
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
              svs <- ee_readMustReceive which
              return $ Just (ds_msg, ds_pay, ds_when, svs, soloSend)
          mtime' <- mtime'm
          return $ ET_ToConsensus at from_v prev last_time_mv which mfrom msg_vs out_vs time_v mtime' ok_l'
    return $ ok_l''m

mk_ev :: DLinExportVal LLBlock -> BApp (DLinExportVal PILBlock)
mk_ev = \case
  DLEV_Fun at args (DLinBlock bat sf ll a) -> do
    let body' = dtReplace DT_Com (DT_Return bat) ll
    return $ DLEV_Fun at args (DLinBlock bat sf body' a)
  DLEV_Arg at a -> return $ DLEV_Arg at a

mk_eb :: DLExportinBlock LLVar -> BApp (DLExportinBlock PILVar)
mk_eb = \case
  DLExportinBlock ll r -> do
    let body' = dtReplace DT_Com (DT_Return $ srclocOf r) ll
    DLExportinBlock body' <$> mk_ev r

epp :: LLProg -> IO PIProg
epp (LLProg at (LLOpts {..}) ps dli dex s) = do
  -- Step 1: Analyze the program to compute basic blocks
  be_handlerc <- newCounter 1
  be_handlers <- newIORef mempty
  be_flowr <- newIORef mempty
  be_more <- newIORef False
  let be_loop = Nothing
  let be_which = 0
  let be_interval = default_interval
  be_output_vs <- newIORef mempty
  let be_toks = mempty
  mkep_ <- flip runReaderT (BEnv {..}) $ be_s s
  hs <- readIORef be_handlers
  -- Step 2: Solve the flow graph
  flowi <- readIORef be_flowr
  let flowi' = M.map (const mempty) hs
  let flow = solve $ flowi <> flowi'
  -- Step 3: Turn the blocks into handlers
  let mkh m = do
        let ce_flow = flow
        ce_vars <- newIORef mempty
        flip runReaderT (CEnv {..}) m
  dex' <-
    flip runReaderT (BEnv {..}) $
      mapM mk_eb dex
  cp <- (CPProg at . CHandlers) <$> mapM mkh hs
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
