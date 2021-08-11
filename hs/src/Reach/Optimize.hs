module Reach.Optimize (optimize_, optimize, Optimize) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.Counter
import Reach.Sanitize
import Reach.UnrollLoops
import Reach.Util
import Safe (atMay)

type App = ReaderT Env IO

type AppT a = a -> App a

class Optimize a where
  opt :: AppT a

data Focus
  = F_Ctor
  | F_All
  | F_One SLPart
  | F_Consensus
  deriving (Eq, Ord, Show)

data CommonEnv = CommonEnv
  { ceReplaced :: M.Map DLVar (DLVar, Maybe DLArg)
  , cePrev :: M.Map DLExpr DLVar
  , ceNots :: M.Map DLVar DLArg
  , ceKnownVariants :: M.Map DLVar (SLVar, DLArg)
  , ceKnownLargeArgs :: M.Map DLVar DLLargeArg
  }

instance Semigroup CommonEnv where
  x <> y =
    CommonEnv
      { ceReplaced = g ceReplaced
      , cePrev = g cePrev
      , ceNots = g ceNots
      , ceKnownVariants = g ceKnownVariants
      , ceKnownLargeArgs = g ceKnownLargeArgs
      }
    where
      g f = f x <> f y

instance Monoid CommonEnv where
  mempty = CommonEnv mempty mempty mempty mempty mempty

data Env = Env
  { eFocus :: Focus
  , eParts :: [SLPart]
  , eEnvsR :: IORef (M.Map Focus CommonEnv)
  , eCounter :: Counter
  }

focus :: Focus -> App a -> App a
focus f = local (\e -> e {eFocus = f})

focus_ctor :: App a -> App a
focus_ctor = focus F_Ctor

focus_all :: App a -> App a
focus_all = focus F_All

focus_one :: SLPart -> App a -> App a
focus_one = focus . F_One

focus_con :: App a -> App a
focus_con = focus F_Consensus

newScope :: App x -> App x
newScope m = do
  Env {..} <- ask
  eEnvsR' <- liftIO $ dupeIORef eEnvsR
  local (\e -> e {eEnvsR = eEnvsR'}) m

lookupCommon :: Ord a => (CommonEnv -> M.Map a b) -> a -> App (Maybe b)
lookupCommon dict obj = do
  Env {..} <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  return $ do
    cenv <- M.lookup eFocus eEnvs
    M.lookup obj (dict cenv)

rewrittenp :: DLVar -> App (Maybe (DLVar, Maybe DLArg))
rewrittenp = lookupCommon ceReplaced

repeated :: DLExpr -> App (Maybe DLVar)
repeated = \case
  DLE_Arg _ (DLA_Var dv') -> return $ Just dv'
  e -> lookupCommon cePrev e

optNotHuh :: DLArg -> App (Maybe DLArg)
optNotHuh = \case
  DLA_Var v -> lookupCommon ceNots v
  _ -> return $ Nothing

recordNotHuh :: DLLetVar -> DLArg -> App ()
recordNotHuh = \case
  DLV_Eff -> const $ return ()
  DLV_Let _ v -> \a ->
    updateLookup
      (\cenv ->
         cenv
           { ceNots = M.insert v a $ ceNots cenv
           })

optKnownVariant :: DLVar -> App (Maybe (SLVar, DLArg))
optKnownVariant = lookupCommon ceKnownVariants

recordKnownVariant :: DLVar -> SLVar -> DLArg -> App ()
recordKnownVariant dv k va =
  updateLookup (\e -> e { ceKnownVariants = M.insert dv (k, va) $ ceKnownVariants e })

optKnownLargeArg :: DLVar -> App (Maybe DLLargeArg)
optKnownLargeArg = lookupCommon ceKnownLargeArgs

recordKnownLargeArg :: DLVar -> DLLargeArg -> App ()
recordKnownLargeArg dv v =
  updateLookup (\e -> e { ceKnownLargeArgs = M.insert dv v $ ceKnownLargeArgs e })

remember_ :: Bool -> DLVar -> DLExpr -> App ()
remember_ always v e =
  updateLookup (\cenv -> cenv {cePrev = up $ cePrev cenv})
  where
    up prev =
      case always || not (M.member e prev) of
        True -> M.insert e v prev
        False -> prev

remember :: DLVar -> DLExpr -> App ()
remember = remember_ True

mremember :: DLVar -> DLExpr -> App ()
mremember = remember_ False

rewrite :: DLVar -> (DLVar, Maybe DLArg) -> App ()
rewrite v r = do
  updateLookup (\cenv -> cenv {ceReplaced = M.insert v r (ceReplaced cenv)})

updateLookup :: (CommonEnv -> CommonEnv) -> App ()
updateLookup up = do
  Env {..} <- ask
  let writeHuh f =
        case eFocus of
          F_Ctor ->
            case f of
              F_Ctor -> True
              F_All -> False
              F_Consensus -> False
              F_One _ -> True
          F_All -> True
          F_Consensus -> True
          F_One _ -> f == eFocus
  let update1 (f, cenv) = (f, (if writeHuh f then up else id) cenv)
  let update = M.fromList . (map update1) . M.toList
  liftIO $ modifyIORef eEnvsR update

mkEnv0 :: Counter -> [SLPart] -> IO Env
mkEnv0 eCounter eParts = do
  let eFocus = F_Ctor
  let eEnvs =
        M.fromList $
          map (\x -> (x, mempty)) $ F_All : F_Consensus : map F_One eParts
  eEnvsR <- liftIO $ newIORef eEnvs
  return $ Env {..}

opt_v2p :: DLVar -> App (DLVar, DLArg)
opt_v2p v = do
  r <- rewrittenp v
  let both v' = return $ (v', DLA_Var v')
  case r of
    Nothing -> both v
    Just (v', Nothing) -> both v'
    Just (v', Just a') -> return $ (v', a')

opt_v2v :: DLVar -> App DLVar
opt_v2v v = fst <$> opt_v2p v

opt_v2a :: DLVar -> App DLArg
opt_v2a v = snd <$> opt_v2p v

instance (Traversable t, Optimize a) => Optimize (t a) where
  opt = traverse opt

instance {-# OVERLAPS #-} (Optimize a, Optimize b) => Optimize (a, b) where
  opt (x, y) = (,) <$> opt x <*> opt y

instance {-# OVERLAPS #-} (Optimize a, Optimize b) => Optimize (Either a b) where
  opt = \case
    Left x -> Left <$> opt x
    Right x -> Right <$> opt x

instance Optimize IType where
  opt = return

instance Optimize DLVar where
  opt = opt_v2v

instance Optimize DLArg where
  opt = \case
    DLA_Var v -> opt_v2a v
    DLA_Constant c -> return $ DLA_Constant c
    DLA_Literal c -> return $ DLA_Literal c
    DLA_Interact p m t -> return $ DLA_Interact p m t

instance Optimize DLLargeArg where
  opt = \case
    DLLA_Array t as -> DLLA_Array t <$> opt as
    DLLA_Tuple as -> DLLA_Tuple <$> opt as
    DLLA_Obj m -> DLLA_Obj <$> opt m
    DLLA_Data t vn vv -> DLLA_Data t vn <$> opt vv
    DLLA_Struct kvs -> DLLA_Struct <$> mapM go kvs
      where
        go (k, v) = (,) k <$> opt v

instance Optimize DLTokenNew where
  opt (DLTokenNew {..}) = DLTokenNew
    <$> opt dtn_name
    <*> opt dtn_sym
    <*> opt dtn_url
    <*> opt dtn_metadata
    <*> opt dtn_supply

instance Optimize DLWithBill where
  opt (DLWithBill y z) =
    DLWithBill <$> opt y <*> opt z

unsafeAt :: [a] -> Int -> a
unsafeAt l i =
  case atMay l i of
    Nothing -> impossible "unsafeMay"
    Just x -> x

instance Optimize DLExpr where
  opt = \case
    DLE_Arg at a -> DLE_Arg at <$> opt a
    DLE_LArg at a -> DLE_LArg at <$> opt a
    DLE_Impossible at tag lab ->
      return $ DLE_Impossible at tag lab
    DLE_PrimOp at p as -> do
      as' <- opt as
      let meh = return $ DLE_PrimOp at p as'
      let zero = DLA_Literal $ DLL_Int at 0
      case (p, as') of
        (ADD, [(DLA_Literal (DLL_Int _ 0)), rhs]) ->
          return $ DLE_Arg at rhs
        (ADD, [lhs, (DLA_Literal (DLL_Int _ 0))]) ->
          return $ DLE_Arg at lhs
        (SUB, [lhs, (DLA_Literal (DLL_Int _ 0))]) ->
          return $ DLE_Arg at lhs
        (MUL, [(DLA_Literal (DLL_Int _ 1)), rhs]) ->
          return $ DLE_Arg at rhs
        (MUL, [lhs, (DLA_Literal (DLL_Int _ 1))]) ->
          return $ DLE_Arg at lhs
        (MUL, [(DLA_Literal (DLL_Int _ 0)), _]) ->
          return $ DLE_Arg at zero
        (MUL, [_, (DLA_Literal (DLL_Int _ 0))]) ->
          return $ DLE_Arg at zero
        (DIV, [lhs, (DLA_Literal (DLL_Int _ 1))]) ->
          return $ DLE_Arg at lhs
        (IF_THEN_ELSE, [c, (DLA_Literal (DLL_Bool True)), (DLA_Literal (DLL_Bool False))]) ->
          return $ DLE_Arg at $ c
        (IF_THEN_ELSE, [(DLA_Literal (DLL_Bool c)), t, f]) ->
          return $ DLE_Arg at $ if c then t else f
        (IF_THEN_ELSE, [c, t, f]) ->
          optNotHuh c >>= \case
            Nothing -> meh
            Just c' ->
              return $ DLE_PrimOp at IF_THEN_ELSE [c', f, t]
        _ -> meh
    DLE_ArrayRef at a i -> DLE_ArrayRef at <$> opt a <*> opt i
    DLE_ArraySet at a i v -> DLE_ArraySet at <$> opt a <*> opt i <*> opt v
    DLE_ArrayConcat at x0 y0 -> DLE_ArrayConcat at <$> opt x0 <*> opt y0
    DLE_ArrayZip at x0 y0 -> DLE_ArrayZip at <$> opt x0 <*> opt y0
    DLE_TupleRef at t i -> do
      t' <- opt t
      let meh = return $ DLE_TupleRef at t' i
      case t' of
        DLA_Var tv ->
          optKnownLargeArg tv >>= \case
            Just (DLLA_Tuple as) ->
              return $ DLE_Arg at $ unsafeAt as $ fromIntegral i
            _ -> meh
        _ -> meh
    DLE_ObjectRef at o k -> DLE_ObjectRef at <$> opt o <*> pure k
    DLE_Interact at fs p m t as -> DLE_Interact at fs p m t <$> opt as
    DLE_Digest at as -> DLE_Digest at <$> opt as
    DLE_Claim at fs t a m -> do
      a' <- opt a
      case a' of
        DLA_Literal (DLL_Bool True) -> nop at
        _ ->
          return $ DLE_Claim at fs t a' m
    DLE_Transfer at t a m -> do
      a' <- opt a
      case a' of
        DLA_Literal (DLL_Int _ 0) -> nop at
        _ ->
          DLE_Transfer at <$> opt t <*> pure a' <*> opt m
    DLE_TokenInit at t -> DLE_TokenInit at <$> opt t
    DLE_CheckPay at fs a m -> DLE_CheckPay at fs <$> opt a <*> opt m
    DLE_Wait at a -> DLE_Wait at <$> opt a
    DLE_PartSet at who a -> DLE_PartSet at who <$> opt a
    DLE_MapRef at mv fa -> DLE_MapRef at mv <$> opt fa
    DLE_MapSet at mv fa na -> DLE_MapSet at mv <$> opt fa <*> opt na
    DLE_Remote at fs av m amta as wbill -> DLE_Remote at fs <$> opt av <*> pure m <*> opt amta <*> opt as <*> opt wbill
    DLE_TokenNew at tns -> DLE_TokenNew at <$> opt tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> opt tok <*> opt amt
    DLE_TokenDestroy at tok -> DLE_TokenDestroy at <$> opt tok
    DLE_TimeOrder at tos -> DLE_TimeOrder at <$> opt tos
    where
      nop at = return $ DLE_Arg at $ DLA_Literal $ DLL_Null

instance Optimize DLAssignment where
  opt (DLAssignment m) = DLAssignment <$> opt m

class Extract a where
  extract :: a -> Maybe DLVar

instance Extract (Maybe DLVar) where
  extract = id

opt_if :: (Eq k, Sanitize k, Optimize k) => (k -> r) -> (SrcLoc -> DLArg -> k -> k -> r) -> SrcLoc -> DLArg -> k -> k -> App r
opt_if mkDo mkIf at c t f =
  opt c >>= \case
    DLA_Literal (DLL_Bool True) -> mkDo <$> opt t
    DLA_Literal (DLL_Bool False) -> mkDo <$> opt f
    c' -> do
      -- XXX We could see if c' is something like `DLVar x == DLArg y` and add x -> y to the optimization environment
      t' <- newScope $ opt t
      f' <- newScope $ opt f
      case sani t' == sani f' of
        True -> return $ mkDo t'
        False ->
          optNotHuh c' >>= \case
            Just c'' ->
              return $ mkIf at c'' f' t'
            Nothing ->
              return $ mkIf at c' t' f'

optSwitch :: Optimize k => (k -> r) -> (DLStmt -> k -> k) -> (SrcLoc -> DLVar -> SwitchCases k -> r) -> SrcLoc -> DLVar -> SwitchCases k -> App r
optSwitch mkDo mkLet mkSwitch at ov csm = do
  ov' <- opt ov
  optKnownVariant ov' >>= \case
    Just (var, var_val) -> do
      let (var_var, _, var_k) = (M.!) csm var
      let var_k' = mkLet (DL_Let at (DLV_Let DVC_Many var_var) (DLE_Arg at var_val)) var_k
      newScope $ mkDo <$> opt var_k'
    Nothing -> do
      let cm1 k (v_v, vnu, n) = (,,) v_v vnu <$> (newScope $ recordKnownVariant ov' k (DLA_Var v_v) >> opt n)
      mkSwitch at ov' <$> mapWithKeyM cm1 csm

instance Optimize DLStmt where
  opt = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at x e -> do
      e' <- opt e
      let meh = return $ DL_Let at x e'
      case (extract x, isPure e && canDupe e) of
        (Just dv, True) ->
          case e' of
            DLE_LArg _ a' | canDupe a' -> do
              recordKnownLargeArg dv a'
              meh
            DLE_Arg _ a' | canDupe a' -> do
              rewrite dv (dv, Just a')
              mremember dv (sani e')
              meh
            _ -> do
              let e'' = sani e'
              common <- repeated e''
              case common of
                Just rt -> do
                  rewrite dv (rt, Nothing)
                  return $ DL_Nop at
                Nothing -> do
                  remember dv e''
                  case e' of
                    DLE_PrimOp _ IF_THEN_ELSE [c, DLA_Literal (DLL_Bool False), DLA_Literal (DLL_Bool True)] ->
                      recordNotHuh x c
                    _ ->
                      return ()
                  meh
        _ -> meh
    DL_Var at v ->
      return $ DL_Var at v
    DL_Set at v a ->
      DL_Set at v <$> opt a
    DL_LocalIf at c t f ->
      opt_if (DL_LocalDo at) DL_LocalIf at c t f
    DL_LocalSwitch at ov csm ->
      optSwitch (DL_LocalDo at) DT_Com DL_LocalSwitch at ov csm
    s@(DL_ArrayMap at ans x a f) -> maybeUnroll s x $
      DL_ArrayMap at ans <$> opt x <*> (pure a) <*> opt f
    s@(DL_ArrayReduce at ans x z b a f) -> maybeUnroll s x $ do
      DL_ArrayReduce at ans <$> opt x <*> opt z <*> (pure b) <*> (pure a) <*> opt f
    DL_MapReduce at mri ans x z b a f -> do
      DL_MapReduce at mri ans x <$> opt z <*> (pure b) <*> (pure a) <*> opt f
    DL_Only at ep l -> do
      let w = case ep of
            Left p -> focus_one p
            Right _ -> id
      l' <- w $ opt l
      case l' of
        DT_Return _ -> return $ DL_Nop at
        _ -> return $ DL_Only at ep l'
    DL_LocalDo at t ->
      opt t >>= \case
        DT_Return _ -> return $ DL_Nop at
        t' -> return $ DL_LocalDo at t'
    where
      maybeUnroll :: DLStmt -> DLArg -> App DLStmt -> App DLStmt
      maybeUnroll s x def =
        case argTypeOf x of
          T_Array _ n ->
            case n <= 1 of
              True -> do
                c <- asks eCounter
                let at = srclocOf s
                let t = DL_LocalDo at $ DT_Com s $ DT_Return at
                UnrollWrapper _ t' <- liftIO $ unrollLoops $ UnrollWrapper c t
                return t'
              _ -> def
          _ -> def

instance Optimize DLTail where
  opt = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> mkCom DT_Com <$> opt m <*> opt k

instance Optimize DLBlock where
  opt (DLBlock at fs b a) =
    newScope $ DLBlock at fs <$> opt b <*> opt a

instance {-# OVERLAPPING #-} Optimize a => Optimize (DLinExportBlock a) where
  opt (DLinExportBlock at vs b) =
    newScope $ DLinExportBlock at vs <$> opt b

instance Optimize LLConsensus where
  opt = \case
    LLC_Com m k -> mkCom LLC_Com <$> opt m <*> opt k
    LLC_If at c t f ->
      opt_if id LLC_If at c t f
    LLC_Switch at ov csm ->
      optSwitch id LLC_Com LLC_Switch at ov csm
    LLC_While at asn inv cond body k ->
      LLC_While at <$> opt asn <*> opt inv <*> opt cond <*> (newScope $ opt body) <*> opt k
    LLC_Continue at asn ->
      LLC_Continue at <$> opt asn
    LLC_FromConsensus at1 at2 s ->
      LLC_FromConsensus at1 at2 <$> (focus_all $ opt s)
    LLC_ViewIs at vn vk a k ->
      LLC_ViewIs at vn vk <$> opt a <*> opt k

opt_mtime :: AppT (Maybe (DLTimeArg, LLStep))
opt_mtime = \case
  Nothing -> pure $ Nothing
  Just (d, s) -> Just <$> (pure (,) <*> (focus_con $ opt d) <*> (newScope $ opt s))

instance Optimize DLPayAmt where
  opt (DLPayAmt {..}) = DLPayAmt <$> opt pa_net <*> opt pa_ks

opt_send :: AppT (SLPart, DLSend)
opt_send (p, DLSend isClass args amta whena) =
  focus_one p $
    (,) p <$> (DLSend isClass <$> opt args <*> opt amta <*> opt whena)

instance Optimize LLStep where
  opt = \case
    LLS_Com m k -> mkCom LLS_Com <$> opt m <*> opt k
    LLS_Stop at -> pure $ LLS_Stop at
    LLS_ToConsensus at send recv mtime ->
      LLS_ToConsensus at <$> send' <*> recv' <*> mtime'
      where
        send' = M.fromList <$> mapM opt_send (M.toList send)
        k' = newScope $ focus_con $ opt $ dr_k recv
        recv' = (\k -> recv {dr_k = k}) <$> k'
        mtime' = opt_mtime mtime

instance Optimize DLInit where
  opt (DLInit {..}) = do
    dli_ctimem' <- opt dli_ctimem
    return $
      DLInit
        { dli_ctimem = dli_ctimem'
        , dli_maps = dli_maps
        }

instance Optimize LLProg where
  opt (LLProg at opts ps dli dex dvs s) = do
    let SLParts m = ps
    let psl = M.keys m
    env0 <- liftIO $ mkEnv0 (getCounter opts) psl
    local (\_ -> env0) $
      focus_ctor $
        LLProg at opts ps <$> opt dli <*> opt dex <*> pure dvs <*> opt s

-- This is a bit of a hack...

instance Extract DLLetVar where
  extract = \case
    DLV_Eff -> Nothing
    DLV_Let _ v -> Just v

opt_svs :: AppT [(DLVar, DLArg)]
opt_svs = mapM $ \(v, a) -> (\x -> (v, x)) <$> opt a

instance Optimize FromInfo where
  opt = \case
    FI_Continue vis svs -> FI_Continue <$> opt vis <*> opt_svs svs
    FI_Halt toks -> FI_Halt <$> opt toks

instance Optimize ViewSave where
  opt = \case
    ViewSave i svs -> ViewSave i <$> opt_svs svs

instance Optimize CTail where
  opt = \case
    CT_Com m k -> mkCom CT_Com <$> opt m <*> opt k
    CT_If at c t f ->
      opt_if id CT_If at c t f
    CT_Switch at ov csm ->
      optSwitch id CT_Com CT_Switch at ov csm
    CT_From at w fi ->
      CT_From at w <$> opt fi
    CT_Jump at which vs asn ->
      CT_Jump at which <$> opt vs <*> opt asn

instance Optimize CHandler where
  opt = \case
    C_Handler {..} -> do
      C_Handler ch_at ch_int ch_from ch_last ch_svs ch_msg ch_timev ch_secsv <$> opt ch_body
    C_Loop {..} -> do
      C_Loop cl_at cl_svs cl_vars <$> opt cl_body

instance Optimize ViewInfo where
  opt (ViewInfo vs vi) = ViewInfo vs <$> (newScope $ opt vi)

instance Optimize CPProg where
  opt (CPProg at csvs vi (CHandlers hs)) =
    CPProg at csvs <$> (newScope $ opt vi) <*> (CHandlers <$> mapM (newScope . opt) hs)

instance Optimize PLProg where
  opt (PLProg at plo dli dex epps cp) =
    PLProg at plo dli <$> opt dex <*> pure epps <*> opt cp

optimize_ :: Optimize a => Counter -> a -> IO a
optimize_ c t = do
  env0 <- mkEnv0 c []
  flip runReaderT env0 $
    opt t

optimize :: (HasCounter a, Optimize a) => a -> IO a
optimize t = optimize_ (getCounter t) t
