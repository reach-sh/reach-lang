module Reach.Optimize (optimize, pltoptimize) where

import Control.Monad.Reader
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.CollectCounts
import Reach.Sanitize
import Reach.Util

import Reach.Texty
import Reach.Pretty ()
import Debug.Trace

type App = ReaderT Env IO

type AppT a = a -> App a

data Focus
  = F_All
  | F_One SLPart
  | F_Consensus
  deriving (Eq, Ord, Show)

data CommonEnv = CommonEnv
  { ceReplaced :: M.Map DLVar DLVar
  , cePrev :: M.Map DLExpr DLVar
  }

instance Semigroup CommonEnv where
  x <> y =
    CommonEnv
      { ceReplaced = g ceReplaced
      , cePrev = g cePrev
      }
    where
      g f = f x <> f y

instance Monoid CommonEnv where
  mempty = CommonEnv mempty mempty

data Env = Env
  { eFocus :: Focus
  , eParts :: [SLPart]
  , eEnvsR :: IORef (M.Map Focus CommonEnv)
  }

focus :: Focus -> App a -> App a
focus f = local (\e -> e {eFocus = f})

focusa :: App a -> App a
focusa = focus F_All

focusp :: SLPart -> App a -> App a
focusp = focus . F_One

focusc :: App a -> App a
focusc = focus F_Consensus

newScope :: App x -> App x
newScope m = do
  Env {..} <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  eEnvsR' <- liftIO $ newIORef eEnvs
  local (\e -> e {eEnvsR = eEnvsR'}) m

lookupCommon :: Ord a => (CommonEnv -> M.Map a b) -> a -> App (Maybe b)
lookupCommon dict obj = do
  Env {..} <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  return $ do
    cenv <- M.lookup eFocus eEnvs
    M.lookup obj (dict cenv)

rewritten :: DLVar -> App (Maybe DLVar)
rewritten = lookupCommon ceReplaced

repeated :: DLExpr -> App (Maybe DLVar)
repeated = \case
  DLE_Arg _ (DLA_Var dv') -> return $ Just dv'
  e -> lookupCommon cePrev e

remember :: DLVar -> DLExpr -> App ()
remember v e =
  updateLookup (\cenv -> cenv {cePrev = M.insert e v (cePrev cenv)})

rewrite :: DLVar -> DLVar -> App ()
rewrite v r = do
  updateLookup (\cenv -> cenv {ceReplaced = M.insert v r (ceReplaced cenv)})

updateLookup :: (CommonEnv -> CommonEnv) -> App ()
updateLookup up = do
  Env {..} <- ask
  let writeHuh f =
        case eFocus of
          F_All -> True
          F_Consensus -> True
          F_One _ -> f == eFocus
  let update1 (f, cenv) = (f, (if writeHuh f then up else id) cenv)
  let update = M.fromList . (map update1) . M.toList
  liftIO $ modifyIORef eEnvsR update

mkEnv0 :: [SLPart] -> IO Env
mkEnv0 eParts = do
  let eFocus = F_All
  let eEnvs =
        M.fromList $
          map (\x -> (x, mempty)) $ F_All : F_Consensus : map F_One eParts
  eEnvsR <- liftIO $ newIORef eEnvs
  return $ Env {..}

opt_v :: AppT DLVar
opt_v v = do
  r <- rewritten v
  return $ fromMaybe v r

opt_mv :: AppT (Maybe DLVar)
opt_mv = \case
  Nothing -> return Nothing
  Just x -> Just <$> opt_v x

opt_vs :: AppT [DLVar]
opt_vs = mapM opt_v

opt_a :: AppT DLArg
opt_a = \case
  DLA_Var v -> DLA_Var <$> opt_v v
  DLA_Constant c -> pure $ DLA_Constant c
  DLA_Literal c -> (pure $ DLA_Literal c)
  DLA_Interact p m t -> (pure $ DLA_Interact p m t)

opt_as :: AppT [DLArg]
opt_as = mapM opt_a

opt_la :: AppT DLLargeArg
opt_la = \case
  DLLA_Array t as -> (pure $ DLLA_Array t) <*> opt_as as
  DLLA_Tuple as -> (pure $ DLLA_Tuple) <*> opt_as as
  DLLA_Obj m -> (pure $ DLLA_Obj) <*> mapM opt_a m
  DLLA_Data t vn vv -> DLLA_Data t vn <$> opt_a vv

opt_e :: AppT DLExpr
opt_e = \case
  DLE_Arg at a -> (pure $ DLE_Arg at) <*> opt_a a
  DLE_LArg at a -> (pure $ DLE_LArg at) <*> opt_la a
  DLE_Impossible at lab -> pure $ DLE_Impossible at lab
  DLE_PrimOp at p as -> (pure $ DLE_PrimOp at p) <*> opt_as as
  DLE_ArrayRef at a i -> (pure $ DLE_ArrayRef at) <*> opt_a a <*> opt_a i
  DLE_ArraySet at a i v -> (pure $ DLE_ArraySet at) <*> opt_a a <*> opt_a i <*> opt_a v
  DLE_ArrayConcat at x0 y0 -> DLE_ArrayConcat at <$> opt_a x0 <*> opt_a y0
  DLE_ArrayZip at x0 y0 -> DLE_ArrayZip at <$> opt_a x0 <*> opt_a y0
  DLE_TupleRef at t i -> (pure $ DLE_TupleRef at) <*> opt_a t <*> pure i
  DLE_ObjectRef at o k -> (pure $ DLE_ObjectRef at) <*> opt_a o <*> pure k
  DLE_Interact at fs p m t as -> (pure $ DLE_Interact at fs p m t) <*> opt_as as
  DLE_Digest at as -> (pure $ DLE_Digest at) <*> opt_as as
  DLE_Claim at fs t a m -> (pure $ DLE_Claim at fs t) <*> opt_a a <*> (pure $ m)
  DLE_Transfer at t a -> (pure $ DLE_Transfer at) <*> opt_a t <*> opt_a a
  DLE_Wait at a -> (pure $ DLE_Wait at) <*> opt_a a
  DLE_PartSet at who a -> (pure $ DLE_PartSet at who) <*> opt_a a

opt_asn :: AppT DLAssignment
opt_asn (DLAssignment m) =
  DLAssignment <$> mapM opt_a m

class Extract a where
  extract :: a -> Maybe DLVar

instance Extract (Maybe DLVar) where
  extract = id

opt_m_ :: Extract b => (DLinStmt b -> a -> a) -> AppT a -> DLinStmt b -> AppT a
opt_m_ mkk opt_k c k = mkk <$> opt_m c <*> opt_k k

opt_m :: Extract a => AppT (DLinStmt a)
opt_m = \case
  DL_Nop at -> return $ DL_Nop at
  DL_Let at x e -> do
    let no = DL_Let at x <$> opt_e e
    let yes dv = do
          e' <- opt_e e
          let e'' = sani e'
          common <- repeated e''
          case common of
            Just rt -> do
              rewrite dv rt
              traceM $ "opt rm " <> (show $ pretty dv) <> " to " <> (show $ pretty rt)
              return $ DL_Nop at
            Nothing -> do
              remember dv e''
              return $ DL_Let at x e'
    case (extract x, isPure e) of
      (Just dv, True) -> yes dv
      _ -> no
  DL_Var at v ->
    return $ DL_Var at v
  DL_Set at v a ->
    DL_Set at v <$> opt_a a
  DL_LocalIf at c t f ->
    DL_LocalIf at <$> opt_a c <*> (newScope $ opt_l t) <*> (newScope $ opt_l f)
  DL_LocalSwitch at ov csm ->
    DL_LocalSwitch at <$> opt_v ov <*> mapM cm1 csm
    where
      cm1 (mov', l) = (,) <$> pure mov' <*> (newScope $ opt_l l)
  DL_ArrayMap at ans x0 a f -> do
    DL_ArrayMap at ans <$> opt_a x0 <*> (pure a) <*> opt_bl f
  DL_ArrayReduce at ans x0 z b a f -> do
    DL_ArrayReduce at ans <$> opt_a x0 <*> opt_a z <*> (pure b) <*> (pure a) <*> opt_bl f

opt_l :: Extract a => AppT (DLinTail a)
opt_l = \case
  DT_Return at -> return $ DT_Return at
  DT_Com m k -> opt_m_ DT_Com opt_l m k

opt_bl :: Extract a => AppT (DLinBlock a)
opt_bl (DLinBlock at fs b a) =
  newScope $ DLinBlock at fs <$> opt_l b <*> opt_a a

opt_n :: AppT LLConsensus
opt_n = \case
  LLC_Com m k -> opt_m_ LLC_Com opt_n m k
  LLC_If at c t f ->
    LLC_If at <$> opt_a c <*> (newScope $ opt_n t) <*> (newScope $ opt_n f)
  LLC_Switch at ov csm ->
    LLC_Switch at <$> opt_v ov <*> mapM cm1 csm
    where
      cm1 (mov', n) = (,) <$> pure mov' <*> (newScope $ opt_n n)
  LLC_While at asn inv cond body k ->
    LLC_While at <$> opt_asn asn <*> opt_bl inv <*> opt_bl cond <*> (newScope $ opt_n body) <*> opt_n k
  LLC_Continue at asn ->
    LLC_Continue at <$> opt_asn asn
  LLC_FromConsensus at1 at2 s ->
    LLC_FromConsensus at1 at2 <$> (focusa $ opt_s s)
  LLC_Only at p l k ->
    LLC_Only at p <$> (focusp p $ opt_l l) <*> opt_n k

opt_mtime :: AppT (Maybe (DLArg, LLStep))
opt_mtime = \case
  Nothing -> pure $ Nothing
  Just (d, s) -> Just <$> (pure (,) <*> (focusc $ opt_a d) <*> (newScope $ opt_s s))

opt_send :: AppT (SLPart, (Bool, [DLArg], DLArg, DLArg))
opt_send (p, (isClass, args, amta, whena)) =
  focusp p $
    (,) p <$> ((\x y z -> (isClass, x, y, z)) <$> opt_as args <*> opt_a amta <*> opt_a whena)

opt_s :: LLStep -> App LLStep
opt_s = \case
  LLS_Com m k -> opt_m_ LLS_Com opt_s m k
  LLS_Stop at -> pure $ LLS_Stop at
  LLS_Only at p l s ->
    LLS_Only at p <$> (focusp p $ opt_l l) <*> opt_s s
  LLS_ToConsensus at send recv mtime ->
    LLS_ToConsensus at <$> send' <*> recv' <*> mtime'
    where
      send' = M.fromList <$> mapM opt_send (M.toList send)
      (last_timev, winner_dv, msg, amtv, timev, cons) = recv
      cons' = newScope $ focusc $ opt_n cons
      recv' = (\x y -> (x, winner_dv, msg, amtv, timev, y)) <$> opt_mv last_timev <*> cons'
      mtime' = opt_mtime mtime

opt_dli :: AppT DLInit
opt_dli (DLInit ctimem) =
  DLInit <$> (opt_mv ctimem)

optimize :: LLProg -> IO LLProg
optimize (LLProg at opts ps dli s) = do
  let SLParts m = ps
  let psl = M.keys m
  env0 <- mkEnv0 psl
  flip runReaderT env0 $
    focusa $
      LLProg at opts ps <$> opt_dli dli <*> opt_s s

-- This is a bit of a hack...

instance Extract PLVar where
  extract = \case
    PV_Eff -> Nothing
    PV_Let _ v -> Just v

plopt_ct :: AppT CTail
plopt_ct = \case
  CT_Com m k ->
    opt_m_ CT_Com plopt_ct m k
  CT_If at c t f ->
    case sani t == sani f of
      True ->
        plopt_ct t
      False ->
        CT_If at <$> opt_a c <*> (newScope $ plopt_ct t) <*> (newScope $ plopt_ct f)
  CT_Switch at ov csm ->
    CT_Switch at ov <$> mapM cm1 csm
    where
      cm1 (mov', t) = (,) <$> pure mov' <*> (newScope $ plopt_ct t)
  CT_From at mvs ->
    CT_From at <$> opt_masn mvs
  CT_Jump at which vs asn ->
    CT_Jump at which <$> opt_vs vs <*> opt_asn asn

opt_masn :: AppT (Maybe [(DLVar, DLArg)])
opt_masn = \case
  Nothing -> return $ Nothing
  Just x -> Just <$> mapM go x
  where
    go (v, a) = (\x -> (v, x)) <$> opt_a a


-- _U_pdate _C_ount_s_
type UCST a = Counts -> a -> (Counts, a)

ucs_m :: UCST PLCommon
ucs_m cs_k = \case
  DL_Nop at -> (cs_k, DL_Nop at)
  DL_Let at (PV_Let _ v) e -> (cs', ct')
    where
      ct' = DL_Let at (PV_Let lc' v) e
      lc' =
        case get_count v cs_k of
          Count Nothing -> impossible "no use"
          Count (Just x) -> x
      cs' = count_rms [v] cs'_
      cs'_ = counts e <> cs_k
  DL_Let at PV_Eff e -> (cs', ct')
    where
      ct' = DL_Let at PV_Eff e
      cs' = counts e <> cs_k
  DL_Var at v -> (cs', ct')
    where
      ct' = DL_Var at v
      cs' = count_rms [v] cs_k
  DL_Set at v a -> (cs', ct')
    where
      ct' = DL_Set at v a
      cs' = count_rms [v] cs'_
      cs'_ = counts a <> cs_k
  DL_LocalIf at c t f -> (cs', ct')
    where
      ct' = DL_LocalIf at c t' f'
      cs' = counts c <> cs_t <> cs_f <> cs_k
      (cs_t, t') = ucs_pt cs_k t
      (cs_f, f') = ucs_pt cs_k f
  DL_LocalSwitch at v csm -> (cs', ct')
    where
      ct' = DL_LocalSwitch at v csm'
      cs' = counts v <> cs_csm
      (cs_csm, csm') = foldl' cm1 (mempty, mempty) $ M.toList csm
      cm1 (cs_csm_, csm_) (var, (mov, t)) = (cs_csm_', csm_')
        where
          cs_csm_' = cs_csm_ <> count_rmm mov cs_t
          (cs_t, t') = ucs_pt cs_k t
          csm_' = M.insert var (mov, t') csm_
  DL_ArrayMap at ans x a f -> (cs', ct')
    where
      ct' = DL_ArrayMap at ans x a f'
      cs' = counts x <> cs_body
      cs_body = count_rms [a] cs_f
      (cs_f, f') = ucs_bl cs_k' f
      cs_k' = count_rms [ans] cs_k
  DL_ArrayReduce at ans x z b a f -> (cs', ct')
    where
      ct' = DL_ArrayReduce at ans x z b a f'
      cs' = counts x <> counts z <> cs_body
      cs_body = count_rms [b, a] cs_f
      (cs_f, f') = ucs_bl cs_k' f
      cs_k' = count_rms [ans] cs_k

ucs_pt :: UCST PLTail
ucs_pt cs_kp = \case
  DT_Return at -> (cs', ct')
    where
      ct' = DT_Return at
      cs' = cs_kp
  DT_Com c k -> (cs'', ct')
    where
      ct' = DT_Com c' k'
      (cs', k') = ucs_pt cs_kp k
      (cs'', c') = ucs_m cs' c

ucs_bl :: UCST PLBlock
ucs_bl cs_k (DLinBlock at _ t a) = (cs_t, DLinBlock at mempty t' a)
  where
    cs_k' = counts a <> cs_k
    (cs_t, t') = ucs_pt cs_k' t

ucs_m_ :: (PLCommon -> a -> a) -> PLCommon -> UCST a -> UCST a
ucs_m_ mkk m ucs_k cs_kp k = (cs'', mkk m' k')
  where
    (cs', k') = ucs_k cs_kp k
    (cs'', m') = ucs_m cs' m

ucs_t :: UCST CTail
ucs_t cs_kp = \case
  CT_Com m k ->
    ucs_m_ CT_Com m ucs_t cs_kp k
  CT_If at c t f -> (cs', ct')
    where
      ct' = CT_If at c t' f'
      (cs_t, t') = ucs_t cs_kp t
      (cs_f, f') = ucs_t cs_kp f
      cs' = counts c <> cs_t <> cs_f
  CT_Switch at ov csm -> (cs', ct')
    where
      ct' = CT_Switch at ov csm'
      cs' = counts ov <> cs_csm
      (cs_csm, csm') = foldl' cm1 (mempty, mempty) $ M.toList csm
      cm1 (cs_csm_, csm_) (var, (mov, t)) = (cs_csm_', csm_')
        where
          cs_csm_' = cs_csm_ <> count_rmm mov cs_t
          (cs_t, t') = ucs_t cs_kp t
          csm_' = M.insert var (mov, t') csm_
  CT_From at mvs -> (cs', ct')
    where
      ct' = CT_From at mvs
      cs' = counts mvs <> cs_kp
  CT_Jump at which vs asn -> (cs', ct')
    where
      ct' = CT_Jump at which vs asn
      cs' = counts vs <> counts asn <> cs_kp

pltoptimize :: CTail -> IO (Counts, CTail)
pltoptimize t = do
  env0 <- mkEnv0 []
  ucs_t mempty
    <$> (flip runReaderT env0 $
           plopt_ct t)
