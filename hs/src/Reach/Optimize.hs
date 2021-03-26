module Reach.Optimize (optimize) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.Sanitize

type App = ReaderT Env IO

type AppT a = a -> App a

class Optimize a where
  opt :: AppT a

data Focus
  = F_All
  | F_One SLPart
  | F_Consensus
  deriving (Eq, Ord, Show)

data CommonEnv = CommonEnv
  { ceReplaced :: M.Map DLVar (DLVar, Maybe DLArg)
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

rewrittenp :: DLVar -> App (Maybe (DLVar, Maybe DLArg))
rewrittenp = lookupCommon ceReplaced

repeated :: DLExpr -> App (Maybe DLVar)
repeated = \case
  DLE_Arg _ (DLA_Var dv') -> return $ Just dv'
  e -> lookupCommon cePrev e

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
      where go (k, v) = (,) k <$> opt v

instance Optimize DLExpr where
  opt = \case
    DLE_Arg at a -> DLE_Arg at <$> opt a
    DLE_LArg at a -> DLE_LArg at <$> opt a
    DLE_Impossible at lab -> return $ DLE_Impossible at lab
    DLE_PrimOp at p as -> DLE_PrimOp at p <$> opt as
    DLE_ArrayRef at a i -> DLE_ArrayRef at <$> opt a <*> opt i
    DLE_ArraySet at a i v -> DLE_ArraySet at <$> opt a <*> opt i <*> opt v
    DLE_ArrayConcat at x0 y0 -> DLE_ArrayConcat at <$> opt x0 <*> opt y0
    DLE_ArrayZip at x0 y0 -> DLE_ArrayZip at <$> opt x0 <*> opt y0
    DLE_TupleRef at t i -> DLE_TupleRef at <$> opt t <*> pure i
    DLE_ObjectRef at o k -> DLE_ObjectRef at <$> opt o <*> pure k
    DLE_Interact at fs p m t as -> DLE_Interact at fs p m t <$> opt as
    DLE_Digest at as -> DLE_Digest at <$> opt as
    DLE_Claim at fs t a m -> DLE_Claim at fs t <$> opt a <*> (pure $ m)
    DLE_Transfer at t a -> DLE_Transfer at <$> opt t <*> opt a
    DLE_Wait at a -> DLE_Wait at <$> opt a
    DLE_PartSet at who a -> DLE_PartSet at who <$> opt a
    DLE_MapRef at mv fa -> DLE_MapRef at mv <$> opt fa
    DLE_MapSet at mv fa na -> DLE_MapSet at mv <$> opt fa <*> opt na
    DLE_MapDel at mv fa -> DLE_MapDel at mv <$> opt fa
    DLE_Remote at fs av m amta as -> DLE_Remote at fs <$> opt av <*> pure m <*> opt amta <*> opt as

instance Optimize DLAssignment where
  opt (DLAssignment m) = DLAssignment <$> opt m

class Extract a where
  extract :: a -> Maybe DLVar

instance Extract (Maybe DLVar) where
  extract = id

-- XXX Add DL_LocalDo
locDo :: DLinTail a -> DLinStmt a
locDo t = DL_LocalIf at (DLA_Literal $ DLL_Bool True) t (DT_Return at)
  where
    at = srcloc_builtin

opt_if :: (Eq k, Sanitize k, Optimize k) => (k -> r) -> (SrcLoc -> DLArg -> k -> k -> r) -> SrcLoc -> DLArg -> k -> k -> App r
opt_if mkDo mkIf at c t f = do
  c' <- opt c
  case c' of
    DLA_Literal (DLL_Bool True) -> mkDo <$> opt t
    DLA_Literal (DLL_Bool False) -> mkDo <$> opt f
    _ -> do
      t' <- newScope $ opt t
      f' <- newScope $ opt f
      case sani t == sani f of
        True -> return $ mkDo t'
        False -> return $ mkIf at c' t' f'

instance {-# OVERLAPPING #-} (Eq a, Sanitize a, Extract a) => Optimize (DLinStmt a) where
  opt = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at x e -> do
      let no = DL_Let at x <$> opt e
      let yes dv = do
            opt e >>= \case
              e'@(DLE_Arg _ a') | canDupe a' -> do
                rewrite dv (dv, Just a')
                mremember dv (sani e')
                return $ DL_Let at x e'
              e' -> do
                let e'' = sani e'
                common <- repeated e''
                case common of
                  Just rt -> do
                    rewrite dv (rt, Nothing)
                    return $ DL_Nop at
                  Nothing -> do
                    remember dv e''
                    return $ DL_Let at x e'
      case (extract x, isPure e && canDupe e) of
        (Just dv, True) -> yes dv
        _ -> no
    DL_Var at v ->
      return $ DL_Var at v
    DL_Set at v a ->
      DL_Set at v <$> opt a
    DL_LocalIf at c t f ->
      opt_if locDo DL_LocalIf at c t f
    DL_LocalSwitch at ov csm ->
      DL_LocalSwitch at <$> opt ov <*> mapM cm1 csm
      where
        cm1 (mov', l) = (,) <$> pure mov' <*> (newScope $ opt l)
    DL_ArrayMap at ans x a f -> do
      DL_ArrayMap at ans <$> opt x <*> (pure a) <*> opt f
    DL_ArrayReduce at ans x z b a f -> do
      DL_ArrayReduce at ans <$> opt x <*> opt z <*> (pure b) <*> (pure a) <*> opt f
    DL_MapReduce at mri ans x z b a f -> do
      DL_MapReduce at mri ans x <$> opt z <*> (pure b) <*> (pure a) <*> opt f

instance {-# OVERLAPPING #-} (Eq a, Sanitize a, Extract a) => Optimize (DLinTail a) where
  opt = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> mkCom DT_Com <$> opt m <*> opt k

instance {-# OVERLAPPING #-} (Eq a, Sanitize a, Extract a) => Optimize (DLinBlock a) where
  opt (DLinBlock at fs b a) =
    newScope $ DLinBlock at fs <$> opt b <*> opt a

instance {-# OVERLAPPING #-} Optimize (DLinExportVal LLBlock) where
  opt = \case
    DLEV_Arg a   -> DLEV_Arg   <$> opt a
    DLEV_LArg a  -> DLEV_LArg  <$> opt a
    DLEV_Fun a b -> DLEV_Fun a <$> opt b

instance {-# OVERLAPPING #-} Optimize LLExports where
  opt m = mapM opt m

instance Optimize LLConsensus where
  opt = \case
    LLC_Com m k -> mkCom LLC_Com <$> opt m <*> opt k
    LLC_If at c t f ->
      opt_if id LLC_If at c t f
    LLC_Switch at ov csm ->
      LLC_Switch at <$> opt ov <*> mapM cm1 csm
      where
        cm1 (mov', n) = (,) <$> pure mov' <*> (newScope $ opt n)
    LLC_While at asn inv cond body k ->
      LLC_While at <$> opt asn <*> opt inv <*> opt cond <*> (newScope $ opt body) <*> opt k
    LLC_Continue at asn ->
      LLC_Continue at <$> opt asn
    LLC_FromConsensus at1 at2 s ->
      LLC_FromConsensus at1 at2 <$> (focusa $ opt s)
    LLC_Only at p l k ->
      LLC_Only at p <$> (focusp p $ opt l) <*> opt k

opt_mtime :: AppT (Maybe (DLArg, LLStep))
opt_mtime = \case
  Nothing -> pure $ Nothing
  Just (d, s) -> Just <$> (pure (,) <*> (focusc $ opt d) <*> (newScope $ opt s))

opt_send :: AppT (SLPart, (Bool, [DLArg], DLArg, DLArg))
opt_send (p, (isClass, args, amta, whena)) =
  focusp p $
    (,) p <$> ((\x y z -> (isClass, x, y, z)) <$> opt args <*> opt amta <*> opt whena)

instance Optimize LLStep where
  opt = \case
    LLS_Com m k -> mkCom LLS_Com <$> opt m <*> opt k
    LLS_Stop at -> pure $ LLS_Stop at
    LLS_Only at p l s ->
      LLS_Only at p <$> (focusp p $ opt l) <*> opt s
    LLS_ToConsensus at send recv mtime ->
      LLS_ToConsensus at <$> send' <*> recv' <*> mtime'
      where
        send' = M.fromList <$> mapM opt_send (M.toList send)
        (last_timev, winner_dv, msg, amtv, timev, cons) = recv
        cons' = newScope $ focusc $ opt cons
        recv' = (\x y -> (x, winner_dv, msg, amtv, timev, y)) <$> opt last_timev <*> cons'
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
  opt (LLProg at opts ps dli dex s) = do
    let SLParts m = ps
    let psl = M.keys m
    env0 <- liftIO $ mkEnv0 psl
    local (\_ -> env0) $
      focusa $
        LLProg at opts ps <$> opt dli <*> opt dex <*> opt s

-- This is a bit of a hack...

instance Extract PLVar where
  extract = \case
    PV_Eff -> Nothing
    PV_Let _ v -> Just v

opt_masn :: AppT (Maybe [(DLVar, DLArg)])
opt_masn = \case
  Nothing -> return $ Nothing
  Just x -> Just <$> mapM go x
  where
    go (v, a) = (\x -> (v, x)) <$> opt a

instance {-# OVERLAPPING #-} (Eq a, Extract a, Sanitize a) => Optimize (CTail_ a) where
  opt = \case
    CT_Com m k -> mkCom CT_Com <$> opt m <*> opt k
    CT_If at c t f ->
      opt_if id CT_If at c t f
    CT_Switch at ov csm ->
      CT_Switch at ov <$> mapM cm1 csm
      where
        cm1 (mov', t) = (,) <$> pure mov' <*> (newScope $ opt t)
    CT_From at w mvs ->
      CT_From at w <$> opt_masn mvs
    CT_Jump at which vs asn ->
      CT_Jump at which <$> opt vs <*> opt asn

instance {-# OVERLAPPING #-} (Eq a, Extract a, Sanitize a) => Optimize (CHandler_ a) where
  opt = \case
    C_Handler {..} -> do
      C_Handler ch_at ch_int ch_last_timev ch_from ch_last ch_svs ch_msg ch_amtv ch_timev <$> opt ch_body
    C_Loop {..} -> do
      C_Loop cl_at cl_svs cl_vars <$> opt cl_body

instance {-# OVERLAPPING #-} (Eq a, Extract a, Sanitize a) => Optimize (CPProg a) where
  opt (CPProg at dex (CHandlers hs)) =
    CPProg at dex . CHandlers <$> mapM (newScope . opt) hs

instance {-# OVERLAPPING #-} (Eq a, Extract a, Sanitize a) => Optimize (PLinProg a) where
  opt (PLProg at plo dli epps cp) =
    PLProg at plo dli epps <$> opt cp

optimize :: Optimize a => a -> IO a
optimize t = do
  env0 <- mkEnv0 []
  flip runReaderT env0 $
    opt t
