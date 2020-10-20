module Reach.Optimize (optimize) where

import Control.Monad.Reader
-- import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.IORef
-- import qualified Data.Sequence as Seq
-- import GHC.Stack (HasCallStack)
import Reach.AST
-- import Reach.Type
-- import Reach.Util
import Reach.Pretty()

sb :: SrcLoc
sb = srcloc_builtin

type App = ReaderT Env IO

data Focus
  = F_All
  | F_One SLPart
  | F_Consensus
  deriving (Eq, Ord, Show)

data CommonEnv = CommonEnv
  { ceReplaced :: M.Map DLVar DLVar
  , cePrev :: M.Map DLExpr DLVar }

instance Semigroup CommonEnv where
  x <> y = CommonEnv
    { ceReplaced = g ceReplaced
    , cePrev = g cePrev }
    where g f = f x <> f y

instance Monoid CommonEnv where
  mempty = CommonEnv mempty mempty

data Env = Env
  { eFocus :: Focus
  , eParts :: [SLPart]
  , eEnvsR :: IORef (M.Map Focus CommonEnv) }

focus :: Focus -> App a -> App a
focus f = local (\e -> e { eFocus = f })

focusa :: App a -> App a
focusa = focus F_All
focusp :: SLPart -> App a -> App a
focusp = focus . F_One
focusc :: App a -> App a
focusc = focus F_Consensus

newScope :: App x -> App x
newScope m = do
  Env { .. } <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  eEnvsR' <- liftIO $ newIORef eEnvs
  local (\e -> e { eEnvsR = eEnvsR' }) m

lookupCommon :: Ord a => (CommonEnv -> M.Map a b) -> a -> App (Maybe b)
lookupCommon dict obj = do
  Env { .. } <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  return $ do
    cenv <- M.lookup eFocus eEnvs
    M.lookup obj (dict cenv)

rewritten :: DLVar -> App (Maybe DLVar)
rewritten = lookupCommon ceReplaced

repeated :: DLExpr -> App (Maybe DLVar)
repeated = lookupCommon cePrev

remember :: DLVar -> DLExpr -> App ()
remember v e =
  updateLookup (\cenv -> cenv { cePrev = M.insert e v (cePrev cenv) })

rewrite :: DLVar -> DLVar -> App ()
rewrite v v' =
  updateLookup (\cenv -> cenv { ceReplaced = M.insert v v' (ceReplaced cenv) })

updateLookup :: (CommonEnv -> CommonEnv) -> App ()
updateLookup up = do
  Env { .. } <- ask
  let writeHuh f =
        case eFocus of
          F_All -> True
          F_Consensus -> True
          F_One _ -> f == eFocus
  let update1 (f, cenv) = (f, (if writeHuh f then up else id ) cenv)
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

opt_v :: DLVar -> App DLVar
opt_v v = do
  r <- rewritten v
  return $ fromMaybe v r

opt_a :: DLArg -> App DLArg
opt_a = \case
  DLA_Var v -> DLA_Var <$> opt_v v
  DLA_Constant c -> pure $ DLA_Constant c
  DLA_Literal c -> (pure $ DLA_Literal c)
  DLA_Array t as -> (pure $ DLA_Array t) <*> opt_as as
  DLA_Tuple as -> (pure $ DLA_Tuple) <*> opt_as as
  DLA_Obj m -> (pure $ DLA_Obj) <*> mapM opt_a m
  DLA_Data t vn vv -> DLA_Data t vn <$> opt_a vv
  DLA_Interact p m t -> (pure $ DLA_Interact p m t)

opt_as :: [DLArg] -> App [DLArg]
opt_as = mapM opt_a

opt_e :: DLExpr -> App DLExpr
opt_e = \case
  DLE_Arg at a -> (pure $ DLE_Arg at) <*> opt_a a
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

sani_l :: DLLiteral -> DLLiteral
sani_l l =
  case l of
    DLL_Null -> l
    DLL_Bool {} -> l
    DLL_Int _ i -> DLL_Int sb i
    DLL_Bytes {} -> l

sani_a :: DLArg -> DLArg
sani_a a =
  case a of
    DLA_Var {} -> a
    DLA_Constant {} -> a
    DLA_Literal l -> DLA_Literal $ sani_l l
    DLA_Array t as -> DLA_Array t (sani_as as)
    DLA_Tuple as -> DLA_Tuple (sani_as as)
    DLA_Obj m -> DLA_Obj (M.map sani_a m)
    DLA_Data t v va -> DLA_Data t v (sani_a va)
    DLA_Interact {} -> a

sani_as :: [DLArg] -> [DLArg]
sani_as = map sani_a

sani_e :: DLExpr -> DLExpr
sani_e = \case
  DLE_Arg _ a -> DLE_Arg sb $ sani_a a
  DLE_Impossible _ m -> DLE_Impossible sb m
  DLE_PrimOp _ f as -> DLE_PrimOp sb f (sani_as as)
  DLE_ArrayRef _ a i -> DLE_ArrayRef sb (sani_a a) (sani_a i)
  DLE_ArraySet _ a i v -> DLE_ArraySet sb (sani_a a) (sani_a i) (sani_a v)
  DLE_ArrayConcat _ x y -> DLE_ArrayConcat sb (sani_a x) (sani_a y)
  DLE_ArrayZip _ x y -> DLE_ArrayZip sb (sani_a x) (sani_a y)
  DLE_TupleRef _ a i -> DLE_TupleRef sb (sani_a a) i
  DLE_ObjectRef _ a f -> DLE_ObjectRef sb (sani_a a) f
  DLE_Interact _ fs p m t as -> DLE_Interact sb fs p m t (sani_as as)
  DLE_Digest _ as -> DLE_Digest sb (sani_as as)
  DLE_Claim _ fs ct a mm -> DLE_Claim sb fs ct (sani_a a) mm
  DLE_Transfer _ x y -> DLE_Transfer sb (sani_a x) (sani_a y)
  DLE_Wait _ x -> DLE_Wait sb (sani_a x)
  DLE_PartSet _ p x -> DLE_PartSet sb p (sani_a x)

opt_asn :: DLAssignment -> App DLAssignment
opt_asn (DLAssignment m) =
  DLAssignment <$> mapM opt_a m

opt_m :: (LLCommon a -> a) -> (a -> App a) -> LLCommon a -> App a
opt_m mkk opt_k = \case
  LL_Return at -> pure $ mkk $ LL_Return at
  LL_Let at Nothing e k ->
    mkk <$> (LL_Let at Nothing <$> opt_e e <*> opt_k k)
  LL_Let at (Just dv) e k -> do
    e' <- opt_e e
    let e'' = sani_e e'
    common <- repeated e''
    case common of
      Just dv' -> do
        rewrite dv dv'
        opt_k k
      Nothing -> do
        remember dv e''
        mkk <$> (LL_Let at (Just dv) e' <$> opt_k k)
  LL_Var at v k ->
    mkk <$> (LL_Var at v <$> opt_k k)
  LL_Set at v a k ->
    mkk <$> (LL_Set at v <$> opt_a a <*> opt_k k)
  LL_LocalIf at c t f k ->
    mkk <$> (LL_LocalIf at <$> opt_a c <*> (newScope $ opt_l t) <*> (newScope $ opt_l f) <*> opt_k k)
  LL_LocalSwitch at ov csm k ->
    mkk <$> (LL_LocalSwitch at ov <$> mapM cm1 csm <*> opt_k k)
    where cm1 (mov', l) = (,) <$> pure mov' <*> (newScope $ opt_l l)
  LL_ArrayMap at ans x0 a f r k -> do
    LLBlock _ _ f' r' <- opt_bl $ LLBlock at [] f r
    mkk <$> (LL_ArrayMap at ans <$> opt_a x0 <*> (pure a) <*> (pure f') <*> (pure r') <*> opt_k k)
  LL_ArrayReduce at ans x0 z b a f r k -> do
    LLBlock _ _ f' r' <- opt_bl $ LLBlock at [] f r
    mkk <$> (LL_ArrayReduce at ans <$> opt_a x0 <*> opt_a z <*> (pure b) <*> (pure a) <*> (pure f') <*> (pure r') <*> opt_k k)

opt_l :: LLLocal -> App LLLocal
opt_l = \case
  LLL_Com m -> opt_m LLL_Com opt_l m

opt_bl :: LLBlock LLLocal -> App (LLBlock LLLocal)
opt_bl (LLBlock at fs b a) =
  newScope $ LLBlock at fs <$> opt_l b <*> opt_a a

opt_n :: LLConsensus -> App LLConsensus
opt_n = \case
  LLC_Com m -> opt_m LLC_Com opt_n m
  LLC_If at c t f ->
    LLC_If at <$> opt_a c <*> (newScope $ opt_n t) <*> (newScope $ opt_n f)
  LLC_Switch at ov csm ->
    LLC_Switch at ov <$> mapM cm1 csm
    where cm1 (mov', n) = (,) <$> pure mov' <*> (newScope $ opt_n n)
  LLC_While at asn inv cond body k ->
    LLC_While at <$> opt_asn asn <*> opt_bl inv <*> opt_bl cond <*> (newScope $ opt_n body) <*> opt_n k
  LLC_Continue at asn ->
    LLC_Continue at <$> opt_asn asn
  LLC_FromConsensus at1 at2 s ->
    LLC_FromConsensus at1 at2 <$> (focusa $ opt_s s)

opt_fs :: FromSpec -> App FromSpec
opt_fs = \case
  FS_Join v -> pure $ FS_Join v
  FS_Again v -> FS_Again <$> opt_v v

opt_mtime :: Maybe (DLArg, LLStep) -> App (Maybe (DLArg, LLStep))
opt_mtime = \case
  Nothing -> pure $ Nothing
  Just (a, s) -> Just <$> (pure (,) <*> opt_a a <*> opt_s s)

opt_s :: LLStep -> App LLStep
opt_s = \case
  LLS_Com m -> opt_m LLS_Com opt_s m
  LLS_Stop at -> pure $ LLS_Stop at
  LLS_Only at p l s ->
    LLS_Only at p <$> (focusp p $ opt_l l) <*> opt_s s
  LLS_ToConsensus at from fs from_as from_msg from_amt from_amtv mtime cons ->
    LLS_ToConsensus at from <$> opt_fs fs <*> (focusp from $ opt_as from_as) <*> (pure from_msg) <*> (focusp from $ opt_a from_amt) <*> (pure from_amtv) <*> opt_mtime mtime <*> (focusc $ opt_n cons)

optimize :: LLProg -> IO LLProg
optimize (LLProg at opts ps s) = do
  let SLParts m = ps
  let psl = M.keys m
  env0 <- mkEnv0 psl
  flip runReaderT env0 $ 
    LLProg at opts ps <$> opt_s s
