module Reach.UnrollLoops (unrollLoops) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Reach.AST
import Reach.STCounter
import Reach.Type
import Reach.Util

type App s = ReaderT (Env s) (ST s)

type Lifts = Seq.Seq (SrcLoc, DLVar, DLExpr)

data Env s = Env
  { eDidUnroll :: STRef s Bool
  , eCounter :: STCounter s
  , eRenaming :: STRef s (M.Map Int Int)
  , emLifts :: Maybe (STRef s Lifts)
  }

mkEnv0 :: ST s (Env s)
mkEnv0 = do
  eDidUnroll <- newSTRef False
  eCounter <- newSTCounter 0
  eRenaming <- newSTRef mempty
  let emLifts = Nothing
  return $ Env {..}

recordUnroll :: App s ()
recordUnroll = do
  Env {..} <- ask
  lift $ writeSTRef eDidUnroll True

allocIdx :: App s Int
allocIdx = do
  Env {..} <- ask
  lift $ incSTCounter eCounter

collectLifts :: App s a -> App s (Lifts, a)
collectLifts m = do
  Env {..} <- ask
  newLifts <- lift $ newSTRef mempty
  res <- local (\e -> e {emLifts = Just newLifts}) m
  lifts <- lift $ readSTRef newLifts
  return $ (lifts, res)

liftLet :: HasCallStack => SrcLoc -> DLVar -> DLExpr -> App s ()
liftLet at v e = do
  Env {..} <- ask
  case emLifts of
    Just r ->
      lift $ modifySTRef r (flip (Seq.|>) (at, v, e))
    Nothing ->
      impossible "no lifts"

addLifts :: (LLCommon a -> a) -> LLCommon a -> Lifts -> a
addLifts mkk k = \case
  Seq.Empty -> mkk k
  (at, v, e) Seq.:<| more ->
    mkk $ LL_Let at (Just v) e $ addLifts mkk k more

freshRenaming :: App s a -> App s a
freshRenaming m = do
  Env {..} <- ask
  oldRenaming <- lift $ readSTRef eRenaming
  newRenaming <- lift $ newSTRef oldRenaming
  local (\e -> e {eRenaming = newRenaming}) m

ul_v_rn :: DLVar -> App s DLVar
ul_v_rn (DLVar at lab t idx) = do
  idx' <- allocIdx
  Env {..} <- ask
  lift $ modifySTRef eRenaming (M.insert idx idx')
  return (DLVar at lab t idx')

ul_vs_rn :: [DLVar] -> App s [DLVar]
ul_vs_rn = mapM ul_v_rn

ul_v :: DLVar -> App s DLVar
ul_v (DLVar at lab t idx) = do
  Env {..} <- ask
  r <- lift $ readSTRef eRenaming
  case M.lookup idx r of
    Just idx' ->
      return (DLVar at lab t idx')
    Nothing ->
      impossible "unbound var"

ul_a :: DLArg -> App s DLArg
ul_a = \case
  DLA_Var v -> (pure $ DLA_Var) <*> ul_v v
  DLA_Con c -> (pure $ DLA_Con c)
  DLA_Array t as -> (pure $ DLA_Array t) <*> ul_as as
  DLA_Tuple as -> (pure $ DLA_Tuple) <*> ul_as as
  DLA_Obj m -> (pure $ DLA_Obj) <*> mapM ul_a m
  DLA_Interact p m t -> (pure $ DLA_Interact p m t)

ul_as :: [DLArg] -> App s [DLArg]
ul_as = mapM ul_a

ul_explode :: SrcLoc -> DLArg -> App s (SLType, [DLArg])
ul_explode at a =
  case a of
    DLA_Array t as -> pure (t, as)
    DLA_Var (DLVar _ _ (T_Array t sz) _) -> do_explode t sz
    DLA_Interact _ _ (T_Array t sz) -> do_explode t sz
    _ -> impossible "explode not array"
  where
    do_explode t sz = pure (,) <*> pure t <*> mapM mk1 [0 .. (sz -1)]
      where
        mk1 i = do
          idx <- allocIdx
          let v = DLVar at "ul" t idx
          liftLet at v $ DLE_ArrayRef at [] a sz (DLA_Con (DLC_Int $ fromIntegral i))
          return $ DLA_Var v

ul_e :: DLExpr -> App s DLExpr
ul_e = \case
  DLE_Arg at a -> (pure $ DLE_Arg at) <*> ul_a a
  DLE_Impossible at lab -> pure $ DLE_Impossible at lab
  DLE_PrimOp at p as -> (pure $ DLE_PrimOp at p) <*> ul_as as
  DLE_ArrayRef at fs a sz i -> (pure $ DLE_ArrayRef at fs) <*> ul_a a <*> pure sz <*> ul_a i
  DLE_ArraySet at fs a sz i v -> (pure $ DLE_ArraySet at fs) <*> ul_a a <*> pure sz <*> ul_a i <*> ul_a v
  DLE_ArrayConcat at x0 y0 -> do
    recordUnroll
    x <- ul_a x0
    y <- ul_a y0
    (x_ty, x') <- ul_explode at x
    (_, y') <- ul_explode at y
    return $ DLE_Arg at $ DLA_Array x_ty $ x' ++ y'
  DLE_TupleRef at t i -> (pure $ DLE_TupleRef at) <*> ul_a t <*> pure i
  DLE_ObjectRef at o k -> (pure $ DLE_ObjectRef at) <*> ul_a o <*> pure k
  DLE_Interact at fs p m t as -> (pure $ DLE_Interact at fs p m t) <*> ul_as as
  DLE_Digest at as -> (pure $ DLE_Digest at) <*> ul_as as
  DLE_Claim at fs t a -> (pure $ DLE_Claim at fs t) <*> ul_a a
  DLE_Transfer at fs t a -> (pure $ DLE_Transfer at fs) <*> ul_a t <*> ul_a a
  DLE_Wait at a -> (pure $ DLE_Wait at) <*> ul_a a
  DLE_PartSet at who a -> (pure $ DLE_PartSet at who) <*> ul_a a

ul_asn1 :: Bool -> (DLVar, DLArg) -> App s (DLVar, DLArg)
ul_asn1 def (v, a) = (pure (,)) <*> ul_v_def v <*> ul_a a
  where
    ul_v_def = if def then ul_v_rn else ul_v

ul_asn :: Bool -> DLAssignment -> App s DLAssignment
ul_asn def (DLAssignment m) = (pure $ DLAssignment) <*> ((pure M.fromList) <*> mapM (ul_asn1 def) (M.toList m))

ul_mdv_rn :: Maybe DLVar -> App s (Maybe DLVar)
ul_mdv_rn = \case
  Nothing -> pure Nothing
  Just v -> pure Just <*> ul_v_rn v

llReplace :: (LLCommon a -> a) -> LLCommon a -> LLCommon LLLocal -> LLCommon a
llReplace mkk nk = \case
  LL_Return {} -> nk
  LL_Let at mdv e k -> LL_Let at mdv e $ iter k
  LL_ArrayMap at ans x a f r k -> LL_ArrayMap at ans x a f r $ iter k
  LL_ArrayReduce at ans x z b a f r k -> LL_ArrayReduce at ans x z b a f r $ iter k
  LL_Var at x k -> LL_Var at x $ iter k
  LL_Set at x v k -> LL_Set at x v $ iter k
  LL_LocalIf at c t f k -> LL_LocalIf at c t f $ iter k
  where
    iter = mkk . llReplace' mkk nk

llReplace' :: (LLCommon a -> a) -> LLCommon a -> LLLocal -> LLCommon a
llReplace' mkk nk (LLL_Com m) = llReplace mkk nk m

llSeqn :: (LLCommon a -> a) -> [LLLocal] -> LLCommon a -> LLCommon a
llSeqn mkk fs k = foldr' (flip $ llReplace' mkk) k fs

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

ul_m :: (LLCommon a -> a) -> (a -> App s a) -> LLCommon a -> App s a
ul_m mkk ul_k = \case
  LL_Return at -> (pure $ mkk $ LL_Return at)
  LL_Let at mdv e k -> do
    (lifts, e') <- collectLifts $ ul_e e
    m' <- (pure $ LL_Let at) <*> ul_mdv_rn mdv <*> (pure e') <*> ul_k k
    return $ addLifts mkk m' lifts
  LL_Var at v k ->
    (pure mkk) <*> ((pure $ LL_Var at) <*> ul_v_rn v <*> ul_k k)
  LL_Set at v a k ->
    (pure mkk) <*> ((pure $ LL_Set at) <*> ul_v v <*> ul_a a <*> ul_k k)
  LL_LocalIf at c t f k ->
    (pure mkk) <*> ((pure $ LL_LocalIf at) <*> ul_a c <*> ul_l t <*> ul_l f <*> ul_k k)
  LL_ArrayMap at ans x0 a f r k -> do
    recordUnroll
    x <- ul_a x0
    (xlifts, (_, x')) <- collectLifts $ ul_explode at x
    let r_ty = argTypeOf r
    let f' a_a = freshRenaming $ do
          a' <- ul_v_rn a
          liftLet at a' $ DLE_Arg at a_a
          (pure (,)) <*> ul_l f <*> ul_a r
    (lifts, (fs, r')) <- collectLifts $ liftM unzip $ mapM f' x'
    ans' <- ul_v_rn ans
    k' <- ul_k k
    let m' = llSeqn mkk fs (LL_Let at (Just ans') (DLE_Arg at $ DLA_Array r_ty r') k')
    return $ addLifts mkk m' (xlifts <> lifts)
  LL_ArrayReduce at ans x0 z b a f r k -> do
    recordUnroll
    x <- ul_a x0
    (xlifts, (_, x')) <- collectLifts $ ul_explode at x
    z' <- ul_a z
    let f' (fs, b_a) a_a = freshRenaming $ do
          b' <- ul_v_rn b
          a' <- ul_v_rn a
          (flifts, f_body') <- collectLifts $ do
            --- FIXME It would be cool to do substitution of these rather than lifting lets
            liftLet at b' $ DLE_Arg at b_a
            liftLet at a' $ DLE_Arg at a_a
            ul_l f
          r' <- ul_a r
          let LLL_Com f_body'm = f_body'
          let f_body'' = addLifts LLL_Com f_body'm flifts
          return $ (snoc fs f_body'', r')
    (fs, r') <- foldlM f' ([], z') x'
    ans' <- ul_v_rn ans
    k' <- ul_k k
    let m' = llSeqn mkk fs (LL_Let at (Just ans') (DLE_Arg at r') k')
    return $ addLifts mkk m' xlifts

ul_l :: LLLocal -> App s LLLocal
ul_l = \case
  LLL_Com m -> ul_m LLL_Com ul_l m

ul_bl :: LLBlock LLLocal -> App s (LLBlock LLLocal)
ul_bl (LLBlock at fs b a) =
  (pure $ LLBlock at fs) <*> ul_l b <*> ul_a a

ul_n :: LLConsensus -> App s LLConsensus
ul_n = \case
  LLC_Com m -> ul_m LLC_Com ul_n m
  LLC_If at c t f ->
    (pure $ LLC_If at) <*> ul_a c <*> ul_n t <*> ul_n f
  LLC_FromConsensus at at' s ->
    (pure $ LLC_FromConsensus at at') <*> ul_s s
  LLC_While at asn inv cond body k ->
    (pure $ LLC_While at) <*> ul_asn True asn <*> ul_bl inv <*> ul_bl cond <*> ul_n body <*> ul_n k
  LLC_Continue at asn ->
    (pure $ LLC_Continue at) <*> ul_asn False asn

ul_fs :: FromSpec -> App s FromSpec
ul_fs = \case
  FS_Join v -> (pure $ FS_Join) <*> ul_v_rn v
  FS_Again v -> (pure $ FS_Again) <*> ul_v v

ul_mtime :: Maybe (DLArg, LLStep) -> App s (Maybe (DLArg, LLStep))
ul_mtime = \case
  Nothing -> (pure $ Nothing)
  Just (a, s) -> (pure $ Just) <*> (pure (,) <*> ul_a a <*> ul_s s)

ul_s :: LLStep -> App s LLStep
ul_s = \case
  LLS_Com m -> ul_m LLS_Com ul_s m
  LLS_Stop at fs ->
    (pure $ LLS_Stop at fs)
  LLS_Only at p l s ->
    (pure $ LLS_Only at p) <*> ul_l l <*> ul_s s
  LLS_ToConsensus at from fs from_as from_msg from_amt mtime cons ->
    (pure $ LLS_ToConsensus at from) <*> ul_fs fs <*> ul_as from_as <*> ul_vs_rn from_msg <*> ul_a from_amt <*> ul_mtime mtime <*> ul_n cons

ul_p :: LLProg -> App s LLProg
ul_p (LLProg at opts ps s) = do
  LLProg at opts ps <$> ul_s s

unrollLoops :: LLProg -> LLProg
unrollLoops lp = runST $ do
  env0 <- mkEnv0
  lp' <- flip runReaderT env0 $ ul_p lp
  let Env {..} = env0
  --- Note: Maybe laziness makes this fast? If not, then it would be
  --- good to do a quick check to see if there is any need before
  --- going through the work of doing it.
  didUnroll <- readSTRef eDidUnroll
  case didUnroll of
    True -> return $ lp'
    False -> return lp
