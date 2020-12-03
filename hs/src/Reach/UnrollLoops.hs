module Reach.UnrollLoops (unrollLoops) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Reach.AST
import Reach.Pretty ()
import Reach.STCounter
import Reach.Texty
import Reach.Type
import Reach.Util

type App s = ReaderT (Env s) (ST s)

type Lifts = Seq.Seq (SrcLoc, DLVar, DLExpr)

type Renaming = Either DLVar DLArg

data Env s = Env
  { eDidUnroll :: STRef s Bool
  , eCounter :: STCounter s
  , eRenaming :: STRef s (M.Map Int Renaming)
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

liftExpr :: HasCallStack => SrcLoc -> SLType -> DLExpr -> App s DLArg
liftExpr at t e = do
  idx <- allocIdx
  let v = DLVar at "ul" t idx
  liftLet at v e
  return $ DLA_Var v

liftArray :: HasCallStack => SrcLoc -> SLType -> [DLArg] -> App s DLExpr
liftArray at ty as = do
  let a_ty = T_Array ty $ fromIntegral $ length as
  na <- liftExpr at a_ty $ DLE_LArg at $ DLLA_Array ty as
  return $ DLE_Arg at na

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
  let v' = DLVar at lab t idx'
  lift $ modifySTRef eRenaming (M.insert idx (Left v'))
  return v'

ul_mv_rn :: Maybe DLVar -> App s (Maybe DLVar)
ul_mv_rn Nothing = return Nothing
ul_mv_rn (Just v) = Just <$> ul_v_rn v

ul_vs_rn :: [DLVar] -> App s [DLVar]
ul_vs_rn = mapM ul_v_rn

lookupRenaming :: DLVar -> App s Renaming
lookupRenaming dv@(DLVar _ _ _ idx) = do
  Env {..} <- ask
  r <- lift $ readSTRef eRenaming
  case M.lookup idx r of
    Just x -> return x
    Nothing -> impossible $ "unbound var: " <> (show $ pretty dv)

ul_v :: DLVar -> App s DLVar
ul_v v = do
  r <- lookupRenaming v
  case r of
    Left v' -> return v'
    Right _ ->
      impossible "var is renamed to arg"

ul_v_rna :: DLVar -> DLArg -> App s ()
ul_v_rna (DLVar _ _ _ idx) a = do
  Env {..} <- ask
  lift $ modifySTRef eRenaming (M.insert idx (Right a))

ul_va :: DLVar -> App s DLArg
ul_va v = do
  r <- lookupRenaming v
  case r of
    Left v' -> return $ DLA_Var v'
    Right a -> return a

ul_a :: DLArg -> App s DLArg
ul_a = \case
  DLA_Var v -> ul_va v
  DLA_Constant c -> pure $ DLA_Constant c
  DLA_Literal c -> (pure $ DLA_Literal c)
  DLA_Interact p m t -> (pure $ DLA_Interact p m t)

ul_as :: [DLArg] -> App s [DLArg]
ul_as = mapM ul_a

ul_explode :: SrcLoc -> DLArg -> App s (SLType, [DLArg])
ul_explode at a =
  case a of
    DLA_Var (DLVar _ _ (T_Array t sz) _) -> do_explode t sz
    DLA_Interact _ _ (T_Array t sz) -> do_explode t sz
    _ -> impossible "explode not array"
  where
    do_explode t sz = pure (,) <*> pure t <*> mapM mk1 [0 .. (sz -1)]
      where
        mk1 i =
          liftExpr at t $
            DLE_ArrayRef at a (DLA_Literal (DLL_Int at $ fromIntegral i))

ul_la :: DLLargeArg -> App s DLLargeArg
ul_la = \case
  DLLA_Array t as -> (pure $ DLLA_Array t) <*> ul_as as
  DLLA_Tuple as -> (pure $ DLLA_Tuple) <*> ul_as as
  DLLA_Obj m -> (pure $ DLLA_Obj) <*> mapM ul_a m
  DLLA_Data t vn vv -> DLLA_Data t vn <$> ul_a vv

ul_e :: DLExpr -> App s DLExpr
ul_e = \case
  DLE_Arg at a -> (pure $ DLE_Arg at) <*> ul_a a
  DLE_LArg at la -> (pure $ DLE_LArg at) <*> ul_la la
  DLE_Impossible at lab -> pure $ DLE_Impossible at lab
  DLE_PrimOp at p as -> (pure $ DLE_PrimOp at p) <*> ul_as as
  DLE_ArrayRef at a i -> (pure $ DLE_ArrayRef at) <*> ul_a a <*> ul_a i
  DLE_ArraySet at a i v -> (pure $ DLE_ArraySet at) <*> ul_a a <*> ul_a i <*> ul_a v
  DLE_ArrayConcat at x0 y0 -> do
    recordUnroll
    x <- ul_a x0
    y <- ul_a y0
    (x_ty, x') <- ul_explode at x
    (_, y') <- ul_explode at y
    liftArray at x_ty $ x' <> y'
  DLE_ArrayZip at x0 y0 -> do
    recordUnroll
    x <- ul_a x0
    y <- ul_a y0
    (x_ty, x') <- ul_explode at x
    (y_ty, y') <- ul_explode at y
    let ty = T_Tuple [x_ty, y_ty]
    let go xa ya = liftExpr at ty $ DLE_LArg at $ DLLA_Tuple [xa, ya]
    as <- zipWithM go x' y'
    liftArray at ty as
  DLE_TupleRef at t i -> (pure $ DLE_TupleRef at) <*> ul_a t <*> pure i
  DLE_ObjectRef at o k -> (pure $ DLE_ObjectRef at) <*> ul_a o <*> pure k
  DLE_Interact at fs p m t as -> (pure $ DLE_Interact at fs p m t) <*> ul_as as
  DLE_Digest at as -> (pure $ DLE_Digest at) <*> ul_as as
  DLE_Claim at fs t a m -> (pure $ DLE_Claim at fs t) <*> ul_a a <*> (pure $ m)
  DLE_Transfer at t a -> (pure $ DLE_Transfer at) <*> ul_a t <*> ul_a a
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
  LL_ArrayMap at ans x a f k -> LL_ArrayMap at ans x a f $ iter k
  LL_ArrayReduce at ans x z b a f k -> LL_ArrayReduce at ans x z b a f $ iter k
  LL_Var at x k -> LL_Var at x $ iter k
  LL_Set at x v k -> LL_Set at x v $ iter k
  LL_LocalIf at c t f k -> LL_LocalIf at c t f $ iter k
  LL_LocalSwitch at ov csm k -> LL_LocalSwitch at ov csm $ iter k
  where
    iter = mkk . llReplace' mkk nk

llReplace' :: (LLCommon a -> a) -> LLCommon a -> LLLocal -> LLCommon a
llReplace' mkk nk (LLL_Com m) = llReplace mkk nk m

llSeqn :: (LLCommon a -> a) -> [LLLocal] -> LLCommon a -> LLCommon a
llSeqn mkk fs k = foldr' (flip $ llReplace' mkk) k fs

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
  LL_LocalSwitch at ov csm k ->
    mkk <$> (LL_LocalSwitch at <$> ul_v ov <*> mapM cm1 csm <*> ul_k k)
    where
      cm1 (mov', l) = (,) <$> (ul_mv_rn mov') <*> ul_l l
  LL_ArrayMap at ans x0 a (LLBlock _ _ f r) k -> do
    recordUnroll
    x <- ul_a x0
    (xlifts, (_, x')) <- collectLifts $ ul_explode at x
    let r_ty = argTypeOf r
    let f' a_a = freshRenaming $ do
          ul_v_rna a a_a
          (pure (,)) <*> ul_l f <*> ul_a r
    (lifts, (fs, r')) <- collectLifts $ liftM unzip $ mapM f' x'
    ans' <- ul_v_rn ans
    k' <- ul_k k
    let m' = llSeqn mkk fs (LL_Let at (Just ans') (DLE_LArg at $ DLLA_Array r_ty r') k')
    return $ addLifts mkk m' (xlifts <> lifts)
  LL_ArrayReduce at ans x0 z b a (LLBlock _ _ f r) k -> do
    recordUnroll
    x <- ul_a x0
    (xlifts, (_, x')) <- collectLifts $ ul_explode at x
    z' <- ul_a z
    let f' (fs, b_a) a_a = freshRenaming $ do
          ul_v_rna b b_a
          ul_v_rna a a_a
          (flifts, f_body') <- collectLifts $ ul_l f
          r' <- ul_a r
          let LLL_Com f_body'm = f_body'
          let f_body'' = addLifts LLL_Com f_body'm flifts
          return $ (fs Seq.|> f_body'', r')
    (fs, r') <- foldlM f' (mempty, z') x'
    ans' <- ul_v_rn ans
    k' <- ul_k k
    let m' = llSeqn mkk (toList fs) (LL_Let at (Just ans') (DLE_Arg at r') k')
    return $ addLifts mkk m' xlifts

ul_l :: LLLocal -> App s LLLocal
ul_l = \case
  LLL_Com m -> ul_m LLL_Com ul_l m

ul_bl :: LLBlock -> App s LLBlock
ul_bl (LLBlock at fs b a) =
  (pure $ LLBlock at fs) <*> ul_l b <*> ul_a a

ul_n :: LLConsensus -> App s LLConsensus
ul_n = \case
  LLC_Com m -> ul_m LLC_Com ul_n m
  LLC_If at c t f ->
    (pure $ LLC_If at) <*> ul_a c <*> ul_n t <*> ul_n f
  LLC_Switch at ov csm ->
    LLC_Switch at <$> ul_v ov <*> mapM cm1 csm
    where
      cm1 (mov', n) = (,) <$> ul_mv_rn mov' <*> ul_n n
  LLC_FromConsensus at at' s ->
    (pure $ LLC_FromConsensus at at') <*> ul_s s
  LLC_While at asn inv cond body k ->
    (pure $ LLC_While at) <*> ul_asn True asn <*> ul_bl inv <*> ul_bl cond <*> ul_n body <*> ul_n k
  LLC_Continue at asn ->
    (pure $ LLC_Continue at) <*> ul_asn False asn

ul_mtime :: Maybe (DLArg, LLStep) -> App s (Maybe (DLArg, LLStep))
ul_mtime = \case
  Nothing -> (pure $ Nothing)
  Just (a, s) -> (pure $ Just) <*> (pure (,) <*> ul_a a <*> ul_s s)

ul_send :: (SLPart, ([DLArg], DLArg)) -> App s (SLPart, ([DLArg], DLArg))
ul_send (p, (args, amta)) =
  (,) p <$> ((,) <$> ul_as args <*> ul_a amta)

ul_s :: LLStep -> App s LLStep
ul_s = \case
  LLS_Com m -> ul_m LLS_Com ul_s m
  LLS_Stop at ->
    (pure $ LLS_Stop at)
  LLS_Only at p l s ->
    (pure $ LLS_Only at p) <*> ul_l l <*> ul_s s
  LLS_ToConsensus at send recv mtime ->
    LLS_ToConsensus at <$> send' <*> recv' <*> mtime'
    where
      send' = M.fromList <$> mapM ul_send (M.toList send)
      (winner_dv, msg, amtv, cons) = recv
      cons' = ul_n cons
      recv' = (\a b c d -> (a, b, c, d)) <$> ul_v_rn winner_dv <*> ul_vs_rn msg <*> ul_v_rn amtv <*> cons'
      mtime' = ul_mtime mtime

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
