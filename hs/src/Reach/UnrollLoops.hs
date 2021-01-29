module Reach.UnrollLoops (unrollLoops) where

import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Texty
import Reach.Util

-- XXX Revise this to not always rename everything and use the standard counter

type App = ReaderT Env IO
type AppT a = a -> App a
type Lifts = Seq.Seq LLCommon
type Renaming = Either DLVar DLArg

data Env = Env
  { eDidUnroll :: IORef Bool
  , eCounter :: Counter
  , eRenaming :: IORef (M.Map Int Renaming)
  , emLifts :: Maybe (IORef Lifts)
  }

mkEnv0 :: IO (Env)
mkEnv0 = do
  eDidUnroll <- newIORef False
  eCounter <- newCounter 0
  eRenaming <- newIORef mempty
  let emLifts = Nothing
  return $ Env {..}

recordUnroll :: App ()
recordUnroll = do
  Env {..} <- ask
  lift $ writeIORef eDidUnroll True

allocIdx :: App Int
allocIdx = do
  Env {..} <- ask
  lift $ incCounter eCounter

addLifts :: (LLCommon -> a -> a) -> Lifts -> a -> a
addLifts mkk ls k = foldr mkk k ls

collectLifts :: App a -> App (Lifts, a)
collectLifts m = do
  Env {..} <- ask
  newLifts <- liftIO $ newIORef mempty
  res <- local (\e -> e {emLifts = Just newLifts}) m
  lifts <- liftIO $ readIORef newLifts
  return $ (lifts, res)

liftCommon :: HasCallStack => LLCommon -> App ()
liftCommon m = do
  Env {..} <- ask
  case emLifts of
    Just r ->
      liftIO $ modifyIORef r (flip (Seq.|>) m)
    Nothing ->
      impossible "no lifts"

liftLocal :: HasCallStack => LLTail -> App ()
liftLocal = \case
  DT_Return _ -> return ()
  DT_Com m k -> liftCommon m >> liftLocal k

liftLet :: HasCallStack => SrcLoc -> DLVar -> DLExpr -> App ()
liftLet at v e = liftCommon (DL_Let at (Just v) e)

liftExpr :: HasCallStack => SrcLoc -> DLType -> DLExpr -> App DLArg
liftExpr at t e = do
  idx <- allocIdx
  let v = DLVar at "ul" t idx
  liftLet at v e
  return $ DLA_Var v

liftArray :: HasCallStack => SrcLoc -> DLType -> [DLArg] -> App DLExpr
liftArray at ty as = do
  let a_ty = T_Array ty $ fromIntegral $ length as
  na <- liftExpr at a_ty $ DLE_LArg at $ DLLA_Array ty as
  return $ DLE_Arg at na

freshRenaming :: App a -> App a
freshRenaming m = do
  Env {..} <- ask
  oldRenaming <- liftIO $ readIORef eRenaming
  newRenaming <- liftIO $ newIORef oldRenaming
  local (\e -> e {eRenaming = newRenaming}) m

ul_v_rn :: AppT DLVar
ul_v_rn (DLVar at lab t idx) = do
  idx' <- allocIdx
  Env {..} <- ask
  let v' = DLVar at lab t idx'
  lift $ modifyIORef eRenaming (M.insert idx (Left v'))
  return v'

ul_mv_rn :: AppT (Maybe DLVar)
ul_mv_rn Nothing = return Nothing
ul_mv_rn (Just v) = Just <$> ul_v_rn v

ul_vs_rn :: AppT [DLVar]
ul_vs_rn = mapM ul_v_rn

lookupRenaming :: DLVar -> App Renaming
lookupRenaming dv@(DLVar _ _ _ idx) = do
  Env {..} <- ask
  r <- liftIO $ readIORef eRenaming
  case M.lookup idx r of
    Just x -> return x
    Nothing -> impossible $ "unbound var: " <> (show $ pretty dv)

ul_v :: AppT DLVar
ul_v v = do
  r <- lookupRenaming v
  case r of
    Left v' -> return v'
    Right _ ->
      impossible "var is renamed to arg"

ul_mv :: AppT (Maybe DLVar)
ul_mv = \case
  Nothing -> return Nothing
  Just v -> Just <$> ul_v v

ul_v_rna :: DLVar -> DLArg -> App ()
ul_v_rna (DLVar _ _ _ idx) a = do
  Env {..} <- ask
  liftIO $ modifyIORef eRenaming (M.insert idx (Right a))

ul_va :: DLVar -> App DLArg
ul_va v = do
  r <- lookupRenaming v
  case r of
    Left v' -> return $ DLA_Var v'
    Right a -> return a

ul_a :: AppT DLArg
ul_a = \case
  DLA_Var v -> ul_va v
  DLA_Constant c -> pure $ DLA_Constant c
  DLA_Literal c -> (pure $ DLA_Literal c)
  DLA_Interact p m t -> (pure $ DLA_Interact p m t)

ul_as :: AppT [DLArg]
ul_as = mapM ul_a

ul_explode :: SrcLoc -> DLArg -> App (DLType, [DLArg])
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

ul_la :: AppT DLLargeArg
ul_la = \case
  DLLA_Array t as -> (pure $ DLLA_Array t) <*> ul_as as
  DLLA_Tuple as -> (pure $ DLLA_Tuple) <*> ul_as as
  DLLA_Obj m -> (pure $ DLLA_Obj) <*> mapM ul_a m
  DLLA_Data t vn vv -> DLLA_Data t vn <$> ul_a vv

ul_e :: AppT DLExpr
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

ul_asn1 :: Bool -> (DLVar, DLArg) -> App (DLVar, DLArg)
ul_asn1 def (v, a) = (pure (,)) <*> ul_v_def v <*> ul_a a
  where
    ul_v_def = if def then ul_v_rn else ul_v

ul_asn :: Bool -> DLAssignment -> App DLAssignment
ul_asn def (DLAssignment m) = (pure $ DLAssignment) <*> ((pure M.fromList) <*> mapM (ul_asn1 def) (M.toList m))

ul_mdv_rn :: AppT (Maybe DLVar)
ul_mdv_rn = \case
  Nothing -> pure Nothing
  Just v -> pure Just <*> ul_v_rn v

ul_m_ :: (LLCommon -> a -> a) -> (a -> App a) -> LLCommon -> a -> App a
ul_m_ mkk ul_k m k = do
  (lifts, m') <- collectLifts $ ul_m m
  let lifts' = lifts <> (return $ m')
  k' <- ul_k k
  return $ addLifts mkk lifts' k'

ul_m :: AppT LLCommon
ul_m = \case
  DL_Nop at ->
    return $ DL_Nop at
  DL_Let at mdv e -> do
    DL_Let at <$> ul_mdv_rn mdv <*> ul_e e
  DL_Var at v ->
    DL_Var at <$> ul_v_rn v
  DL_Set at v a ->
    DL_Set at <$> ul_v v <*> ul_a a
  DL_LocalIf at c t f ->
    DL_LocalIf at <$> ul_a c <*> ul_l t <*> ul_l f
  DL_LocalSwitch at ov csm ->
    DL_LocalSwitch at <$> ul_v ov <*> mapM cm1 csm
    where
      cm1 (mov', l) = (,) <$> (ul_mv_rn mov') <*> ul_l l
  DL_ArrayMap at ans x0 a (DLinBlock _ _ f r) -> do
    recordUnroll
    x <- ul_a x0
    (_, x') <- ul_explode at x
    let r_ty = argTypeOf r
    let f' a_a = freshRenaming $ do
          ul_v_rna a a_a
          liftLocal =<< ul_l f
          ul_a r
    r' <- mapM f' x'
    ans' <- ul_v_rn ans
    return $ DL_Let at (Just ans') (DLE_LArg at $ DLLA_Array r_ty r')
  DL_ArrayReduce at ans x0 z b a (DLinBlock _ _ f r) -> do
    recordUnroll
    x <- ul_a x0
    (_, x') <- ul_explode at x
    z' <- ul_a z
    let f' b_a a_a = freshRenaming $ do
          ul_v_rna b b_a
          ul_v_rna a a_a
          liftLocal =<< ul_l f
          ul_a r
    r' <- foldlM f' z' x'
    ans' <- ul_v_rn ans
    return $ DL_Let at (Just ans') (DLE_Arg at r')

ul_l :: AppT LLTail
ul_l = \case
  DT_Return at -> return $ DT_Return at
  DT_Com m k -> ul_m_ DT_Com ul_l m k

ul_bl :: AppT LLBlock
ul_bl (DLinBlock at fs b a) =
  (pure $ DLinBlock at fs) <*> ul_l b <*> ul_a a

ul_n :: AppT LLConsensus
ul_n = \case
  LLC_Com m k -> ul_m_ LLC_Com ul_n m k
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
  LLC_Only at p l k ->
    (pure $ LLC_Only at p) <*> ul_l l <*> ul_n k

ul_mtime :: AppT (Maybe (DLArg, LLStep))
ul_mtime = \case
  Nothing -> (pure $ Nothing)
  Just (b, s) -> (pure $ Just) <*> (pure (,) <*> ul_a b <*> ul_s s)

ul_send :: AppT (SLPart, (Bool, [DLArg], DLArg, DLArg))
ul_send (p, (isClass, args, amta, whena)) =
  (,) p <$> ((\x y z -> (isClass, x, y, z)) <$> ul_as args <*> ul_a amta <*> ul_a whena)

ul_s :: AppT LLStep
ul_s = \case
  LLS_Com m k -> ul_m_ LLS_Com ul_s m k
  LLS_Stop at ->
    (pure $ LLS_Stop at)
  LLS_Only at p l s ->
    (pure $ LLS_Only at p) <*> ul_l l <*> ul_s s
  LLS_ToConsensus at send recv mtime ->
    LLS_ToConsensus at <$> send' <*> recv' <*> mtime'
    where
      send' = M.fromList <$> mapM ul_send (M.toList send)
      (last_timev, winner_dv, msg, amtv, timev, cons) = recv
      cons' = ul_n cons
      recv' = (\a b c d e f -> (a, b, c, d, e, f)) <$> ul_mv last_timev <*> ul_v_rn winner_dv <*> ul_vs_rn msg <*> ul_v_rn amtv <*> ul_v_rn timev <*> cons'
      mtime' = ul_mtime mtime

ul_dli :: AppT DLInit
ul_dli (DLInit ctimem) = do
  DLInit <$> ul_mdv_rn ctimem

ul_p :: AppT LLProg
ul_p (LLProg at opts ps dli s) = do
  LLProg at opts ps <$> ul_dli dli <*> ul_s s

unrollLoops :: LLProg -> IO LLProg
unrollLoops lp = do
  env0 <- mkEnv0
  lp' <- flip runReaderT env0 $ ul_p lp
  let Env {..} = env0
  --- Note: Maybe laziness makes this fast? If not, then it would be
  --- good to do a quick check to see if there is any need before
  --- going through the work of doing it.
  didUnroll <- readIORef eDidUnroll
  case didUnroll of
    True -> return $ lp'
    False -> return lp
