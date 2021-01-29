module Reach.UnrollLoops (unrollLoops) where

import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Util

type FApp = ReaderT FEnv IO
type FAppT a = a -> FApp a

data FEnv = FEnv
  { fCounter :: Counter
  , fRho :: IORef (M.Map DLVar DLVar)
  }

class Freshen a where
  fu :: FAppT a

fu_v :: DLVar -> FApp DLVar
fu_v v@(DLVar at lab t _) = do
  FEnv {..} <- ask
  idx <- liftIO $ incCounter fCounter
  let v' = DLVar at lab t idx
  liftIO $ modifyIORef fRho (M.insert v v')
  return $ v'

fu_mv :: LLVar -> FApp LLVar
fu_mv = mapM fu_v

instance (Traversable f, Freshen a) => Freshen (f a) where
  fu = traverse fu

instance Freshen DLVar where
  fu v = do
    FEnv {..} <- ask
    rho <- liftIO $ readIORef fRho
    return $ fromMaybe v $ M.lookup v rho

instance Freshen DLArg where
  fu = \case
    DLA_Var v -> DLA_Var <$> fu v
    x -> return x

instance Freshen DLLargeArg where
  fu = \case
    DLLA_Array t as -> DLLA_Array t <$> fu as
    DLLA_Tuple as -> DLLA_Tuple <$> fu as
    DLLA_Obj m -> DLLA_Obj <$> fu m
    DLLA_Data t v a -> DLLA_Data t v <$> fu a

instance Freshen DLExpr where
  fu = \case
    DLE_Arg at a -> DLE_Arg at <$> fu a
    DLE_LArg at a -> DLE_LArg at <$> fu a
    e@(DLE_Impossible {}) -> return $ e
    DLE_PrimOp at p as -> DLE_PrimOp at p <$> fu as
    DLE_ArrayRef at a b -> DLE_ArrayRef at <$> fu a <*> fu b
    DLE_ArraySet at a b c -> DLE_ArraySet at <$> fu a <*> fu b <*> fu c
    DLE_ArrayConcat at a b -> DLE_ArrayConcat at <$> fu a <*> fu b
    DLE_ArrayZip at a b -> DLE_ArrayZip at <$> fu a <*> fu b
    DLE_TupleRef at x y -> DLE_TupleRef at <$> fu x <*> pure y
    DLE_ObjectRef at x y -> DLE_ObjectRef at <$> fu x <*> pure y
    DLE_Interact a b c d e f -> DLE_Interact a b c d e <$> fu f
    DLE_Digest at as -> DLE_Digest at <$> fu as
    DLE_Claim a b c d e -> DLE_Claim a b c <$> fu d <*> pure e
    DLE_Transfer at x y -> DLE_Transfer at <$> fu x <*> fu y
    DLE_Wait at x -> DLE_Wait at <$> fu x
    DLE_PartSet at x y -> DLE_PartSet at x <$> fu y

instance {-# OVERLAPPING #-} Freshen LLCommon where
  fu = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at v e -> do
      f' <- fu_mv v
      DL_Let at f' <$> fu e
    DL_Var at v -> DL_Var at <$> fu_v v
    DL_Set at v a -> DL_Set at <$> fu v <*> fu a
    DL_LocalIf at c t f -> DL_LocalIf at <$> fu c <*> fu t <*> fu f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at <$> fu ov <*> fu csm
    DL_ArrayMap at ans x a fb -> do
      x' <- fu x
      a' <- fu_v a
      fb' <- fu fb
      ans' <- fu_v ans
      return $ DL_ArrayMap at ans' x' a' fb'
    DL_ArrayReduce at ans x z b a fb -> do
      ans' <- fu_v ans
      x' <- fu x
      z' <- fu z
      b' <- fu_v b
      a' <- fu_v a
      fb' <- fu fb
      return $ DL_ArrayReduce at ans' x' z' b' a' fb'

instance {-# OVERLAPPING #-} Freshen LLTail where
  fu = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> DT_Com <$> fu m <*> fu k

instance {-# OVERLAPPING #-} Freshen LLBlock where
  fu (DLinBlock at fs t a) =
    DLinBlock at fs <$> fu t <*> fu a

--

type App = ReaderT Env IO
type AppT a = a -> App a
type Lifts = Seq.Seq LLCommon

data Env = Env
  { eCounter :: Counter
  , emLifts :: Maybe (IORef Lifts)
  }

class Unroll a where
  ul :: AppT a

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

liftExpr :: HasCallStack => SrcLoc -> DLType -> DLExpr -> App DLArg
liftExpr at t e = do
  idx <- allocIdx
  let v = DLVar at "ul" t idx
  liftCommon (DL_Let at (Just v) e)
  return $ DLA_Var v

liftArray :: HasCallStack => SrcLoc -> DLType -> [DLArg] -> App DLExpr
liftArray at ty as = do
  let a_ty = T_Array ty $ fromIntegral $ length as
  na <- liftExpr at a_ty $ DLE_LArg at $ DLLA_Array ty as
  return $ DLE_Arg at na

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

fu_ :: LLBlock -> [(DLVar, DLArg)] -> App DLArg
fu_ b nvs = do
  let DLinBlock at fs t a = b
  let lets = map (\(nv, na) -> DL_Let at (Just nv) (DLE_Arg at na)) nvs
  let t' = foldr DT_Com t lets
  let b' = DLinBlock at fs t' a
  Env {..} <- ask
  let fCounter = eCounter
  fRho <- liftIO $ newIORef mempty
  DLinBlock _ _ t'' a' <-
    liftIO $ flip runReaderT (FEnv {..}) $ fu b'
  liftLocal =<< ul t''
  return $ a'

instance Unroll DLExpr where
  ul = \case
    DLE_ArrayConcat at x y -> do
      (x_ty, x') <- ul_explode at x
      (_, y') <- ul_explode at y
      liftArray at x_ty $ x' <> y'
    DLE_ArrayZip at x y -> do
      (x_ty, x') <- ul_explode at x
      (y_ty, y') <- ul_explode at y
      let ty = T_Tuple [x_ty, y_ty]
      let go xa ya = liftExpr at ty $ DLE_LArg at $ DLLA_Tuple [xa, ya]
      as <- zipWithM go x' y'
      liftArray at ty as
    e -> return $ e

instance Unroll LLCommon where
  ul = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at mdv e -> DL_Let at mdv <$> ul e
    DL_Var at v -> return $ DL_Var at v
    DL_Set at v a -> return $ DL_Set at v a
    DL_LocalIf at c t f -> DL_LocalIf at c <$> ul t <*> ul f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at ov <$> ul csm
    DL_ArrayMap at ans x a fb -> do
      (_, x') <- ul_explode at x
      r' <- mapM (\xa -> fu_ fb [(a, xa)]) x'
      let r_ty = arrType $ varType ans
      return $ DL_Let at (Just ans) (DLE_LArg at $ DLLA_Array r_ty r')
    DL_ArrayReduce at ans x z b a fb -> do
      (_, x') <- ul_explode at x
      r' <- foldlM (\za xa -> fu_ fb [(b, za), (a, xa)]) z x'
      return $ DL_Let at (Just ans) (DLE_Arg at r')

ul_m :: Unroll a => (LLCommon -> a -> a) -> LLCommon -> AppT a
ul_m mkk m k = do
  (lifts, m') <- collectLifts $ ul m
  let lifts' = lifts <> (return $ m')
  k' <- ul k
  return $ addLifts mkk lifts' k'

instance Unroll LLTail where
  ul = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> ul_m DT_Com m k

instance Unroll LLBlock where
  ul (DLinBlock at fs b a) =
    DLinBlock at fs <$> ul b <*> pure a

instance Unroll LLConsensus where
  ul = \case
    LLC_Com m k -> ul_m LLC_Com m k
    LLC_If at c t f -> LLC_If at c <$> ul t <*> ul f
    LLC_Switch at ov csm -> LLC_Switch at ov <$> ul csm
    LLC_FromConsensus at at' s -> LLC_FromConsensus at at' <$> ul s
    LLC_While at asn inv cond body k ->
      LLC_While at asn <$> ul inv <*> ul cond <*> ul body <*> ul k
    LLC_Continue at asn -> return $ LLC_Continue at asn
    LLC_Only at p l k -> LLC_Only at p <$> ul l <*> ul k

instance Unroll k => Unroll (a, k) where
  ul (a, k) = (,) a <$> ul k

instance Unroll a => Unroll (M.Map k a) where
  ul = mapM ul

instance Unroll a => Unroll (Maybe a) where
  ul = mapM ul

instance Unroll k => Unroll (a, b, c, d, e, k) where
  ul (a, b, c, d, e, k) = (\k' -> (a, b, c, d, e, k')) <$> ul k

instance Unroll LLStep where
  ul = \case
    LLS_Com m k -> ul_m LLS_Com m k
    LLS_Stop at -> pure $ LLS_Stop at
    LLS_Only at p l s -> LLS_Only at p <$> ul l <*> ul s
    LLS_ToConsensus at send recv mtime ->
      LLS_ToConsensus at send <$> ul recv <*> ul mtime

instance Unroll LLProg where
  ul (LLProg at opts ps dli s) =
    LLProg at opts ps dli <$> ul s

unrollLoops :: LLProg -> IO LLProg
unrollLoops lp@(LLProg _ llo _ _ _) = do
  let LLOpts {..} = llo
  let eCounter = llo_counter
  let emLifts = Nothing
  flip runReaderT (Env {..}) $ ul lp
