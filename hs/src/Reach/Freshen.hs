module Reach.Freshen
  ( freshen
  , freshen_
  )
where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter

type App = ReaderT Env IO

type AppT a = a -> App a

data Env = Env
  { fCounter :: Counter
  , fRho :: IORef (M.Map DLVar DLVar)
  }

class Freshen a where
  fu :: AppT a

fu_v :: DLVar -> App DLVar
fu_v v@(DLVar at lab t _) = do
  Env {..} <- ask
  idx <- liftIO $ incCounter fCounter
  let v' = DLVar at lab t idx
  liftIO $ modifyIORef fRho (M.insert v v')
  return $ v'

fu_mv :: LLVar -> App LLVar
fu_mv = mapM fu_v

instance {-# OVERLAPPABLE #-} (Traversable f, Freshen a) => Freshen (f a) where
  fu = traverse fu

instance Freshen DLVar where
  fu v = do
    Env {..} <- ask
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
    DLLA_Struct kvs -> DLLA_Struct <$> mapM go kvs
      where go (k, v) = (,) k <$> fu v

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
    DLE_Transfer at x y z -> DLE_Transfer at <$> fu x <*> fu y <*> fu z
    DLE_CheckPay at x y z -> DLE_CheckPay at x <$> fu y <*> fu z
    DLE_Wait at x -> DLE_Wait at <$> fu x
    DLE_PartSet at x y -> DLE_PartSet at x <$> fu y
    DLE_MapRef at mv fa -> DLE_MapRef at mv <$> fu fa
    DLE_MapSet at mv fa na -> DLE_MapSet at mv <$> fu fa <*> fu na
    DLE_MapDel at mv fa -> DLE_MapDel at mv <$> fu fa
    DLE_Remote at fs av m amta as -> DLE_Remote at fs <$> fu av <*> pure m <*> fu amta <*> fu as

instance {-# OVERLAPS #-} Freshen LLCommon where
  fu = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at v e -> do
      f' <- fu_mv v
      DL_Let at f' <$> fu e
    DL_Var at v -> DL_Var at <$> fu_v v
    DL_Set at v a -> DL_Set at <$> fu v <*> fu a
    DL_LocalIf at c t f -> DL_LocalIf at <$> fu c <*> fu t <*> fu f
    DL_LocalSwitch at ov csm ->
      DL_LocalSwitch at <$> fu ov <*> mapM go csm
      where
        go (vn, k) = (,) <$> fu_mv vn <*> fu k
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
    DL_MapReduce at mri ans x z b a fb -> do
      ans' <- fu_v ans
      z' <- fu z
      b' <- fu_v b
      a' <- fu_v a
      fb' <- fu fb
      return $ DL_MapReduce at mri ans' x z' b' a' fb'

instance {-# OVERLAPS #-} Freshen LLTail where
  fu = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> DT_Com <$> fu m <*> fu k

instance {-# OVERLAPS #-} Freshen LLBlock where
  fu (DLinBlock at fs t a) =
    DLinBlock at fs <$> fu t <*> fu a

freshen_ :: Freshen a => Counter -> a -> [DLVar] -> IO (a, [DLVar])
freshen_ fCounter x vs = do
  fRho <- newIORef mempty
  flip runReaderT (Env {..}) $ do
    vs' <- mapM fu_v vs
    x' <- fu x
    return $ (x', vs')

freshen :: Freshen a => Counter -> a -> IO a
freshen fCounter x = fst <$> freshen_ fCounter x []
