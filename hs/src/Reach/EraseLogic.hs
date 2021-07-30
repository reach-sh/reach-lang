module Reach.EraseLogic (erase_logic) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.CollectCounts

data Env = Env
  {e_vs :: IORef (S.Set DLVar)}

type App = ReaderT Env IO

isUsed :: DLLetVar -> App Bool
isUsed DLV_Eff = return $ False
isUsed (DLV_Let _ v) = do
  vs <- (liftIO . readIORef) =<< (e_vs <$> ask)
  return $ S.member v vs

isUsedv :: DLVar -> App Bool
isUsedv = isUsed . DLV_Let DVC_Many

class Erase a where
  el :: a -> App a

viaCount :: Countable a => a -> App a
viaCount x = do
  let cs = countsS x
  vsr <- e_vs <$> ask
  liftIO $ modifyIORef vsr $ S.union cs
  return $ x

instance (Traversable t, Erase a) => Erase (t a) where
  el = traverse el

instance {-# OVERLAPS #-} (Erase a, Erase b) => Erase (a, b) where
  el (x, y) = (,) <$> el x <*> el y

instance Erase DLVar where
  el = viaCount

instance Erase DLExpr where
  el = viaCount

instance Erase DLArg where
  el = viaCount

instance Erase DLAssignment where
  el = viaCount

instance {-# OVERLAPS #-} Erase a => Erase (SwitchCases a) where
  el = mapM (\(x, y) -> (,) x <$> el y)

instance Erase DLStmt where
  el = \case
    DL_Nop at -> skip at
    DL_Let at mdv de ->
      case de of
        DLE_Claim _ _ CT_Assert _ _ -> skip at
        DLE_Claim _ _ CT_Possible _ _ -> skip at
        DLE_Claim _ _ (CT_Unknowable {}) _ _ -> skip at
        _ -> do
          let keep = DL_Let at mdv <$> el de
          case isPure de of
            False -> keep
            True ->
              isUsed mdv >>= \case
                False -> skip at
                True -> keep
    DL_ArrayMap at ans x a f ->
      isUsedv ans >>= \case
        False -> skip at
        True -> DL_ArrayMap at ans <$> el x <*> pure a <*> el f
    DL_ArrayReduce at ans x z b a f ->
      isUsedv ans >>= \case
        False -> skip at
        True -> DL_ArrayReduce at ans <$> el x <*> el z <*> pure b <*> pure a <*> el f
    DL_Var at dv ->
      isUsedv dv >>= \case
        False -> skip at
        True -> return $ DL_Var at dv
    DL_Set at dv da ->
      isUsedv dv >>= \case
        False -> skip at
        True -> DL_Set at <$> el dv <*> el da
    DL_LocalIf at c t f -> DL_LocalIf at <$> el c <*> el t <*> el f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at <$> el ov <*> el csm
    DL_Only at who b -> DL_Only at who <$> el b
    DL_MapReduce at mri ans x z b a f ->
      isUsedv ans >>= \case
        False -> skip at
        True -> DL_MapReduce at mri ans x <$> el z <*> pure b <*> pure a <*> el f
    DL_LocalDo at t -> DL_LocalDo at <$> el t
    where
      skip at = return $ DL_Nop at

instance Erase DLTail where
  el = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> do
      k' <- el k
      m' <- el m
      return $ mkCom DT_Com m' k'

instance Erase DLBlock where
  el (DLBlock at fs t r) = do
    r' <- el r
    t' <- el t
    return $ DLBlock at fs t' r'

restrictToUsed :: DLAssignment -> App DLAssignment
restrictToUsed (DLAssignment m) = do
  let go = isUsedv . fst
  m' <- M.fromList <$> (filterM go $ M.toList m)
  return $ DLAssignment m'

instance Erase LLConsensus where
  el = \case
    LLC_Com m k -> do
      k' <- el k
      m' <- el m
      return $ mkCom LLC_Com m' k'
    LLC_If at c t f -> do
      f' <- el f
      t' <- el t
      c' <- el c
      return $ LLC_If at c' t' f'
    LLC_Switch at v csm -> do
      csm' <- el csm
      v' <- el v
      return $ LLC_Switch at v' csm'
    LLC_FromConsensus at1 at2 s ->
      LLC_FromConsensus at1 at2 <$> el s
    LLC_While at asn inv cond body k -> do
      k' <- el k
      cond' <- el cond
      let loop m = do
            before <- m
            body' <- el body
            asn'' <- el before
            after <- m
            case before == after of
              True -> return (body', asn'')
              False -> loop m
      (body', asn'') <- loop (restrictToUsed asn)
      let DLBlock inv_at inv_fs _inv_t _inv_a = inv
      let inv' = DLBlock inv_at inv_fs (DT_Return inv_at) (DLA_Literal $ DLL_Null)
      return $ LLC_While at asn'' inv' cond' body' k'
    LLC_Continue at asn -> do
      asn' <- restrictToUsed asn
      asn'' <- el asn'
      return $ LLC_Continue at asn''
    LLC_ViewIs at vn vk a k -> do
      k' <- el k
      a' <- el a
      return $ LLC_ViewIs at vn vk a' k'

instance Erase LLStep where
  el = \case
    LLS_Com m k -> do
      k' <- el k
      m' <- el m
      return $ mkCom LLS_Com m' k'
    LLS_Stop at -> return $ LLS_Stop at
    LLS_ToConsensus at send recv mtime -> do
      k' <- el $ dr_k recv
      let recv' = recv {dr_k = k'}
      let mel (d, s) = (,) <$> el d <*> el s
      mtime' <- traverse mel mtime
      send' <- traverse viaCount send
      return $ LLS_ToConsensus at send' recv' mtime'

instance {-# OVERLAPS #-} Erase a => Erase (DLinExportBlock a) where
  el (DLinExportBlock at vs b) =
    DLinExportBlock at vs <$> el b

instance Erase LLProg where
  el (LLProg at llo ps dli dex dvs s) =
    LLProg at llo ps dli <$> el dex <*> pure dvs <*> el s

erase_logic :: LLProg -> IO LLProg
erase_logic p = do
  e_vs <- newIORef mempty
  flip runReaderT (Env {..}) $ el p
