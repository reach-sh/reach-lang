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

instance {-# OVERLAPS #-} (Erase a, Erase b) => Erase (Either a b) where
  el = \case
    Left x -> Left <$> el x
    Right x -> Right <$> el x

instance Erase DLVar where
  el = viaCount

instance Erase DLExpr where
  el = viaCount

instance Erase DLArg where
  el = viaCount

instance Erase DLAssignment where
  el = viaCount

instance {-# OVERLAPS #-} Erase a => Erase (SwitchCase a) where
  el (SwitchCase {..}) = do
    k' <- el sc_k
    return $ SwitchCase sc_vl k'

instance {-# OVERLAPS #-} Erase a => Erase (SwitchCases a) where
  el (SwitchCases m) = SwitchCases <$> mapM el m

instance Erase DLStmt where
  el = \case
    DL_Nop at -> skip at
    DL_Let at mdv de ->
      case de of
        DLE_VerifyMuldiv {} -> skip at
        DLE_TimeOrder {} -> skip at
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
    DL_ArrayMap at ans x a i f -> do
      f' <- el f
      isUsed ans >>= \case
        False | isPure f' -> skip at
        _ -> DL_ArrayMap at ans <$> el x <*> pure a <*> pure i <*> pure f'
    DL_ArrayReduce at ans x z b a i f -> do
      f' <- el f
      isUsed ans >>= \case
        False | isPure f' -> skip at
        _ -> DL_ArrayReduce at ans <$> el x <*> el z <*> pure b <*> pure a <*> pure i <*> pure f'
    DL_Var at dv ->
      isUsedv dv >>= \case
        False -> skip at
        True -> return $ DL_Var at dv
    DL_Set at dv da ->
      isUsedv dv >>= \case
        False -> skip at
        True -> DL_Set at <$> el dv <*> el da
    DL_LocalIf at mans c t f -> DL_LocalIf at mans <$> el c <*> el t <*> el f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at <$> el ov <*> el csm
    DL_Only at who b -> DL_Only at who <$> el b
    DL_MapReduce at mri ans x z b k a f -> do
      f' <- el f
      isUsed ans >>= \case
        False | isPure f' -> skip at
        _ -> DL_MapReduce at mri ans x <$> el z <*> pure b <*> pure k <*> pure a <*> pure f'
    DL_LocalDo at mans t -> DL_LocalDo at mans <$> el t
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

instance {-# OVERLAPS #-} Erase (DLInvariant DLBlock) where
  el (DLInvariant (DLBlock inv_at inv_fs _inv_t _inv_a) inv_lab) =
    return $ DLInvariant (DLBlock inv_at inv_fs (DT_Return inv_at) (DLA_Literal $ DLL_Null)) inv_lab

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
    LLC_FromConsensus at1 at2 fs s ->
      LLC_FromConsensus at1 at2 fs <$> el s
    LLC_While at asn invs cond body k -> do
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
      invs' <- mapM el invs
      return $ LLC_While at asn'' invs' cond' body' k'
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
    LLS_ToConsensus at lct send recv mtime -> do
      k' <- el $ dr_k recv
      let recv' = recv {dr_k = k'}
      let mel (d, s) = (,) <$> el d <*> el s
      mtime' <- traverse mel mtime
      send' <- traverse viaCount send
      lct' <- el lct
      return $ LLS_ToConsensus at lct' send' recv' mtime'

instance {-# OVERLAPS #-} Erase a => Erase (DLinExportBlock a) where
  el (DLinExportBlock at vs b) =
    DLinExportBlock at vs <$> el b

instance Erase LLProg where
  el (LLProg llp_at llp_opts llp_parts llp_init llp_exports llp_views llv_apis llp_apis llp_events llp_step) =
    LLProg llp_at llp_opts llp_parts llp_init <$> el llp_exports <*> pure llp_views
           <*> pure llv_apis <*> pure llp_apis <*> pure llp_events <*> el llp_step

erase_logic :: LLProg -> IO LLProg
erase_logic p = do
  e_vs <- newIORef mempty
  flip runReaderT (Env {..}) $ el p
