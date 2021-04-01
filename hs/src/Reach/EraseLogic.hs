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

isUsed :: Maybe DLVar -> App Bool
isUsed Nothing = return $ False
isUsed (Just v) = do
  vs <- (liftIO . readIORef) =<< (e_vs <$> ask)
  return $ S.member v vs

class Erase a where
  el :: a -> App a

viaCount :: Countable a => a -> App a
viaCount x = do
  let cs = countsS x
  vsr <- e_vs <$> ask
  liftIO $ modifyIORef vsr $ S.union cs
  return $ x

instance Erase DLVar where
  el = viaCount

instance Erase DLExpr where
  el = viaCount

instance Erase DLArg where
  el = viaCount

instance Erase DLAssignment where
  el = viaCount

instance Erase a => Erase (SwitchCases a) where
  el = mapM (\(x, y) -> (,) x <$> el y)

instance Erase LLCommon where
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
      isUsed (Just ans) >>= \case
        False -> skip at
        True -> DL_ArrayMap at ans <$> el x <*> pure a <*> el f
    DL_ArrayReduce at ans x z b a f ->
      isUsed (Just ans) >>= \case
        False -> skip at
        True -> DL_ArrayReduce at ans <$> el x <*> el z <*> pure b <*> pure a <*> el f
    DL_Var at dv ->
      isUsed (Just dv) >>= \case
        False -> skip at
        True -> return $ DL_Var at dv
    DL_Set at dv da -> DL_Set at <$> el dv <*> el da
    DL_LocalIf at c t f -> DL_LocalIf at <$> el c <*> el t <*> el f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at <$> el ov <*> el csm
    DL_MapReduce at mri ans x z b a f ->
      isUsed (Just ans) >>= \case
        False -> skip at
        True -> DL_MapReduce at mri ans x <$> el z <*> pure b <*> pure a <*> el f
    where
      skip at = return $ DL_Nop at

instance Erase LLTail where
  el = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> do
      k' <- el k
      m' <- el m
      return $ mkCom DT_Com m' k'

instance Erase LLBlock where
  el (DLinBlock at fs t r) = do
    r' <- el r
    t' <- el t
    return $ DLinBlock at fs t' r'

restrictToUsed :: DLAssignment -> App DLAssignment
restrictToUsed (DLAssignment m) = do
  let go = isUsed . Just . fst
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
      let DLinBlock inv_at inv_fs _inv_t _inv_a = inv
      let inv' = DLinBlock inv_at inv_fs (DT_Return inv_at) (DLA_Literal $ DLL_Null)
      return $ LLC_While at asn'' inv' cond' body' k'
    LLC_Continue at asn -> do
      asn' <- restrictToUsed asn
      asn'' <- el asn'
      return $ LLC_Continue at asn''
    LLC_Only at p l k -> do
      k' <- el k
      l' <- el l
      return $ LLC_Only at p l' k'

instance Erase LLStep where
  el = \case
    LLS_Com m k -> do
      k' <- el k
      m' <- el m
      return $ mkCom LLS_Com m' k'
    LLS_Stop at -> return $ LLS_Stop at
    LLS_Only at p l k -> do
      k' <- el k
      l' <- el l
      return $ LLS_Only at p l' k'
    LLS_ToConsensus at send recv mtime -> do
      let (lt_mv, from_v, msg_vs, amt_v, time_v, c) = recv
      c' <- el c
      lt_mv' <- viaCount lt_mv
      let recv' = (lt_mv', from_v, msg_vs, amt_v, time_v, c')
      let mel (d, s) = (,) <$> el d <*> el s
      mtime' <- traverse mel mtime
      send' <- traverse viaCount send
      return $ LLS_ToConsensus at send' recv' mtime'

instance Erase (DLinExportVal LLBlock) where
  el = \case
    DLEV_Fun at args body ->
      DLEV_Fun at <$> mapM el args <*> el body
    DLEV_Arg at a  -> DLEV_Arg at <$> el a

instance Erase (DLExportinBlock LLVar) where
  el = \case
    DLExportinBlock s r -> do
      r' <- el r
      s' <- el s
      return $ DLExportinBlock s' r'

instance Erase LLExports where
  el dex = mapM el dex

instance Erase LLProg where
  el (LLProg at llo ps dli dex s) =
    LLProg at llo ps dli <$> el dex <*> el s

erase_logic :: LLProg -> IO LLProg
erase_logic p = do
  e_vs <- newIORef mempty
  flip runReaderT (Env {..}) $ el p
