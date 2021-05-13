module Reach.AddCounts (add_counts) where

import Control.Monad.Reader
import Data.IORef
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.CollectCounts
import Reach.Util

data Env = Env
  {e_cs :: IORef Counts}

type App = ReaderT Env IO
type AppT a = a -> App a

fresh :: App m -> App m
fresh m = do
  e_cs' <- (liftIO . dupeIORef) =<< (e_cs <$> ask)
  local (\e -> e { e_cs = e_cs' }) m

class AC a where
  ac :: AppT a

ac_visit :: Countable a => a -> App ()
ac_visit x = do
  csr <- e_cs <$> ask
  liftIO $ modifyIORef csr $ (<>) (counts x)

ac_vdef :: Bool -> AppT DLLetVar
ac_vdef _ DLV_Eff = return $ DLV_Eff
ac_vdef okToDupe (DLV_Let _ v) = do
  cs <- (liftIO . readIORef) =<< (e_cs <$> ask)
  case get_count v cs of
    Count Nothing -> return $ DLV_Eff
    Count (Just lc) -> do
      let lc' = if okToDupe then lc else DVC_Many
      return $ DLV_Let lc' v

instance (AC a, Traversable t) => AC (t a) where
  ac = mapM ac

instance AC DLArg where
  ac a = do
    ac_visit a
    return $ a

instance AC DLStmt where
  ac = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at x de -> do
      x' <- ac_vdef (canDupe de) x
      case (isPure de, x') of
        (True, DLV_Eff) -> return $ DL_Nop at
        _ -> do
          ac_visit $ de
          return $ DL_Let at x' de
    DL_ArrayMap at ans x a f -> do
      f' <- ac f
      ac_visit $ x
      return $ DL_ArrayMap at ans x a f'
    DL_ArrayReduce at ans x z b a f -> do
      f' <- ac f
      ac_visit $ [x, z]
      return $ DL_ArrayReduce at ans x z b a f'
    DL_Var at dv -> return $ DL_Var at dv
    DL_Set at dv da -> do
      ac_visit $ da
      return $ DL_Set at dv da
    DL_LocalIf at c t f -> do
      f' <- ac f
      t' <- ac t
      ac_visit $ c
      return $ DL_LocalIf at c t' f'
    DL_LocalSwitch at ov csm -> do
      csm' <- ac csm
      ac_visit $ ov
      return $ DL_LocalSwitch at ov csm'
    DL_Only at who b -> do
      b' <- ac b
      return $ DL_Only at who b'
    DL_MapReduce at mri ans x z b a f -> do
      f' <- ac f
      ac_visit $ z
      return $ DL_MapReduce at mri ans x z b a f'

instance AC DLTail where
  ac = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> do
      k' <- ac k
      m' <- ac m
      return $ mkCom DT_Com m' k'

instance AC DLBlock where
  ac (DLBlock at fs t a) = do
    ac_visit $ a
    t' <- ac t
    return $ DLBlock at fs t' a

instance {-# OVERLAPS #-} AC a => AC (SwitchCases a) where
  ac = mapM $ \(mv, k) -> (,) mv <$> ac k

instance AC ETail where
  ac = \case
    ET_Com m k -> do
      k' <- ac k
      m' <- ac m
      return $ mkCom ET_Com m' k'
    ET_Stop at -> return $ ET_Stop at
    ET_If at c t f -> do
      f' <- ac f
      t' <- ac t
      ac_visit $ c
      return $ ET_If at c t' f'
    ET_Switch at v csm -> do
      csm' <- ac csm
      ac_visit v
      return $ ET_Switch at v csm'
    ET_FromConsensus at vi vs fi k -> do
      k' <- ac k
      ac_visit fi
      ac_visit vs
      return $ ET_FromConsensus at vi vs fi k'
    ET_ToConsensus {..} -> do
      et_tc_cons' <- ac et_tc_cons
      et_tc_from_mtime' <- ac et_tc_from_mtime
      ac_visit et_tc_from_me
      ac_visit et_tc_last_timev
      return $ ET_ToConsensus et_tc_at et_tc_from et_tc_prev et_tc_last_timev et_tc_which et_tc_from_me et_tc_from_msg et_tc_from_out et_tc_from_timev et_tc_from_mtime' et_tc_cons'
    ET_While {..} -> do
      et_w_k' <- ac et_w_k
      et_w_body' <- ac et_w_body
      et_w_cond' <- ac et_w_cond
      ac_visit et_w_asn
      return $ ET_While et_w_at et_w_asn et_w_cond' et_w_body' et_w_k'
    ET_Continue at asn -> do
      ac_visit asn
      return $ ET_Continue at asn

instance AC CTail where
  ac = \case
    CT_Com m k -> do
      k' <- ac k
      m' <- ac m
      return $ mkCom CT_Com m' k'
    CT_If at c t f -> do
      f' <- ac f
      t' <- ac t
      ac_visit $ c
      return $ CT_If at c t' f'
    CT_Switch at v csm -> do
      csm' <- ac csm
      ac_visit $ v
      return $ CT_Switch at v csm'
    CT_From at w v fi -> do
      ac_visit $ v
      ac_visit $ fi
      return $ CT_From at w v fi
    CT_Jump at which svs asn -> do
      ac_visit $ svs
      ac_visit $ asn
      return $ CT_Jump at which svs asn

instance AC CHandler where
  ac = \case
    C_Loop {..} -> fresh $ do
      body' <- ac cl_body
      return $ C_Loop cl_at cl_svs cl_vars body'
    C_Handler {..} -> fresh $ do
      ch_body' <- ac ch_body
      return $ C_Handler ch_at ch_int ch_last_timev ch_from ch_last ch_svs ch_msg ch_timev ch_body'

instance {-# OVERLAPS #-} AC a => AC (DLinExportBlock a) where
  ac (DLinExportBlock at vs a) =
    DLinExportBlock at vs <$> ac a

instance AC EPProg where
  ac (EPProg at ie et) = fresh $
    EPProg at ie <$> ac et

instance AC EPPs where
  ac (EPPs m) = EPPs <$> ac m

instance AC CHandlers where
  ac (CHandlers m) = CHandlers <$> ac m

instance AC CPProg where
  ac (CPProg at vs chs) =
    CPProg at <$> ac vs <*> ac chs

instance AC ViewInfo where
  ac (ViewInfo vs vi) =
    ViewInfo vs <$> ac vi

instance AC PLProg where
  ac (PLProg at plo dli dex epps cp) =
    PLProg at plo dli <$> ac dex <*> ac epps <*> ac cp

add_counts :: AC a => a -> IO a
add_counts x = do
  e_cs <- newIORef $ mempty
  flip runReaderT (Env {..}) $ ac x
