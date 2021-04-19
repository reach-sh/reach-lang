module Reach.AddCounts (add_counts) where

import Control.Monad.Reader
import Data.IORef
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.CollectCounts

data Env = Env
  {e_cs :: IORef Counts}

type App = ReaderT Env IO

ac_visit :: Countable a => a -> App ()
ac_visit x = do
  csr <- e_cs <$> ask
  liftIO $ modifyIORef csr $ (<>) (counts x)

ac_vdef :: Bool -> PILVar -> App PLVar
ac_vdef _ Nothing = return $ PV_Eff
ac_vdef okToDupe (Just v) = do
  cs <- (liftIO . readIORef) =<< (e_cs <$> ask)
  case get_count v cs of
    Count Nothing -> return $ PV_Eff
    Count (Just lc) -> do
      let lc' = if okToDupe then lc else PL_Many
      return $ PV_Let lc' v

ac_m :: PILCommon -> App PLCommon
ac_m = \case
  DL_Nop at -> return $ DL_Nop at
  DL_Let at x de -> do
    ac_visit $ de
    x' <- ac_vdef (canDupe de) x
    case (isPure de, x') of
      (True, PV_Eff) -> return $ DL_Nop at
      _ -> return $ DL_Let at x' de
  DL_ArrayMap at ans x a f -> do
    f' <- ac_bl f
    ac_visit $ x
    return $ DL_ArrayMap at ans x a f'
  DL_ArrayReduce at ans x z b a f -> do
    f' <- ac_bl f
    ac_visit $ [x, z]
    return $ DL_ArrayReduce at ans x z b a f'
  DL_Var at dv -> return $ DL_Var at dv
  DL_Set at dv da -> do
    ac_visit $ da
    return $ DL_Set at dv da
  DL_LocalIf at c t f -> do
    f' <- ac_lt f
    t' <- ac_lt t
    ac_visit $ c
    return $ DL_LocalIf at c t' f'
  DL_LocalSwitch at ov csm -> do
    csm' <- ac_csm ac_lt csm
    ac_visit $ ov
    return $ DL_LocalSwitch at ov csm'
  DL_MapReduce at mri ans x z b a f -> do
    f' <- ac_bl f
    ac_visit $ z
    return $ DL_MapReduce at mri ans x z b a f'

ac_lt :: PILTail -> App PLTail
ac_lt = \case
  DT_Return at -> return $ DT_Return at
  DT_Com m k -> do
    k' <- ac_lt k
    m' <- ac_m m
    return $ mkCom DT_Com m' k'

ac_bl :: PILBlock -> App PLBlock
ac_bl (DLinBlock at fs t a) = do
  ac_visit $ a
  t' <- ac_lt t
  return $ DLinBlock at fs t' a

ac_csm :: (a -> App b) -> SwitchCases a -> App (SwitchCases b)
ac_csm iter = mapM go
  where
    go (mv, k) = (,) mv <$> iter k

ac_ct :: CITail -> App CTail
ac_ct = \case
  CT_Com m k -> do
    k' <- ac_ct k
    m' <- ac_m m
    return $ mkCom CT_Com m' k'
  CT_If at c t f -> do
    f' <- ac_ct f
    t' <- ac_ct t
    ac_visit $ c
    return $ CT_If at c t' f'
  CT_Switch at v csm -> do
    csm' <- ac_csm ac_ct csm
    ac_visit $ v
    return $ CT_Switch at v csm'
  CT_From at w fi -> do
    ac_visit $ fi
    return $ CT_From at w fi
  CT_Jump at which svs asn -> do
    ac_visit $ svs
    ac_visit $ asn
    return $ CT_Jump at which svs asn

ac_ch :: CIHandler -> App CHandler
ac_ch = \case
  C_Loop {..} -> do
    body' <- ac_ct cl_body
    return $ C_Loop cl_at cl_svs cl_vars body'
  C_Handler {..} -> do
    ch_body' <- ac_ct ch_body
    return $ C_Handler ch_at ch_int ch_last_timev ch_from ch_last ch_svs ch_msg ch_timev ch_body'

ac_top :: CIHandler -> IO CHandler
ac_top x = do
  e_cs <- newIORef $ mempty
  flip runReaderT (Env {..}) $ ac_ch x

ac_ev :: DLinExportVal PILBlock -> App (DLinExportVal PLBlock)
ac_ev = \case
  DLEV_Arg at a -> return $ DLEV_Arg at a
  DLEV_Fun at a b -> do
    ac_visit a
    DLEV_Fun at a <$> ac_bl b

ac_eb :: DLExportinBlock PILVar -> IO (DLExportinBlock PLVar)
ac_eb = \case
  DLExportinBlock b a -> do
    e_cs <- newIORef $ mempty
    flip runReaderT (Env {..}) $ do
      DLExportinBlock <$> ac_lt b <*> ac_ev a

add_counts :: PIProg -> IO PLProg
add_counts (PLProg at plo dli dex _epps cp) = do
  let epps' = EPPs mempty -- XXX hack
  let CPProg cat (CHandlers chs) = cp
  chs' <- mapM ac_top chs
  dex' <- mapM ac_eb dex
  let cp' = CPProg cat $ CHandlers chs'
  return $ PLProg at plo dli dex' epps' cp'
