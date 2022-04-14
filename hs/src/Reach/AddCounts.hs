module Reach.AddCounts (add_counts, AC (..), ac_vdef, ac_visit, ac_vls) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.CollectCounts
import Reach.AnalyzeVars
import Reach.Util

data Env = Env
  {e_cs :: IORef Counts}

type App = ReaderT Env IO

type AppT a = a -> App a

fresh :: App m -> App m
fresh m = do
  e_cs' <- (liftIO . dupeIORef) =<< (e_cs <$> ask)
  local (\e -> e {e_cs = e_cs'}) m

class AC a where
  ac :: AppT a

ac_visit :: Countable a => a -> App ()
ac_visit x = do
  csr <- e_cs <$> ask
  liftIO $ modifyIORef csr $ (<>) (counts x)

ac_getCount :: DLVar -> App (Maybe DLVarCat)
ac_getCount v = do
  cs <- (liftIO . readIORef) =<< (e_cs <$> ask)
  case get_count v cs of
    Count Nothing -> return $ Nothing
    Count (Just lc) -> return $ Just lc

ac_vdef :: Bool -> AppT DLLetVar
ac_vdef _ DLV_Eff = return $ DLV_Eff
ac_vdef okToDupe (DLV_Let _ v) = do
  ac_getCount v >>= \case
    Nothing -> return $ DLV_Eff
    Just lc -> do
      let lc' = if okToDupe then lc else DVC_Many
      return $ DLV_Let lc' v

ac_vl :: AppT DLVarLet
ac_vl (DLVarLet _ v) = do
  mvc <- ac_getCount v
  return $ DLVarLet mvc v

ac_vls :: AppT [DLVarLet]
ac_vls = mapM ac_vl

instance (AC a, Traversable t) => AC (t a) where
  ac = mapM ac

instance {-# OVERLAPS #-} (AC a, AC b) => AC (a, b) where
  ac (x, y) = (,) <$> ac x <*> ac y

instance {-# OVERLAPS #-} (AC a, AC b) => AC (Either a b) where
  ac = \case
    Left x -> Left <$> ac x
    Right x -> Right <$> ac x

viaVisit :: Countable a => AppT a
viaVisit a = do
  ac_visit a
  return $ a

instance AC DLVar where
  ac = viaVisit

instance AC DLArg where
  ac = viaVisit

instance AC IType where
  ac = return

instance AC DLStmt where
  ac = \case
    DL_Nop at -> skip at
    DL_Let at x de -> do
      x' <- ac_vdef (canDupe de) x
      case (isPure de, x') of
        (True, DLV_Eff) -> skip at
        _ -> do
          ac_visit $ de
          return $ DL_Let at x' de
    DL_ArrayMap at ans xs as i f -> do
      count <- ac_getCount ans
      case (count, isPure f) of
        (Nothing, True) -> skip at
        _ -> do
          f' <- ac f
          ac_visit $ xs
          return $ DL_ArrayMap at ans xs as i f'
    DL_ArrayReduce at ans xs z b as i f -> do
      count <- ac_getCount ans
      case (count, isPure f) of
        (Nothing, True) -> skip at
        _ -> do
          f' <- ac f
          ac_visit $ xs <> [z]
          return $ DL_ArrayReduce at ans xs z b as i f'
    DL_Var at dv ->
      ac_getCount dv >>= \case
        Nothing -> skip at
        Just _ -> return $ DL_Var at dv
    DL_Set at dv da -> do
      ac_getCount dv >>= \case
        Nothing -> skip at
        Just _ -> do
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
      -- XXX remove if ans not used
      f' <- ac f
      ac_visit $ z
      return $ DL_MapReduce at mri ans x z b a f'
    DL_LocalDo at t -> DL_LocalDo at <$> ac t
    where
      skip at = return $ DL_Nop at

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
  ac = mapM $ \(v, _, k) -> do
    k' <- ac k
    vu' <-
      ac_vdef True (DLV_Let DVC_Many v) >>= \case
        DLV_Eff -> return False
        _ -> return True
    return $ (v, vu', k')

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
    ET_FromConsensus at vi fi k -> do
      k' <- ac k
      ac_visit fi
      return $ ET_FromConsensus at vi fi k'
    ET_ToConsensus {..} -> do
      et_tc_cons' <- ac et_tc_cons
      et_tc_from_mtime' <- ac et_tc_from_mtime
      ac_visit et_tc_from_me
      ac_visit et_tc_lct
      return $ ET_ToConsensus et_tc_at et_tc_from et_tc_prev et_tc_lct et_tc_which et_tc_from_me et_tc_from_msg et_tc_from_out et_tc_from_timev et_tc_from_secsv et_tc_from_didSendv et_tc_from_mtime' et_tc_cons'
    ET_While {..} -> do
      et_w_k' <- ac et_w_k
      et_w_body' <- ac et_w_body
      et_w_cond' <- ac et_w_cond
      ac_visit et_w_asn
      return $ ET_While et_w_at et_w_asn et_w_cond' et_w_body' et_w_k'
    ET_Continue at asn -> do
      ac_visit asn
      return $ ET_Continue at asn

-- XXX Make a type class for this stupid thing
comSeq :: CTail -> ([DLStmt], CTail)
comSeq = \case
  CT_Com m c -> (m : ms, c')
    where
      (ms, c') = comSeq c
  c -> ([], c)

condBlock :: CTail -> Maybe (DLStmt, CTail)
condBlock c =
  case (vs, ms') of
    ([], _) -> Nothing
    (_, c'@(DL_LocalIf {}) : ms'') -> good c' ms''
    (_, c'@(DL_LocalSwitch {}) : ms'') -> good c' ms''
    _ -> Nothing
  where
    at = srclocOf c
    (ms, k) = comSeq c
    isVar = \case
      DL_Var {} -> True
      _ -> False
    (vs, ms') = span isVar ms
    good c' ms'' = Just (DL_LocalDo at dt, k')
      where
        dt = dtList at (vs <> [c'])
        k' = dtReplace CT_Com k (dtList at ms'')

instance AC CTail where
  ac = \case
    CT_Com m k -> do
      k' <- ac k
      m' <- ac m
      let mk = mkCom CT_Com
      let meh = return $ mk m' k'
      -- XXX This optimization would be better in `LLConsensus` or even better
      -- as part of mkCom
      case condBlock k' of
        Nothing -> meh
        Just (dt, k'') -> do
          -- I want to do this, but it is too inefficient
          let _freeNotInBound = not $ S.isSubsetOf (freeVars dt) (boundVars m')
          let canFloat = False -- freeNotInBound
          case canFloat of
            True -> return $ mk dt $ mk m' k''
            False -> meh
    CT_If at c t f -> do
      f' <- ac f
      t' <- ac t
      ac_visit $ c
      return $ CT_If at c t' f'
    CT_Switch at v csm -> do
      csm' <- ac csm
      ac_visit $ v
      return $ CT_Switch at v csm'
    CT_From at w fi -> do
      ac_visit $ fi
      return $ CT_From at w fi
    CT_Jump at which svs asn -> do
      ac_visit $ svs
      ac_visit $ asn
      return $ CT_Jump at which svs asn

instance AC CHandler where
  ac = \case
    C_Loop {..} -> fresh $ do
      body' <- ac cl_body
      svs' <- ac_vls cl_svs
      vars' <- ac_vls cl_vars
      return $ C_Loop cl_at svs' vars' body'
    C_Handler {..} -> fresh $ do
      body' <- ac ch_body
      ac_visit ch_int
      ac_visit ch_from
      svs' <- ac_vls ch_svs
      msg' <- ac_vls ch_msg
      return $ C_Handler ch_at ch_int ch_from ch_last svs' msg' ch_timev ch_secsv body'

instance {-# OVERLAPS #-} AC a => AC (DLinExportBlock a) where
  ac (DLinExportBlock at vs a) = do
    a' <- ac a
    vs' <- mapM ac_vls vs
    return $ DLinExportBlock at vs' a'

instance AC EPProg where
  ac (EPProg at x ie et) =
    fresh $
      EPProg at x ie <$> ac et

instance AC EPPs where
  ac (EPPs {..}) = EPPs epps_apis <$> ac epps_m

instance AC CHandlers where
  ac (CHandlers m) = CHandlers <$> ac m

instance AC CPProg where
  ac (CPProg at vs ai devts chs) =
    CPProg at <$> ac vs <*> pure ai <*> pure devts <*> ac chs

ac_vi :: AppT ViewsInfo
ac_vi = mapM (mapM (fresh . ac))

instance AC ViewInfo where
  ac (ViewInfo vs vi) =
    ViewInfo vs <$> ac_vi vi

instance AC PLProg where
  ac (PLProg at plo dli dex ssm epps cp) =
    PLProg at plo dli <$> ac dex <*> pure ssm <*> ac epps <*> ac cp

instance AC DLSend where
  ac = viaVisit

instance {-# OVERLAPS #-} AC a => AC (DLRecv a) where
  ac (DLRecv {..}) = do
    dr_k' <- ac dr_k
    return $ DLRecv dr_from dr_msg dr_time dr_secs dr_didSend dr_k'

instance AC LLConsensus where
  ac = \case
    LLC_Com m c -> do
      c' <- ac c
      m' <- ac m
      c'' <-
        case c' of
          -- XXX This is ineffective, because we need two more things:
          -- 1. We need to notice when two variables are always the same thing
          -- and then combine them into one thing.
          -- 2. We need to be able to float a localswitch up past other
          -- expressions until it touches another and this applies
          ( LLC_Com
              (DL_Var at_v1 dv_v1)
              ( LLC_Com
                  (DL_LocalSwitch at_s1 dv_s1 csm_s1)
                  ( LLC_Com
                      (DL_Var at_v2 dv_v2)
                      ( LLC_Com
                          (DL_LocalSwitch at_s2 dv_s2 csm_s2)
                          k
                        )
                    )
                )
            ) | False && dv_s1 == dv_s2 -> do
              let combine :: SLVar -> (DLVar, Bool, DLTail) -> (DLVar, Bool, DLTail)
                  combine vn (vv1, vb1, vk1) = (vv1, vb', vk')
                    where
                      (vv2, vb2, vk2) = csm_s2 M.! vn
                      vb' = vb1 || vb2
                      vk2' = DT_Com (DL_Let at_s2 (DLV_Let DVC_Many vv2) (DLE_Arg at_s2 $ DLA_Var vv1)) vk2
                      vk' = dtReplace DT_Com vk2' vk1
              let csm_s12 = M.mapWithKey combine csm_s1
              return $ LLC_Com (DL_Var at_v1 dv_v1) $ LLC_Com (DL_Var at_v2 dv_v2) $ LLC_Com (DL_LocalSwitch at_s1 dv_s1 csm_s12) k
          _ ->
            return c'
      return $ mkCom LLC_Com m' c''
    LLC_If at c t f -> do
      f' <- ac f
      t' <- ac t
      ac_visit c
      return $ LLC_If at c t' f'
    LLC_Switch at c csm -> do
      csm' <- ac csm
      ac_visit c
      return $ LLC_Switch at c csm'
    LLC_FromConsensus at1 at2 fs s ->
      LLC_FromConsensus at1 at2 fs <$> ac s
    LLC_While {..} -> do
      k' <- ac llc_w_k
      body' <- ac llc_w_body
      cond' <- ac llc_w_cond
      inv' <- ac llc_w_inv
      ac_visit llc_w_asn
      return $ LLC_While llc_w_at llc_w_asn inv' cond' body' k'
    c@(LLC_Continue _ asn) -> do
      ac_visit asn
      return c
    LLC_ViewIs at who v mev c -> do
      c' <- ac c
      mev' <- ac mev
      return $ LLC_ViewIs at who v mev' c'

instance AC LLStep where
  ac = \case
    LLS_Com m s -> do
      s' <- ac s
      m' <- ac m
      return $ mkCom LLS_Com m' s'
    LLS_Stop at -> return $ LLS_Stop at
    LLS_ToConsensus {..} -> do
      mtime' <- ac lls_tc_mtime
      recv' <- ac lls_tc_recv
      send' <- ac lls_tc_send
      lct' <- ac lls_tc_lct
      return $ LLS_ToConsensus lls_tc_at lct' send' recv' mtime'

instance AC LLProg where
  ac (LLProg at llo ps dli dex vs dac das alias devts s) =
    LLProg at llo ps dli <$> ac dex <*> pure vs <*> pure dac <*> pure das <*> pure alias <*> pure devts <*> ac s

add_counts :: AC a => a -> IO a
add_counts x = do
  e_cs <- newIORef $ mempty
  flip runReaderT (Env {..}) $ ac x
