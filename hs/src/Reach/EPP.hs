module Reach.EPP (epp) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.STRef
import qualified Data.Set as S
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.CollectCounts
import Reach.Optimize
import Reach.Pretty ()
import Reach.STCounter
import Reach.Util

-- import Debug.Trace
-- import Reach.Texty

data EPPError
  = Err_ContinueDomination
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance Show EPPError where
  show Err_ContinueDomination =
    "Continue must be dominated by communication"

data ProRes_ a = ProRes_ Counts a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (ProRes_ a) where
  (ProRes_ x_cs x) <> (ProRes_ y_cs y) =
    ProRes_ (x_cs <> y_cs) (x <> y)

instance Monoid a => Monoid (ProRes_ a) where
  mempty = ProRes_ mempty mempty

data ProResL = ProResL (ProRes_ (UndefdVars, PLTail))
  deriving (Eq, Show)

type SLPartETs = (M.Map SLPart (ProRes_ ETail))

type UndefdVars = S.Set DLVar

data ProResC = ProResC SLPartETs (ProRes_ (UndefdVars, CTail))
  deriving (Eq, Show)

data ProSt s = ProSt
  { pst_prev_handler :: Int
  , pst_handlers :: STRef s CHandlers
  , pst_handlerc :: STCounter s
  , pst_interval :: CInterval DLArg
  , pst_parts :: [SLPart]
  , pst_loop_fixed_point :: Bool
  , --- FIXME These would be maps when we have labelled loops
    pst_loop_svs :: Maybe [DLVar]
  , pst_loop_num :: Maybe Int
  , pst_forced_svs :: Counts
  }
  deriving (Eq)

default_interval :: CInterval a
default_interval = CBetween [] []

interval_from :: CInterval a -> [a]
interval_from (CBetween froml _) = froml

interval_add_from :: CInterval a -> a -> CInterval a
interval_add_from (CBetween froml tol) x =
  CBetween (x : froml) (x : tol)

interval_add_to :: CInterval a -> a -> CInterval a
interval_add_to (CBetween froml tol) x =
  CBetween froml (x : tol)

interval_no_to :: CInterval a -> CInterval a
interval_no_to (CBetween froml _) =
  CBetween froml []

updateHandlerSVS :: ProSt s -> Int -> [DLVar] -> ST s ()
updateHandlerSVS st target new_svs = modifySTRef (pst_handlers st) update
  where
    update (CHandlers hs) = CHandlers $ fmap update1 hs
    update1 = \case
      C_Handler at int ltv fs prev old_svs msg amtv tv body ->
        C_Handler at int ltv fs prev svs msg amtv tv body
        where
          svs =
            case target == prev of
              True -> new_svs
              False -> old_svs
      h -> h

newHandler :: ProSt s -> ST s Int
newHandler st =
  case pst_loop_fixed_point st of
    True -> return $ 0
    False ->
      incSTCounter $ pst_handlerc st

addHandler :: ProSt s -> Int -> CHandler -> ST s ()
addHandler st which this_h =
  case pst_loop_fixed_point st of
    True -> mempty
    False ->
      modifySTRef (pst_handlers st) $ ((CHandlers $ M.singleton which this_h) <>)

pmap :: ProSt s -> (SLPart -> b) -> M.Map SLPart b
pmap st f = M.fromList $ map (\p -> (p, f p)) $ pst_parts st

pall :: ProSt s -> a -> M.Map SLPart a
pall st x = pmap st (\_ -> x)

data ProResS = ProResS SLPartETs (ProRes_ Bool)
  deriving (Eq, Show)

epp_m :: Counts -> UndefdVars -> LLCommon -> ProRes_ (UndefdVars, PLCommon)
epp_m k_cs k_udvs = \case
  DL_Nop at -> skip at
  DL_Let at mdv de ->
    case de of
      DLE_Claim _ _ CT_Assert _ _ -> skip at
      DLE_Claim _ _ CT_Possible _ _ -> skip at
      DLE_Claim _ _ (CT_Unknowable {}) _ _ -> skip at
      _ ->
        case mdv of
          Nothing -> maybe_skip []
          Just dv ->
            case get_count dv k_cs of
              Count Nothing -> maybe_skip [dv]
              Count (Just lc) ->
                ProRes_ (cs' [dv]) (k_udvs, (DL_Let at (PV_Let lc dv) de))
    where
      maybe_skip vs =
        case isPure de of
          True -> ProRes_ (k_cs' vs) (k_udvs, DL_Nop at)
          False -> ProRes_ (cs' vs) (k_udvs, (DL_Let at PV_Eff de))
      cs' vs = counts de <> k_cs' vs
      k_cs' vs = count_rms vs k_cs
  DL_ArrayMap at ans x a (DLinBlock fat _ f r) ->
    case get_count ans k_cs of
      Count Nothing -> skip at
      Count (Just _) ->
        --- Note: Maybe use LetCat for fusing?
        ProRes_ cs' (udvs', (DL_ArrayMap at ans x a (DLinBlock fat mempty f' r)))
    where
      cs' = (counts x <> f_cs' <> k_cs')
      udvs' = f_udvs <> k_udvs
      k_cs' = count_rms [ans] k_cs
      f_cs' = count_rms [a] f_cs
      --- FIXME: We force the variables in r to appear twice so they won't get the PL_Once tag; it would be better to get the threading of the information correct in Sol
      fk_cs' = k_cs' <> (counts r <> counts r)
      ProResL (ProRes_ f_cs (f_udvs, f')) = epp_l fk_cs' f
  DL_ArrayReduce at ans x z b a (DLinBlock fat _ f r) ->
    case get_count ans k_cs of
      Count Nothing -> skip at
      Count (Just _) ->
        --- Note: Maybe use LetCat for fusing?
        ProRes_ cs' (udvs', (DL_ArrayReduce at ans x z b a (DLinBlock fat mempty f' r)))
    where
      cs' = (counts x <> counts z <> f_cs' <> k_cs')
      udvs' = k_udvs <> f_udvs
      k_cs' = count_rms [ans] k_cs
      f_cs' = count_rms [b, a] f_cs
      --- FIXME: We force the variables in r to appear twice so they won't get the PL_Once tag; it would be better to get the threading of the information correct in Sol
      fk_cs' = k_cs' <> (counts r <> counts r)
      ProResL (ProRes_ f_cs (f_udvs, f')) = epp_l fk_cs' f
  DL_Var at dv ->
    let cs' = count_rms [dv] k_cs
        udvs' = S.delete dv k_udvs
     in ProRes_ cs' (udvs', (DL_Var at dv))
  DL_Set at dv da ->
    let cs' = counts da <> count_rms [dv] k_cs
        udvs' = S.insert dv k_udvs
     in ProRes_ cs' (udvs', (DL_Set at dv da))
  DL_LocalIf at ca t f ->
    let cs' = counts ca <> t'_cs <> f'_cs
        ProResL (ProRes_ t'_cs (t_udvs, t')) = epp_l k_cs t
        ProResL (ProRes_ f'_cs (f_udvs, f')) = epp_l k_cs f
        udvs' = t_udvs <> f_udvs <> k_udvs
     in ProRes_ cs' (udvs', DL_LocalIf at ca t' f')
  DL_LocalSwitch at ov csm ->
    let cm1 (mov', l) = (l'_cs', (l'_udvs, (mov', l')))
          where
            ProResL (ProRes_ l'_cs (l'_udvs, l')) = epp_l k_cs l
            l'_cs' = count_rms (maybeToList mov') l'_cs
        csm'0 = M.map cm1 csm
        csm' = M.map (snd . snd) csm'0
        c_udvs = mconcatMap (fst . snd) $ M.elems csm'0
        udvs' = c_udvs <> k_udvs
        cs' = counts ov <> (mconcatMap fst $ M.elems csm'0)
     in ProRes_ cs' (udvs', DL_LocalSwitch at ov csm')
  where
    skip at = ProRes_ k_cs (k_udvs, DL_Nop at)

epp_l :: Counts -> LLTail -> ProResL
epp_l ok_cs = \case
  DT_Return at ->
    ProResL (ProRes_ ok_cs (mempty, DT_Return at))
  DT_Com c k ->
    ProResL (ProRes_ cs'' (udvs'', DT_Com c' k'))
    where
      ProRes_ cs'' (udvs'', c') = epp_m cs' udvs' c
      ProResL (ProRes_ cs' (udvs', k')) = epp_l ok_cs k

extend_locals_look :: LLCommon -> SLPartETs -> SLPartETs
extend_locals_look c p_prts_s = M.map add p_prts_s
  where
    add (ProRes_ cs_k k) = ProRes_ cs' (ET_Com c' k)
      where
        ProRes_ cs' (_, c') = epp_m cs_k mempty c

pltReplace :: (PLCommon -> a -> a) -> a -> PLTail -> a
pltReplace mkk nk = \case
  DT_Return _ -> nk
  DT_Com c pt -> mkk c $ pltReplace mkk nk pt

var_addlc :: Counts -> DLVar -> (PLLetCat, DLVar)
var_addlc cs v = (lc, v)
  where
    lc =
      case get_count v cs of
        Count Nothing -> PL_Once
        Count (Just x) -> x

epp_n :: forall s. ProSt s -> LLConsensus -> ST s ProResC
epp_n st = \case
  LLC_Com c k -> do
    ProResC p_prts_s (ProRes_ cs_k (udvs_k, k')) <- epp_n st k
    let ProRes_ cs_k' (udvs_k', c') = epp_m cs_k udvs_k c
    let p_prts_s' = extend_locals_look c p_prts_s
    return $ ProResC p_prts_s' (ProRes_ cs_k' (udvs_k', CT_Com c' k'))
  LLC_If at ca t f -> do
    ProResC p_prts_t (ProRes_ cs_t (udvs_t, ct_t)) <- epp_n st t
    ProResC p_prts_f (ProRes_ cs_f (udvs_f, ct_f)) <- epp_n st f
    let mkp p = ProRes_ cs_p et_p
          where
            ProRes_ cs_p_t et_p_t = p_prts_t M.! p
            ProRes_ cs_p_f et_p_f = p_prts_f M.! p
            cs_p = counts ca <> cs_p_t <> cs_p_f
            et_p = ET_If at ca et_p_t et_p_f
    let p_prts' = pmap st mkp
    let cs' = counts ca <> cs_t <> cs_f
    let ct' = CT_If at ca ct_t ct_f
    let udvs' = udvs_t <> udvs_f
    return $ ProResC p_prts' (ProRes_ cs' (udvs', ct'))
  LLC_Switch at ov csm -> do
    let cm1 (ov', l) = (,) <$> (pure ov') <*> epp_n st l
    csm'0 <- mapM cm1 csm
    let mkp p = ProRes_ cs_p $ ET_Switch at ov csm'p
          where
            csm'0p =
              M.map
                (\(mov', ProResC p_prts _) ->
                   let ProRes_ cs_p_c et_p_c = p_prts M.! p
                    in (count_rms (maybeToList mov') cs_p_c, (mov', et_p_c)))
                csm'0
            cs_p = mconcatMap fst $ M.elems csm'0p
            csm'p = M.map snd csm'0p
    let p_prts' = pmap st mkp
    let csm'ct = M.map (\(mov', ProResC _ (ProRes_ _ (_, ct))) -> (mov', ct)) csm'0
    let cs' = counts ov <> (mconcatMap (\(mov', ProResC _ (ProRes_ cs _)) -> count_rms (maybeToList mov') cs) $ M.elems csm'0)
    let udvs' = mconcatMap (\(_, ProResC _ (ProRes_ _ (udvs, _))) -> udvs) $ M.elems csm'0
    return $ ProResC p_prts' (ProRes_ cs' (udvs', CT_Switch at ov csm'ct))
  LLC_FromConsensus at1 _at2 s -> do
    let st' = st {pst_interval = default_interval}
    ProResS p_prts_s (ProRes_ cons_cs more_chb) <- epp_s st' s
    let svs = counts_nzs cons_cs
    let which = pst_prev_handler st
    let from_info =
          case more_chb of
            True -> Just svs
            False -> Nothing
    updateHandlerSVS st which svs
    let mkp (ProRes_ cs_p et_p) =
          ProRes_ cs_p (ET_FromConsensus at1 which from_info et_p)
    let p_prts_s' = M.map mkp p_prts_s
    let ctw = CT_From at1 from_info
    return $ ProResC p_prts_s' (ProRes_ cons_cs (mempty, ctw))
  LLC_While at asn _inv cond body k -> do
    loop_num <- newHandler st
    let st_k = st {pst_prev_handler = loop_num}
    ProResC p_prts_k (ProRes_ cs_k (udvs_k, ct_k)) <- epp_n st_k k
    let loop_vars = assignment_vars asn
    let DLinBlock cond_at _ cond_l cond_da = cond
    let st_body0 = st_k {pst_loop_num = Just loop_num}

    --- This might be insane
    let fixSVS loop_svs0 run = do
          (loop_svs1, _) <- run loop_svs0 False
          case loop_svs0 == loop_svs1 of
            True -> run loop_svs1 True
            False -> fixSVS loop_svs1 run
    (loop_svs, (p_prts_body, udvs_body, ct_body, pt_cond)) <- fixSVS mempty $ \loop_svs0 done -> do
      let st_body =
            st_body0
              { pst_loop_svs = Just loop_svs0
              , pst_loop_fixed_point = not done
              }
      ProResC p_prts_body_ (ProRes_ cs_body (udvs_body_, ct_body_)) <-
        epp_n st_body body
      let post_cond_cs = counts cond_da <> cs_body <> cs_k
      let ProResL (ProRes_ cs_cond (cond_udvs, pt_cond_)) = epp_l post_cond_cs cond_l
      let udvs' = cond_udvs <> udvs_body_
      let loop_svs_ = counts_nzs $ count_rms loop_vars $ cs_cond
      return $ (loop_svs_, (p_prts_body_, udvs', ct_body_, pt_cond_))

    let loop_if = CT_If cond_at cond_da ct_body ct_k
    let loop_top = pltReplace CT_Com loop_if pt_cond
    (loop_top'_cs, loop_top') <- unsafeIOToST $ pltoptimize loop_top
    let loop_lcvars = map (var_addlc loop_top'_cs) loop_vars
    let this_h = C_Loop at loop_svs loop_lcvars loop_top'
    addHandler st loop_num this_h
    let ct' = CT_Jump at loop_num loop_svs asn
    let cons_cs' = counts loop_svs <> counts asn
    let mkp p = ProRes_ cs_p t_p
          where
            ProRes_ p_cs_k t_k = p_prts_k M.! p
            ProRes_ p_cs_body t_body = p_prts_body M.! p
            cs_p = counts asn <> (count_rms loop_vars $ counts cond_da <> p_cs_body <> p_cs_k)
            t_p = ET_While at asn (DLinBlock cond_at mempty pt_cond cond_da) t_body t_k
    let p_prts' = pmap st mkp
    let udvs' = udvs_k <> udvs_body
    return $ ProResC p_prts' (ProRes_ cons_cs' (udvs', ct'))
  LLC_Continue at asn -> do
    let this_loop = fromMaybe (impossible "no loop") $ pst_loop_num st
    let loop_svs = fromMaybe (impossible "no loop") $ pst_loop_svs st
    when (this_loop == pst_prev_handler st) $
      expect_thrown at Err_ContinueDomination
    let asn_cs = counts asn
    let cons_cs' = asn_cs <> counts loop_svs
    let ct' = CT_Jump at this_loop loop_svs asn
    let p_prts' = pall st (ProRes_ asn_cs (ET_Continue at asn))
    return $ ProResC p_prts' (ProRes_ cons_cs' (mempty, ct'))
  LLC_Only at who body_l k_n -> do
    ProResC p_prts_k (ProRes_ cs_k (udvs_k, prchs_k)) <- epp_n st k_n
    let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
    let ProResL (ProRes_ who_body_cs (who_body_udvs, who_body_lt)) = epp_l who_k_cs body_l
    let who_prt_only =
          ProRes_ who_body_cs $ ET_ConsensusOnly at who_body_lt who_k_et
    let p_prts = M.insert who who_prt_only p_prts_k
    let udvs' = udvs_k <> who_body_udvs
    return $ ProResC p_prts (ProRes_ cs_k (udvs', prchs_k))

epp_s :: ProSt s -> LLStep -> ST s ProResS
epp_s st = \case
  LLS_Com c k -> do
    let st' =
          case c of
            (DL_Let _ _ (DLE_Wait _ amt)) ->
              st {pst_interval = interval_add_from (pst_interval st) amt}
            _ -> st
    ProResS p_prts_s (ProRes_ cs_k morech) <- epp_s st' k
    let ProRes_ cs_k' _ = epp_m cs_k mempty c
    let p_prts_s' = extend_locals_look c p_prts_s
    return $ ProResS p_prts_s' (ProRes_ cs_k' morech)
  LLS_Stop at -> do
    let cs = pst_forced_svs st
    let p_prts_s = pall st (ProRes_ cs (ET_Stop at))
    return $ ProResS p_prts_s $ ProRes_ cs False
  LLS_Only _at who body_l k_s -> do
    ProResS p_prts_k prchs_k <- epp_s st k_s
    let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
    let ProResL (ProRes_ who_body_cs (_, who_body_lt)) = epp_l who_k_cs body_l
    let who_prt_only =
          ProRes_ who_body_cs $ pltReplace ET_Com who_k_et who_body_lt
    let p_prts = M.insert who who_prt_only p_prts_k
    return $ ProResS p_prts prchs_k
  LLS_ToConsensus at send (last_timev, fromv, msg, amt_dv, timev, cons) mtime -> do
    let prev_int = pst_interval st
    which <- newHandler st
    let (int_ok, delay_cs, continue_time) =
          case mtime of
            Nothing ->
              (int_ok_, mempty, continue_time_)
              where
                int_ok_ = interval_no_to prev_int
                continue_time_ ok_cons_cs =
                  return $ (ok_cons_cs, pall st $ ProRes_ mempty Nothing)
            Just (delaya, delays) -> (int_ok_, delay_cs_, continue_time_)
              where
                -- LLBlock _ _ delay_ss delaya = delayb
                delayas = interval_from int_to
                delay_cs_ = counts delayas
                int_to = interval_add_from prev_int delaya
                int_ok_ = interval_add_to prev_int delaya
                continue_time_ ok_cons_cs = do
                  let st_to =
                        st
                          { pst_interval = int_to
                          , pst_forced_svs = ok_cons_cs
                          }
                  ProResS delay_prts (ProRes_ tcons_cs _) <-
                    epp_s st_to delays
                  let cs' = delay_cs_ <> tcons_cs
                  let update (ProRes_ tk_cs tk_et) =
                        ProRes_ (tk_cs <> delay_cs_) (Just (delayas, tk_et))
                  return $ (cs', M.map update delay_prts)
    let st_cons =
          st
            { pst_prev_handler = which
            , pst_interval = int_ok
            }
    ProResC p_prts_cons (ProRes_ cons_vs (cons_udvs, ct_cons0)) <-
      epp_n st_cons cons
    let add_udv_def uk udv = CT_Com (DL_Var at udv) uk
    let ct_cons = S.foldl' add_udv_def ct_cons0 cons_udvs
    let (fs_uses, fs_defns) = (mempty, [fromv])
    let msg_and_defns = (msg <> [amt_dv, timev] <> fs_defns)
    let int_ok_cs = counts int_ok
    let ok_cons_cs = int_ok_cs <> delay_cs <> count_rms msg_and_defns (fs_uses <> cons_vs) <> pst_forced_svs st <> (counts [last_timev])
    (time_cons_cs, mtime'_ps) <- continue_time ok_cons_cs
    let svs = counts_nzs time_cons_cs
    let prev = pst_prev_handler st
    let this_h = C_Handler at int_ok last_timev fromv prev svs msg amt_dv timev ct_cons
    let soloSend0 = (M.size send) == 1
    let soloSend1 = not $ getAll $ mconcatMap (\(isClass, _, _, _) -> All isClass) $ M.elems send
    -- It is only a solo send if we are the only sender AND we are not a
    -- class
    let soloSend = soloSend0 && soloSend1
    let mk_et mfrom (ProRes_ cs_ et_) (ProRes_ mtime'_cs mtime') =
          ProRes_ cs_' $ ET_ToConsensus at fromv prev last_timev which mfrom msg amt_dv timev mtime' et_
          where
            cs_' = mtime'_cs <> fs_uses <> counts mfrom <> count_rms msg_and_defns cs_
    let mk_p_prt p prt = mker prt mtime'
          where
            mtime' = mtime'_ps M.! p
            mker =
              case M.lookup p send of
                Nothing -> mk_et Nothing
                Just (_isClass, from_as, amt_da, when_da) ->
                  mk_et $ Just (from_as, amt_da, when_da, svs, soloSend)
    let p_prts = M.mapWithKey mk_p_prt p_prts_cons
    addHandler st which this_h
    return $ ProResS p_prts (ProRes_ time_cons_cs True)

epp :: LLProg -> PLProg
epp (LLProg at (LLOpts {..}) ps dli s) = runST $ do
  let SLParts p_to_ie = ps
  nhr <- newSTCounter 1
  hsr <- newSTRef $ mempty
  let st =
        ProSt
          { pst_prev_handler = 0
          , pst_handlers = hsr
          , pst_handlerc = nhr
          , pst_interval = default_interval
          , pst_parts = M.keys p_to_ie
          , pst_loop_fixed_point = False
          , pst_loop_svs = Nothing
          , pst_loop_num = Nothing
          , pst_forced_svs = mempty
          }
  ProResS p_prts _ <- epp_s st s
  chs <- readSTRef hsr
  let cp = CPProg at chs
  let mk_pp p ie =
        case M.lookup p p_prts of
          Just (ProRes_ _ et) -> EPProg at ie et
          Nothing ->
            impossible $ "part not in projection"
  let pps = EPPs $ M.mapWithKey mk_pp p_to_ie
  let plo_deployMode = llo_deployMode
  let plo_verifyOverflow = llo_verifyOverflow
  return $ PLProg at (PLOpts {..}) dli pps cp
