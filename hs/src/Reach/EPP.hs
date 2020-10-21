module Reach.EPP (epp) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef
import Generics.Deriving (Generic)
import Reach.AST
import Reach.CollectCounts
import Reach.Optimize
import Reach.Pretty ()
import Reach.STCounter
import Reach.Util

data EPPError
  = Err_ContinueDomination
  deriving (Eq, Generic)

instance Show EPPError where
  show Err_ContinueDomination =
    "Continue must be dominated by communication"

data ProRes_ a = ProRes_ Counts a
  deriving (Eq, Show)

data ProResL = ProResL (ProRes_ PLTail)
  deriving (Eq, Show)

type SLPartETs = (M.Map SLPart (ProRes_ ETail))

data ProResC = ProResC SLPartETs (ProRes_ CTail)
  deriving (Eq, Show)

data ProSt s = ProSt
  { pst_prev_handler :: Int
  , pst_handlers :: STRef s CHandlers
  , pst_handlerc :: STCounter s
  , pst_interval :: CInterval
  , pst_parts :: [SLPart]
  , pst_loop_fixed_point :: Bool
  , --- FIXME These would be maps when we have labelled loops
    pst_loop_svs :: Maybe [DLVar]
  , pst_loop_num :: Maybe Int
  , pst_forced_svs :: Counts
  }
  deriving (Eq)

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

type MDone res = (SrcLoc -> res)

type MBack a res = (Counts -> (forall c. c -> PLCommon c) -> a -> res)

type MLookCommon = forall d lookres. ((Counts -> PLCommon d -> lookres) -> (Counts -> d -> lookres) -> Counts -> d -> lookres)

--- FIXME Try to simplify these types after all the cases are covered... maybe some of the values are always the same.
epp_m
  :: MDone res
  -> MBack a res
  -> (a -> res)
  -> (a -> MLookCommon -> res)
  -> LLCommon a
  -> res
epp_m done _back skip look c =
  case c of
    LL_Return at -> done at
    LL_Let at mdv de k ->
      case de of
        DLE_Claim _ _ CT_Assert _ _ -> skip k
        DLE_Claim _ _ CT_Possible _ _ -> skip k
        DLE_Claim _ _ (CT_Unknowable {}) _ _ -> skip k
        _ ->
          look
            k
            (\back' skip' k_cs k' ->
               let maybe_skip vs =
                     case isPure de of
                       True -> skip' (k_cs' vs) k'
                       False -> back' (cs' vs) (PL_Eff at de k')
                   cs' vs = counts de <> k_cs' vs
                   k_cs' vs = count_rms vs k_cs
                in case mdv of
                     Nothing -> maybe_skip []
                     Just dv ->
                       case get_count dv k_cs of
                         Count Nothing -> maybe_skip [dv]
                         Count (Just lc) ->
                           back' (cs' [dv]) (PL_Let at lc dv de k'))
    LL_ArrayMap at ans x a (LLBlock fat _ f r) k ->
      look
        k
        (\back' skip' k_cs k' ->
           case get_count ans k_cs of
             Count Nothing -> skip' k_cs k'
             Count (Just _) ->
               --- Note: Maybe use LetCat for fusing?
               back' cs' (PL_ArrayMap at ans x a (PLBlock fat f' r) k')
               where
                 cs' = (counts x <> f_cs' <> k_cs')
                 k_cs' = count_rms [ans] k_cs
                 f_cs' = count_rms [a] f_cs
                 --- FIXME: We force the variables in r to appear twice so they won't get the PL_Once tag; it would be better to get the threading of the information correct in Sol
                 fk_cs' = k_cs' <> (counts r <> counts r)
                 ProResL (ProRes_ f_cs f') = epp_l f fk_cs')
    LL_ArrayReduce at ans x z b a (LLBlock fat _ f r) k ->
      look
        k
        (\back' skip' k_cs k' ->
           case get_count ans k_cs of
             Count Nothing -> skip' k_cs k'
             Count (Just _) ->
               --- Note: Maybe use LetCat for fusing?
               back' cs' (PL_ArrayReduce at ans x z b a (PLBlock fat f' r) k')
               where
                 cs' = (counts x <> counts z <> f_cs' <> k_cs')
                 k_cs' = count_rms [ans] k_cs
                 f_cs' = count_rms [b, a] f_cs
                 --- FIXME: We force the variables in r to appear twice so they won't get the PL_Once tag; it would be better to get the threading of the information correct in Sol
                 fk_cs' = k_cs' <> (counts r <> counts r)
                 ProResL (ProRes_ f_cs f') = epp_l f fk_cs')
    LL_Var at dv k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cs' = count_rms [dv] k_cs
            in back' cs' (PL_Var at dv k'))
    LL_Set at dv da k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cs' = counts da <> count_rms [dv] k_cs
            in back' cs' (PL_Set at dv da k'))
    LL_LocalIf at ca t f k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cs' = counts ca <> t'_cs <> f'_cs
               ProResL (ProRes_ t'_cs t') = epp_l t k_cs
               ProResL (ProRes_ f'_cs f') = epp_l f k_cs
            in back' cs' $ PL_LocalIf at ca t' f' k')
    LL_LocalSwitch at ov csm k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cm1 (mov', l) = (l'_cs', (mov', l'))
                 where
                   ProResL (ProRes_ l'_cs l') = epp_l l k_cs
                   l'_cs' = count_rms (maybeToList mov') l'_cs
               csm'0 = M.map cm1 csm
               csm' = M.map snd csm'0
               cs' = counts ov <> (mconcatMap fst $ M.elems csm'0)
            in back' cs' $ PL_LocalSwitch at ov csm' k')

epp_l :: LLLocal -> Counts -> ProResL
epp_l (LLL_Com com) ok_cs = epp_m done back skip look com
  where
    done :: MDone ProResL
    done rat =
      ProResL (ProRes_ ok_cs $ PLTail $ PL_Return rat)
    back :: MBack LLLocal ProResL
    back cs' mkpl k = ProResL $ ProRes_ (cs' <> k_cs) $ PLTail (mkpl k')
      where
        ProResL (ProRes_ k_cs k') = skip k
    skip k = epp_l k ok_cs
    look :: LLLocal -> MLookCommon -> ProResL
    look k common = ProResL $ common back' skip' k_cs k'
      where
        ProResL (ProRes_ k_cs k') = skip k
        skip' = ProRes_
        back' k_cs' k'' = ProRes_ k_cs' $ PLTail k''

extend_locals :: Counts -> (forall c. c -> PLCommon c) -> SLPartETs -> SLPartETs
extend_locals cs' mkpl p_prts_s = M.map add p_prts_s
  where
    add :: ProRes_ ETail -> ProRes_ ETail
    add (ProRes_ p_cs p_et) =
      ProRes_ (cs' <> p_cs) (ET_Com $ mkpl p_et)

extend_locals_look :: MLookCommon -> SLPartETs -> SLPartETs
extend_locals_look common p_prts_s = M.map add p_prts_s
  where
    add (ProRes_ p_cs p_et) =
      common back' skip' p_cs p_et
      where
        skip' = ProRes_
        back' p_cs' p_ct' = ProRes_ p_cs' $ ET_Com $ p_ct'

plReplace :: (PLCommon a -> a) -> a -> PLCommon PLTail -> a
plReplace mkk nk = \case
  PL_Return {} -> nk
  PL_Let a b c d k ->
    mkk $ PL_Let a b c d $ iter k
  PL_ArrayMap a b c d e k ->
    mkk $ PL_ArrayMap a b c d e $ iter k
  PL_ArrayReduce a b c d e f g k ->
    mkk $ PL_ArrayReduce a b c d e f g $ iter k
  PL_Eff a b k ->
    mkk $ PL_Eff a b $ iter k
  PL_Var a b k ->
    mkk $ PL_Var a b $ iter k
  PL_Set a b c k ->
    mkk $ PL_Set a b c $ iter k
  PL_LocalIf a b c d k ->
    mkk $ PL_LocalIf a b c d $ iter k
  PL_LocalSwitch a b c k ->
    mkk $ PL_LocalSwitch a b c $ iter k
  where iter = pltReplace mkk nk

pltReplace :: (PLCommon a -> a) -> a -> PLTail -> a
pltReplace mkk nk (PLTail m) = plReplace mkk nk m

epp_n :: forall s. ProSt s -> LLConsensus -> ST s ProResC
epp_n st n =
  case n of
    LLC_Com c -> epp_m done back skip look c
      where
        done :: MDone (ST s ProResC)
        done rat =
          return $ ProResC (pall st (ProRes_ mempty $ ET_Com $ PL_Return rat)) (ProRes_ mempty $ CT_Com $ PL_Return rat)
        back :: MBack LLConsensus (ST s ProResC)
        back cs' mkpl k = do
          ProResC p_prts_s (ProRes_ cs_k ct_k) <- skip k
          let p_prts_s' = extend_locals cs' mkpl p_prts_s
          let cs_k' = cs' <> cs_k
          let ct_k' = CT_Com $ mkpl ct_k
          return $ ProResC p_prts_s' (ProRes_ cs_k' ct_k')
        skip k = epp_n st k
        look :: LLConsensus -> MLookCommon -> (ST s ProResC)
        look k common = do
          ProResC p_prts_s (ProRes_ cs_k ct_k) <- skip k
          let cr' = common back' skip' cs_k ct_k
                where
                  skip' = ProRes_
                  back' cs_k' ct_k' = ProRes_ cs_k' $ CT_Com $ ct_k'
          let p_prts_s' = extend_locals_look common p_prts_s
          return $ ProResC p_prts_s' cr'
    LLC_If at ca t f -> do
      ProResC p_prts_t (ProRes_ cs_t ct_t) <- epp_n st t
      ProResC p_prts_f (ProRes_ cs_f ct_f) <- epp_n st f
      let mkp p = ProRes_ cs_p et_p
            where
              ProRes_ cs_p_t et_p_t = p_prts_t M.! p
              ProRes_ cs_p_f et_p_f = p_prts_f M.! p
              cs_p = counts ca <> cs_p_t <> cs_p_f
              et_p = ET_If at ca et_p_t et_p_f
      let p_prts' = pmap st mkp
      let cs' = counts ca <> cs_t <> cs_f
      let ct' = CT_If at ca ct_t ct_f
      return $ ProResC p_prts' (ProRes_ cs' ct')
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
      let csm'ct = M.map (\(mov', ProResC _ (ProRes_ _ ct)) -> (mov', ct)) csm'0
      let cs' = counts ov <> (mconcatMap (\(mov', ProResC _ (ProRes_ cs _)) -> count_rms (maybeToList mov') cs) $ M.elems csm'0)
      return $ ProResC p_prts' (ProRes_ cs' $ CT_Switch at ov csm'ct)
    LLC_FromConsensus at1 _at2 s -> do
      let st' = st {pst_interval = default_interval}
      ProResS p_prts_s (ProRes_ cons_cs more_chb) <- epp_s st' s
      let svs = counts_nzs cons_cs
      let which = pst_prev_handler st
      let from_info =
            case more_chb of
              True -> Just svs
              False -> Nothing
      let mkp (ProRes_ cs_p et_p) =
            ProRes_ cs_p (ET_FromConsensus at1 which from_info et_p)
      let p_prts_s' = M.map mkp p_prts_s
      let ctw = CT_From at1 from_info
      return $ ProResC p_prts_s' (ProRes_ cons_cs ctw)
    LLC_While at asn _inv cond body k -> do
      loop_num <- newHandler st
      let st_k = st {pst_prev_handler = loop_num}
      ProResC p_prts_k (ProRes_ cs_k ct_k) <- epp_n st_k k
      let loop_vars = assignment_vars asn
      let LLBlock cond_at _ cond_l cond_da = cond
      let st_body0 = st_k {pst_loop_num = Just loop_num}

      --- This might be insane
      let fixSVS loop_svs0 run = do
            (loop_svs1, _) <- run loop_svs0 False
            case loop_svs0 == loop_svs1 of
              True -> run loop_svs1 True
              False -> fixSVS loop_svs1 run
      (loop_svs, (p_prts_body, ct_body, pt_cond)) <- fixSVS mempty $ \loop_svs0 done -> do
        let st_body =
              st_body0
                { pst_loop_svs = Just loop_svs0
                , pst_loop_fixed_point = not done
                }
        ProResC p_prts_body_ (ProRes_ cs_body ct_body_) <- epp_n st_body body
        let post_cond_cs = counts cond_da <> cs_body <> cs_k
        let ProResL (ProRes_ cs_cond pt_cond_) = epp_l cond_l post_cond_cs
        let loop_svs_ = counts_nzs $ count_rms loop_vars $ cs_cond
        return $ (loop_svs_, (p_prts_body_, ct_body_, pt_cond_))

      let loop_if = CT_If cond_at cond_da ct_body ct_k
      let loop_top = pltReplace CT_Com loop_if pt_cond
      (loop_top'_cs, loop_top') <- unsafeIOToST $ pltoptimize loop_top
      let loop_addlc v = (lc, v)
            where
              lc =
                case get_count v loop_top'_cs of
                  Count Nothing -> PL_Once
                  Count (Just x) -> x
      let loop_lcvars = map loop_addlc loop_vars
      let this_h = C_Loop at loop_svs loop_lcvars loop_top'
      addHandler st loop_num this_h
      let ct' = CT_Jump at loop_num loop_svs asn
      let cons_cs' = counts loop_svs <> counts asn
      let mkp p = ProRes_ cs_p t_p
            where
              ProRes_ p_cs_k t_k = p_prts_k M.! p
              ProRes_ p_cs_body t_body = p_prts_body M.! p
              cs_p = counts asn <> (count_rms loop_vars $ counts cond_da <> p_cs_body <> p_cs_k)
              t_p = ET_While at asn (PLBlock cond_at pt_cond cond_da) t_body t_k
      let p_prts' = pmap st mkp
      return $ ProResC p_prts' (ProRes_ cons_cs' ct')
    LLC_Continue at asn -> do
      let this_loop = fromMaybe (impossible "no loop") $ pst_loop_num st
      let loop_svs = fromMaybe (impossible "no loop") $ pst_loop_svs st
      when (this_loop == pst_prev_handler st) $
        expect_throw at Err_ContinueDomination
      let asn_cs = counts asn
      let cons_cs' = asn_cs <> counts loop_svs
      let ct' = CT_Jump at this_loop loop_svs asn
      let p_prts' = pall st (ProRes_ asn_cs (ET_Continue at asn))
      return $ ProResC p_prts' (ProRes_ cons_cs' ct')

epp_s :: forall s. ProSt s -> LLStep -> ST s ProResS
epp_s st s =
  case s of
    LLS_Com c ->
      epp_m done back skip look c
      where
        done :: MDone (ST s ProResS)
        done rat =
          return $
            ProResS
              (pall st' (ProRes_ mempty $ ET_Com $ PL_Return rat))
              (ProRes_ mempty False)
        back :: MBack LLStep (ST s ProResS)
        back cs' mkpl k = do
          ProResS p_prts_s (ProRes_ cs_k morech) <- skip k
          let p_prts_s' = extend_locals cs' mkpl p_prts_s
          let cs_k' = cs' <> cs_k
          return $ ProResS p_prts_s' (ProRes_ cs_k' morech)
        skip k = epp_s st' k
        look :: LLStep -> MLookCommon -> (ST s ProResS)
        look k common = do
          ProResS p_prts_s (ProRes_ cs_k morech) <- skip k
          let ProRes_ cs_k' () = common back' skip' cs_k ()
                where
                  skip' = ProRes_
                  back' x _ = ProRes_ x ()
          let p_prts_s' = extend_locals_look common p_prts_s
          return $ ProResS p_prts_s' (ProRes_ cs_k' morech)
        st' =
          case c of
            (LL_Let _ _ (DLE_Wait _ amt) _) ->
              st {pst_interval = interval_add_from (pst_interval st) amt}
            _ -> st
    LLS_Stop at -> do
      let p_prts_s = pall st (ProRes_ mempty (ET_Stop at))
      return $ ProResS p_prts_s $ ProRes_ mempty False
    LLS_Only _at who body_l k_s -> do
      ProResS p_prts_k prchs_k <- epp_s st k_s
      let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
      let ProResL (ProRes_ who_body_cs who_body_lt) = epp_l body_l who_k_cs
      let who_prt_only =
            ProRes_ who_body_cs $ pltReplace ET_Com who_k_et who_body_lt
      let p_prts = M.insert who who_prt_only p_prts_k
      return $ ProResS p_prts prchs_k
    LLS_ToConsensus at from fs from_as msg amt_da amt_dv mtime cons -> do
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
      ProResC p_prts_cons (ProRes_ cons_vs ct_cons) <- epp_n st_cons cons
      let (fs_uses, fs_defns) =
            case fs of
              FS_Join dv -> (mempty, [dv])
              FS_Again dv -> (counts dv, mempty)
      let msg_and_defns = (msg <> [amt_dv] <> fs_defns)
      let int_ok_cs = counts int_ok
      let ok_cons_cs = int_ok_cs <> delay_cs <> count_rms msg_and_defns (fs_uses <> cons_vs) <> pst_forced_svs st
      (time_cons_cs, mtime'_ps) <- continue_time ok_cons_cs
      let svs = counts_nzs time_cons_cs
      let from_me = Just (from_as, amt_da, svs)
      let prev = pst_prev_handler st
      let this_h = C_Handler at int_ok fs prev svs msg amt_dv ct_cons
      let mk_et mfrom (ProRes_ cs_ et_) (ProRes_ mtime'_cs mtime') =
            ProRes_ cs_' $ ET_ToConsensus at fs prev which mfrom msg amt_dv mtime' et_
            where
              cs_' = mtime'_cs <> fs_uses <> counts mfrom <> count_rms msg_and_defns cs_
      let mk_sender_et = mk_et from_me
      let mk_receiver_et = mk_et Nothing
      let mk_p_prt p prt = mker prt mtime'
            where
              mtime' = mtime'_ps M.! p
              mker =
                case p == from of
                  True -> mk_sender_et
                  False -> mk_receiver_et
      let p_prts = M.mapWithKey mk_p_prt p_prts_cons
      addHandler st which this_h
      return $ ProResS p_prts (ProRes_ time_cons_cs True)

epp :: LLProg -> PLProg
epp (LLProg at (LLOpts {..}) ps s) = runST $ do
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
  return $ PLProg at (PLOpts {..}) pps cp
