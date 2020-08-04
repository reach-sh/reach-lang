module Reach.NL_EPP where

---import qualified Data.Sequence as Seq

import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef
import Reach.NL_AST
import Reach.STCounter
import Reach.Util

--- Variable Usage Counting
data Count
  = C_Never
  | C_Once
  | C_Many
  deriving (Show, Eq)

instance Monoid Count where
  mempty = C_Never

instance Semigroup Count where
  C_Never <> x = x
  x <> C_Never = x
  C_Once <> C_Once = C_Many
  _ <> _ = C_Many

newtype Counts = Counts (M.Map DLVar Count)
  deriving (Show, Eq)

instance Monoid Counts where
  mempty = Counts mempty

instance Semigroup Counts where
  (Counts m1) <> (Counts m2) = Counts m3
    where
      m3 = M.unionWith (<>) m1 m2

get_count :: DLVar -> Counts -> Count
get_count v (Counts m) =
  case M.lookup v m of
    Nothing -> mempty
    Just x -> x

count_rms :: [DLVar] -> Counts -> Counts
count_rms vs (Counts cs) = Counts $ foldr M.delete cs vs

counts_nzs :: Counts -> [DLVar]
counts_nzs (Counts cs) =
  map fst $ filter (\(_, c) -> c /= C_Never) $ M.toList cs

class Countable a where
  counts :: a -> Counts

instance (Countable x, Countable y) => Countable (x, y) where
  counts (a, b) = counts a <> counts b

instance (Countable x, Countable y, Countable z) => Countable (x, y, z) where
  counts (a, b, c) = counts a <> counts b <> counts c

instance Countable v => Countable (Maybe v) where
  counts Nothing = mempty
  counts (Just x) = counts x

instance Countable v => Countable [v] where
  counts l = mconcat $ map counts l

instance Countable v => Countable (M.Map k v) where
  counts m = counts $ M.elems m

instance Countable DLVar where
  counts dv = Counts $ M.singleton dv C_Once

instance Countable DLArg where
  counts a =
    case a of
      DLA_Var v -> counts v
      DLA_Con {} -> mempty
      DLA_Array as -> counts as
      DLA_Obj as -> counts as
      DLA_Interact {} -> mempty

instance Countable DLExpr where
  counts e =
    case e of
      DLE_PrimOp _ _ as -> counts as
      DLE_ArrayRef _ aa ea -> counts [aa, ea]
      DLE_Interact _ _ as -> counts as
      DLE_Digest _ as -> counts as

instance Countable DLAssignment where
  counts (DLAssignment m) = counts m

--- Projection
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
  --- FIXME These would be maps when we have labelled loops
  , pst_loop_vars :: Maybe [DLVar]
  , pst_loop_num :: Maybe Int
  , pst_forced_svs :: Counts
  }
  deriving (Eq)

newHandler :: ProSt s -> ST s Int
newHandler st = incSTCounter $ pst_handlerc st

addHandler :: ProSt s -> Int -> CHandler -> ST s ()
addHandler st which this_h =
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
epp_m done back skip look c =
  case c of
    LL_Return at -> done at
    LL_Let at dv de k ->
      look
        k
        (\back' skip' k_cs k' ->
           let cs' = counts de <> count_rms [dv] k_cs
               doLet lc = back' cs' (PL_Let at lc dv de k')
            in case get_count dv k_cs of
                 C_Never ->
                   case expr_pure de of
                     True -> skip' k_cs k'
                     False -> back' cs' (PL_Eff at de k')
                 C_Once -> doLet PL_Once
                 C_Many -> doLet PL_Many)
    LL_Var at dv k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cs' = count_rms [dv] k_cs
            in back' cs' (PL_Var at dv k'))
    LL_Set at dv da k ->
      back cs' (PL_Set at dv da) k
      where
        cs' = counts da
    LL_Claim at f ct ca k ->
      case ct of
        CT_Assert -> skip k
        CT_Possible -> skip k
        _ -> back cs' (PL_Claim at f ct ca) k
          where
            cs' = counts ca
    LL_LocalIf at ca t f k ->
      look
        k
        (\back' _skip' k_cs k' ->
           let cs' = counts ca <> t'_cs <> f'_cs
               ProResL (ProRes_ t'_cs t') = epp_l t k_cs
               ProResL (ProRes_ f'_cs f') = epp_l f k_cs
            in back' cs' $ PL_LocalIf at ca t' f' k')

epp_l :: LLLocal -> Counts -> ProResL
epp_l (LLL_Com com) ok_cs = epp_m done back skip look com
  where
    done :: MDone ProResL
    done rat =
      ProResL (ProRes_ ok_cs $ PLTail $ PL_Return rat)
    back :: MBack LLLocal ProResL
    back cs' mkpl k = ProResL $ ProRes_ (cs' <> k_cs) $ PLTail (mkpl k')
      where ProResL (ProRes_ k_cs k') = skip k
    skip k = epp_l k ok_cs
    look :: LLLocal -> MLookCommon -> ProResL
    look k common = ProResL $ common back' skip' k_cs k'
      where ProResL (ProRes_ k_cs k') = skip k
            skip' = ProRes_
            back' k_cs' k'' = ProRes_ k_cs' $ PLTail k''

extend_locals :: Counts -> (forall c. c -> PLCommon c) -> SLPartETs -> SLPartETs
extend_locals cs' mkpl p_prts_s = M.map add p_prts_s
  where
    add :: ProRes_ ETail -> ProRes_ ETail
    add (ProRes_ p_cs p_et) =
      ProRes_ (cs' <> p_cs) (ET_Com $ mkpl p_et)

extend_localsif :: Counts -> (forall c. c -> c -> c -> PLCommon c) -> SLPartETs -> SLPartETs -> SLPartETs -> SLPartETs
extend_localsif cs' mkpl p_prts_s_t p_prts_s_f p_prts_s_k = M.mapWithKey add p_prts_s_k
  where
    add :: SLPart -> ProRes_ ETail -> ProRes_ ETail
    add p (ProRes_ k_cs kt) =
      ProRes_ (cs' <> t_cs <> f_cs <> k_cs) (ET_Com $ mkpl tt ft kt)
      where
        ProRes_ t_cs tt = p_prts_s_t M.! p
        ProRes_ f_cs ft = p_prts_s_f M.! p

extend_locals_look :: MLookCommon -> SLPartETs -> SLPartETs
extend_locals_look common p_prts_s = M.map add p_prts_s
  where
    add (ProRes_ p_cs p_et) =
      common back' skip' p_cs p_et
      where
        skip' = ProRes_
        back' p_cs' p_ct' = ProRes_ p_cs' $ ET_Com $ p_ct'

epp_n :: forall s. ProSt s -> LLConsensus -> ST s ProResC
epp_n st n =
  case n of
    LLC_Com c ->
      epp_m done back skip look c
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
            where ProRes_ cs_p_t et_p_t = p_prts_t M.! p
                  ProRes_ cs_p_f et_p_f = p_prts_f M.! p
                  cs_p = counts ca <> cs_p_t <> cs_p_f
                  et_p = ET_If at ca et_p_t et_p_f
      let p_prts' = pmap st mkp 
      let cs' = counts ca <> cs_t <> cs_f
      let ct' = CT_If at ca ct_t ct_f
      return $ ProResC p_prts' (ProRes_ cs' ct')
    LLC_Transfer at to amt k -> do
      ProResC p_prts_s (ProRes_ cs_k ct_k) <- epp_n st k
      let cs_k' = cs_k <> counts amt
      let ct_k' = CT_Transfer at to amt ct_k
      return $ ProResC p_prts_s (ProRes_ cs_k' ct_k')
    LLC_FromConsensus at1 _at2 s -> do
      ProResS p_prts_s (ProRes_ cons_cs more_chb) <- epp_s st s
      let svs = counts_nzs cons_cs
      let ctw =
            case more_chb of
              True -> CT_Wait at1 svs
              False -> CT_Halt at1
      return $ ProResC p_prts_s (ProRes_ cons_cs ctw)
    LLC_While at asn _inv cond body k -> do
      ProResC p_prts_k (ProRes_ cs_k ct_k) <- epp_n st k
      let loop_vars = assignment_vars asn
      loop_num <- newHandler st
      let st_body = st { pst_prev_handler = loop_num
                       , pst_loop_vars = Just loop_vars
                       , pst_loop_num = Just loop_num }
      ProResC p_prts_body (ProRes_ cs_body ct_body) <- epp_n st_body body
      let LLBlock cond_at cond_l cond_da = cond
      let post_cond_cs = counts cond_da <> cs_body <> cs_k
      let ProResL (ProRes_ cs_cond pt_cond) = epp_l cond_l post_cond_cs
      let loop_if = CT_If cond_at cond_da ct_body ct_k
      let loop_top = CT_Seqn cond_at pt_cond loop_if
      let loop_svs = counts_nzs $ count_rms loop_vars $ cs_cond
      let this_h = C_Loop at loop_svs loop_vars loop_top
      addHandler st loop_num this_h
      let ct' = CT_Jump at loop_num loop_svs asn
      let cons_cs' = counts loop_svs <> counts asn
      let mkp p = ProRes_ cs_p t_p
            where ProRes_ p_cs_k t_k = p_prts_k M.! p
                  ProRes_ p_cs_body t_body = p_prts_body M.! p
                  cs_p = counts asn <> (count_rms loop_vars $ counts cond_da <> p_cs_body <> p_cs_k)
                  t_p = ET_While at asn (PLBlock cond_at pt_cond cond_da) t_body t_k
      let p_prts' = pmap st mkp
      return $ ProResC p_prts' (ProRes_ cons_cs' ct')
    LLC_Continue at asn -> do
      let this_loop = fromMaybe (impossible "no loop") $ pst_loop_num st
      let these_loop_vars = fromMaybe (impossible "no loop") $ pst_loop_vars st
      let asn_cs = counts asn
      let cons_cs' = asn_cs <> counts these_loop_vars
      let ct' = CT_Jump at this_loop these_loop_vars asn
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
          return $ ProResS (pall st (ProRes_ mempty $ ET_Com $ PL_Return rat)) (ProRes_ mempty False)
        back :: MBack LLStep (ST s ProResS)
        back cs' mkpl k = do
          ProResS p_prts_s cr <- skip k
          let p_prts_s' = extend_locals cs' mkpl p_prts_s
          return $ ProResS p_prts_s' cr
        skip k = epp_s st k
        look :: LLStep -> MLookCommon -> (ST s ProResS)
        look k common = do
          ProResS p_prts_s cr <- skip k
          let p_prts_s' = extend_locals_look common p_prts_s
          return $ ProResS p_prts_s' cr
    LLS_Stop at da -> do
      let p_prts_s = pall st (ProRes_ (counts da) (ET_Stop at da))
      return $ ProResS p_prts_s $ ProRes_ mempty False
    LLS_Only at who body_l k_s -> do
      ProResS p_prts_k prchs_k <- epp_s st k_s
      let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
      let ProResL (ProRes_ who_body_cs who_body_lt) = epp_l body_l who_k_cs
      let who_prt_only = ProRes_ who_body_cs $ ET_Seqn at who_body_lt who_k_et
      let p_prts = M.insert who who_prt_only p_prts_k
      return $ ProResS p_prts prchs_k
    LLS_ToConsensus at from fs from_as msg amt_da mtime cons -> do
      let prev_int = pst_interval st
      which <- newHandler st
      let (int_ok, delay_cs, continue_time ) =
            case mtime of
              Nothing -> (prev_int, mempty, continue_time_)
                where continue_time_ ok_cons_cs =
                        return $ (ok_cons_cs, pall st $ ProRes_ mempty Nothing)
              Just (delaya, delays) -> (int_ok_, delay_cs_, continue_time_)
                where delay_cs_ = counts delaya
                      int_to = interval_add_from prev_int delaya
                      int_ok_ = interval_add_to prev_int delaya
                      continue_time_ ok_cons_cs = do
                        let st_to = st { pst_interval = int_to
                                       , pst_forced_svs = ok_cons_cs }
                        ProResS delay_prts (ProRes_ tcons_cs _) <-
                          epp_s st_to delays
                        let cs' = delay_cs_ <> tcons_cs
                        let update (ProRes_ tk_cs tk_et) =
                              ProRes_ (tk_cs <> delay_cs_) (Just (delaya, tk_et))
                        return $ (cs', M.map update delay_prts)
      let st_cons = st { pst_prev_handler = which
                       , pst_interval = int_ok }
      ProResC p_prts_cons (ProRes_ cons_vs ct_cons) <- epp_n st_cons cons
      let ok_cons_cs = delay_cs <> count_rms msg cons_vs <> pst_forced_svs st
      (time_cons_cs, mtime'_ps) <- continue_time ok_cons_cs
      let (fs_uses, fs_defns) =
            case fs of
              FS_Join dv -> (mempty, [dv])
              FS_Again dv -> (counts dv, mempty)
      let svs = counts_nzs time_cons_cs
      let from_me = Just (from_as, amt_da, svs)
      let prev = pst_prev_handler st
      let this_h = C_Handler at int_ok fs prev svs msg ct_cons
      let mk_et mfrom (ProRes_ cs_ et_) (ProRes_ mtime'_cs mtime') =
            ProRes_ cs_' $ ET_ToConsensus at fs which mfrom msg mtime' et_
            where
              cs_' = mtime'_cs <> fs_uses <> counts mfrom <> count_rms (msg <> fs_defns) cs_
      let mk_sender_et = mk_et from_me
      let mk_receiver_et = mk_et Nothing
      let mk_p_prt p prt = mker prt mtime'
            where mtime' = mtime'_ps M.! p
                  mker =
                    case p == from of
                      True -> mk_sender_et 
                      False -> mk_receiver_et
      let p_prts = M.mapWithKey mk_p_prt p_prts_cons
      addHandler st which this_h
      return $ ProResS p_prts (ProRes_ time_cons_cs True)

epp :: LLProg -> PLProg
epp (LLProg at ps s) = runST $ do
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
          , pst_loop_vars = Nothing
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
  return $ PLProg at pps cp
