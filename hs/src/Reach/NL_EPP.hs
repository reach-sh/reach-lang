module Reach.NL_EPP where

---import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Control.Monad.ST
import Reach.STCounter
import Reach.NL_AST
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
    where m3 = M.unionWith (<>) m1 m2

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

data ProResC = ProResC (M.Map SLPart (ProRes_ ETail)) (ProRes_ (CTail, CHandlers))
  deriving (Eq, Show)

data ProSt s =
  ProSt { pst_prev_handler :: Int
        , pst_handlerc :: STCounter s
        , pst_interval :: CInterval
        , pst_parts :: [SLPart] }
  deriving (Eq)
data ProResS = ProResS (M.Map SLPart (ProRes_ ETail)) (ProRes_ CHandlers)
  deriving (Eq, Show)

{-
pro_com :: ProSt -> (ProSt -> a -> ProResS ETail) -> Bool -> LLCommon a -> Counts -> ProResS ETail
pro_com st iter isConsensus l topk_me_cs =
  case l of
    LL_Return at ->
      ProResS topk_me_cs mempty $ ET_Com $ PL_Return at
    LL_Let at dv de k ->
      ProResS me_cs con_cs s
      where
        (mde_cs, s) =
          case get_count dv (if isConsensus then k_con_cs else k_me_cs) of
            C_Never ->
              case expr_pure de of
                True -> (mempty, k')
                False -> (de_cs, ET_Com $ PL_Eff at de k')
            C_Once -> doLet PL_Once
            C_Many -> doLet PL_Many
        doLet lc = (de_cs, ET_Com $ PL_Let at lc dv de k')
        de_cs = counts de
        me_cs = mde_cs <> count_rms [dv] k_me_cs
        con_cs = mde_cons_cs <> count_rms [dv] k_con_cs
        mde_cons_cs = if isConsensus then mde_cs else mempty
        ProResS k_me_cs k_con_cs k' = iter st k
    LL_Var at dv k ->
      ProResS me_cs cons_cs s'
      where s' = ET_Com $ PL_Var at dv k'
            ProResS k_me_cs k_cons_cs k' = iter st k
            me_cs = count_rms [dv] k_me_cs
            cons_cs = count_rms [dv] k_cons_cs
    LL_Set at dv aa k ->
      ProResS me_cs cons_cs s'
      where s' = ET_Com $ PL_Set at dv aa k'
            ProResS k_me_cs k_cons_cs k' = iter st k
            me_cs = aa_cs <> k_me_cs
            aa_cs = counts aa
            cons_cs = maa_cons_cs <> k_cons_cs
            maa_cons_cs = if isConsensus then me_cs else mempty
    LL_Claim at f ct a k ->
      case ct of
        CT_Assert -> skip
        CT_Possible -> skip
        _ ->
          ProResS me_cs cons_cs s'
          where s' = ET_Com $ PL_Claim at f ct a k'
                ProResS k_me_cs k_cons_cs k' = iter st k
                me_cs = k_me_cs <> a_cs
                a_cs = counts a
                ma_cons_cs = if isConsensus then a_cs else mempty
                cons_cs = k_cons_cs <> ma_cons_cs
      where skip = iter st k
    LL_LocalIf at ca t f k ->
      ProResS me_cs con_cs s'
      where
        me_cs = counts ca <> t_me_cs <> f_me_cs <> k_me_cs
        con_cs = t_con_cs <> f_con_cs <> k_cons_cs
        s' = ET_Com $ PL_LocalIf at ca t' f' k'
        ProResS t_me_cs t_con_cs t' = iter st t
        ProResS f_me_cs f_con_cs f' = iter st f
        ProResS k_me_cs k_cons_cs k' = iter st k
        
pro_con :: SLPart -> ProSt -> LLConsensus -> ProResS ETail
pro_con me st l =
  case l of
    LLC_Com c -> pro_com st (pro_con me) True c mempty
    LLC_If at ca t f -> ProResS me_cs con_cs s'
      where
        me_cs = counts ca <> t_me_cs <> f_me_cs
        con_cs = t_con_cs <> f_con_cs
        s' = ET_If at ca t' f'
        ProResS t_me_cs t_con_cs t' = pro_con me st t
        ProResS f_me_cs f_con_cs f' = pro_con me st f
    LLC_FromConsensus _ _ s -> pro_s me st s
    LLC_Transfer _ _XXX_to aa k ->
      ProResS k_me_cs con_cs k'
      where
        con_cs = k_con_cs <> counts aa
        ProResS k_me_cs k_con_cs k' = pro_con me st k
    LLC_While _XXX_at _XXX_init _inv _XXX_cond _XXX_body _XXX_k ->
      error "XXX"
    LLC_Stop {} ->
      impossible "pro_con stop"
    LLC_Continue at da ->
      ProResS mempty (counts da) $ ET_Continue at da

pro_l :: Counts -> ProSt -> LLLocal -> ProResS ETail
pro_l me_cs st (LLL_Com c) = pro_com st (pro_l me_cs) False c me_cs

pro_s :: SLPart -> ProSt -> LLStep -> ProResS ETail
pro_s me st s =
  case s of
    LLS_Com c -> pro_com st (pro_s me) False c mempty
    LLS_Stop at da ->
      ProResS (counts da) mempty $ ET_Stop at da
    LLS_Only at who loc k ->
      ProResS me_cs k_con_cs s'
      where
        me_cs = k_me_cs <> loc_me_cs
        s' = ET_Seqn at loc' k'
        ProResS k_me_cs k_con_cs k' = pro_s me st k
        ProResS loc_me_cs _ loc' =
          case who == me of
            False -> ProResS mempty mempty $ ET_Com $ PL_Return at
            True -> pro_l k_me_cs st loc
    LLS_ToConsensus at who _XXX_fs send msg amt mtime k ->
      ProResS me_cs con_cs s'
      where
        s' = if isMe then mk_amt_s' else tc_s'
        mk_amt_s' = ET_Seqn amt_at amt_s' tc_s'
        ProResS amt_me_cs amt_con_cs amt_s' = pro_l (counts amt_da) st amt_l
        tc_s' = ET_ToConsensus at msend msg mtime' run_k'
        --- sim = error $ "XXX sim "
        LLBlock amt_at amt_l amt_da = amt
        (time_me_cs, time_cons_cs, mtime') =
          case mtime of
            Nothing -> (mempty, mempty, Nothing)
            Just (delay_a, delay_s) ->
              (time_me_cs_, time_cons_cs_, Just (delay_a, delay_t))
              where ProResS time_me_cs_ time_cons_cs_ delay_t = pro_s me st delay_s
        msend = if isMe then Just (send, amt_da) else Nothing
        isMe = who == me
        me_cs = time_me_cs <> send_cs <> count_rms msg (k_me_cs <> (if isMe then amt_me_cs else mempty))
        con_cs = time_cons_cs <> count_rms msg (k_con_cs <> (if isMe then amt_me_cs else mempty) <> amt_con_cs)
        send_cs = counts msend
        ProResS k_me_cs k_con_cs run_k' = pro_con me st k

project :: SLPart -> LLStep -> ETail
project me s = et
  where ProResS _ _ et = pro_s me st s
        st = ProSt ()

contract :: LLStep -> (Seq.Seq CHandler)
contract _s = error "XXX"
-}

epp_l :: ProSt s -> LLLocal -> ProResL -> ST s ProResL
epp_l _st (LLL_Com _com) _kr = error "XXX"

epp_n :: ProSt s -> LLConsensus -> ST s ProResC
epp_n _st _con = error "XXX"

epp_s :: ProSt s -> LLStep -> ST s ProResS
epp_s st s =
  case s of
    LLS_Stop at da -> do
      return $ ProResS (pall (ProRes_ (counts da) (ET_Stop at da))) (ProRes_ mempty mempty)
    LLS_Only at who body_l k_s -> do
      ProResS p_prts_k prchs_k <- epp_s st k_s
      let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
      ProResL (ProRes_ who_body_cs who_body_lt) <- epp_l st body_l (ProResL (ProRes_ who_k_cs $ PLTail $ PL_Return at))
      let who_prt_only = ProRes_ who_body_cs $ ET_Seqn at who_body_lt who_k_et
      let p_prts = M.insert who who_prt_only p_prts_k
      return $ ProResS p_prts prchs_k
    LLS_ToConsensus at from fs from_as msg amt mtime cons -> do
      let LLBlock _XXX_amt_at _XXX_amt_l amt_da = amt
      let mtime' = error $ "XXX " ++ show mtime
      let (fs_uses, fs_defns) =
            case fs of
              FS_Join dv -> (mempty, [dv])
              FS_Again dv -> (counts dv, mempty)
      let int = pst_interval st
      which <- incSTCounter $ pst_handlerc st
      let st_cons = st { pst_prev_handler = which }
      ProResC p_prts_cons (ProRes_ cons_vs (ct_cons, ct_chs)) <- epp_n st_cons cons
      let cons'_vs = count_rms msg cons_vs 
      let svs = counts_nzs cons'_vs
      let from_me = Just (from_as, amt_da, svs)
      let prev = pst_prev_handler st
      let this_h = C_Handler at int fs prev svs msg ct_cons
      let mk_et mfrom (ProRes_ cs_ et_) =
            ProRes_ cs_' $ ET_ToConsensus at fs which mfrom msg mtime' et_
            where
              --- XXX mtime' vs
              cs_' = fs_uses <> counts mfrom <> count_rms (msg <> fs_defns) cs_ 
      let mk_sender_et = mk_et from_me
      let mk_receiver_et = mk_et Nothing
      let mk_p_prt p prt =
            case p == from of
              True -> mk_sender_et prt
              False -> mk_receiver_et prt
      let p_prts = M.mapWithKey mk_p_prt p_prts_cons
      let prchs = ProRes_ cons'_vs $ (CHandlers $ M.singleton which this_h) <> ct_chs
      return $ ProResS p_prts prchs
  where
    pmap f = M.fromList $ map f $ pst_parts st
    pall x = pmap (\p -> (p, x))

epp :: LLProg -> PLProg
epp (LLProg at ps s) = runST $ do
  let SLParts p_to_ie = ps
  nhr <- newSTCounter
  let st = ProSt
           { pst_prev_handler = 0
           , pst_handlerc = nhr
           , pst_interval = default_interval
           , pst_parts = M.keys p_to_ie }
  ProResS p_prts (ProRes_ _ chs) <- epp_s st s
  let cp = CPProg at chs
  let mk_pp p ie =
        case M.lookup p p_prts of
          Just (ProRes_ _ et) -> EPProg at ie et
          Nothing ->
            impossible $ "part not in projection"
  let pps = EPPs $ M.mapWithKey mk_pp p_to_ie
  return $ PLProg at pps cp
        
        
