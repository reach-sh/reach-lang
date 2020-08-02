module Reach.NL_EPP where

---import qualified Data.Sequence as Seq

import Control.Monad.ST
import qualified Data.Map.Strict as M
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
  }
  deriving (Eq)

data ProResS = ProResS SLPartETs Counts
  deriving (Eq, Show)

type MBack a res = (Counts -> (forall c. c -> PLCommon c) -> a -> res)
type MBackIf a res = (Counts -> (forall c. c -> c -> c -> PLCommon c) -> a -> a -> a -> res)

type MLookCommon = forall d lookres. ((Counts -> PLCommon d -> lookres) -> (Counts -> d -> lookres) -> Counts -> d -> lookres)

--- FIXME Try to simplify these types after all the cases are covered... maybe some of the values are always the same.
epp_m
  :: MBack a res
  -> MBackIf a res
  -> (a -> res)
  -> (a -> MLookCommon -> res)
  -> LLCommon a
  -> res
epp_m back backif skip look c =
  case c of
    LL_Return {} -> error "XXX"
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
    LL_Set {} -> error "XXX"
    LL_Claim at f ct ca k ->
      case ct of
        CT_Assert -> skip k
        CT_Possible -> skip k
        _ -> back cs' (PL_Claim at f ct ca) k
          where
            cs' = counts ca
    LL_LocalIf at ca t f k ->
      backif cs' (\x y z -> PL_LocalIf at ca x y z) t f k
      where cs' = counts ca

epp_l :: forall s. LLLocal -> Counts -> ST s ProResL
epp_l (LLL_Com com) _XXX_cs = epp_m back backif skip look com
  where
    back :: MBack LLLocal (ST s ProResL)
    back _XXX_cs' _XXX_mkpl _XXX_k = (error "XXX back")
    backif :: MBackIf LLLocal (ST s ProResL)
    backif _XXX_cs' _XXX_mkpl _XXX_t _XXX_f _XXX_k = (error "XXX back if")
    skip _XXX = (error "XXX skip")
    look _XXX _XXXcommon = (error "XXX look")

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
      where ProRes_ t_cs tt = p_prts_s_t M.! p
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
      epp_m back backif skip look c
      where
        back :: MBack LLConsensus (ST s ProResC)
        back cs' mkpl k = do
          ProResC p_prts_s (ProRes_ cs_k ct_k) <- skip k
          let p_prts_s' = extend_locals cs' mkpl p_prts_s
          let cs_k' = cs' <> cs_k
          let ct_k' = CT_Com $ mkpl ct_k
          return $ ProResC p_prts_s' (ProRes_ cs_k' ct_k')
        backif :: MBackIf LLConsensus (ST s ProResC)
        backif cs' mkpl t f k = do
          ProResC p_prts_s_t (ProRes_ cs_t ct_t) <- skip t
          ProResC p_prts_s_f (ProRes_ cs_f ct_f) <- skip f
          ProResC p_prts_s_k (ProRes_ cs_k ct_k) <- skip k
          let p_prts_s' = extend_localsif cs' mkpl p_prts_s_t p_prts_s_f p_prts_s_k
          let cs_k' = cs' <> cs_t <> cs_f <> cs_k
          let ct_k' = CT_Com $ mkpl ct_t ct_f ct_k
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
    LLC_If {} -> error "XXX"
    LLC_Transfer {} -> error "XXX"
    LLC_FromConsensus at1 _at2 s -> do
      ProResS p_prts_s cons_cs <- epp_s st s
      let svs = counts_nzs cons_cs
      let ctw = CT_Wait at1 svs
      return $ ProResC p_prts_s (ProRes_ cons_cs ctw)
    LLC_While {} -> error "XXX"
    LLC_Stop {} -> error "XXX"
    LLC_Continue {} -> error "XXX"

epp_s :: forall s. ProSt s -> LLStep -> ST s ProResS
epp_s st s =
  case s of
    LLS_Com c ->
      epp_m back backif skip look c
      where
        back :: MBack LLStep (ST s ProResS)
        back cs' mkpl k = do
          ProResS p_prts_s cr <- skip k
          let p_prts_s' = extend_locals cs' mkpl p_prts_s
          return $ ProResS p_prts_s' cr
        backif :: MBackIf LLStep (ST s ProResS)
        backif _XXX_cs' _XXX_mkpl _XXX_t _XXX_f _XXX_k =
          error "epp_s backif"
        skip k = epp_s st k
        look :: LLStep -> MLookCommon -> (ST s ProResS)
        look k common = do
          ProResS p_prts_s cr <- skip k
          let p_prts_s' = extend_locals_look common p_prts_s
          return $ ProResS p_prts_s' cr
    LLS_Stop at da -> do
      let p_prts_s = pall (ProRes_ (counts da) (ET_Stop at da))
      return $ ProResS p_prts_s mempty
    LLS_Only at who body_l k_s -> do
      ProResS p_prts_k prchs_k <- epp_s st k_s
      let ProRes_ who_k_cs who_k_et = p_prts_k M.! who
      ProResL (ProRes_ who_body_cs who_body_lt) <- epp_l body_l who_k_cs
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
      let st_cons = st {pst_prev_handler = which}
      ProResC p_prts_cons (ProRes_ cons_vs ct_cons) <- epp_n st_cons cons
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
      modifySTRef (pst_handlers st) $ ((CHandlers $ M.singleton which this_h) <>)
      return $ ProResS p_prts cons'_vs
  where
    pmap f = M.fromList $ map f $ pst_parts st
    pall x = pmap (\p -> (p, x))

epp :: LLProg -> PLProg
epp (LLProg at ps s) = runST $ do
  let SLParts p_to_ie = ps
  nhr <- newSTCounter
  hsr <- newSTRef $ mempty
  let st =
        ProSt
          { pst_prev_handler = 0
          , pst_handlers = hsr
          , pst_handlerc = nhr
          , pst_interval = default_interval
          , pst_parts = M.keys p_to_ie
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
