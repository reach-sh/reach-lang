module Reach.NL_EPP where

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Reach.NL_AST
import Reach.Util

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

class Countable a where
  counts :: a -> Counts

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

data ProSt = ProSt ()
  deriving (Eq, Show)

data ProResS a = ProResS Counts Counts a
  deriving (Eq, Show)

instance Functor ProResS where
  fmap f (ProResS x y a) = (ProResS x y (f a))

pro_com :: ProSt -> (ProSt -> a -> ProResS ETail) -> Bool -> LLCommon a -> ProResS ETail
pro_com st iter isConsensus l =
  case l of
    LL_Return at ->
      ProResS mempty mempty $ ET_Com $ PL_Return at
    LL_Let at dv de k ->
      ProResS me_cs con_cs s
      where
        s =
          case get_count dv (if isConsensus then k_con_cs else k_me_cs) of
            C_Never ->
              case expr_pure de of
                True -> k'
                False -> ET_Com $ PL_Eff at de k'
            C_Once -> doLet PL_Once
            C_Many -> doLet PL_Many
        doLet lc = ET_Com $ PL_Let at lc dv de k'
        de_cs = counts de
        me_cs = de_cs <> count_rms [dv] k_me_cs
        con_cs = mde_cs <> count_rms [dv] k_con_cs
        mde_cs = if isConsensus then de_cs else mempty
        ProResS k_me_cs k_con_cs k' = iter st k
    LL_Var {} ->
      error "XXX var"
    LL_Set {} ->
      error "XXX set"
    LL_Claim {} ->
      error "XXX claim"
    LL_LocalIf {} ->
      error "XXX local if"

pro_con :: SLPart -> ProSt -> LLConsensus -> ProResS ETail
pro_con me st l =
  case l of
    LLC_Com c -> pro_com st (pro_con me) True c
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

pro_l :: ProSt -> LLLocal -> ProResS ETail
pro_l st (LLL_Com c) = pro_com st pro_l False c

pro_s :: SLPart -> ProSt -> LLStep -> ProResS ETail
pro_s me st s =
  case s of
    LLS_Com c -> pro_com st (pro_s me) False c
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
            True -> pro_l st loc
    LLS_ToConsensus at who send msg amt mtime k ->
      ProResS me_cs con_cs s'
      where
        s' = ET_ToConsensus at msend msg amt' mtime' sim k'
        sim = error $ "XXX sim "
        amt' = error $ "XXX amt " ++ show amt
        mtime' = error $ "XXX mtime " ++ show mtime
        msend = if isMe then Just send else Nothing
        isMe = who == me
        me_cs = send_cs <> count_rms msg k_me_cs
        con_cs = count_rms msg k_con_cs
        send_cs = if isMe then counts send else mempty
        ProResS k_me_cs k_con_cs k' = pro_con me st k

project :: SLPart -> LLStep -> ETail
project me s = et
  where ProResS _ _ et = pro_s me st s
        st = ProSt ()

contract :: LLStep -> (Seq.Seq CHandler)
contract _s = error "XXX"

epp :: LLProg -> PLProg
epp (LLProg at ps s) = PLProg at pps cp
  where SLParts p_to_ie = ps
        pps = M.mapWithKey mk_pp p_to_ie
        mk_pp p ie = EPProg ie $ project p s
        cp = CPProg at $ contract s
