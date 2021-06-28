module Reach.Verify.Knowledge (verify_knowledge) where

import qualified Algorithm.Search as G
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.IORefRef
import Reach.Texty
import Reach.Util
import Reach.Verify.Shared
import System.Directory
import System.IO

--- Knowledge Graph & Queries
data Point
  = P_Var DLVar
  | P_Con
  | P_Interact SLPart String
  | P_Part SLPart
  | P_Map DLMVar
  deriving (Eq, Ord)

instance Pretty Point where
  pretty (P_Var v) = viaShow v
  pretty P_Con = "constant"
  pretty (P_Interact p m) = pretty p <> "." <> pretty m
  pretty (P_Part p) = pretty p
  pretty (P_Map mv) = pretty mv

data KCtxt = KCtxt
  { ctxt_mlog :: Maybe Handle
  , ctxt_loglvl :: IORefRef Int
  , ctxt_vst :: VerifySt
  , ctxt_ps :: [SLPart]
  , ctxt_back_ptrs :: S.Set Point
  , ctxt_kg :: IORefRef (M.Map Point (S.Set Point))
  }

klog :: KCtxt -> String -> IO ()
klog ctxt msg =
  case ctxt_mlog ctxt of
    Nothing -> mempty
    Just h -> do
      lvl <- readIORefRef $ ctxt_loglvl ctxt
      hPutStrLn h $ (replicate lvl ' ') <> msg

ctxtNewScope :: KCtxt -> IO a -> IO a
ctxtNewScope ctxt m =
  paramIORefRef (ctxt_kg ctxt) $ do
    let llr = (ctxt_loglvl ctxt)
    paramIORefRef llr $ do
      modifyIORefRef llr (1 +)
      m

ctxt_restrict :: KCtxt -> SLPart -> KCtxt
ctxt_restrict ctxt who = ctxt {ctxt_ps = [who]}

ctxt_add_back :: KCtxt -> DLArg -> KCtxt
ctxt_add_back ctxt a = ctxt {ctxt_back_ptrs = ctxt_back_ptrs ctxt <> all_points a}

know1 :: KCtxt -> Point -> Point -> IO ()
know1 ctxt from to = do
  klog ctxt $ show $ pretty to <+> "->" <+> pretty from
  let f = \case
        Nothing -> Just $ S.singleton from
        Just x -> Just $ S.insert from x
  modifyIORefRef (ctxt_kg ctxt) (M.alter f to)

query1 :: KCtxt -> SLPart -> Point -> IO (Maybe [Point])
query1 ctxt who what = do
  klog ctxt $ show $ "$" <+> pretty what <+> "->" <+> pretty who <+> "?"
  kg <- readIORefRef $ ctxt_kg ctxt
  let look p = case M.lookup p kg of
        Just c -> c
        Nothing -> mempty
  return $ G.dfs look (== (P_Part who)) what

displayPath :: SLPart -> (Point, Maybe [Point]) -> IO ()
displayPath who = \case
  (up, Just ps)
    | length ps > 1 -> do
      let ps' = drop 1 $ reverse ps
      case ps' of
        [] -> return ()
        (h : tl) ->
          putStrLn $
            "  " <> B.unpack who <> " could learn of " <> sp up <> " via " <> sp h <> ".\n\n  "
              <> publishInfo h
              <> "\n  ^ which contains info about "
              <> intercalate "\n  ^ which contains info about " (map bindingInfo $ tl <> [up])
              <> "\n"
  (up, _) ->
    putStrLn $ "  " <> B.unpack who <> " knows of " <> sp up <> " because it is published.\n"
  where
    sp = show . pretty
    bindingInfo = \case
      P_Var v@(DLVar _ (Just (at, _)) _ _) -> showBinding at $ viaShow v
      P_Var v@(DLVar at _ _ _) -> showBinding at $ viaShow v
      ow -> sp ow
    showBinding at v = show $ v <> " (defined at " <> pretty at <> ")"
    publishInfo = \case
      P_Var v@(DLVar at _ _ _) -> show $ viaShow v <> " was published at " <> pretty at
      ow -> sp ow

query :: KCtxt -> SrcLoc -> [SLCtxtFrame] -> Maybe B.ByteString -> SLPart -> S.Set Point -> IO ()
query ctxt at f mmsg who whats = do
  let whatsl = S.toList whats
  mpaths <- mapM (query1 ctxt who) whatsl
  let good = void $ incCounter $ vst_res_succ $ ctxt_vst ctxt
  let bad = void $ incCounter $ vst_res_fail $ ctxt_vst ctxt
  let disp = do
        cwd <- getCurrentDirectory
        putStrLn $ "Verification failed:"
        putStrLn $ "  of theorem " ++ "unknowable(" ++ show who ++ ", " ++ show (hcat $ punctuate " , " $ map pretty whatsl) ++ ")"
        putStrLn $ redactAbsStr cwd $ "  at " ++ show at
        case mmsg of
          Nothing -> mempty
          Just x -> putStrLn $ "  msg: " <> show x
        mapM_ (putStrLn . ("  " ++) . show) f
        putStrLn $ ""
        mapM_ (displayPath who) $ zip whatsl mpaths
  case getAll $ mconcatMap (All . (maybe False (const True))) mpaths of
    True -> bad >> disp
    False -> good

knows :: KCtxt -> Point -> S.Set Point -> IO ()
knows ctxt from tos = do
  let tons = S.union tos (ctxt_back_ptrs ctxt)
  mapM_ (know1 ctxt from) $ S.toList tons

class AllPoints a where
  all_points :: a -> S.Set Point

instance (Foldable f, AllPoints a) => AllPoints (f a) where
  all_points = foldMap all_points

instance AllPoints DLArg where
  all_points = \case
    DLA_Var v -> S.singleton $ P_Var v
    DLA_Constant _ -> S.singleton $ P_Con
    DLA_Literal _ -> S.singleton $ P_Con
    DLA_Interact who what _ -> S.singleton $ P_Interact who what

instance AllPoints DLTokenNew where
  all_points (DLTokenNew {..}) =
    all_points dtn_name
    <> all_points dtn_sym
    <> all_points dtn_url
    <> all_points dtn_metadata
    <> all_points dtn_supply

kgq_a_all :: AllPoints a => KCtxt -> a -> IO ()
kgq_a_all ctxt a =
  mapM_ (flip (knows ctxt) (all_points a)) $ map P_Part (ctxt_ps ctxt)

kgq_a_only :: KCtxt -> DLVar -> DLArg -> IO ()
kgq_a_only ctxt v a =
  knows ctxt (P_Var v) (all_points a)

kgq_a_onlym :: KCtxt -> Maybe DLVar -> DLArg -> IO ()
kgq_a_onlym ctxt mv a =
  case mv of
    Nothing -> mempty
    Just v -> kgq_a_only ctxt v a

kgq_v_onlym :: KCtxt -> Maybe DLVar -> S.Set Point -> IO ()
kgq_v_onlym ctxt mv ps =
  case mv of
    Nothing -> mempty
    Just v -> knows ctxt (P_Var v) ps

kgq_la :: KCtxt -> Maybe DLVar -> DLLargeArg -> IO ()
kgq_la ctxt mv = \case
  DLLA_Array _ as -> moreas as
  DLLA_Tuple as -> moreas as
  DLLA_Obj m -> moreas $ M.elems m
  DLLA_Data _ _ a -> onea a
  DLLA_Struct kvs -> moreas $ map snd kvs
  where
    moreas = mconcatMap onea
    onea = kgq_a_onlym ctxt mv

kgq_e :: KCtxt -> Maybe DLVar -> DLExpr -> IO ()
kgq_e ctxt mv = \case
  DLE_Arg _ a -> kgq_a_onlym ctxt mv a
  DLE_LArg _ la -> kgq_la ctxt mv la
  DLE_Impossible {} -> mempty
  DLE_PrimOp _ _ as -> kgq_la ctxt mv (DLLA_Tuple as)
  DLE_ArrayRef _ a e -> kgq_la ctxt mv (DLLA_Tuple [a, e])
  DLE_ArraySet _ a e n -> kgq_la ctxt mv (DLLA_Tuple [a, e, n])
  DLE_ArrayConcat _ x_da y_da ->
    kgq_a_onlym ctxt mv x_da >> kgq_a_onlym ctxt mv y_da
  DLE_ArrayZip _ x_da y_da ->
    kgq_a_onlym ctxt mv x_da >> kgq_a_onlym ctxt mv y_da
  DLE_TupleRef _ a _ -> kgq_a_onlym ctxt mv a
  DLE_ObjectRef _ a _ -> kgq_a_onlym ctxt mv a
  DLE_Interact _ _ who what t as ->
    kgq_la ctxt mv (DLLA_Tuple $ (DLA_Interact who what t) : as)
  DLE_Digest _ _ ->
    --- This line right here is where all the magic happens
    mempty
  DLE_Claim at f ct _ mmsg -> this
    where
      this =
        case ct of
          CT_Assert -> mempty
          CT_Assume _ -> mempty
          CT_Require -> mempty
          CT_Possible -> mempty
          CT_Unknowable who whats ->
            mapM_ query_one whats
            where
              query_one what =
                query ctxt at f mmsg who $ all_points what
  DLE_Transfer _ _ amt _ ->
    kgq_a_all ctxt amt
  DLE_TokenInit _ tok ->
    kgq_a_all ctxt tok
  DLE_CheckPay _ _ amt _ ->
    kgq_a_all ctxt amt
  DLE_Wait _ amt ->
    kgq_a_all ctxt amt
  DLE_PartSet _ _ arg ->
    kgq_a_all ctxt arg
  DLE_MapRef _ mpv _ ->
    kgq_v_onlym ctxt mv $ S.singleton $ P_Map mpv
  DLE_MapSet _ mpv _ mva ->
    knows ctxt (P_Map mpv) $ maybe mempty all_points mva
  DLE_Remote _ _ av _ pamt as _ -> do
    kgq_pa ctxt pamt
    kgq_la ctxt mv $ DLLA_Tuple $ av : as
  DLE_TokenNew _ tns ->
    kgq_a_all ctxt tns
  DLE_TokenBurn _ t a ->
    kgq_a_all ctxt [t, a]
  DLE_TokenDestroy _ t ->
    kgq_a_all ctxt t

kgq_m :: KCtxt -> DLStmt -> IO ()
kgq_m ctxt = \case
  DL_Nop _ -> mempty
  DL_Let _ lv de -> kgq_e ctxt (lv2mdv lv) de
  DL_ArrayMap _ ans x a (DLBlock _ _ f r) ->
    kgq_a_only ctxt a x
      >> kgq_a_only ctxt ans r
      >> kgq_l ctxt f
  DL_ArrayReduce _ ans x z b a (DLBlock _ _ f r) ->
    kgq_a_only ctxt b z
      >> kgq_a_only ctxt a x
      >> kgq_a_only ctxt ans r
      >> kgq_l ctxt f
  DL_Var {} -> mempty
  DL_Set _ dv da -> kgq_a_only ctxt dv da
  DL_LocalIf _ ca t f -> kgq_l ctxt' t >> kgq_l ctxt' f
    where
      ctxt' = ctxt_add_back ctxt ca
  DL_LocalSwitch _ ov csm -> mapM_ cm1 csm
    where
      oa = DLA_Var ov
      ctxt' = ctxt_add_back ctxt oa
      cm1 (mov', l) =
        kgq_a_onlym ctxt mov' oa
          >> kgq_l ctxt' l
  DL_Only _at (Left who) loc ->
    kgq_l (ctxt_restrict ctxt who) loc
  DL_Only {} -> impossible $ "right only before EPP"
  DL_MapReduce _ _ ans x z b a (DLBlock _ _ f r) ->
    kgq_a_only ctxt b z
      >> knows ctxt (P_Var a) (S.singleton (P_Map x))
      >> kgq_a_only ctxt ans r
      >> kgq_l ctxt f
  DL_LocalDo _ t -> kgq_l ctxt t

kgq_l :: KCtxt -> DLTail -> IO ()
kgq_l ctxt = \case
  DT_Return _ -> mempty
  DT_Com m k -> kgq_m ctxt m >> kgq_l ctxt k

kgq_asn :: KCtxt -> DLAssignment -> IO ()
kgq_asn ctxt (DLAssignment m) = mapM_ (uncurry (kgq_a_only ctxt)) $ M.toList m

kgq_asn_def :: KCtxt -> DLAssignment -> IO ()
kgq_asn_def ctxt (DLAssignment m) = mapM_ (kgq_a_all ctxt . DLA_Var) $ M.keys m

kgq_n :: KCtxt -> LLConsensus -> IO ()
kgq_n ctxt = \case
  LLC_Com m k -> kgq_m ctxt m >> kgq_n ctxt k
  LLC_If _ ca t f ->
    ctxtNewScope ctxt' (kgq_n ctxt' t)
      >> ctxtNewScope ctxt' (kgq_n ctxt' f)
    where
      ctxt' = ctxt_add_back ctxt ca
  LLC_Switch _ ov csm ->
    mapM_ cm1 csm
    where
      oa = DLA_Var ov
      ctxt' = ctxt_add_back ctxt oa
      cm1 (mov', n) =
        ctxtNewScope ctxt' $
          kgq_a_onlym ctxt' mov' oa
            >> kgq_n ctxt' n
  LLC_FromConsensus _ _ k ->
    kgq_s ctxt k
  LLC_While _ asn _ (DLBlock _ _ cond_l ca) body k ->
    kgq_asn_def ctxt asn
      >> kgq_asn ctxt asn
      >> kgq_l ctxt cond_l
      >> kgq_n ctxt' body
      >> kgq_n ctxt' k
    where
      ctxt' = ctxt_add_back ctxt ca
  LLC_Continue _ asn ->
    kgq_asn ctxt asn
  LLC_ViewIs _ _ _ _ k ->
    kgq_n ctxt k

kgq_pa :: KCtxt -> DLPayAmt -> IO ()
kgq_pa ctxt (DLPayAmt {..}) = do
  let one = kgq_a_all ctxt
  one pa_net
  mapM_ (one . fst) pa_ks

kgq_s :: KCtxt -> LLStep -> IO ()
kgq_s ctxt = \case
  LLS_Com m k -> kgq_m ctxt m >> kgq_s ctxt k
  LLS_Stop {} -> mempty
  LLS_ToConsensus _ send recv mtime ->
    ctxtNewScope ctxt (maybe mempty (kgq_s ctxt . snd) mtime)
      >> mapM_ (ctxtNewScope ctxt . go) (M.toList send)
    where
      DLRecv whov msgvs timev (_last_timev, next_n) = recv
      common =
        kgq_a_all ctxt (DLA_Var whov)
          >> kgq_a_all ctxt (DLA_Var timev)
          >> mapM (kgq_a_all ctxt) (map DLA_Var msgvs)
          >> kgq_n ctxt next_n
      go (_, DLSend _ msgas amta whena) = do
        mapM_ (uncurry (kgq_a_only ctxt)) (zip msgvs msgas)
          >> kgq_pa ctxt amta
          -- This is a bit suspicious: we can't necessarily know what the value
          -- of this is just because of things being published, because they
          -- might be dishonest
          >> kgq_a_all ctxt whena
          >> common

kgq_pie1 :: KCtxt -> SLPart -> SLVar -> IO ()
kgq_pie1 ctxt who what = knows ctxt (P_Part who) $ S.singleton $ P_Interact who what

kgq_pie :: KCtxt -> SLPart -> InteractEnv -> IO ()
kgq_pie ctxt who (InteractEnv m) =
  (knows ctxt (P_Part who) $ S.singleton $ P_Con)
    >> (mapM_ (kgq_pie1 ctxt who) $ M.keys m)

kgq_lp :: Maybe Handle -> VerifySt -> LLProg -> IO ()
kgq_lp mh vst (LLProg _ (LLOpts {}) (SLParts psm) _dli _ _ s) = do
  putStrLn $ "Verifying knowledge assertions"
  let ps = M.keys psm
  llr <- newIORefRef 0
  kgr <- newIORefRef mempty
  let ctxt =
        KCtxt
          { ctxt_mlog = mh
          , ctxt_loglvl = llr
          , ctxt_vst = vst
          , ctxt_ps = ps
          , ctxt_back_ptrs = mempty
          , ctxt_kg = kgr
          }
  mapM_ (uncurry (kgq_pie ctxt)) $ M.toList psm
  kgq_s ctxt s

verify_knowledge :: Maybe FilePath -> VerifySt -> LLProg -> IO ()
verify_knowledge mout vst lp =
  case mout of
    Nothing -> go Nothing
    Just p -> withFile p WriteMode (go . Just)
  where
    go mh = kgq_lp mh vst lp
