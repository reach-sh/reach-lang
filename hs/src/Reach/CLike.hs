module Reach.CLike
  ( clike
  , nameMap
  , nameReturn
  ) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AddCounts
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.CP
import Reach.AST.PL
import Reach.AST.CL
import Reach.BinaryLeafTree
import Reach.Counter
import Reach.Sanitize
import Reach.Util

type App = ReaderT Env IO

data Env = Env
  { eDefsR :: IORef CLDefs
  , eFunsR :: IORef CLFuns
  , eAPIR :: IORef CLAPI
  , eStateR :: IORef CLState
  , eCounter :: Counter
  }

instance HasStateR Env where
  getStateR = eStateR

instance HasCounter Env where
  getCounter = eCounter

nameBase :: CLVar -> CLVar -> CLVar
nameBase pre post = "_reach" <> pre <> "_" <> post
nameInt :: CLVar -> Int -> CLVar
nameInt pre i = nameBase pre (bpack $ show i)
nameLoop :: Int -> CLVar
nameLoop = nameInt "l"
nameMeth :: Int -> CLVar
nameMeth = nameInt "p"
nameMethi :: Int -> CLVar
nameMethi = nameInt "i"
nameMap :: Int -> CLVar
nameMap = nameInt "m"
nameApi :: CLVar -> CLVar
nameApi = nameBase "a"
nameReturn :: CLVar -> CLVar
nameReturn = nameBase "r"

class HasStateR a where
  getStateR :: a -> IORef CLState

recordState :: (HasStateR e) => Int -> [DLVar] -> ReaderT e IO ()
recordState which svs = do
  sr <- asks getStateR
  liftIO $ modifyIORef sr $ \m ->
    case M.lookup which m of
      Just svs' ->
        case svs == svs' of
          True -> m
          False -> impossible $ "recordState: mismatch " <> show which <> " " <> show svs <> " vs " <> show svs'
      Nothing ->
        M.insert which svs m

env_insert :: (Show k, Ord k) => k -> v -> M.Map k v -> M.Map k v
env_insert k v m =
  case M.member k m of
    True -> impossible $ show k <> " is already defined"
    False -> M.insert k v m

env_insert_ :: (Show k, Ord k) => (Env -> IORef (M.Map k v)) -> k -> v -> App ()
env_insert_ f k v = do
  er <- asks f
  liftIO $ modifyIORef er $ env_insert k v

def :: CLVar -> CLDef -> App ()
def v d = env_insert_ eDefsR v d

funi :: CLVar -> CLIntFun -> App ()
funi n d = env_insert_ eFunsR n d

fune :: CLVar -> CLExtFun -> App ()
fune n d = env_insert_ eAPIR s d
  where
    s = CLSym n dom rng
    dom = map varLetType $ clf_dom cef_fun
    rng = cef_rng
    CLExtFun {..} = d

funw :: CLVar -> [CLVar] -> SrcLoc -> [DLVarLet] -> Bool -> DLType -> CLExtKind -> Maybe CLVar -> CLTail -> App ()
funw ni ns at int_dom clf_view cef_rng cef_kind mret intt = do
  let clf_at = at
  let cif_mwhich = case cef_kind of
                     CE_Publish n -> Just n
                     _ -> Nothing
  let cif_fun = CLFun { clf_tail = intt, clf_dom = int_dom, ..}
  let di = CLIntFun {..}
  ext_dom_vs <- mapM freshenVar $ map varLetVar int_dom
  let extt = CL_Jump at ni (map DLA_Var ext_dom_vs) False (Just mret)
  let ext_dom = map (DLVarLet (Just DVC_Once)) ext_dom_vs
  let cef_fun = CLFun { clf_tail = extt, clf_dom = ext_dom, ..}
  let de = CLExtFun {..}
  funi ni di
  -- XXX optimize when one ns?
  -- I can't in Solidity because of the argument number issue, but I can in AVM
  -- and EVM
  forM_ ns $ flip fune de

class CLike a where
  cl :: a -> App ()

newtype DLMI = DLMI (DLMVar, DLMapInfo)

instance CLike DLMI where
  cl (DLMI ((DLMVar i), (DLMapInfo {..}))) =
    -- XXX generate the map reference functions here?
    def (nameMap i) (CLD_Map dlmi_kt dlmi_ty)

data CMap t k v = CMap ((k, v) -> t) (M.Map k v)

instance (CLike t) => CLike (CMap t k v) where
  cl (CMap f m) = forM_ (map f $ M.toAscList m) cl

instance CLike DLInit where
  cl (DLInit {..}) = cl $ CMap DLMI dli_maps

data CILMap a v = CILMap ((SLPart, v) -> a) (InterfaceLikeMap v)

instance (CLike a) => CLike (CILMap a v) where
  cl (CILMap f m) = cl $ CMap f $ flattenInterfaceLikeMap m

newtype DLEI = DLEI (SLPart, [DLType])

instance CLike DLEI where
  cl (DLEI (p, ts)) = def p $ CLD_Evt ts

instance CLike DLEvents where
  cl = cl . CILMap DLEI

data CLViewY = CLViewY
  { cvy_svs :: [DLVar]
  , cvy_body :: DLExportBlock
  }

instance CLikeF CLViewY where
  clf (CLViewY {..}) = do
    prev <- fromMaybe (impossible "clf cvy") <$> asks f_staten
    let svs_vls = map v2vl cvy_svs
    let (DLinExportBlock _ mfargs (DLBlock at _ t r)) = cvy_body
    rng <- asks f_rng
    let k = CL_Com (CLMemorySet at rng r) $ CL_Halt at HM_Pure
    let t0 = dtReplace (CL_Com . CLDL) k t
    let fargs = fromMaybe mempty mfargs
    dom <- asks f_dom
    let bind (dvl, avl) = CL_Com $ CLDL $ DL_Let at (vl2lv avl) $ DLE_Arg at $ DLA_Var $ varLetVar dvl
    let t1 = foldr bind t0 $ zip dom fargs
    recordState prev cvy_svs
    let t2 = CL_Com (CLStateBind at True svs_vls prev) t1
    return $ t2

type CLViewX = FunInfo CLViewY

type CLViewsX = M.Map SLPart CLViewX

viewReorg :: DLViewsX -> CLViewsX
viewReorg (DLViewsX vs vis) = vx
  where
    igo (ViewInfo svs vsi) = (svs, flattenInterfaceLikeMap vsi)
    vism = M.map igo vis
    vx = M.mapWithKey go $ flattenInterfaceLikeMap_ (,) vs
    go k (p, (DLView {..})) = FunInfo {..}
      where
        fi_noCheck = False
        fi_isApi = False
        fi_at = dvw_at
        (fi_dom, fi_rng) = itype2arr dvw_it
        fi_as = map (p <>) dvw_as
        fi_isView = True
        fi_steps = M.mapMaybe fgo vism
        fgo (cvy_svs, m) = do
          cvy_body <- M.lookup k m
          return $ CLViewY {..}

newtype FIX a = FIX (SLPart, FunInfo a)

type FApp = ReaderT FEnv IO

data FEnv = FEnv
  { fCounter :: Counter
  , fStateR :: IORef CLState
  , f_at :: SrcLoc
  , f_dom :: [DLVarLet]
  , f_rng :: CLVar
  , f_statev :: DLVar
  , f_staten :: Maybe Int
  , f_noCheck :: Bool
  }

instance HasStateR FEnv where
  getStateR = fStateR

instance HasCounter FEnv where
  getCounter = fCounter

class CLikeF a where
  clf :: a -> FApp CLTail

clf_ :: (CLikeF a) => FEnv -> a -> App CLTail
clf_ e x = liftIO $ flip runReaderT e $ clf x

instance (CLikeF a) => CLikeF (BLT Int a) where
  clf = \case
    Empty -> do
      at <- asks f_at
      noCheck <- asks f_noCheck
      let f = CL_Com (CLDL (DL_Let at DLV_Eff (DLE_Claim at [] CT_Enforce (DLA_Literal $ DLL_Bool False) (Just "Incorrect state: empty blt"))))
      return
        $ (if noCheck then id else f)
        $ CL_Halt at HM_Pure
    Leaf i mc a -> do
      -- XXX ^ make sure blt actually fills in mc
      at <- asks f_at
      noCheck <- asks f_noCheck
      a' <- local (\e -> e { f_staten = Just i }) $ clf a
      case mc && not noCheck of
        False -> return a'
        True -> do
          cmpv <- allocVar at T_Bool
          statev <- asks f_statev
          return
            $ CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once cmpv) (DLE_PrimOp at (PEQ UI_Word) [ DLA_Var statev, DLA_Literal $ DLL_Int at UI_Word $ fromIntegral i ])))
            $ CL_Com (CLDL (DL_Let at DLV_Eff (DLE_Claim at [] CT_Enforce (DLA_Var cmpv) (Just "Incorrect state: not leaf"))))
            $ a'
    Branch i l r -> do
      at <- asks f_at
      l' <- clf l
      r' <- clf r
      cmpv <- allocVar at T_Bool
      statev <- asks f_statev
      return
        $ CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once cmpv) (DLE_PrimOp at (PLT UI_Word) [ DLA_Var statev, DLA_Literal $ DLL_Int at UI_Word $ fromIntegral i ])))
        $ CL_If at (DLA_Var cmpv) l' r'

data FunInfo a = FunInfo
  { fi_at :: SrcLoc
  , fi_dom :: [DLType]
  , fi_isView :: Bool
  , fi_isApi :: Bool
  , fi_rng :: DLType
  , fi_as :: [SLPart]
  , fi_steps :: M.Map Int a
  , fi_noCheck :: Bool
  }

instance (CLikeF a) => CLike (FIX a) where
  cl (FIX (v, FunInfo {..})) = do
    let dom = fi_dom
    let rng = fi_rng
    domvs <- forM dom $ allocVar fi_at
    let domvls = map v2vl domvs
    let f_rng = nameReturn v
    def f_rng $ CLD_Mem rng
    let f_at = fi_at
    let f_dom = domvls
    f_statev <- allocVar fi_at $ T_UInt UI_Word
    fCounter <- asks getCounter
    fStateR <- asks getStateR
    let f_staten = Nothing
    let f_noCheck = fi_noCheck
    -- XXX when the state is a data, this would be a real switch
    -- XXX optimize case where there's one step?
    stept <- clf_ (FEnv {..}) $ bltM fi_steps
    let intt = CL_Com (CLBindSpecial fi_at (v2lv f_statev) CLS_StorageState) stept
    let ns = v : fi_as
    let k = (if fi_isApi then CE_API else CE_View) $ bunpack v
    funw (nameApi v) ns fi_at domvls fi_isView rng k (Just f_rng) intt

instance (CLikeF a) => CLike (M.Map SLPart (FunInfo a)) where
  cl = cl . CMap FIX

data ApiInfoY = ApiInfoY
  { aiy_at :: SrcLoc
  , aiy_which :: Int
  , aiy_wrap :: [DLVarLet] -> FApp (DLVar, [(DLVar, DLExpr)])
  }

instance CLikeF ApiInfoY where
  clf (ApiInfoY {..}) = do
    let at = aiy_at
    dom <- asks f_dom
    (argv, lets) <- aiy_wrap dom
    timev <- allocVar at $ T_UInt UI_Word
    let go (v, e) = CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once v) e))
    return
      $ CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once timev) $ DLE_Arg at $ DLA_Literal $ DLL_Int at UI_Word $ 0))
      $ flip (foldr go) lets
      $ CL_Jump at (nameMethi aiy_which) (map DLA_Var [timev, argv]) True Nothing

type ApiInfoX = FunInfo ApiInfoY
type ApiInfosX = M.Map SLPart ApiInfoX

apiReorg :: ApiInfos -> ApiInfosX
apiReorg ais = flip M.map ais apiReorgX

apiReorgX :: M.Map Int ApiInfo -> ApiInfoX
apiReorgX aim = FunInfo {..}
  where
    fi_noCheck = True
    fi_isApi = True
    fi_at = get ai_at
    imp = impossible "apiSig"
    fi_dom = flip get' id fi_dom'
    fi_isView = False
    fi_rng = get ai_ret_ty
    fi_as = case malias of
              Nothing -> []
              Just x -> [x]
    malias = get ai_alias
    get :: forall v . (Sanitize v, Eq v) => (ApiInfo -> v) -> v
    get = get' aim
    get' :: forall k mv v . (Sanitize v, Eq v) => M.Map k mv -> (mv -> v) -> v
    get' m f = feq $ sani $ map f $ M.elems m
    feq :: forall v . (Eq v) => [v] -> v
    feq = \case
      [] -> impossible "No API infos"
      x : xs ->
        case all ((==) x) xs of
          True -> x
          False -> impossible "API infos disagree"
    (fi_dom', fi_steps) = funzip $ flip M.map aim $ go
    go ApiInfo {..} = (dom, ApiInfoY {..})
      where
        aiy_at = ai_at
        aiy_which = ai_which
        (dom, aiy_wrap) = case ai_compile of
          AIC_SpreadArg ->
            case ai_msg_tys of
              [T_Tuple ts] -> (ts, tupleCase)
              _ -> imp
          AIC_Case ->
            case ai_msg_tys of
              [T_Data tm] ->
                case M.lookup cid tm of
                  Just (T_Tuple ts) -> (ts, dataCase tm)
                  _ -> imp
              _ -> imp
        cid = fromMaybe imp ai_mcase_id
        tupleCase domvs = do
          argv <- allocVar ai_at $ T_Tuple dom
          return $ (argv, [(argv, DLE_LArg ai_at $ DLLA_Tuple $ for domvs $ \vl -> DLA_Var $ varLetVar vl)])
        dataCase tm domvs = do
          (tuplev, lets) <- tupleCase domvs
          argv <- allocVar ai_at $ T_Data tm
          return $ (argv, lets <> [(argv, DLE_LArg ai_at $ DLLA_Data tm cid $ DLA_Var tuplev)])

funzip :: Functor f => f (a,b) -> (f a, f b)
funzip xs = (fst <$> xs, snd <$> xs)

for :: [a] -> (a -> b) -> [b]
for = flip map

type TApp = ReaderT TEnv IO

data TEnv = TEnv
  { tCounter :: Counter
  , tStateR :: IORef CLState
  }

instance HasStateR TEnv where
  getStateR = tStateR

instance HasCounter TEnv where
  getCounter = tCounter

class CLikeTr a b where
  tr :: a -> TApp b

tr_ :: (CLikeTr a b) => TEnv -> a -> App b
tr_ e x = liftIO $ flip runReaderT e $ tr x

instance (CLikeTr a b) => CLikeTr (SwitchCases a) (SwitchCases b) where
  tr (SwitchCases m) = SwitchCases <$> mapM tr m

instance CLikeTr a b => CLikeTr (SwitchCase a) (SwitchCase b) where
  tr (SwitchCase {..}) = SwitchCase sc_vl <$> tr sc_k

instance CLikeTr CTail CLTail where
  tr = \case
    CT_Com d t -> CL_Com (CLDL d) <$> tr t
    CT_If at c t f -> CL_If at c <$> tr t <*> tr f
    CT_Switch at x csm -> CL_Switch at x <$> tr csm
    CT_From at which fi -> do
      let ht = CL_Halt at
      case fi of
        FI_Continue svs -> do
          -- XXX change to StoreSet
          -- XXX expose saving of time
          recordState which $ map svsp_svs svs
          return $ CL_Com (CLStateSet at which svs) (ht HM_Impure)
        FI_Halt toks -> do
          let ht' = ht HM_Forever
          -- XXX move this into language
          return $ foldr (CL_Com . CLTokenUntrack at) ht' toks
    CT_Jump at which svs (DLAssignment asnm) -> do
      let asn_as = map snd $ M.toAscList asnm
      let args = map DLA_Var svs <> asn_as
      -- XXX move this concept backwards so that CT_Jump is just a sequence of
      -- variables
      -- XXX make it so that all CLike Internal calls pass things through
      -- memory
      return $ CL_Jump at (nameLoop which) args False Nothing

newtype CHX = CHX (Int, CHandler)

instance CLike CHX where
  cl (CHX (which, (C_Handler {..}))) = do
    given_timev <- allocVar ch_at $ T_UInt UI_Word
    let eff_dom = (v2vl given_timev) : ch_msg
    let msg_vars = map varLetVar eff_dom
    let eff_ty = T_Tuple $ map varType msg_vars
    act_var <- allocVar ch_at eff_ty
    let clf_dom = [ v2vl act_var ]
    let act_arg = DLA_Var act_var
    tCounter <- asks getCounter
    tStateR <- asks eStateR
    let addArg (v, i) = CL_Com $ CLDL $ DL_Let ch_at (v2lv v) (DLE_TupleRef ch_at act_arg i)
    let addArgs = flip (foldr addArg) $ zip msg_vars [0..]
    body' <- tr_ (TEnv {..}) ch_body
    let isCtor = which == 0
    recordState ch_last $ map varLetVar ch_svs
    let mStateBind =
          -- XXX change to StoreRead and something to decompose a Data instance
          -- and fail if the tag doesn't match
          if isCtor then id else CL_Com (CLStateBind ch_at False ch_svs ch_last)
    let intt =
          -- XXX add extensions to DLE so these can be read directly
            CL_Com (CLBindSpecial ch_at (v2lv ch_from) CLS_TxnFrom)
          $ CL_Com (CLBindSpecial ch_at (v2lv ch_timev) CLS_TxnTime)
          $ CL_Com (CLBindSpecial ch_at (v2lv ch_secsv) CLS_TxnSecs)
          $ mStateBind
          $ addArgs
          -- XXX include this in the program itself?
          $ CL_Com (CLEmitPublish ch_at which msg_vars)
          -- XXX put given_timev into DL and does this in Core
          -- XXX there is implicitly a reference to "current_time"
          $ CL_Com (CLTimeCheck ch_at given_timev)
          -- XXX move this back to EPP
          $ CL_Com (CLIntervalCheck ch_at ch_timev ch_secsv ch_int)
          $ body'
    let isView = False
    funw (nameMethi which) [ nameMeth which ] ch_at clf_dom isView T_Null (CE_Publish which) Nothing intt
  cl (CHX (which, (C_Loop {..}))) = do
    let n = nameLoop which
    let clf_dom = cl_svs <> cl_vars
    tCounter <- asks getCounter
    tStateR <- asks eStateR
    clf_tail <- tr_ (TEnv {..}) cl_body
    let clf_view = False
    let cif_mwhich = Nothing
    let clf_at = cl_at
    let cif_fun = CLFun {..}
    funi n $ CLIntFun {..}

instance CLike CHandlers where
  cl (CHandlers hm) = cl $ CMap CHX hm

clike :: PLProg a CPProg -> IO (PLProg a CLProg)
clike = plp_cpp_mod $ \CPProg {..} -> do
  let clp_at = cpp_at
  let clo_counter = getCounter cpp_opts
  let clo_aem = getALGOExitMode cpp_opts
  let clp_opts = CLOpts {..}
  eDefsR <- newIORef mempty
  eFunsR <- newIORef mempty
  eAPIR <- newIORef mempty
  eStateR <- newIORef mempty
  let eCounter = getCounter clp_opts
  flip runReaderT (Env {..}) $ do
    -- XXX creation time
    -- XXX current state & creation time views
    cl cpp_init
    cl cpp_events
    cl $ viewReorg cpp_views
    cl $ apiReorg cpp_apis
    cl cpp_handlers
  clp_defs <- readIORef eDefsR
  clp_funs <- readIORef eFunsR
  clp_api <- readIORef eAPIR
  clp_state <- readIORef eStateR
  -- We created new references to some variables, so we need to correct the
  -- counts
  add_counts $ CLProg {..}
