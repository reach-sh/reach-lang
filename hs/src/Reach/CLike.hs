module Reach.CLike (clike) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.CP
import Reach.AST.PL
import Reach.AST.CL
import Reach.Counter
import Reach.Sanitize
import Reach.Util

type App = ReaderT Env IO

data Env = Env
  { eDefsR :: IORef CLDefs
  , eFunsR :: IORef CLFuns
  , eCounter :: Counter
  }

instance HasCounter Env where
  getCounter = eCounter

allocSym :: CLVar -> App CLVar
allocSym t = (\i -> "_" <> t <> (bpack $ show i)) <$> allocVarIdx

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

fun :: CLVar -> CLFun -> App ()
fun n d = env_insert_ eFunsR s d
  where
    s = CLSym n dom rng
    dom = map varLetType clf_dom
    rng = varType clf_rng
    CLFun {..} = d

class CLike a where
  cl :: a -> App ()

newtype DLMI = DLMI (DLMVar, DLMapInfo)

instance CLike DLMI where
  cl (DLMI ((DLMVar i), (DLMapInfo {..}))) =
    def (bpack $ "m" <> show i) (CLD_Map dlmi_kt dlmi_ty)

clm :: (CLike a) => ((k, v) -> a) -> M.Map k v -> App ()
clm f m = forM_ (map f $ M.toAscList m) cl

instance CLike DLInit where
  cl (DLInit {..}) = clm DLMI dli_maps

clilm :: (CLike a) => ((SLPart, v) -> a) -> InterfaceLikeMap v -> App ()
clilm f = clm f . flattenInterfaceLikeMap

newtype DLEI = DLEI (SLPart, [DLType])

instance CLike DLEI where
  cl (DLEI (p, ts)) = def p $ CLD_Evt ts

instance CLike DLEvents where
  cl = clilm DLEI

data FunInfo a = FunInfo
  { fi_at :: SrcLoc
  , fi_dom :: [DLType]
  , fi_rng :: DLType
  , fi_as :: [SLPart]
  , fi_steps :: M.Map Int a
  }

newtype CLViewY = CLViewY DLExportBlock

type CLViewX = FunInfo CLViewY

type CLViewsX = M.Map SLPart CLViewX

viewReorg :: DLViewsX -> CLViewsX
viewReorg (DLViewsX vs vis) = vx
  where
    igo (ViewInfo _ vsi) = flattenInterfaceLikeMap vsi
    vism = M.map igo vis
    vx = M.mapWithKey go $ flattenInterfaceLikeMap vs
    go k (DLView {..}) = FunInfo {..}
      where
        fi_at = dvw_at
        (fi_dom, fi_rng) = itype2arr dvw_it
        fi_as = dvw_as
        fi_steps = M.map (\m -> CLViewY $ m M.! k) vism

newtype FIX a = FIX (SLPart, FunInfo a)

instance CLike CLViewY where
  cl _ = return () -- XXX

instance CLike ApiInfoY where
  cl _ = return () -- XXX

instance (CLike a) => CLike (FIX a) where
  cl (FIX (v, FunInfo {..})) = do
    let dom = fi_dom
    let rng = fi_rng
    domvs <- forM dom $ allocVar fi_at
    let domvls = map v2vl domvs
    rngv <- allocVar fi_at rng
    mapM_ cl $ M.elems fi_steps
    -- XXX for views, we know DLVarLet info about them
    -- XXX "optimize" case where there is only one step?
    let intt = CL_Jump fi_at "XXX" [] rngv
    wrapv <- allocSym $ v <> "_w"
    fun wrapv $ CLFun domvls rngv CLFM_Internal intt
    let extt = CL_Jump fi_at wrapv domvs rngv
    let extd = CLFun domvls rngv CLFM_External extt
    -- XXX "optimize" case where there is only one name?
    forM_ (v : fi_as) $ flip fun extd

instance (CLike a) => CLike (M.Map SLPart (FunInfo a)) where
  cl = clm FIX

data ApiInfoY = ApiInfoY
  { aiy_mcase :: Maybe String
  , aiy_which :: Int
  , aiy_compile :: ApiInfoCompilation
  }

type ApiInfoX = FunInfo ApiInfoY
type ApiInfosX = M.Map SLPart ApiInfoX

apiReorg :: ApiInfos -> ApiInfosX
apiReorg ais = flip M.map ais apiReorgX

apiReorgX :: M.Map Int ApiInfo -> ApiInfoX
apiReorgX aim = FunInfo {..}
  where
    fi_at = get ai_at
    imp = impossible "apiSig"
    fi_dom = flip get' id $ flip M.map aim $ \ApiInfo {..} ->
      case ai_compile of
        AIC_SpreadArg ->
          case ai_msg_tys of
            [T_Tuple ts] -> ts
            _ -> imp
        AIC_Case ->
          case ai_msg_tys of
            [T_Data tm] ->
              case M.lookup (fromMaybe imp ai_mcase_id) tm of
                Just (T_Tuple ts) -> ts
                _ -> imp
            _ -> imp
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
    fi_steps = flip M.map aim $ go
    go ApiInfo {..} = ApiInfoY {..}
      where
        aiy_mcase = ai_mcase_id
        aiy_which = ai_which
        aiy_compile = ai_compile

type TApp = ReaderT TEnv IO

data TEnv = TEnv
  { tCounter :: Counter
  , t_rngv :: DLVar
  }

instance HasCounter TEnv where
  getCounter = tCounter

class CLikeTr a b where
  tr :: a -> TApp b

tr_ :: (CLikeTr a b) => TEnv -> a -> App b
tr_ e x = liftIO $ flip runReaderT e $ tr x

instance (CLikeTr a b) => CLikeTr (SwitchCases a) (SwitchCases b) where
  tr = mapM $ \(x, y, z) -> (,,) x y <$> tr z

instance CLikeTr CTail CLTail where
  tr = \case
    CT_Com d t -> CL_Com (CLDL d) <$> tr t
    CT_If at c t f -> CL_If at c <$> tr t <*> tr f
    CT_Switch at x csm -> CL_Switch at x <$> tr csm
    CT_From at which fi -> do
      let ht = CL_Halt at
      case fi of
        FI_Continue svs -> do
          return $ CL_Com (CLStateSet at which svs) ht
        FI_Halt toks -> do
          let ht' = CL_Com (CLStateDestroy at) ht
          return $ foldr (CL_Com . CLTokenUntrack at) ht' toks
    CT_Jump at which svs (DLAssignment asnm) -> do
      rngv <- asks t_rngv
      let asnl = M.toAscList asnm
      asn' <- forM asnl $ \(v, a) -> do
        v' <- freshenVar v
        return (v', a)
      let asnvs' = map fst asn'
      let args = svs <> asnvs'
      let kt = CL_Jump at (nameLoop which) args rngv
      let go (v', a) = CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once v') (DLE_Arg at a)))
      let t = foldr go kt asn'
      return t

newtype CHX = CHX (Int, CHandler)

handlerName :: String -> Int -> CLVar
handlerName hc which = bpack $ "_reach_" <> hc <> show which

nameLoop :: Int -> CLVar
nameLoop = handlerName "l"
nameMeth :: Int -> CLVar
nameMeth = handlerName "m"

instance CLike CHX where
  cl (CHX (which, (C_Handler {..}))) = do
    let n = nameMeth which
    given_timev <- allocVar ch_at $ T_UInt UI_Word
    let clf_dom = (v2vl given_timev) : ch_msg
    t_rngv <- allocVar ch_at T_Null
    let clf_rng = t_rngv
    tCounter <- asks getCounter
    body' <- tr_ (TEnv {..}) ch_body
    let clf_tail =
            CL_Com (CLTxnBind ch_at ch_from ch_timev ch_secsv)
          $ CL_Com (CLTimeCheck ch_at ch_timev given_timev)
          $ CL_Com (CLStateBind ch_at ch_svs ch_last)
          $ CL_Com (CLIntervalCheck ch_at ch_timev ch_int)
          $ body'
    let clf_mode = CLFM_External
    fun n $ CLFun {..}
  cl (CHX (which, (C_Loop {..}))) = do
    let n = nameLoop which
    let clf_dom = cl_svs <> cl_vars
    t_rngv <- allocVar cl_at T_Null
    let clf_rng = t_rngv
    tCounter <- asks getCounter
    clf_tail <- tr_ (TEnv {..}) cl_body
    let clf_mode = CLFM_Internal
    fun n $ CLFun {..}

instance CLike CHandlers where
  cl (CHandlers hm) = clm CHX hm

clike :: PLProg a CPProg -> IO (PLProg a CLProg)
clike = plp_cpp_mod $ \CPProg {..} -> do
  let CPOpts {..} = cpp_opts
  let clp_at = cpp_at
  let clp_opts = CLOpts cpo_untrustworthyMaps cpo_counter
  eDefsR <- newIORef mempty
  eFunsR <- newIORef mempty
  let eCounter = getCounter clp_opts
  flip runReaderT (Env {..}) $ do
    cl cpp_init
    cl cpp_events
    cl $ viewReorg cpp_views
    cl $ apiReorg cpp_apis
    cl cpp_handlers
  clp_defs <- readIORef eDefsR
  clp_funs <- readIORef eFunsR
  return $ CLProg {..}
