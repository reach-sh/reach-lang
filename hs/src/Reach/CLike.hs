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

def :: CLSym -> CLDef -> App ()
def v d = do
  er <- asks eDefsR
  liftIO $ modifyIORef er $ env_insert v d

class CLike a where
  cl :: a -> App ()

newtype DLMI = DLMI (DLMVar, DLMapInfo)

instance CLike DLMI where
  cl (DLMI ((DLMVar i), (DLMapInfo {..}))) =
    def (CSVar (bpack $ "m" <> show i)) (CLD_Map dlmi_kt dlmi_ty)

clm :: (CLike a) => ((k, v) -> a) -> M.Map k v -> App ()
clm f m = forM_ (map f $ M.toAscList m) cl

instance CLike DLInit where
  cl (DLInit {..}) = clm DLMI dli_maps

clilm :: (CLike a) => ((SLPart, v) -> a) -> InterfaceLikeMap v -> App ()
clilm f = clm f . flattenInterfaceLikeMap

newtype DLEI = DLEI (SLPart, [DLType])

instance CLike DLEI where
  cl (DLEI (p, ts)) = def (CSVar p) $ CLD_Evt ts

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
  cl _ = return ()

instance CLike ApiInfoY where
  cl _ = return ()

instance (CLike a) => CLike (FIX a) where
  cl (FIX (v, FunInfo {..})) = do
    let dom = fi_dom
    let rng = fi_rng
    domvs <- forM dom $ allocVar fi_at
    rngv <- allocVar fi_at rng
    mapM_ cl $ M.elems fi_steps
    let intt = CL_Jump fi_at "XXX" [] rngv
    wrapv <- allocSym $ v <> "_w"
    let cf x = CSFun x dom rng
    def (cf wrapv) $ CLD_Fun $ CLFun domvs rngv CLFM_Internal intt
    let extt = CL_Jump fi_at wrapv domvs rngv
    let extd = CLD_Fun $ CLFun domvs rngv CLFM_External extt
    -- XXX "optimize" case where there is only one name?
    forM_ (v : fi_as) $ flip def extd . cf

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

clike :: PLProg a CPProg -> IO (PLProg a CLProg)
clike = plp_cpp_mod $ \CPProg {..} -> do
  let CPOpts {..} = cpp_opts
  let clp_at = cpp_at
  let clp_opts = CLOpts cpo_untrustworthyMaps cpo_counter
  eDefsR <- newIORef mempty
  let eCounter = getCounter clp_opts
  flip runReaderT (Env {..}) $ do
    cl cpp_init
    cl cpp_events
    cl $ viewReorg cpp_views
    cl $ apiReorg cpp_apis
    -- XXX cl cpp_handlers
  clp_defs <- readIORef eDefsR
  return $ CLProg {..}
