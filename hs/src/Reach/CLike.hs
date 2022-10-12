module Reach.CLike (clike) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.IORef
import qualified Data.Map.Strict as M
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

data CLViewX = CLViewX
  { cvx_at :: SrcLoc
  , cvx_dom :: [DLType]
  , cvx_rng :: DLType
  , cvx_as :: [B.ByteString]
  , cvx_steps :: M.Map Int DLExportBlock
  }

type CLViewsX = M.Map SLPart CLViewX

viewReorg :: DLViewsX -> CLViewsX
viewReorg (DLViewsX vs vis) = vx
  where
    igo (ViewInfo _ vsi) = flattenInterfaceLikeMap vsi
    vism = M.map igo vis
    vx = M.mapWithKey go $ flattenInterfaceLikeMap vs
    go k (DLView {..}) = CLViewX {..}
      where
        cvx_at = dvw_at
        (cvx_dom, cvx_rng) = itype2arr dvw_it
        cvx_as = dvw_as
        cvx_steps = M.map (\m -> m M.! k) vism

newtype CLVX = CLVX (SLPart, CLViewX)

instance CLike CLVX where
  cl (CLVX (v, CLViewX {..})) = do
    let dom = cvx_dom
    let rng = cvx_rng
    domvs <- forM dom $ allocVar cvx_at
    rngv <- allocVar cvx_at rng
    let intt = CL_Jump cvx_at "XXX" [] rngv
    wrapv <- allocSym $ v <> "_w"
    let cf x = CSFun x dom rng
    def (cf wrapv) $ CLD_Fun $ CLFun domvs rngv CLFM_Internal intt
    let extt = CL_Jump cvx_at wrapv domvs rngv
    let extd = CLD_Fun $ CLFun domvs rngv CLFM_External extt
    forM_ (v : cvx_as) $ flip def extd . cf

instance CLike CLViewsX where
  cl = clm CLVX

data ApiInfoY = ApiInfoY
  { aiy_mcase :: Maybe String
  , aiy_which :: Int
  , aiy_compile :: ApiInfoCompilation
  }

data ApiInfoX = ApiInfoX
  { aix_at :: SrcLoc
  , aix_dom :: [DLType]
  , aix_rng :: DLType
  , aix_alias :: Maybe SLPart
  , aix_steps :: M.Map Int ApiInfoY
  }

type ApiInfosX = M.Map SLPart ApiInfoX

apiReorg :: ApiInfos -> ApiInfosX
apiReorg ais = flip M.map ais apiReorgX

apiReorgX :: M.Map Int ApiInfo -> ApiInfoX
apiReorgX aim = ApiInfoX {..}
  where
    aix_at = get ai_at
    aix_dom = get ai_msg_tys
    aix_rng = get ai_ret_ty
    aix_alias = get ai_alias
    get :: forall v . (Sanitize v, Eq v) => (ApiInfo -> v) -> v
    get f = feq $ sani $ map f $ M.elems aim
    feq :: forall v . (Eq v) => [v] -> v
    feq = \case
      [] -> impossible "No API infos"
      x : xs ->
        case all ((==) x) xs of
          True -> x
          False -> impossible "API infos disagree"
    aix_steps = flip M.map aim $ go
    go ApiInfo {..} = ApiInfoY {..}
      where
        aiy_mcase = ai_mcase_id
        aiy_which = ai_which
        aiy_compile = ai_compile

newtype AIX = AIX (SLPart, ApiInfoX)

instance CLike AIX where
  cl (AIX (_f, ApiInfoX {..})) = error "XXX"

instance CLike ApiInfosX where
  cl = clm AIX

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
