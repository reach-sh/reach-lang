module Reach.CLike
  ( clike
  , nameMap
  , nameReturn
  , nameMeth
  ) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
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
  , eCounter :: Counter
  }

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
    rng = case clf_mode of
            CLFM_Internal -> T_Null
            CLFM_External t -> t
    CLFun {..} = d

funw :: CLVar -> [CLVar] -> SrcLoc -> [DLVarLet] -> Bool -> DLType -> Maybe CLVar -> CLTail -> App ()
funw ni ns at clf_dom clf_view rng mret intt = do
  let clf_at = at
  let di = CLFun { clf_mode = CLFM_Internal, clf_tail = intt, .. }
  let domvs = map varLetVar clf_dom
  let extt = CL_Jump at ni domvs (Just mret)
  let de = CLFun { clf_mode = CLFM_External rng, clf_tail = extt, .. }
  fun ni di
  -- XXX optimize when one ns?
  -- I can't in Solidity because of the argument number issue, but I can in AVM
  -- and EVM
  forM_ ns $ flip fun de

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
  { cvy_svs :: [DLVarLet]
  , cvy_body :: DLExportBlock
  }

instance CLikeF CLViewY where
  clf (CLViewY {..}) = do
    prev <- fromMaybe (impossible "clf cvy") <$> asks f_staten
    let (DLinExportBlock _ mfargs (DLBlock at _ t r)) = cvy_body
    rng <- asks f_rng
    let k = CL_Com (CLMemorySet at rng r) $ CL_Halt at
    let t0 = dtReplace (CL_Com . CLDL) k t
    let fargs = fromMaybe mempty mfargs
    dom <- asks f_dom
    let bind (dvl, avl) = CL_Com $ CLDL $ DL_Let at (vl2lv avl) $ DLE_Arg at $ DLA_Var $ varLetVar dvl
    let t1 = foldr bind t0 $ zip dom fargs
    let t2 = CL_Com (CLStateBind at True cvy_svs prev) t1
    return $ t2

type CLViewX = FunInfo CLViewY

type CLViewsX = M.Map SLPart CLViewX

viewReorg :: DLViewsX -> CLViewsX
viewReorg (DLViewsX vs vis) = vx
  where
    igo (ViewInfo svs vsi) = (map v2vl svs, flattenInterfaceLikeMap vsi)
    vism = M.map igo vis
    vx = M.mapWithKey go $ flattenInterfaceLikeMap_ (,) vs
    go k (p, (DLView {..})) = FunInfo {..}
      where
        fi_noCheck = False
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
  , f_at :: SrcLoc
  , f_dom :: [DLVarLet]
  , f_rng :: CLVar
  , f_statev :: DLVar
  , f_staten :: Maybe Int
  , f_noCheck :: Bool
  }

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
        $ CL_Halt at
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
    let f_staten = Nothing
    let f_noCheck = fi_noCheck
    -- XXX when the state is a data, this would be a real switch
    -- XXX optimize case where there's one step?
    stept <- clf_ (FEnv {..}) $ bltM fi_steps
    let intt = CL_Com (CLStateRead fi_at f_statev) stept
    let ns = v : fi_as
    funw (nameApi v) ns fi_at domvls fi_isView rng (Just f_rng) intt

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
      $ CL_Jump at (nameMethi aiy_which) [timev, argv] Nothing

type ApiInfoX = FunInfo ApiInfoY
type ApiInfosX = M.Map SLPart ApiInfoX

apiReorg :: ApiInfos -> ApiInfosX
apiReorg ais = flip M.map ais apiReorgX

apiReorgX :: M.Map Int ApiInfo -> ApiInfoX
apiReorgX aim = FunInfo {..}
  where
    fi_noCheck = True
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
          -- XXX change to StoreSet
          -- XXX expose saving of time
          return $ CL_Com (CLStateSet at which svs) ht
        FI_Halt toks -> do
          -- XXX change to StoreSet
          let ht' = CL_Com (CLStateDestroy at) ht
          -- XXX move this into language
          return $ foldr (CL_Com . CLTokenUntrack at) ht' toks
    CT_Jump at which svs (DLAssignment asnm) -> do
      let asnl = M.toAscList asnm
      asn' <- forM asnl $ \(v, a) -> do
        v' <- freshenVar v
        return (v', a)
      let asnvs' = map fst asn'
      let args = svs <> asnvs'
      -- XXX move this concept backwards so that CT_Jump is just a sequence of
      -- variables
      -- XXX make it so that all CLike Internal calls pass things through
      -- memory
      let kt = CL_Jump at (nameLoop which) args Nothing
      let go (v', a) = CL_Com (CLDL (DL_Let at (DLV_Let DVC_Once v') (DLE_Arg at a)))
      let t = foldr go kt asn'
      return t

newtype CHX = CHX (Int, CHandler)

instance CLike CHX where
  cl (CHX (which, (C_Handler {..}))) = do
    given_timev <- allocVar ch_at $ T_UInt UI_Word
    let eff_dom = (v2vl given_timev) : ch_msg
    let eff_ty = T_Tuple $ map varType $ map varLetVar eff_dom
    act_var <- allocVar ch_at eff_ty
    let clf_dom = [ v2vl act_var ]
    let act_arg = DLA_Var act_var
    tCounter <- asks getCounter
    let addArg (vl, i) = CL_Com $ CLDL $ DL_Let ch_at (vl2lv vl) (DLE_TupleRef ch_at act_arg i)
    let addArgs = flip (foldr addArg) $ zip eff_dom [0..]
    body' <- tr_ (TEnv {..}) ch_body
    let isCtor = which == 0
    let mStateBind =
          -- XXX change to StoreRead and something to decompose a Data instance
          -- and fail if the tag doesn't match
          if isCtor then id else CL_Com (CLStateBind ch_at False ch_svs ch_last)
    let intt =
          -- XXX include this in the program itself?
            CL_Com (CLEmitPublish ch_at which eff_ty)
          -- XXX add extensions to DLE so these can be read directly
          $ CL_Com (CLTxnBind ch_at ch_from ch_timev ch_secsv)
          $ mStateBind
          $ addArgs
          -- XXX puit given_timev into DL and does this in Core
          -- XXX there is implicitly a reference to "current_time"
          $ CL_Com (CLTimeCheck ch_at given_timev)
          -- XXX move this back to EPP
          $ CL_Com (CLIntervalCheck ch_at ch_timev ch_secsv ch_int)
          $ body'
    let isView = False
    funw (nameMethi which) [ nameMeth which ] ch_at clf_dom isView T_Null Nothing intt
  cl (CHX (which, (C_Loop {..}))) = do
    let n = nameLoop which
    let clf_dom = cl_svs <> cl_vars
    tCounter <- asks getCounter
    clf_tail <- tr_ (TEnv {..}) cl_body
    let clf_mode = CLFM_Internal
    let clf_view = False
    let clf_at = cl_at
    fun n $ CLFun {..}

instance CLike CHandlers where
  cl (CHandlers hm) = cl $ CMap CHX hm

clike :: PLProg a CPProg -> IO (PLProg a CLProg)
clike = plp_cpp_mod $ \old@(CPProg {..}) -> do
  let CPOpts {..} = cpp_opts
  let clp_at = cpp_at
  let clp_opts = CLOpts cpo_untrustworthyMaps cpo_counter
  eDefsR <- newIORef mempty
  eFunsR <- newIORef mempty
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
  let clp_old = old
  return $ CLProg {..}
