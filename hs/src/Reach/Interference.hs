module Reach.Interference (colorProgram) where

import           Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT),
                                       asks, forM_)
import           Data.IORef           (IORef, modifyIORef, newIORef, readIORef,
                                       writeIORef)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe, maybeToList)
import qualified Data.Set             as S

import           Control.Monad        (forM)
import           Control.Monad.Extra  (mconcatMapM)
import           Data.Foldable        (maximumBy)
import           Data.List            ((\\))
import           Data.Ord             (comparing)
import           Reach.AST.DLBase     (DLType, DLVar, varType)
import           Reach.AST.PL
import           Reach.Util           (impossible)
import Reach.Texty (pretty)


type DLVarS = S.Set DLVar

type InterferenceGraph = M.Map DLVar DLVarS

type TypeInterferenceGraphs = M.Map DLType InterferenceGraph

type TypeXVars = M.Map DLType DLVarS

newtype Env = Env {
  e_tv :: IORef TypeXVars }

type App = ReaderT Env IO

-- Add edges from k -> vs && from v -> k, for every v in vs
updateInterferenceGraph :: InterferenceGraph -> DLVar -> DLVarS -> InterferenceGraph
updateInterferenceGraph g k vs =
  let m = M.insertWith S.union k vs g in
  foldr (\ v acc ->
      M.insertWith S.union v (S.singleton k) acc) m vs

getWrittenVars :: CTail_ a -> [DLVar]
getWrittenVars = \case
  CT_Com _ t       -> getWrittenVars t
  CT_If _ _ t f    -> concatMap getWrittenVars [t, f]
  CT_From _ _ fi   -> maybe [] (map fst) fi
  CT_Switch _ _ sc -> getSwitchWrittenVars sc
  CT_Jump {}       -> []
  where
    getSwitchWrittenVars sc =
      concatMap (\ (_, (mv, c)) ->
        maybeToList mv <> getWrittenVars c) $ M.toList sc

getInterference :: IORef TypeInterferenceGraphs -> S.Set DLVar -> [CHandler_ a] -> App ()
getInterference tyInterGRef liveAfter = \case
  C_Handler {..} : rst -> process ch_svs ch_body rst
  C_Loop {..}    : rst -> process cl_svs cl_body rst
  []                   -> return ()
  where
    process readVars body rst = do
      addEdges readVars
      let readVarsS   = S.fromList readVars
      let writtenVars = S.fromList $ getWrittenVars body
      let liveAfter'  = S.union readVarsS $ S.difference liveAfter writtenVars
      getInterference tyInterGRef liveAfter' rst
    addEdges readVars = do
      forM_ readVars $ \ readVar -> do
        _ <- recordTy readVar
        let varTy = varType readVar
        tyInterG <- liftIO $ readIORef tyInterGRef
        let interGraph = case M.lookup varTy tyInterG of
                    Just t -> t
                    _      -> impossible "getInterference: type not in graph"
        let shouldMark la = la /= readVar && varType la == varTy
        let liveAftersWithType = S.filter shouldMark liveAfter
        let interGraph' = updateInterferenceGraph interGraph readVar liveAftersWithType
        liftIO $ writeIORef tyInterGRef $ M.insert varTy interGraph' tyInterG
    recordTy v = do
      tvRef <- asks e_tv
      liftIO $ modifyIORef tvRef $
        M.insertWith S.union (varType v) (S.singleton v)

getHandlerTypes :: CHandler_ a -> [DLType]
getHandlerTypes = \case
  C_Handler {..} -> map varType ch_svs
  C_Loop {..}    -> map varType cl_svs

makeInterferenceGraph :: M.Map Int CIHandler -> App TypeInterferenceGraphs
makeInterferenceGraph hs = do
  let handlers = M.toDescList hs
  let allTypes = concatMap (getHandlerTypes . snd) handlers
  interGraph <- liftIO $ newIORef mempty
  forM_ allTypes $ \ ty -> liftIO $ modifyIORef interGraph $ M.insert ty M.empty
  getInterference interGraph S.empty (map snd handlers)
  liftIO $ readIORef interGraph

newtype Graph a = Graph (IORef (M.Map a (S.Set a)))

lookupGraph :: Ord a => Graph a -> a -> IO (S.Set a)
lookupGraph (Graph gr) u =
  fromMaybe mempty . M.lookup u <$> readIORef gr

color :: forall a . Ord a => S.Set a -> Graph a -> M.Map a Int -> IO (M.Map a Int)
color s gInter asn0 = do
  asnr <- newIORef asn0
  let w0 = S.toAscList $ s `S.difference` M.keysSet asn0
  w <- newIORef $ w0
  let consult_asn cv =
        maybeToList . M.lookup cv <$> readIORef asnr
  let neighbors_colors :: Graph a -> a -> IO (S.Set Int)
      neighbors_colors g v = do
        ns <- S.toList <$> (S.intersection s <$> lookupGraph g v)
        ns_cs <- mconcatMapM consult_asn ns
        return $ S.fromList ns_cs
  let compute_sat :: a -> IO (a, S.Set Int)
      compute_sat v = (,) v <$> neighbors_colors gInter v
  let cmp_sizes = comparing (S.size . snd)
  let extractMaxSat f = do
        readIORef w >>= \case
          [] -> return ()
          cw -> do
            cw_ws <- mapM compute_sat cw
            let (v, sv) = maximumBy cmp_sizes cw_ws
            writeIORef w $ cw \\ [v]
            f v sv
  let tryToAssign v sv = \case
        [] -> error $ "no color opts"
        c0 : cols ->
          case S.member c0 sv of
            True  -> tryToAssign v sv cols
            False -> modifyIORef asnr $ M.insert v c0
  let color_loop = extractMaxSat $ \v sv -> do
        tryToAssign v sv [0..]
        color_loop
  color_loop
  readIORef asnr

colorProgram :: PIProg -> IO PIProg
colorProgram (PLProg at plo dli dex epps (CPProg cat _ (CHandlers handlers))) = do
  e_tv <- newIORef mempty
  interGraph <- flip runReaderT (Env {..}) $
                  makeInterferenceGraph handlers
  ty_x_vars  <- readIORef e_tv
  ty_x_cg <- forM (M.toList interGraph) $ colorVarsWithType ty_x_vars
  let typeGraph   = M.map maximum $ M.fromList ty_x_cg
  let varGraph    = M.unions $ map snd ty_x_cg
  liftIO $ putStrLn $ "typeGraph: " <> show (pretty typeGraph)
  let colorGraphs = ColorGraph { typeGraph, varGraph }
  return $ PLProg at plo dli dex epps $ CPProg cat colorGraphs (CHandlers handlers)
  where
    colorVarsWithType :: TypeXVars -> (DLType, InterferenceGraph) -> IO (DLType, M.Map DLVar Int)
    colorVarsWithType ty_x_vars (ty, typeGraph) = do
        tgRef <- newIORef typeGraph
        liftIO $ putStrLn $ show ty <> " interference: " <> show (pretty typeGraph)
        let varsWithTy = fromMaybe S.empty $ M.lookup ty ty_x_vars
        colors <- color varsWithTy (Graph tgRef) mempty
        liftIO $ putStrLn $ show ty <> " colors: " <> show (pretty colors)
        return (ty, colors)
