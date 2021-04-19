module Reach.Interference (colorProgram) where

import Control.Monad (forM)
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.Reader
  ( MonadIO (liftIO)
  , ReaderT (runReaderT)
  , asks
  , forM_
  )
import Data.IORef
  ( IORef
  , modifyIORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List ((\\))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ord (comparing)
import qualified Data.Set as S
import Reach.AST.DLBase (DLType, DLVar, varType)
import Reach.AST.PL
import Reach.Util (impossible)
import Safe.Foldable (maximumByMay)

type DLVarS = S.Set DLVar

type InterferenceGraph = M.Map DLVar DLVarS

type TypeInterferenceGraphs = M.Map DLType InterferenceGraph

type TypeXVars = M.Map DLType DLVarS

data Env = Env
  { e_tv :: IORef TypeXVars
  , e_ti :: IORef TypeInterferenceGraphs
  }

type App = ReaderT Env IO

-- Add edges from k -> vs && from v -> k, for every v in vs
updateInterferenceGraph :: InterferenceGraph -> DLVar -> DLVarS -> InterferenceGraph
updateInterferenceGraph g k vs =
  let m = M.insertWith S.union k vs g
   in foldr (\v -> M.insertWith S.union v $ S.singleton k) m vs

getWrittenVars :: CTail_ a -> [DLVar]
getWrittenVars = \case
  CT_From _ _ fi -> case fi of
    FI_Continue vs -> map fst vs
    _ -> []
  CT_Com _ t -> getWrittenVars t
  CT_If _ _ t f -> concatMap getWrittenVars [t, f]
  CT_Switch _ _ sc -> getSwitchWrittenVars sc
  CT_Jump {} -> []
  where
    getSwitchWrittenVars sc =
      concatMap (getWrittenVars . snd . snd) $ M.toList sc

buildInterference :: S.Set DLVar -> [CHandler_ a] -> App ()
buildInterference liveAfter = \case
  C_Handler {..} : rst -> process ch_svs ch_body rst
  C_Loop {..} : rst -> process cl_svs cl_body rst
  [] -> return ()
  where
    process readVars body rst = do
      addEdges readVars
      let readVarsS = S.fromList readVars
      let writtenVars = S.fromList $ getWrittenVars body
      let liveAfter' = S.union readVarsS $ S.difference liveAfter writtenVars
      buildInterference liveAfter' rst
    addEdges readVars = do
      forM_ readVars $ \readVar -> do
        _ <- recordTy readVar
        let varTy = varType readVar
        tiRef <- asks e_ti
        ti <- liftIO $ readIORef tiRef
        let ig = case M.lookup varTy ti of
              Just t -> t
              _ -> impossible "buildInterference: type not in graph"
        let shouldMark la = la /= readVar && varType la == varTy
        let liveAftersWithType = S.filter shouldMark liveAfter
        let ig' = updateInterferenceGraph ig readVar liveAftersWithType
        liftIO $ writeIORef tiRef $ M.insert varTy ig' ti
    recordTy v = do
      tvRef <- asks e_tv
      liftIO $
        modifyIORef tvRef $
          M.insertWith S.union (varType v) (S.singleton v)

getTypes :: CHandler_ a -> [DLType]
getTypes = \case
  C_Handler {..} -> map varType ch_svs
  C_Loop {..} -> map varType cl_svs

makeInterferenceGraph :: M.Map Int CIHandler -> App ()
makeInterferenceGraph hs = do
  let handlers = map snd $ M.toDescList hs
  let allTypes = concatMap getTypes handlers
  forM_ allTypes $ \ty -> do
    g <- asks e_ti
    liftIO $ modifyIORef g $ M.insert ty M.empty
  buildInterference S.empty handlers

lookupGraph :: IORef InterferenceGraph -> DLVar -> IO DLVarS
lookupGraph gr u =
  fromMaybe mempty . M.lookup u <$> readIORef gr

color :: DLVarS -> IORef InterferenceGraph -> M.Map DLVar Int -> IO (M.Map DLVar Int)
color s gInter asn0 = do
  asnr <- newIORef asn0
  let w0 = S.toAscList $ s `S.difference` M.keysSet asn0
  w <- newIORef $ w0
  let consult_asn cv =
        maybeToList . M.lookup cv <$> readIORef asnr
  let neighbors_colors :: IORef InterferenceGraph -> DLVar -> IO (S.Set Int)
      neighbors_colors g v = do
        ns <- S.toList <$> (S.intersection s <$> lookupGraph g v)
        ns_cs <- mconcatMapM consult_asn ns
        return $ S.fromList ns_cs
  let compute_sat :: DLVar -> IO (DLVar, S.Set Int)
      compute_sat v = (,) v <$> neighbors_colors gInter v
  let cmp_sizes = comparing (S.size . snd)
  let extractMaxSat f = do
        readIORef w >>= \case
          [] -> return ()
          cw -> do
            cw_ws <- mapM compute_sat cw
            case maximumByMay cmp_sizes cw_ws of
              Nothing -> impossible "color: maximumByMay"
              Just (v, sv) -> do
                writeIORef w $ cw \\ [v]
                f v sv
  let tryToAssign v sv = \case
        [] -> error $ "no color opts"
        c0 : cols ->
          case S.member c0 sv of
            True -> tryToAssign v sv cols
            False -> modifyIORef asnr $ M.insert v c0
  let color_loop = extractMaxSat $ \v sv -> do
        tryToAssign v sv [0 ..]
        color_loop
  color_loop
  readIORef asnr

colorProgram :: PIProg -> IO ColorGraphs
colorProgram (PLProg _ _ _ _ _ (CPProg _ (CHandlers handlers))) = do
  e_tv <- newIORef mempty
  e_ti <- newIORef mempty
  flip runReaderT (Env {..}) $ makeInterferenceGraph handlers
  ty_x_vars <- readIORef e_tv
  ti <- readIORef e_ti
  ty_x_cg <- forM (M.toList ti) $ colorVarsWithType ty_x_vars
  let typeGraph = M.map maximum $ M.fromList ty_x_cg
  let varGraph = M.unions $ map snd ty_x_cg
  return ColorGraph {typeGraph, varGraph}
  where
    colorVarsWithType :: TypeXVars -> (DLType, InterferenceGraph) -> IO (DLType, M.Map DLVar Int)
    colorVarsWithType ty_x_vars (ty, typeGraph) = do
      tgRef <- newIORef typeGraph
      let varsWithTy = fromMaybe S.empty $ M.lookup ty ty_x_vars
      colors <- color varsWithTy tgRef mempty
      return (ty, colors)
