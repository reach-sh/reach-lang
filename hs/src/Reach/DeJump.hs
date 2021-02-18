module Reach.DeJump (dejump) where

import Control.Monad.Reader
import Data.IORef
import Data.Foldable (foldr')
import qualified Data.Map.Strict as M
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Util
import Reach.Subst
import Reach.Counter

data Env = Env
  { e_hs :: M.Map Int CIHandler
  , e_rho :: IORef (M.Map DLVar DLVar)
  , e_idx :: Counter }
type App = ReaderT Env IO

allocVar :: DLVar -> App DLVar
allocVar (DLVar at s t _) = do
  Env {..} <- ask
  idx <- liftIO $ incCounter e_idx
  return $ DLVar at s t idx

renameVar :: DLVar -> App DLVar
renameVar v = do
  v' <- allocVar v
  rhor <- e_rho <$> ask
  liftIO $ modifyIORef rhor $ M.insert v v'
  return $ v'

getLoop :: Int -> App CITail
getLoop w = do
  hs <- e_hs <$> ask
  case M.lookup w hs of
    Just (C_Loop {..}) -> return $ cl_body
    _ -> impossible $ "no loop"

djs :: Subst a => a -> App a
djs x = do
  rhor <- e_rho <$> ask
  rho <- liftIO $ readIORef rhor
  return $ subst_ rho x

djs_fi :: FromInfo -> App FromInfo
djs_fi = mapM (mapM go)
  where
    go (v, a) = (,) v <$> djs a

class DeJump a where
  dj :: a -> App a

instance (Subst b, DeJump a) => DeJump (M.Map k (b, a)) where
  dj = mapM (\(x, y) -> (,) <$> djs x <*> dj y)

instance DeJump CITail where
  dj = \case
    CT_Com m k -> CT_Com <$> djs m <*> dj k
    CT_If at c t f -> CT_If at <$> djs c <*> dj t <*> dj f
    CT_Switch at ov csm -> CT_Switch at <$> djs ov <*> dj csm
    CT_From at w fi -> CT_From at w <$> djs_fi fi
    CT_Jump at dst _ (DLAssignment asnm) -> do
      t <- getLoop dst
      let go (v, a) = do
            v' <- renameVar v
            -- Note: We don't subst a, because it will get subst'd later
            return $ DL_Let at (Just v') $ DLE_Arg at a
      nms <- mapM go $ M.toList asnm
      dj $ foldr' CT_Com t nms

instance DeJump CIHandler where
  dj (C_Loop {}) = impossible $ "dejump loop"
  dj (C_Handler {..}) = do
    ch_body' <- dj ch_body
    return $ C_Handler ch_at ch_int ch_last_timev ch_from ch_last ch_svs ch_msg ch_amtv ch_timev ch_body'

dejump :: PIProg -> IO PIProg
dejump (PLProg at plo dli epps cp) = do
  let PLOpts {..} = plo
  let CPProg cat (CHandlers hs) = cp
  let go (_, C_Loop {}) = return mempty
      go (i, h) = do
        let e_hs = hs
        e_rho <- newIORef mempty
        let e_idx = plo_counter
        h' <- flip runReaderT (Env {..}) $ dj h
        return $ M.singleton i h'
  hs' <- mconcat <$> mapM go (M.toList hs)
  let cp' = CPProg cat (CHandlers hs')
  return $ PLProg at plo dli epps cp'
