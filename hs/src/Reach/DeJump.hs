module Reach.DeJump (dejump) where

import Control.Monad.Reader
import Data.Foldable (foldr')
import Data.IORef
import qualified Data.Map.Strict as M
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Counter
import Reach.Subst
import Reach.Util

data Env = Env
  { e_hs :: M.Map Int CIHandler
  , e_rho :: M.Map DLVar DLVar
  , e_idx :: Counter
  }

type App = ReaderT Env IO

allocVar :: DLVar -> App DLVar
allocVar (DLVar at s t _) = do
  Env {..} <- ask
  idx <- liftIO $ incCounter e_idx
  return $ DLVar at s t idx

getLoop :: Int -> App CITail
getLoop w = do
  hs <- e_hs <$> ask
  case M.lookup w hs of
    Just (C_Loop {..}) -> return $ cl_body
    _ -> impossible $ "no loop"

djs :: Subst a => a -> App a
djs x = flip subst_ x . e_rho <$> ask

djs_fi :: FromInfo -> App FromInfo
djs_fi = \case
  FI_Halt toks -> FI_Halt <$> mapM djs toks
  FI_Continue svs -> FI_Continue <$> mapM go svs
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
      rho <- e_rho <$> ask
      rho'r <- liftIO $ newIORef rho
      -- Rebind them and add the renaming to rho
      let go2 (v, a) = do
            v' <- allocVar v
            liftIO $ modifyIORef rho'r $ M.insert v v'
            a' <- djs a
            return $ DL_Let at (Just v') $ DLE_Arg at a'
      nms <- mapM go2 $ M.toList asnm
      rho' <- liftIO $ readIORef rho'r
      -- Process the body in the context of the new substitution
      t' <- local (\e -> e {e_rho = rho'}) $ dj t
      -- Include the new variable definitions
      return $ foldr' CT_Com t' nms

instance DeJump CIHandler where
  dj (C_Loop {}) = impossible $ "dejump loop"
  dj (C_Handler {..}) = do
    ch_body' <- dj ch_body
    return $ C_Handler ch_at ch_int ch_last_timev ch_from ch_last ch_svs ch_msg ch_timev ch_body'

dejump :: PIProg -> IO PIProg
dejump (PLProg at plo dli dex epps cp) = do
  let PLOpts {..} = plo
  let CPProg cat (CHandlers hs) = cp
  let go h@(C_Loop {}) =
        -- XXX: We leave these unchanged because the ALGO backend uses an
        -- array rather than a map. It would be good to change that.
        return h
      go h = do
        let e_hs = hs
        let e_rho = mempty
        let e_idx = plo_counter
        flip runReaderT (Env {..}) $ dj h
  hs' <- mapM go hs
  let cp' = CPProg cat (CHandlers hs')
  return $ PLProg at plo dli dex epps cp'
