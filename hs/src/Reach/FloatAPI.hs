module Reach.FloatAPI (floatAPI) where

import Control.Monad.Reader
import Data.IORef
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util

type App = ReaderT Env IO

type AppT a = a -> App a

data Env = Env
  { eAPI :: Maybe (IORef (Maybe DLStmt))
  }

save :: DLStmt -> App ()
save c = asks eAPI >>= \case
  Nothing -> impossible $ "no api slot"
  Just r -> (liftIO $ readIORef r) >>= \case
    Just _ -> impossible $ "api slot used"
    Nothing -> liftIO $ writeIORef r $ Just c

catchFloat :: App LLConsensus -> App LLConsensus
catchFloat m = do
    r <- liftIO $ newIORef $ Nothing
    k' <- local (\e -> e { eAPI = Just r }) m
    f <- (liftIO $ readIORef r) >>= \case
      Nothing -> return $ id
      Just c -> return $ LLC_Com c
    return $ f k'

cffa :: AppT LLConsensus
cffa = catchFloat . fa

nf :: App a -> App a
nf = local (\e -> e { eAPI = Nothing })

class FloatAPI a where
  fa :: AppT a

instance FloatAPI (SwitchCases LLConsensus) where
  fa (SwitchCases m) = SwitchCases <$> mapM fa m

instance FloatAPI (SwitchCase LLConsensus) where
  fa (SwitchCase {..}) = SwitchCase sc_vl <$> cffa sc_k

instance FloatAPI LLConsensus where
  fa = \case
    LLC_Com c@(DL_Let _ _ (DLE_setApiDetails {})) k -> do
      save c
      fa k
    LLC_Com m k -> LLC_Com m <$> fa k
    LLC_If at c t f -> nf $ LLC_If at c <$> cffa t <*> cffa f
    LLC_Switch at v csm -> LLC_Switch at v <$> fa csm
    LLC_FromConsensus at1 at2 fs s ->
      nf $ LLC_FromConsensus at1 at2 fs <$> fa s
    LLC_While at asn inv cond body k ->
      nf $ LLC_While at asn inv cond <$> fa body <*> fa k
    c@(LLC_Continue {}) -> return c
    LLC_ViewIs at mwho v meb k ->
      LLC_ViewIs at mwho v meb <$> fa k

instance FloatAPI (DLRecv LLConsensus) where
  fa (DLRecv from msg time secs didSend k) =
    DLRecv from msg time secs didSend <$> cffa k

fa_mtime :: AppT (Maybe (DLTimeArg, LLStep))
fa_mtime = mapM (\(ta, s) -> (,) ta <$> fa s)

instance FloatAPI LLStep where
  fa = \case
    LLS_Com m s -> LLS_Com m <$> fa s
    LLS_Stop at -> return $ LLS_Stop at
    LLS_ToConsensus at lct send recv mtime ->
      LLS_ToConsensus at lct send <$> fa recv <*> fa_mtime mtime

floatAPI :: LLProg -> IO LLProg
floatAPI (LLProg llp_at llp_opts llp_parts llp_init llp_exports llp_views llp_apis llp_aliases llp_events llp_step) = do
  let eAPI = Nothing
  llp_step' <- flip runReaderT (Env {..}) $ fa llp_step
  return $ LLProg llp_at llp_opts llp_parts llp_init llp_exports llp_views llp_apis llp_aliases llp_events llp_step'
