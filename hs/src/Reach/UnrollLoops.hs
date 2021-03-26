module Reach.UnrollLoops (unrollLoops) where

import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.Counter
import Reach.Freshen
import Reach.Util

type App = ReaderT Env IO

type AppT a = a -> App a

type Lifts = Seq.Seq LLCommon

data Env = Env
  { eCounter :: Counter
  , emLifts :: Maybe (IORef Lifts)
  }

class Unroll a where
  ul :: AppT a

allocIdx :: App Int
allocIdx = do
  Env {..} <- ask
  lift $ incCounter eCounter

addLifts :: (LLCommon -> a -> a) -> Lifts -> a -> a
addLifts mkk ls k = foldr mkk k ls

collectLifts :: App a -> App (Lifts, a)
collectLifts m = do
  -- Env {..} <- ask -- XXX unused (DELETEME?)
  newLifts <- liftIO $ newIORef mempty
  res <- local (\e -> e {emLifts = Just newLifts}) m
  lifts <- liftIO $ readIORef newLifts
  return $ (lifts, res)

liftCommon :: HasCallStack => LLCommon -> App ()
liftCommon m = do
  Env {..} <- ask
  case emLifts of
    Just r ->
      liftIO $ modifyIORef r (flip (Seq.|>) m)
    Nothing ->
      impossible "no lifts"

liftLocal :: HasCallStack => LLTail -> App ()
liftLocal = \case
  DT_Return _ -> return ()
  DT_Com m k -> liftCommon m >> liftLocal k

liftExpr :: HasCallStack => SrcLoc -> DLType -> DLExpr -> App DLArg
liftExpr at t e = do
  idx <- allocIdx
  let v = DLVar at Nothing t idx
  liftCommon (DL_Let at (Just v) e)
  return $ DLA_Var v

liftArray :: HasCallStack => SrcLoc -> DLType -> [DLArg] -> App DLExpr
liftArray at ty as = do
  let a_ty = T_Array ty $ fromIntegral $ length as
  na <- liftExpr at a_ty $ DLE_LArg at $ DLLA_Array ty as
  return $ DLE_Arg at na

ul_explode :: SrcLoc -> DLArg -> App (DLType, [DLArg])
ul_explode at a =
  case a of
    DLA_Var (DLVar _ _ (T_Array t sz) _) -> do_explode t sz
    DLA_Interact _ _ (T_Array t sz) -> do_explode t sz
    _ -> impossible "explode not array"
  where
    do_explode t sz = pure (,) <*> pure t <*> mapM mk1 [0 .. (sz -1)]
      where
        mk1 i =
          liftExpr at t $
            DLE_ArrayRef at a (DLA_Literal (DLL_Int at $ fromIntegral i))

fu_ :: LLBlock -> [(DLVar, DLArg)] -> App DLArg
fu_ b nvs = do
  let DLinBlock at fs t a = b
  let lets = map (\(nv, na) -> DL_Let at (Just nv) (DLE_Arg at na)) nvs
  let t' = foldr DT_Com t lets
  let b' = DLinBlock at fs t' a
  Env {..} <- ask
  DLinBlock _ _ t'' a' <-
    liftIO $ freshen eCounter b'
  liftLocal =<< ul t''
  return $ a'

instance Unroll DLExpr where
  ul = \case
    DLE_ArrayConcat at x y -> do
      (x_ty, x') <- ul_explode at x
      (_, y') <- ul_explode at y
      liftArray at x_ty $ x' <> y'
    DLE_ArrayZip at x y -> do
      (x_ty, x') <- ul_explode at x
      (y_ty, y') <- ul_explode at y
      let ty = T_Tuple [x_ty, y_ty]
      let go xa ya = liftExpr at ty $ DLE_LArg at $ DLLA_Tuple [xa, ya]
      as <- zipWithM go x' y'
      liftArray at ty as
    e -> return $ e

instance Unroll LLCommon where
  ul = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at mdv e -> DL_Let at mdv <$> ul e
    DL_Var at v -> return $ DL_Var at v
    DL_Set at v a -> return $ DL_Set at v a
    DL_LocalIf at c t f -> DL_LocalIf at c <$> ul t <*> ul f
    DL_LocalSwitch at ov csm -> DL_LocalSwitch at ov <$> ul csm
    DL_ArrayMap at ans x a fb -> do
      (_, x') <- ul_explode at x
      r' <- mapM (\xa -> fu_ fb [(a, xa)]) x'
      let r_ty = arrType $ varType ans
      return $ DL_Let at (Just ans) (DLE_LArg at $ DLLA_Array r_ty r')
    DL_ArrayReduce at ans x z b a fb -> do
      (_, x') <- ul_explode at x
      r' <- foldlM (\za xa -> fu_ fb [(b, za), (a, xa)]) z x'
      return $ DL_Let at (Just ans) (DLE_Arg at r')
    DL_MapReduce at mri ans x z b a fb ->
      DL_MapReduce at mri ans x z b a <$> ul fb

ul_m :: Unroll a => (LLCommon -> a -> a) -> LLCommon -> AppT a
ul_m mkk m k = do
  (lifts, m') <- collectLifts $ ul m
  let lifts' = lifts <> (return $ m')
  k' <- ul k
  return $ addLifts mkk lifts' k'

instance Unroll LLTail where
  ul = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> ul_m DT_Com m k

instance Unroll LLBlock where
  ul (DLinBlock at fs b a) =
    DLinBlock at fs <$> ul b <*> pure a

instance Unroll (DLinExportVal LLBlock) where
  ul = \case
    DLEV_Fun a b -> DLEV_Fun a <$> ul b
    DLEV_Arg a   -> return $ DLEV_Arg a
    DLEV_LArg a  -> return $ DLEV_LArg a

instance Unroll LLConsensus where
  ul = \case
    LLC_Com m k -> ul_m LLC_Com m k
    LLC_If at c t f -> LLC_If at c <$> ul t <*> ul f
    LLC_Switch at ov csm -> LLC_Switch at ov <$> ul csm
    LLC_FromConsensus at at' s -> LLC_FromConsensus at at' <$> ul s
    LLC_While at asn inv cond body k ->
      LLC_While at asn <$> ul inv <*> ul cond <*> ul body <*> ul k
    LLC_Continue at asn -> return $ LLC_Continue at asn
    LLC_Only at p l k -> LLC_Only at p <$> ul l <*> ul k

instance Unroll k => Unroll (a, k) where
  ul (a, k) = (,) a <$> ul k

instance Unroll a => Unroll (M.Map k a) where
  ul = mapM ul

instance Unroll a => Unroll (Maybe a) where
  ul = mapM ul

instance Unroll k => Unroll (a, b, c, d, e, k) where
  ul (a, b, c, d, e, k) = (\k' -> (a, b, c, d, e, k')) <$> ul k

instance Unroll LLStep where
  ul = \case
    LLS_Com m k -> ul_m LLS_Com m k
    LLS_Stop at -> pure $ LLS_Stop at
    LLS_Only at p l s -> LLS_Only at p <$> ul l <*> ul s
    LLS_ToConsensus at send recv mtime ->
      LLS_ToConsensus at send <$> ul recv <*> ul mtime

instance Unroll LLProg where
  ul (LLProg at opts ps dli dex s) =
    LLProg at opts ps dli dex <$> ul s

instance Unroll CITail where
  ul = \case
    CT_Com m k -> ul_m CT_Com m k
    CT_If at c t f -> CT_If at c <$> ul t <*> ul f
    CT_Switch at ov csm -> CT_Switch at ov <$> ul csm
    e@(CT_From {}) -> return $ e
    e@(CT_Jump {}) -> return $ e

instance Unroll CIHandler where
  ul = \case
    C_Handler at int last_timev from lasti svs msg amtv timev body ->
      C_Handler at int last_timev from lasti svs msg amtv timev <$> ul body
    C_Loop at svs vars body ->
      C_Loop at svs vars <$> ul body

instance Unroll CIHandlers where
  ul (CHandlers m) = CHandlers <$> ul m

instance Unroll CIProg where
  ul (CPProg at hs) =
    CPProg at <$> ul hs

instance Unroll (EPPs a) where
  ul (EPPs m) = pure $ EPPs m

instance Unroll PIProg where
  ul (PLProg at opts dli dex ep cp) =
    PLProg at opts dli <$> ul dex <*> ul ep <*> ul cp

unrollLoops :: (HasCounter a, Unroll a) => a -> IO a
unrollLoops x = do
  let eCounter = getCounter x
  let emLifts = Nothing
  flip runReaderT (Env {..}) $ ul x
