module Reach.LiftConsensus (liftcon) where

import Control.Monad.Reader
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Sequence as Seq
import Reach.AST.DLBase
import Reach.AST.LL

type App = ReaderT Env IO

type AppT a = a -> App a

data Env = Env
  { eLifts :: Maybe (IORef (Seq.Seq LLCommon))
  }

class CanLift a where
  canLift :: a -> Bool

instance CanLift DLExpr where
  canLift = isLocal

instance CanLift a => CanLift (SwitchCases a) where
  canLift = getAll . mconcatMap (All . canLift . snd . snd) . M.toList

instance CanLift (DLinStmt a) where
  canLift = \case
    DL_Nop {} -> True
    DL_Let _ _ e -> canLift e
    DL_ArrayMap _ _ _ _ f -> canLift f
    DL_ArrayReduce _ _ _ _ _ _ f -> canLift f
    DL_Var {} -> True
    DL_Set {} -> True
    DL_LocalIf _ _ t f -> canLift t && canLift f
    DL_LocalSwitch _ _ csm -> canLift csm

instance CanLift (DLinTail a) where
  canLift = \case
    DT_Return {} -> True
    DT_Com m k -> canLift m && canLift k

instance CanLift (DLinBlock a) where
  canLift = \case
    DLinBlock _ _ t _ -> canLift t

doLift :: LLCommon -> App LLStep -> App LLStep
doLift m mk = do
  Env {..} <- ask
  case (eLifts, canLift m) of
    (Just lr, True) -> do
      liftIO $ modifyIORef lr (Seq.|> m)
      mk
    _ -> LLS_Com m <$> mk

captureLifts :: App LLConsensus -> App LLConsensus
captureLifts mc = do
  lr <- liftIO $ newIORef mempty
  c <- local (\e -> e {eLifts = Just lr}) mc
  ls <- liftIO $ readIORef lr
  return $ foldr LLC_Com c ls

class LiftCon a where
  lc :: AppT a

instance LiftCon a => LiftCon (Maybe a) where
  lc = \case
    Nothing -> return $ Nothing
    Just x -> Just <$> lc x

instance LiftCon z => LiftCon (a, z) where
  lc (a, z) = (\z' -> (a, z')) <$> lc z

instance LiftCon z => LiftCon (a, b, c, d, e, z) where
  lc (a, b, c, d, e, z) = (\z' -> (a, b, c, d, e, z')) <$> lc z

instance LiftCon a => LiftCon (SwitchCases a) where
  lc = traverse lc

instance LiftCon LLConsensus where
  lc = \case
    LLC_Com m k -> LLC_Com m <$> lc k
    LLC_If at c t f -> LLC_If at c <$> lc t <*> lc f
    LLC_Switch at v csm -> LLC_Switch at v <$> lc csm
    LLC_FromConsensus at1 at2 s ->
      captureLifts $ LLC_FromConsensus at1 at2 <$> lc s
    LLC_While at asn inv cond body k ->
      LLC_While at asn inv cond <$> lc body <*> lc k
    LLC_Continue at asn -> return $ LLC_Continue at asn
    LLC_Only at who l c -> LLC_Only at who l <$> lc c

instance LiftCon LLStep where
  lc = \case
    LLS_Com m k -> doLift m (lc k)
    LLS_Stop at -> return $ LLS_Stop at
    LLS_Only at who l s -> LLS_Only at who l <$> lc s
    LLS_ToConsensus at send recv mtime ->
      LLS_ToConsensus at send <$> lc recv <*> lc mtime

liftcon :: LLProg -> IO LLProg
liftcon (LLProg at opts ps dli s) = do
  let eLifts = Nothing
  flip runReaderT (Env {..}) $
    LLProg at opts ps dli <$> lc s
