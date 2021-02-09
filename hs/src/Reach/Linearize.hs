module Reach.Linearize (linearize) where

import Control.Monad.Reader
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Reach.AST.Base
import Reach.AST.DK
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Texty
import Reach.Util

-- Remove returns, duplicate continuations, and transform into dk
type DKApp = ReaderT DKEnv IO

type LLRetRHS = (Maybe (DLVar, M.Map Int (DLStmts, DLArg)))

type LLRets = M.Map Int LLRetRHS

data DKEnv = DKEnv
  {eRets :: LLRets}

lookupRet :: Int -> DKApp (Maybe LLRetRHS)
lookupRet r = do
  DKEnv {..} <- ask
  return $ M.lookup r eRets

captureRets :: DKApp LLRets
captureRets = do
  DKEnv {..} <- ask
  return eRets

restoreRets :: LLRets -> DKApp a -> DKApp a
restoreRets rets' = local (\e -> e {eRets = rets'})

setRetsToEmpty :: DKApp a -> DKApp a
setRetsToEmpty = restoreRets mempty

withReturn :: Int -> LLRetRHS -> DKApp a -> DKApp a
withReturn rv rvv = local (\e@DKEnv {..} -> e {eRets = M.insert rv rvv eRets})

dk2lin :: DKTail -> LLTail
dk2lin = \case
  DK_Com (DKC_ m) k -> DT_Com m $ dk2lin k
  DK_Stop at -> DT_Return at
  _ -> impossible "dk2lin"

lin :: DLStmt -> DKApp LLCommon
lin = \case
  DLS_Let at mdv de ->
    return $ DL_Let at mdv de
  DLS_ArrayMap at ans x a f ->
    DL_ArrayMap at ans x a <$> lin_block at f
  DLS_ArrayReduce at ans x z b a f ->
    DL_ArrayReduce at ans x z b a <$> lin_block at f
  DLS_If at ca _ ts fs ->
    DL_LocalIf at ca <$> lin_local_rets at ts <*> lin_local_rets at fs
  DLS_Switch at dv _ cm ->
    DL_LocalSwitch at dv <$> mapM cm1 cm
    where
      cm1 (dv', l) = (\x -> (dv', x)) <$> lin_local_rets at l
  _ -> impossible "lin"

lin_local_rets :: SrcLoc -> DLStmts -> DKApp LLTail
lin_local_rets at ks = do
  r <- dk_ at ks
  return $ dk2lin r

lin_local :: SrcLoc -> DLStmts -> DKApp LLTail
lin_local at ks = setRetsToEmpty $ lin_local_rets at ks

lin_block :: SrcLoc -> DLBlock -> DKApp LLBlock
lin_block _at (DLBlock at fs l a) =
  DLinBlock at fs <$> lin_local at l <*> pure a

dk_block :: SrcLoc -> DLBlock -> DKApp DKBlock
dk_block _at (DLBlock at fs l a) =
  DKBlock at fs <$> (setRetsToEmpty $ dk_ at l) <*> pure a

dk1 :: SrcLoc -> DLStmts -> DLStmt -> DKApp DKTail
dk1 at_top ks s =
  case s of
    DLS_Let {} -> com
    DLS_ArrayMap {} -> com
    DLS_ArrayReduce {} -> com
    DLS_If at c _ t f ->
      case isLocal s of
        True -> com
        False ->
          DK_If at c <$> dk_ at (t <> ks) <*> dk_ at (f <> ks)
    DLS_Switch at v _ csm ->
      case isLocal s of
        True -> com
        False ->
          DK_Switch at v <$> mapM cm1 csm
          where
            cm1 (dv', c) = (\x -> (dv', x)) <$> dk_ at (c <> ks)
    DLS_Return at ret eda -> do
      rv <- lookupRet ret
      case rv of
        Nothing ->
          impossible $ "unknown ret " <> show ret
        Just Nothing ->
          dk_ at ks
        Just (Just (dv, retsm)) ->
          case eda of
            Left reti ->
              case M.lookup reti retsm of
                Nothing -> impossible $ "missing return da"
                Just (das, da) ->
                  case das <> (return $ DLS_Return at ret (Right da)) <> ks of
                    s' Seq.:<| ks' ->
                      dk1 at ks' s'
                    _ ->
                      impossible $ "no cons"
            Right da ->
              com' $ DL_Set at dv da
    DLS_Prompt at (Left ret) ss ->
      withReturn ret Nothing $
        dk_ at (ss <> ks)
    DLS_Prompt at (Right (dv@(DLVar _ _ _ ret), retms)) ss -> do
      withReturn ret (Just (dv, retms)) $
        com'' (DL_Var at dv) (ss <> ks)
    DLS_Stop at ->
      return $ DK_Stop at
    DLS_Only at who ss ->
      DK_Only at who <$> lin_local at ss <*> dk_ at ks
    DLS_ToConsensus at send recv mtime -> do
      let (winner_dv, msg, amtv, timev, cs) = recv
      let cs' = dk_ at (cs <> ks)
      let recv' =
            (\x -> (winner_dv, msg, amtv, timev, x)) <$> cs'
      let mtime' =
            case mtime of
              Just (delay_da, time_ss) ->
                (\x y -> Just (x, y)) <$> pure delay_da <*> dk_ at (time_ss <> ks)
              Nothing ->
                return $ Nothing
      DK_ToConsensus at send <$> recv' <*> mtime'
    DLS_FromConsensus at ss ->
      DK_FromConsensus at at_top <$> dk_ at (ss <> ks)
    DLS_While at asn inv_b cond_b body -> do
      let body' = dk_ at body
      let block = dk_block at
      DK_While at asn <$> block inv_b <*> block cond_b <*> body' <*> dk_ at ks
    DLS_Continue at asn ->
      return $ DK_Continue at asn
    DLS_FluidSet at fv a ->
      DK_Com (DKC_FluidSet at fv a) <$> dk_ at ks
    DLS_FluidRef at v fv ->
      DK_Com (DKC_FluidRef at v fv) <$> dk_ at ks
  where
    com :: DKApp DKTail
    com = com' =<< lin s
    com' :: LLCommon -> DKApp DKTail
    com' m = com'' m ks
    com'' :: LLCommon -> DLStmts -> DKApp DKTail
    com'' m ks' = DK_Com (DKC_ m) <$> dk_ (srclocOf s) ks'

dk_ :: SrcLoc -> DLStmts -> DKApp DKTail
dk_ at = \case
  Seq.Empty -> return $ DK_Stop at
  s Seq.:<| ks -> dk1 at ks s

dekont :: DLProg -> IO DKProg
dekont (DLProg at opts sps dli ss) = do
  let eRets = mempty
  flip runReaderT (DKEnv {..}) $
    DKProg at opts sps dli <$> dk_ at ss

-- Lift common things to the previous consensus
type LCApp = ReaderT LCEnv IO

type LCAppT a = a -> LCApp a

data LCEnv = LCEnv
  { eLifts :: Maybe (IORef (Seq.Seq DKCommon))
  }

-- FIXME: I think this always returns True
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

instance CanLift DKCommon where
  canLift = \case
    DKC_ m -> canLift m
    DKC_FluidSet {} -> True
    DKC_FluidRef {} -> True

noLifts :: LCApp a -> LCApp a
noLifts = local (\e -> e {eLifts = Nothing})

doLift :: DKCommon -> LCApp DKTail -> LCApp DKTail
doLift m mk = do
  LCEnv {..} <- ask
  case (eLifts, canLift m) of
    (Just lr, True) -> do
      liftIO $ modifyIORef lr (Seq.|> m)
      mk
    _ -> DK_Com m <$> mk

captureLifts :: LCApp DKTail -> LCApp DKTail
captureLifts mc = do
  lr <- liftIO $ newIORef mempty
  c <- local (\e -> e {eLifts = Just lr}) mc
  ls <- liftIO $ readIORef lr
  return $ foldr DK_Com c ls

class LiftCon a where
  lc :: LCAppT a

instance LiftCon a => LiftCon (Maybe a) where
  lc = \case
    Nothing -> return $ Nothing
    Just x -> Just <$> lc x

instance LiftCon z => LiftCon (a, z) where
  lc (a, z) = (\z' -> (a, z')) <$> lc z

instance LiftCon z => LiftCon (b, c, d, e, z) where
  lc (b, c, d, e, z) = (\z' -> (b, c, d, e, z')) <$> lc z

instance LiftCon a => LiftCon (SwitchCases a) where
  lc = traverse lc

instance LiftCon DKTail where
  lc = \case
    DK_Com m k -> doLift m (lc k)
    DK_Stop at -> return $ DK_Stop at
    DK_Only at who l s -> DK_Only at who l <$> lc s
    DK_ToConsensus at send recv mtime -> do
      DK_ToConsensus at send <$> (noLifts $ lc recv) <*> lc mtime
    DK_If at c t f -> DK_If at c <$> lc t <*> lc f
    DK_Switch at v csm -> DK_Switch at v <$> lc csm
    DK_FromConsensus at1 at2 k ->
      captureLifts $ DK_FromConsensus at1 at2 <$> lc k
    DK_While at asn inv cond body k ->
      DK_While at asn inv cond <$> lc body <*> lc k
    DK_Continue at asn -> return $ DK_Continue at asn

liftcon :: DKProg -> IO DKProg
liftcon (DKProg at opts sps dli k) = do
  let eLifts = Nothing
  flip runReaderT (LCEnv {..}) $
    DKProg at opts sps dli <$> lc k

-- Remove fluid variables and convert to proper linear shape
type FluidEnv = M.Map FluidVar (SrcLoc, DLArg)

type FVMap = M.Map FluidVar DLVar

type DFApp = ReaderT DFEnv IO

data DFEnv = DFEnv
  { eCounterR :: Counter
  , eFVMm :: Maybe FVMap
  , eFVE :: FluidEnv
  }

allocVar :: (Int -> a) -> DFApp a
allocVar mk = do
  DFEnv {..} <- ask
  mk <$> (liftIO $ incCounter eCounterR)

fluidRefm :: FluidVar -> DFApp (Maybe (SrcLoc, DLArg))
fluidRefm fv = do
  DFEnv {..} <- ask
  return $ M.lookup fv eFVE

fluidRef :: FluidVar -> DFApp (SrcLoc, DLArg)
fluidRef fv = do
  r <- fluidRefm fv
  case r of
    Nothing -> impossible $ "fluid ref unbound: " <> show fv
    Just x -> return x

fluidSet :: FluidVar -> (SrcLoc, DLArg) -> DFApp a -> DFApp a
fluidSet fv fvv = local (\e@DFEnv {..} -> e {eFVE = M.insert fv fvv eFVE})

withWhileFVMap :: FVMap -> DFApp a -> DFApp a
withWhileFVMap fvm' = local (\e -> e {eFVMm = Just fvm'})

readWhileFVMap :: DFApp FVMap
readWhileFVMap = do
  DFEnv {..} <- ask
  case eFVMm of
    Nothing -> impossible "attempt to read fvm with no fvm"
    Just x -> return x

unpackFVMap :: SrcLoc -> DKTail -> DFApp DKTail
unpackFVMap at k = do
  fvm <- readWhileFVMap
  let go k' (fv, dv) = DK_Com (DKC_FluidSet at fv (DLA_Var dv)) k'
  let k' = foldl' go k (M.toList fvm)
  return $ k'

block_unpackFVMap :: SrcLoc -> DKBlock -> DFApp DKBlock
block_unpackFVMap uat (DKBlock at fs t a) =
  (\x -> DKBlock at fs x a) <$> unpackFVMap uat t

expandFromFVMap :: DLAssignment -> DFApp DLAssignment
expandFromFVMap (DLAssignment updatem) = do
  fvm <- readWhileFVMap
  let go (fv, dv) = do
        (_, da) <- fluidRef fv
        return $ (dv, da)
  fvm'l <- mapM go $ M.toList fvm
  let updatem' = M.union (M.fromList $ fvm'l) updatem
  return $ DLAssignment updatem'

df_com :: (LLCommon -> a -> a) -> (DKTail -> DFApp a) -> DKTail -> DFApp a
df_com mkk back = \case
  DK_Com (DKC_FluidSet at fv da) k ->
    fluidSet fv (at, da) (back k)
  DK_Com (DKC_FluidRef at dv fv) k -> do
    (at', da) <- fluidRef fv
    mkk <$> (pure $ DL_Let at (Just dv) (DLE_Arg at' da)) <*> back k
  DK_Com (DKC_ m) k ->
    mkk m <$> back k
  t -> impossible $ "df_com " <> show t

df_bl :: DKBlock -> DFApp LLBlock
df_bl (DKBlock at fs t a) =
  DLinBlock at fs <$> go t <*> pure a
  where
    go = \case
      DK_Stop sat -> return $ DT_Return sat
      x -> df_com DT_Com go x

df_con :: DKTail -> DFApp LLConsensus
df_con = \case
  DK_If a c t f ->
    LLC_If a c <$> df_con t <*> df_con f
  DK_Switch a v csm ->
    LLC_Switch a v <$> mapM cm1 csm
    where
      cm1 (dv', c) = (\x -> (dv', x)) <$> df_con c
  DK_While at asn inv cond body k -> do
    let go fv = do
          r <- fluidRefm fv
          case r of
            Nothing -> return $ Nothing
            Just _ -> do
              dv <- allocVar $ DLVar at (show fv) (fluidVarType fv)
              return $ Just (fv, dv)
    fvm <- M.fromList <$> catMaybes <$> mapM go allFluidVars
    let body_fvs' = df_con =<< unpackFVMap at body
    --- Note: The invariant and condition can't return
    let block b = df_bl =<< block_unpackFVMap at b
    withWhileFVMap fvm $
      LLC_While at <$> expandFromFVMap asn <*> block inv <*> block cond <*> body_fvs' <*> (df_con =<< unpackFVMap at k)
  DK_Continue at asn ->
    LLC_Continue at <$> expandFromFVMap asn
  DK_Only at who body k ->
    LLC_Only at who body <$> df_con k
  DK_FromConsensus at1 at2 t -> do
    -- This was formerly done inside of Eval.hs, but that meant that these refs
    -- and sets would dominate the lifted ones in the step body, which defeats
    -- the purpose of lifting fluid variable interactions, so we instead build
    -- it into this pass
    tct <- fluidRef FV_thisConsensusTime
    fluidSet FV_lastConsensusTime tct $
      LLC_FromConsensus at1 at2 <$> df_step t
  x -> df_com LLC_Com df_con x

df_step :: DKTail -> DFApp LLStep
df_step = \case
  DK_Stop at -> return $ LLS_Stop at
  DK_Only at who body k -> LLS_Only at who body <$> df_step k
  DK_ToConsensus at send recv mtime -> do
    let (b, c, d, e, k) = recv
    let cvt = \case
          DLA_Var v -> v
          _ -> impossible $ "lct not a variable"
    ltv <- fmap (cvt . snd) <$> fluidRefm FV_lastConsensusTime
    k' <- df_con k
    let recv' = (ltv, b, c, d, e, k')
    mtime' <-
      case mtime of
        Nothing -> return $ Nothing
        Just (ta, tk) -> do
          tk' <- df_step tk
          return $ Just (ta, tk')
    return $ LLS_ToConsensus at send recv' mtime'
  x -> df_com LLS_Com df_step x

defluid :: DKProg -> IO LLProg
defluid (DKProg at (DLOpts {..}) sps dli k) = do
  let llo_deployMode = dlo_deployMode
  let llo_verifyOverflow = dlo_verifyOverflow
  let llo_counter = dlo_counter
  let opts' = LLOpts {..}
  let eCounterR = llo_counter
  let eFVMm = mempty
  let eFVE = mempty
  flip runReaderT (DFEnv {..}) $ do
    k' <- df_step k
    return $ LLProg at opts' sps dli k'

-- Stich it all together
linearize :: (forall a. Pretty a => String -> a -> IO ()) -> DLProg -> IO LLProg
linearize outm p =
  return p >>= out "dk" dekont >>= out "lc" liftcon >>= out "df" defluid
  where
    out lab f p' = do
      p'' <- f p'
      outm lab p''
      return p''
