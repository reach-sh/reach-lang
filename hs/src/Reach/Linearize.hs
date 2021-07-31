module Reach.Linearize (linearize, Error(..)) where

import Control.Monad.Reader
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DK
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Texty
import Reach.Util

-- Remove returns, duplicate continuations, and transform into dk
data Error
  = Err_Unreachable String
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode Error where
  errPrefix = const "RL"
  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If you delete a constructor, do NOT re-allocate the number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_Unreachable {} -> 0

instance Show Error where
  show = \case
    Err_Unreachable s -> "code must not be reachable: " <> s

type DKApp = ReaderT DKEnv IO

type LLRetRHS = (DLStmts, Maybe (DLVar, M.Map Int (DLStmts, DLArg)))

type LLRets = M.Map Int LLRetRHS

data Handler = Handler
  { hV :: DLVar
  , hS :: DLStmts
  }

data DKEnv = DKEnv
  { eRets :: LLRets
  , eExnHandler :: Maybe Handler
  }

lookupRet :: Int -> DKApp (Maybe LLRetRHS)
lookupRet r = do
  DKEnv {..} <- ask
  return $ M.lookup r eRets

restoreRets :: LLRets -> DKApp a -> DKApp a
restoreRets rets' = local (\e -> e {eRets = rets'})

setRetsToEmpty :: DKApp a -> DKApp a
setRetsToEmpty = restoreRets mempty

abortRets :: DKApp a -> DKApp a
abortRets = local (\e -> e { eRets = M.map h $ eRets e })
  where
    h (_, rv) = (mempty, rv)

withReturn :: Int -> LLRetRHS -> DKApp a -> DKApp a
withReturn rv rvv = local (\e@DKEnv {..} -> e {eRets = M.insert rv rvv eRets})

dkc :: DLSStmt -> DKApp DKCommon
dkc = \case
  DLS_Let at mdv de ->
    return $ DKC_Let at mdv de
  DLS_ArrayMap at ans x a f ->
    DKC_ArrayMap at ans x a <$> dk_block at f
  DLS_ArrayReduce at ans x z b a f ->
    DKC_ArrayReduce at ans x z b a <$> dk_block at f
  DLS_If at ca _ ts fs ->
    DKC_LocalIf at ca <$> dk_ at ts <*> dk_ at fs
  DLS_Switch at dv _ cm ->
    DKC_LocalSwitch at dv <$> mapM cm1 cm
    where
      cm1 (dv', l) = (\x -> (dv', x)) <$> dk_ at l
  DLS_MapReduce at mri ans x z b a f ->
    DKC_MapReduce at mri ans x z b a <$> dk_block at f
  DLS_FluidSet at fv a -> return $ DKC_FluidSet at fv a
  DLS_FluidRef at v fv -> return $ DKC_FluidRef at v fv
  DLS_Only at who ss -> DKC_Only at who <$> dk_ at ss
  _ -> impossible "dkc"

dk_block :: SrcLoc -> DLSBlock -> DKApp DKBlock
dk_block _at (DLSBlock at fs l a) =
  DKBlock at fs <$> (setRetsToEmpty $ dk_ at l) <*> pure a

dk1 :: SrcLoc -> DLStmts -> DLSStmt -> DKApp DKTail
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
        Just (rks, Nothing) -> do
          dk_ at rks
          -- OLD dk_ at ks
        Just (rks, (Just (dv, retsm))) -> do
          let nks = rks
          -- OLD let nks = ks
          case eda of
            Left reti ->
              case M.lookup reti retsm of
                Nothing -> impossible $ "missing return da"
                Just (das, da) ->
                  case das <> (return $ DLS_Return at ret (Right da)) <> nks of
                    s' Seq.:<| ks' ->
                      dk1 at ks' s'
                    _ ->
                      impossible $ "no cons"
            Right da ->
              com'' (DKC_Set at dv da) nks
    DLS_Prompt at (Left ret) ss ->
      withReturn ret (ks, Nothing) $ do
        -- In this case, and the next one, we include `ks` in the
        -- continuation/body, because the body may actually be a LocalSwitch/If
        -- that will ignore the continuation. If we were *really* doing CPS
        -- then we wouldn't do this and instead we would remove the `abortRets`
        -- thing and in fact we'd remove Var/Set altogether and just turn them
        -- into Lets. Is it worth doing it *this* way? I'm not sure... my
        -- assumption is that duplicating code is bad so we should avoid it at
        -- all costs.
        dk_ at (ss <> ks)
    DLS_Prompt at (Right (dv@(DLVar _ _ _ ret), retms)) ss -> do
      withReturn ret (ks, (Just (dv, retms))) $ do
        com'' (DKC_Var at dv) (ss <> ks)
    DLS_Stop at ->
      return $ DK_Stop at
    DLS_Unreachable at fs m ->
      expect_throw (Just fs) at $ Err_Unreachable m
    DLS_ToConsensus at send recv mtime -> do
      let cs = dr_k recv
      cs' <- dk_ at (cs <> ks)
      let recv' = recv {dr_k = cs'}
      let mtime' =
            case mtime of
              Just (delay_da, time_ss) ->
                (\x y -> Just (x, y)) <$> pure delay_da <*> dk_ at (time_ss <> ks)
              Nothing ->
                return $ Nothing
      DK_ToConsensus at send recv' <$> mtime'
    DLS_FromConsensus at ss ->
      DK_FromConsensus at at_top <$> dk_ at (ss <> ks)
    DLS_While at asn inv_b cond_b body -> do
      let body' = dk_ at body
      let block = dk_block at
      DK_While at asn <$> block inv_b <*> block cond_b <*> body' <*> dk_ at ks
    DLS_Continue at asn ->
      return $ DK_Continue at asn
    DLS_FluidSet {} -> com
    DLS_FluidRef {} -> com
    DLS_MapReduce {} -> com
    DLS_Only {} -> com
    DLS_Throw at da _ -> do
      handler <- asks eExnHandler
      case handler of
        Nothing -> impossible "dk: encountered `throw` without an exception handler"
        Just h ->
          com'' (DKC_Let at (DLV_Let DVC_Many $ hV h) $ DLE_Arg at da) $ hS h
    DLS_Try at e hv hs ->
      local
        (\env ->
           env {eExnHandler = Just (Handler hv (hs <> ks))})
        $ dk_ at (e <> ks)
    DLS_ViewIs at vn vk mva -> do
      mva' <- maybe (return $ Nothing) (\eb -> Just <$> dk_eb eb) mva
      DK_ViewIs at vn vk mva' <$> dk_ at ks
  where
    com :: DKApp DKTail
    com = com' =<< (abortRets $ dkc s)
    com' :: DKCommon -> DKApp DKTail
    com' m = com'' m ks
    com'' :: DKCommon -> DLStmts -> DKApp DKTail
    com'' m ks' = DK_Com m <$> dk_ (srclocOf s) ks'

dk_ :: SrcLoc -> DLStmts -> DKApp DKTail
dk_ at = \case
  Seq.Empty -> return $ DK_Stop at
  s Seq.:<| ks -> dk1 at ks s

dk_eb :: DLSExportBlock -> DKApp DKExportBlock
dk_eb (DLinExportBlock at vs b) =
  resetDK $
    DLinExportBlock at vs <$> dk_block at b

resetDK :: DKApp a -> DKApp a
resetDK = local (const $ DKEnv {..})
  where
    eRets = mempty
    eExnHandler = Nothing

dekont :: DLProg -> IO DKProg
dekont (DLProg at opts sps dli dex dvs ss) = do
  let eRets = mempty
  let eExnHandler = Nothing
  flip runReaderT (DKEnv {..}) $ do
    dex' <- mapM dk_eb dex
    DKProg at opts sps dli dex' dvs <$> dk_ at ss

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

instance CanLift DKTail where
  canLift = \case
    DK_Stop {} -> True
    DK_Com m k -> canLift m && canLift k
    _ -> impossible "canLift on DKLTail"

instance CanLift DKBlock where
  canLift = \case
    DKBlock _ _ t _ -> canLift t

instance CanLift DKCommon where
  canLift = \case
    DKC_Let _ _ e -> canLift e
    DKC_ArrayMap _ _ _ _ f -> canLift f
    DKC_ArrayReduce _ _ _ _ _ _ f -> canLift f
    DKC_Var {} -> True
    DKC_Set {} -> True
    DKC_LocalIf _ _ t f -> canLift t && canLift f
    DKC_LocalSwitch _ _ csm -> canLift csm
    DKC_MapReduce _ _ _ _ _ _ _ f -> canLift f
    DKC_FluidSet {} -> True
    DKC_FluidRef {} -> True
    DKC_Only {} -> False --- XXX maybe okay

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

instance LiftCon z => LiftCon (DLRecv z) where
  lc r = (\z' -> r {dr_k = z'}) <$> lc (dr_k r)

instance LiftCon a => LiftCon (SwitchCases a) where
  lc = traverse lc

instance LiftCon DKBlock where
  lc (DKBlock at sf b a) =
    DKBlock at sf <$> lc b <*> pure a

instance LiftCon a => LiftCon (DLinExportBlock a) where
  lc (DLinExportBlock at vs b) =
    DLinExportBlock at vs <$> lc b

instance LiftCon DKExports where
  lc = mapM lc

instance LiftCon DKTail where
  lc = \case
    DK_Com m k -> doLift m (lc k)
    DK_Stop at -> return $ DK_Stop at
    DK_ToConsensus at send recv mtime -> do
      DK_ToConsensus at send <$> (noLifts $ lc recv) <*> lc mtime
    DK_If at c t f -> DK_If at c <$> lc t <*> lc f
    DK_Switch at v csm -> DK_Switch at v <$> lc csm
    DK_FromConsensus at1 at2 k ->
      captureLifts $ DK_FromConsensus at1 at2 <$> lc k
    DK_While at asn inv cond body k ->
      DK_While at asn inv cond <$> lc body <*> lc k
    DK_Continue at asn -> return $ DK_Continue at asn
    DK_ViewIs at vn vk a k ->
      DK_ViewIs at vn vk a <$> lc k

liftcon :: DKProg -> IO DKProg
liftcon (DKProg at opts sps dli dex dvs k) = do
  let eLifts = Nothing
  flip runReaderT (LCEnv {..}) $
    DKProg at opts sps dli <$> lc dex <*> pure dvs <*> lc k

-- Remove fluid variables and convert to proper linear shape
type FluidEnv = M.Map FluidVar (SrcLoc, DLArg)

type FVMap = M.Map FluidVar DLVar

type DFApp = ReaderT DFEnv IO

data DFEnv = DFEnv
  { eCounterR :: Counter
  , eFVMm :: Maybe FVMap
  , eFVE :: FluidEnv
  , eFVs :: [FluidVar]
  }

allocVar :: (Int -> a) -> DFApp a
allocVar mk = do
  DFEnv {..} <- ask
  mk <$> (liftIO $ incCounter eCounterR)

fluidRefm :: FluidVar -> DFApp (Maybe (SrcLoc, DLArg))
fluidRefm fv = do
  DFEnv {..} <- ask
  return $ M.lookup fv eFVE

fluidRef :: SrcLoc -> FluidVar -> DFApp (SrcLoc, DLArg)
fluidRef at fv = do
  r <- fluidRefm fv
  case r of
    Nothing -> impossible $ "fluid ref unbound: " <> show fv <> " at: " <> show at
    Just x -> return x

fluidSet_ :: FluidVar -> (SrcLoc, DLArg) -> DFApp a -> DFApp a
fluidSet_ fv fvv = local (\e@DFEnv {..} -> e {eFVE = M.insert fv fvv eFVE})

fluidSet :: FluidVar -> (SrcLoc, DLArg) -> DFApp a -> DFApp a
fluidSet fv fvv m = fluidSet_ fv fvv $ more m
  where
    more =
      case fv of
        FV_thisConsensusTime -> fluidSet_ FV_baseWaitTime fvv
        FV_thisConsensusSecs -> fluidSet_ FV_baseWaitSecs fvv
        _ -> id

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

expandFromFVMap :: SrcLoc -> DLAssignment -> DFApp DLAssignment
expandFromFVMap at (DLAssignment updatem) = do
  fvm <- readWhileFVMap
  let go (fv, dv) = do
        (_, da) <- fluidRef at fv
        return $ (dv, da)
  fvm'l <- mapM go $ M.toList fvm
  let updatem' = M.union (M.fromList $ fvm'l) updatem
  return $ DLAssignment updatem'

df_com :: HasCallStack => (DLStmt -> a -> a) -> (DKTail -> DFApp a) -> DKTail -> DFApp a
df_com mkk back = \case
  DK_Com (DKC_FluidSet at fv da) k ->
    fluidSet fv (at, da) (back k)
  DK_Com (DKC_FluidRef at dv fv) k -> do
    (at', da) <- fluidRef at fv
    mkk <$> (pure $ DL_Let at (DLV_Let DVC_Many dv) (DLE_Arg at' da)) <*> back k
  DK_Com m k -> do
    m' <-
      case m of
        DKC_Let a b c -> return $ DL_Let a b c
        DKC_ArrayMap a b c d x -> DL_ArrayMap a b c d <$> df_bl x
        DKC_ArrayReduce a b c d e f x -> DL_ArrayReduce a b c d e f <$> df_bl x
        DKC_Var a b -> return $ DL_Var a b
        DKC_Set a b c -> return $ DL_Set a b c
        DKC_LocalIf a b x y -> DL_LocalIf a b <$> df_t x <*> df_t y
        DKC_LocalSwitch a b x -> DL_LocalSwitch a b <$> mapM go x
          where
            go (c, y) = (,) c <$> df_t y
        DKC_MapReduce a mri b c d e f x -> DL_MapReduce a mri b c d e f <$> df_bl x
        DKC_Only a b c -> DL_Only a (Left b) <$> df_t c
        _ -> impossible "df_com"
    mkk m' <$> back k
  DK_ViewIs _ _ _ _ k ->
    -- This can only occur inside of the while cond & invariant and it is safe
    -- to throw out
    back k
  t -> impossible $ show $ "df_com " <> pretty t

df_bl :: DKBlock -> DFApp DLBlock
df_bl (DKBlock at fs t a) =
  DLBlock at fs <$> df_t t <*> pure a

df_t :: DKTail -> DFApp DLTail
df_t = \case
  DK_Stop at -> return $ DT_Return at
  x -> df_com (mkCom DT_Com) df_t x

df_con :: DKTail -> DFApp LLConsensus
df_con = \case
  DK_If a c t f ->
    LLC_If a c <$> df_con t <*> df_con f
  DK_Switch a v csm ->
    LLC_Switch a v <$> mapM cm1 csm
    where
      cm1 (dv', c) = (\x -> (dv', x)) <$> df_con c
  DK_While at asn inv cond body k -> do
    fvs <- eFVs <$> ask
    let go fv = do
          r <- fluidRefm fv
          case r of
            Nothing -> return $ Nothing
            Just _ -> do
              dv <- allocVar $ DLVar at (Just (srcloc_builtin, show $ pretty fv)) (fluidVarType fv)
              return $ Just (fv, dv)
    fvm <- M.fromList <$> catMaybes <$> mapM go fvs
    let body_fvs' = df_con =<< unpackFVMap at body
    --- Note: The invariant and condition can't return
    let block b = df_bl =<< block_unpackFVMap at b
    (makeWhile, k') <-
      withWhileFVMap fvm $
        (,) <$> (LLC_While at <$> expandFromFVMap at asn <*> block inv <*> block cond <*> body_fvs') <*> (unpackFVMap at k)
    makeWhile <$> df_con k'
  DK_Continue at asn ->
    LLC_Continue at <$> expandFromFVMap at asn
  DK_FromConsensus at1 at2 t -> do
    -- This was formerly done inside of Eval.hs, but that meant that these refs
    -- and sets would dominate the lifted ones in the step body, which defeats
    -- the purpose of lifting fluid variable interactions, so we instead build
    -- it into this pass
    tct <- fluidRef at1 FV_thisConsensusTime
    tcs <- fluidRef at1 FV_thisConsensusSecs
    fluidSet FV_lastConsensusTime tct $
      fluidSet FV_lastConsensusSecs tcs $
        LLC_FromConsensus at1 at2 <$> df_step t
  DK_ViewIs at vn vk mva k -> do
    mva' <- maybe (return $ Nothing) (\eb -> Just <$> df_eb eb) mva
    k' <- df_con k
    return $ LLC_ViewIs at vn vk mva' k'
  x -> df_com (mkCom LLC_Com) df_con x

df_step :: DKTail -> DFApp LLStep
df_step = \case
  DK_Stop at -> return $ LLS_Stop at
  DK_ToConsensus at send recv mtime -> do
    k' <- df_con $ dr_k recv
    let recv' = recv {dr_k = k'}
    mtime' <-
      case mtime of
        Nothing -> return $ Nothing
        Just (ta, tk) -> do
          tk' <- df_step tk
          return $ Just (ta, tk')
    return $ LLS_ToConsensus at send recv' mtime'
  x -> df_com (mkCom LLS_Com) df_step x

df_eb :: DKExportBlock -> DFApp DLExportBlock
df_eb (DLinExportBlock at vs b) =
  DLinExportBlock at vs <$> df_bl b

defluid :: DKProg -> IO LLProg
defluid (DKProg at (DLOpts {..}) sps dli dex dvs k) = do
  let llo_deployMode = dlo_deployMode
  let llo_verifyArithmetic = dlo_verifyArithmetic
  let llo_counter = dlo_counter
  let llo_droppedAsserts = dlo_droppedAsserts
  let opts' = LLOpts {..}
  let eCounterR = llo_counter
  let eFVMm = mempty
  let eFVE = mempty
  let eFVs = allFluidVars dlo_bals
  flip runReaderT (DFEnv {..}) $ do
    dex' <- mapM df_eb dex
    k' <- df_step k
    return $ LLProg at opts' sps dli dex' dvs k'

-- Stich it all together
linearize :: (forall a. Pretty a => String -> a -> IO ()) -> DLProg -> IO LLProg
linearize outm p =
  return p >>= out "dk" dekont >>= out "lc" liftcon >>= out "df" defluid
  where
    out lab f p' = do
      p'' <- f p'
      outm lab p''
      return p''
