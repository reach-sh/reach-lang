module Reach.Linearize (linearize, Error (..)) where

import Control.Monad.Reader
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DK
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Freshen
import Reach.Texty
import Reach.Util

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

allocVar :: (e -> Counter) -> ReaderT e IO Int
allocVar ef = asks ef >>= (liftIO . incCounter)

-- Remove returns, duplicate continuations, and transform into dk

type DKApp = ReaderT DKEnv IO

type LLRetRHS = (DLVar, Bool, DKTail)

type LLRet = (Int, LLRetRHS)

data Handler = Handler
  { hV :: DLVar
  , hK :: DKTail
  }

data DKEnv = DKEnv
  { eRet :: Maybe LLRet
  , eExnHandler :: Maybe Handler
  }

withReturn :: Int -> LLRetRHS -> DKApp a -> DKApp a
withReturn rv rvv = local (\e -> e {eRet = Just (rv, rvv)})

data DKBranchMode
  = DKBM_Con
  | DKBM_Do

getDKBM :: IsLocal a => a -> DKApp DKBranchMode
getDKBM x =
  case isLocal x of
    False -> return $ DKBM_Con
    True -> do
      asks eRet >>= \case
        Nothing -> return $ DKBM_Do
        Just (_, (_, True, _)) -> return $ DKBM_Con
        Just (_, (_, False, _)) -> return $ DKBM_Do

dk_block :: SrcLoc -> DLSBlock -> DKApp DKBlock
dk_block _ (DLSBlock at fs l a) =
  DKBlock at fs <$> dk_top at l <*> pure a

turnVarIntoLet :: Bool
turnVarIntoLet = True

dk1 :: DKTail -> DLSStmt -> DKApp DKTail
dk1 k s =
  case s of
    DLS_Let at mdv de -> com $ DKC_Let at mdv de
    DLS_ArrayMap at ans x a i f ->
      com' $ DKC_ArrayMap at ans x a i <$> dk_block at f
    DLS_ArrayReduce at ans x z b a i f ->
      com' $ DKC_ArrayReduce at ans x z b a i <$> dk_block at f
    DLS_If at c _ t f -> do
      let con = DK_If at c
      let loc t' f' = DK_Com (DKC_LocalIf at c t' f') k
      let mt = DK_Stop at
      (mk, k') <-
        getDKBM s >>= \case
          DKBM_Con -> return $ (con, k)
          DKBM_Do -> return $ (loc, mt)
      mk <$> dk_ k' t <*> dk_ k' f
    DLS_Switch at v _ csm -> do
      let con = DK_Switch at v
      let loc csm' = DK_Com (DKC_LocalSwitch at v csm') k
      let mt = DK_Stop at
      (mk, k') <-
        getDKBM s >>= \case
          DKBM_Con -> return $ (con, k)
          DKBM_Do -> return $ (loc, mt)
      let cm1 (dv', b, l) = (,,) dv' b <$> dk_ k' l
      mk <$> mapM cm1 csm
    DLS_Return at ret da ->
      asks eRet >>= \case
        Nothing -> impossible $ "return not in prompt"
        Just (ret', (dv, isCon, rk)) ->
          case ret == ret' of
            False ->
              impossible $ "return not nested: " <> show (ret, ret')
            True ->
              case turnVarIntoLet && isCon of
                True ->
                  return $ DK_Com (DKC_Let at (DLV_Let DVC_Many dv) $ DLE_Arg at da) rk
                False ->
                  return $ DK_Com (DKC_Set at dv da) rk
    DLS_Prompt at dv@(DLVar _ _ _ ret) _ ss ->
      case isLocal s of
        True -> do
          ss' <- withReturn ret (dv, False, DK_Stop at) $ dk_top at ss
          return $ DK_Com (DKC_Var at dv) $ DK_Com (DKC_LocalDo at ss') k
        False ->
          withReturn ret (dv, True, k) $
            case turnVarIntoLet of
              True -> dk_ k ss
              False -> DK_Com (DKC_Var at dv) <$> dk_ k ss
    DLS_Stop at -> return $ DK_Stop at
    DLS_Unreachable at fs m -> return $ DK_Unreachable at fs m
    DLS_ToConsensus at send recv mtime -> do
      let cs0 = dr_k recv
      let cs =
            case cs0 of
              -- We are forcing an initial switch to be in CPS, assuming that
              -- this is a fork and that this is a good idea
              ((Seq.:<|) (DLS_Prompt pa pb pc ((Seq.:<|) (DLS_Switch sa swb sc sd) Seq.Empty)) r) ->
                ((Seq.<|) (DLS_Prompt pa pb (go pc) ((Seq.<|) (DLS_Switch sa swb (go sc) sd) Seq.empty)) r)
                where
                  go x = x {sa_local = False}
              _ -> cs0
      cs' <- dk_ k cs
      let recv' = recv {dr_k = cs'}
      let go (ta, time_ss) = (,) ta <$> dk_ k time_ss
      DK_ToConsensus at send recv' <$> mapM go mtime
    DLS_FromConsensus at ss -> DK_FromConsensus at at <$> dk_ k ss
    DLS_While at asn inv_b cond_b body -> do
      let body' = dk_top at body
      let block = dk_block at
      DK_While at asn <$> block inv_b <*> block cond_b <*> body' <*> pure k
    DLS_Continue at asn -> return $ DK_Continue at asn
    DLS_FluidSet at fv a -> com $ DKC_FluidSet at fv a
    DLS_FluidRef at v fv -> com $ DKC_FluidRef at v fv
    DLS_MapReduce at mri ans x z b a f ->
      com' $ DKC_MapReduce at mri ans x z b a <$> dk_block at f
    DLS_Only at who ss ->
      com' $ DKC_Only at who <$> dk_top at ss
    DLS_Throw at da _ -> do
      asks eExnHandler >>= \case
        Nothing ->
          impossible "dk: encountered `throw` without an exception handler"
        Just (Handler {..}) ->
          com'' (DKC_Let at (DLV_Let DVC_Many hV) $ DLE_Arg at da) hK
    DLS_Try _at e hV hs -> do
      hK <- dk_ k hs
      local (\env -> env {eExnHandler = Just (Handler {..})}) $
        dk_ k e
    DLS_ViewIs at vn vk mva -> do
      mva' <- maybe (return $ Nothing) (\eb -> Just <$> dk_eb eb) mva
      return $ DK_ViewIs at vn vk mva' k
  where
    com :: DKCommon -> DKApp DKTail
    com = flip com'' k
    com' :: DKApp DKCommon -> DKApp DKTail
    com' m = com =<< m
    com'' :: DKCommon -> DKTail -> DKApp DKTail
    com'' m k' = return $ DK_Com m k'

dk_ :: DKTail -> DLStmts -> DKApp DKTail
dk_ k = \case
  Seq.Empty -> return k
  s Seq.:<| ks -> flip dk1 s =<< dk_ k ks

dk_top :: SrcLoc -> DLStmts -> DKApp DKTail
dk_top = dk_ . DK_Stop

dk_eb :: DLSExportBlock -> DKApp DKExportBlock
dk_eb (DLinExportBlock at vs b) =
  resetDK $ DLinExportBlock at vs <$> dk_block at b

resetDK :: DKApp a -> DKApp a
resetDK = local (\e -> e {eRet = Nothing, eExnHandler = Nothing})

dekont :: DLProg -> IO DKProg
dekont (DLProg at opts sps dli dex dvs das devts ss) = do
  let eRet = Nothing
  let eExnHandler = Nothing
  flip runReaderT (DKEnv {..}) $ do
    dex' <- mapM dk_eb dex
    DKProg at opts sps dli dex' dvs das devts <$> dk_top at ss

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
  canLift = getAll . mconcatMap (All . go) . M.toList
    where
      go (_, (_, _, k)) = canLift k

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
    DKC_ArrayMap _ _ _ _ _ f -> canLift f
    DKC_ArrayReduce _ _ _ _ _ _ _ f -> canLift f
    DKC_Var {} -> True
    DKC_Set {} -> True
    DKC_LocalDo _ t -> canLift t
    DKC_LocalIf _ _ t f -> canLift t && canLift f
    DKC_LocalSwitch _ _ csm -> canLift csm
    DKC_MapReduce _ _ _ _ _ _ _ f -> canLift f
    DKC_FluidSet {} -> True
    DKC_FluidRef {} -> True
    DKC_Only {} -> False --- XXX maybe okay
    DKC_setApiDetails {} -> False

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

captureLifts :: SrcLoc -> LCApp DKTail -> LCApp DKTail
captureLifts at mc = do
  lr <- liftIO $ newIORef mempty
  c <- local (\e -> e {eLifts = Just lr}) mc
  ls <- liftIO $ readIORef lr
  return $ DK_LiftBoundary at $ foldr DK_Com c ls

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
  lc = mapM (\(a, b, c) -> (,,) a b <$> lc c)

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
      captureLifts at1 $ DK_FromConsensus at1 at2 <$> lc k
    DK_While at asn inv cond body k ->
      DK_While at asn inv cond <$> lc body <*> lc k
    DK_Continue at asn -> return $ DK_Continue at asn
    DK_ViewIs at vn vk a k ->
      DK_ViewIs at vn vk a <$> lc k
    DK_Unreachable at fs m ->
      expect_throw (Just fs) at $ Err_Unreachable m
    DK_LiftBoundary {} ->
      impossible "lift boundary before liftcon"

liftcon :: DKProg -> IO DKProg
liftcon (DKProg at opts sps dli dex dvs das devts k) = do
  let eLifts = Nothing
  flip runReaderT (LCEnv {..}) $
    DKProg at opts sps dli <$> lc dex <*> pure dvs <*> pure das <*> pure devts <*> lc k

-- Remove fluid variables and convert to proper linear shape
type FluidEnv = M.Map FluidVar (SrcLoc, DLArg)

type FVMap = M.Map FluidVar DLVar

type DFApp = ReaderT DFEnv IO

data DFEnv = DFEnv
  { eCounter_df :: Counter
  , eFVMm :: Maybe FVMap
  , eFVE :: FluidEnv
  , eFVs :: [FluidVar]
  }

df_allocVar :: DFApp Int
df_allocVar = allocVar eCounter_df

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
        DKC_ArrayMap a b c d e x -> DL_ArrayMap a b c d e <$> df_bl x
        DKC_ArrayReduce a b c d e f g x -> DL_ArrayReduce a b c d e f g <$> df_bl x
        DKC_Var a b -> return $ DL_Var a b
        DKC_Set a b c -> return $ DL_Set a b c
        DKC_LocalDo a x -> DL_LocalDo a <$> df_t x
        DKC_LocalIf a b x y -> DL_LocalIf a b <$> df_t x <*> df_t y
        DKC_LocalSwitch a b x -> DL_LocalSwitch a b <$> mapM go x
          where
            go (c, vu, y) = (,,) c vu <$> df_t y
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
      cm1 (dv', b, c) = (\x -> (dv', b, x)) <$> df_con c
  DK_While at asn inv cond body k -> do
    fvs <- eFVs <$> ask
    let go fv = do
          r <- fluidRefm fv
          case r of
            Nothing -> return $ Nothing
            Just _ -> do
              dv <- DLVar at (Just (sb, show $ pretty fv)) (fluidVarType fv) <$> df_allocVar
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
  DK_LiftBoundary at t -> do
    -- This was formerly done inside of Eval.hs, but that meant that these refs
    -- and sets would dominate the lifted ones in the step body, which defeats
    -- the purpose of lifting fluid variable interactions, so we instead build
    -- it into this pass
    tct <- fluidRef at FV_thisConsensusTime
    tcs <- fluidRef at FV_thisConsensusSecs
    fluidSet FV_lastConsensusTime tct $
      fluidSet FV_lastConsensusSecs tcs $
        fluidSet FV_baseWaitTime tct $
          fluidSet FV_baseWaitSecs tcs $
            df_con t
  DK_FromConsensus at1 at2 t -> do
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
    lt <- fmap snd <$> fluidRefm FV_thisConsensusTime
    let tt = dr_time recv
    ls <- fmap snd <$> fluidRefm FV_thisConsensusSecs
    let ts = dr_secs recv
    k' <-
      df_con $
        DK_Com (DKC_Let at DLV_Eff (DLE_TimeOrder at [(lt, tt), (ls, ts)])) $
          dr_k recv
    let recv' = recv {dr_k = k'}
    mtime' <-
      case mtime of
        Nothing -> return $ Nothing
        Just (ta, tk) -> do
          tk' <- df_step tk
          return $ Just (ta, tk')
    let lt' = fromMaybe (DLA_Literal $ DLL_Int at 0) lt
    return $ LLS_ToConsensus at lt' send recv' mtime'
  x -> df_com (mkCom LLS_Com) df_step x

df_eb :: DKExportBlock -> DFApp DLExportBlock
df_eb (DLinExportBlock at vs b) =
  DLinExportBlock at vs <$> df_bl b

defluid :: DKProg -> IO LLProg
defluid (DKProg at (DLOpts {..}) sps dli dex dvs das devts k) = do
  let llo_verifyArithmetic = dlo_verifyArithmetic
  let llo_untrustworthyMaps = dlo_untrustworthyMaps
  let llo_counter = dlo_counter
  let llo_droppedAsserts = dlo_droppedAsserts
  let opts' = LLOpts {..}
  let eCounter_df = getCounter opts'
  let eFVMm = mempty
  let eFVE = mempty
  let eFVs = allFluidVars dlo_bals
  flip runReaderT (DFEnv {..}) $ do
    dex' <- mapM df_eb dex
    k' <- df_step k
    return $ LLProg at opts' sps dli dex' dvs das devts k'

-- Stich it all together
linearize :: (forall a. Pretty a => T.Text -> a -> IO ()) -> DLProg -> IO LLProg
linearize outm p =
  return p >>= out "dk" dekont >>= out "lc" liftcon >>= out "df" defluid >>= out "fu" freshen_top
  where
    out lab f p' = do
      p'' <- f p'
      outm lab p''
      return p''
