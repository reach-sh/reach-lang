module Reach.Linearize (linearize) where

import Control.Monad.Reader
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import Generics.Deriving
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Util

type FluidEnv = M.Map FluidVar (SrcLoc, DLArg)

type FVMap = M.Map FluidVar DLVar

type LLRetRHS = (Maybe (DLVar, M.Map Int (DLStmts, DLArg)))

type LLRets = M.Map Int LLRetRHS

type App = ReaderT Env IO

data Env = Env
  { eCounterR :: IORef Int
  , eFVMm :: Maybe FVMap
  , eFVE :: FluidEnv
  , eRets :: LLRets
  }

allocVar :: (Int -> a) -> App a
allocVar mk = do
  Env {..} <- ask
  idx <- liftIO $ readIORef eCounterR
  liftIO $ modifyIORef eCounterR (1 +)
  return $ mk idx

fluidRefm :: FluidVar -> App (Maybe (SrcLoc, DLArg))
fluidRefm fv = do
  Env {..} <- ask
  return $ M.lookup fv eFVE

fluidRef :: FluidVar -> App (SrcLoc, DLArg)
fluidRef fv = do
  r <- fluidRefm fv
  case r of
    Nothing -> impossible $ "fluid ref unbound: " <> show fv
    Just x -> return x

fluidSet :: FluidVar -> (SrcLoc, DLArg) -> App a -> App a
fluidSet fv fvv = local (\e@Env {..} -> e {eFVE = M.insert fv fvv eFVE})

lookupRet :: Int -> App (Maybe LLRetRHS)
lookupRet r = do
  Env {..} <- ask
  return $ M.lookup r eRets

captureRets :: App LLRets
captureRets = do
  Env {..} <- ask
  return eRets

restoreRets :: LLRets -> App a -> App a
restoreRets rets' = local (\e -> e {eRets = rets'})

setRetsToEmpty :: App a -> App a
setRetsToEmpty = restoreRets mempty

withReturn :: Int -> LLRetRHS -> App a -> App a
withReturn rv rvv = local (\e@Env {..} -> e {eRets = M.insert rv rvv eRets})

withWhileFVMap :: FVMap -> App a -> App a
withWhileFVMap fvm' = local (\e -> e {eFVMm = Just fvm'})

readWhileFVMap :: App FVMap
readWhileFVMap = do
  Env {..} <- ask
  case eFVMm of
    Nothing -> impossible "attempt to read fvm with no fvm"
    Just x -> return x

unpackFVMap :: SrcLoc -> DLStmts -> App DLStmts
unpackFVMap at ss = do
  fvm <- readWhileFVMap
  let go ss' (fv, dv) = (return $ DLS_FluidSet at fv (DLA_Var dv)) <> ss'
  let ss' = foldl' go ss (M.toList fvm)
  return $ ss'

block_unpackFVMap :: SrcLoc -> DLBlock -> App DLBlock
block_unpackFVMap uat (DLBlock at fs ss a) =
  (\x -> DLBlock at fs x a) <$> unpackFVMap uat ss

expandFromFVMap :: DLAssignment -> App DLAssignment
expandFromFVMap (DLAssignment updatem) = do
  fvm <- readWhileFVMap
  let go (fv, dv) = do
        (_, da) <- fluidRef fv
        return $ (dv, da)
  fvm'l <- mapM go $ M.toList fvm
  let updatem' = M.union (M.fromList $ fvm'l) updatem
  return $ DLAssignment updatem'

lin_com :: String -> (SrcLoc -> DLStmts -> App a) -> (LLCommon a -> a) -> DLStmt -> DLStmts -> App a
lin_com who back mkk s ks =
  case s of
    DLS_FluidSet at fv da ->
      fluidSet fv (at, da) $ back at ks
    DLS_FluidRef at dv fv -> do
      (at', da) <- fluidRef fv
      mkk <$> (LL_Let at (Just dv) (DLE_Arg at' da) <$> back at ks)
    DLS_Let at mdv de -> mkk <$> (LL_Let at mdv de <$> back at ks)
    DLS_ArrayMap at ans x a f ->
      mkk <$> (LL_ArrayMap at ans x a <$> f' <*> back at ks)
      where
        f' = lin_block at f
    DLS_ArrayReduce at ans x z b a f ->
      mkk <$> (LL_ArrayReduce at ans x z b a <$> f' <*> back at ks)
      where
        f' = lin_block at f
    DLS_If at ca _ ts fs
      | isLocal s ->
        mkk <$> (LL_LocalIf at ca <$> t' <*> f' <*> back at ks)
      where
        t' = lin_local_rets at ts
        f' = lin_local_rets at fs
    DLS_Switch at dv _ cm
      | isLocal s ->
        mkk <$> (LL_LocalSwitch at dv <$> cm' <*> back at ks)
      where
        cm' = mapM cm1 cm
        cm1 (dv', l) = (\x -> (dv', x)) <$> lin_local_rets at l
    DLS_Return at ret eda -> do
      rv <- lookupRet ret
      case rv of
        Nothing ->
          impossible $ "unknown ret " <> show ret
        Just Nothing ->
          back at ks
        Just (Just (dv, retsm)) ->
          case eda of
            Left reti ->
              case M.lookup reti retsm of
                Nothing -> impossible $ "missing return da"
                Just (das, da) ->
                  case das <> (return $ DLS_Return at ret (Right da)) <> ks of
                    s' Seq.:<| ks' ->
                      lin_com who back mkk s' ks'
                    _ ->
                      impossible $ "no cons"
            Right da ->
              (mkk . LL_Set at dv da) <$> back at ks
    DLS_Prompt at (Left ret) ss ->
      withReturn ret Nothing $
        back at (ss <> ks)
    DLS_Prompt at (Right (dv@(DLVar _ _ _ ret), retms)) ss -> do
      withReturn ret (Just (dv, retms)) $
        (mkk . LL_Var at dv) <$> back at (ss <> ks)
    DLS_If {} -> bad
    DLS_Switch {} -> bad
    DLS_Stop {} -> bad
    DLS_Only {} -> bad
    DLS_ToConsensus {} -> bad
    DLS_FromConsensus {} -> bad
    DLS_While {} -> bad
    DLS_Continue {} -> bad
  where
    bad = impossible $ who <> " cannot " <> conNameOf s <> " at " <> show (srclocOf s)

lin_local_rets :: SrcLoc -> DLStmts -> App LLLocal
lin_local_rets at Seq.Empty =
  return $ LLL_Com $ LL_Return at
lin_local_rets _ (s Seq.:<| ks) =
  lin_com "local" lin_local_rets LLL_Com s ks

lin_local :: SrcLoc -> DLStmts -> App LLLocal
lin_local at ks = setRetsToEmpty $ lin_local_rets at ks

lin_block :: SrcLoc -> DLBlock -> App LLBlock
lin_block _at (DLBlock at fs l a) =
  LLBlock at fs <$> lin_local at l <*> pure a

lin_con :: SrcLoc -> DLStmts -> App LLConsensus
lin_con at Seq.Empty =
  return $ LLC_Com $ LL_Return at
lin_con at_top (s Seq.:<| ks) =
  case s of
    DLS_If at ca _ ts fs
      | not (isLocal s) ->
        LLC_If at ca <$> t' <*> f'
      where
        t' = lin_con at (ts <> ks)
        f' = lin_con at (fs <> ks)
    DLS_Switch at dv _ cm
      | not (isLocal s) ->
        LLC_Switch at dv <$> cm'
      where
        cm' = mapM cm1 cm
        cm1 (dv', c) = (\x -> (dv', x)) <$> lin_con at (c <> ks)
    DLS_FromConsensus at cons ->
      LLC_FromConsensus at at_top <$> lin_step at (cons <> ks)
    DLS_While at asn inv_b cond_b body -> do
      let go fv = do
            r <- fluidRefm fv
            case r of
              Nothing -> return $ Nothing
              Just _ -> do
                dv <- allocVar $ DLVar at (show fv) (fluidVarType fv)
                return $ Just (fv, dv)
      fvm <- M.fromList <$> catMaybes <$> mapM go allFluidVars
      let body_fvs' = lin_con at =<< unpackFVMap at body
      --- Note: The invariant and condition can't return
      let block b = lin_block at =<< block_unpackFVMap at b
      withWhileFVMap fvm $
        LLC_While at <$> expandFromFVMap asn <*> block inv_b <*> block cond_b <*> body_fvs' <*> (lin_con at =<< unpackFVMap at ks)
    DLS_Continue at update ->
      case ks of
        Seq.Empty ->
          LLC_Continue at <$> expandFromFVMap update
        _ ->
          impossible $ "consensus cannot continue w/ non-empty k"
    DLS_Only at who ss ->
      LLC_Only at who <$> ls <*> lin_con at ks
      where
        ls = lin_local at ss
    _ ->
      lin_com "consensus" lin_con LLC_Com s ks

lin_step :: SrcLoc -> DLStmts -> App LLStep
lin_step at Seq.Empty =
  return $ LLS_Stop at
lin_step _ (s Seq.:<| ks) =
  case s of
    DLS_If {}
      | not (isLocal s) ->
        impossible $ "step cannot unlocal if, must occur in consensus"
    DLS_Switch {}
      | not (isLocal s) ->
        impossible $ "step cannot unlocal switch, must occur in consensus"
    DLS_Stop at ->
      return $ LLS_Stop at
    DLS_Only at who ss ->
      LLS_Only at who <$> ls <*> lin_step at ks
      where
        ls = lin_local at ss
    DLS_ToConsensus at send recv mtime -> do
      -- rets <- captureRets
      -- let back = restoreRets rets . lin_step at
      let (last_timev, winner_dv, msg, amtv, timev, cons) = recv
      let cons' = lin_con at (cons <> ks)
      let recv' =
            (\x -> (last_timev, winner_dv, msg, amtv, timev, x)) <$> cons'
      let mtime' =
            case mtime of
              Just (delay_da, time_ss) ->
                (\x y -> Just (x, y)) <$> pure delay_da <*> lin_step at (time_ss <> ks)
              Nothing ->
                return $ Nothing
      LLS_ToConsensus at send <$> recv' <*> mtime'
    _ ->
      lin_com "step" lin_step LLS_Com s ks

linearize :: DLProg -> IO LLProg
linearize (DLProg at (DLOpts {..}) sps dli ss) = do
  let llo_deployMode = dlo_deployMode
  let llo_verifyOverflow = dlo_verifyOverflow
  let opts' = LLOpts {..}
  eCounterR <- newIORef dlo_counter
  let eFVMm = mempty
  let eFVE = mempty
  let eRets = mempty
  let env0 = Env {..}
  flip runReaderT env0 $
    LLProg at opts' sps dli <$> lin_step at ss
