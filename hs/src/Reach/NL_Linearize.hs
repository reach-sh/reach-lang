module Reach.NL_Linearize where

import qualified Data.Map.Strict as M
import Reach.NL_AST
import Reach.NL_Type
import Reach.Util

type LLRets = M.Map Int DLVar

lin_ss :: (LLRets -> DLStmt -> a -> a) -> LLRets -> DLStmts -> a -> a
lin_ss lin_s rets ss k = foldr (lin_s rets) k ss

lin_local_s :: LLRets -> DLStmt -> LLLocal -> LLLocal
lin_local_s rets s k =
  case s of
    DLS_Let at dv de -> LLL_Let at dv de k
    DLS_Claim at f ct da -> LLL_Claim at f ct da k
    DLS_If at ca ts fs -> LLL_If at ca t' f'
      where
        t' = iters rets ts LLL_LocalStop
        f' = iters rets fs LLL_LocalStop
    DLS_Transfer {} ->
      impossible $ "local cannot transfer"
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLL_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLL_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only {} ->
      impossible $ "local cannot only"
    DLS_ToConsensus {} ->
      impossible $ "local cannot consensus"
    DLS_FromConsensus {} ->
      impossible $ "local cannot from consensus"
  where
    iters = lin_ss lin_local_s

lin_local :: DLStmts -> LLLocal
lin_local ss = lin_ss lin_local_s mempty ss LLL_LocalStop

lin_con_s :: (DLStmts -> LLStep) -> LLRets -> DLStmt -> LLConsensus -> LLConsensus
lin_con_s back rets s k =
  case s of
    DLS_Let at dv de -> LLC_Let at dv de k
    DLS_Claim at f ct da -> LLC_Claim at f ct da k
    DLS_If at ca ts fs ->
      case stmt_local s of
        True ->
          LLC_LocalIf at ca t' f' k
          where
            t' = iters rets ts LLC_ConStop
            f' = iters rets fs LLC_ConStop
        False ->
          LLC_If at ca t' f'
          where
            t' = iters rets ts k
            f' = iters rets fs k
    DLS_Transfer at who aa -> LLC_Transfer at who aa k
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLC_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLC_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only {} -> impossible $ "consensus cannot only"
    DLS_ToConsensus {} -> impossible $ "consensus cannot toconsensus"
    DLS_FromConsensus at cons ->
      case k of
        LLC_ConStop ->
          LLC_FromConsensus at $ back cons
        _ ->
          impossible $ "consensus cannot fromconsensus w/ non-empty k"
  where
    iters = lin_ss (lin_con_s back)

lin_con :: (DLStmts -> LLStep) -> DLStmts -> LLConsensus
lin_con back ss = lin_ss (lin_con_s back) mempty ss LLC_ConStop

lin_step_s :: LLRets -> DLStmt -> LLStep -> LLStep
lin_step_s rets s k =
  case s of
    DLS_Let at dv de -> LLS_Let at dv de k
    DLS_Claim at f ct da -> LLS_Claim at f ct da k
    DLS_If at ca ts fs ->
      case stmt_local s of
        True ->
          LLS_LocalIf at ca t' f' k
          where
            t' = iters rets ts LLS_LocalStop
            f' = iters rets fs LLS_LocalStop
        False ->
          LLS_If at ca t' f'
          where
            t' = iters rets ts k
            f' = iters rets fs k
    DLS_Transfer {} -> impossible $ "step cannot transfer"
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLS_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLS_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only at who ss ->
      LLS_Only at who ls k
      where
        ls = lin_local ss
    DLS_ToConsensus at who as ms mamt mtime cons ->
      LLS_ToConsensus at who as ms mamt' mtime' cons'
      where
        cons' = lin_con back cons
        back more = iters rets more k
        mamt' = do
          DLProg amt_ss amt_da <- mamt
          return $ (lin_local amt_ss, amt_da)
        mtime' = do
          (delay_da, DLProg time_ss time_da) <- mtime
          --- XXX maybe k is needed here?
          let time_ll = lin_ss lin_step_s rets time_ss (LLS_Stop time_da)
          return $ (delay_da, time_ll)
    DLS_FromConsensus {} -> impossible $ "step cannot fromconsensus"
  where
    iters = lin_ss lin_step_s

linearize :: DLProg -> LLStep
linearize (DLProg ss da) = lin_ss lin_step_s mempty ss (LLS_Stop da)
