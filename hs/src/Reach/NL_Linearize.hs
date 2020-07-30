module Reach.NL_Linearize where

import qualified Data.Map.Strict as M
import Reach.NL_AST
import Reach.NL_Type
import Reach.Util

type LLRets = M.Map Int DLVar

lin_ss :: (LLRets -> DLStmt -> a -> a) -> LLRets -> DLStmts -> a -> a
lin_ss lin_s rets ss k = foldr (lin_s rets) k ss

lin_com_s :: String -> (LLRets -> DLStmts -> a -> a) -> (LLCommon a -> a) -> LLRets -> DLStmt -> a -> a
lin_com_s who iters mkk rets s k =
  case s of
    DLS_Let at dv de -> mkk $ LL_Let at dv de k
    DLS_Claim at f ct da -> mkk $ LL_Claim at f ct da k
    DLS_If at ca ts fs -> mkk $ LL_LocalIf at ca t' f' k
      where
        t' = iters rets ts nk
        f' = iters rets fs nk
        nk = mkk $ LL_Return at
    DLS_Transfer {} ->
      impossible $ who ++ " cannot transfer"
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> mkk $ LL_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      mkk $ LL_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only {} ->
      impossible $ who ++ " cannot only"
    DLS_ToConsensus {} ->
      impossible $ who ++ " cannot consensus"
    DLS_FromConsensus {} ->
      impossible $ who ++ " cannot from consensus"

lin_local_s :: LLRets -> DLStmt -> LLLocal -> LLLocal
lin_local_s rets s k =
  lin_com_s "local" (lin_ss lin_local_s) LLL_Com rets s k

lin_local :: SrcLoc -> DLStmts -> LLLocal
lin_local at ss = lin_ss lin_local_s mempty ss $ LLL_Com $ LL_Return at

lin_con_s :: (DLStmts -> LLStep) -> LLRets -> DLStmt -> LLConsensus -> LLConsensus
lin_con_s back rets s k =
  case s of
    DLS_If at ca ts fs
      | not (stmt_local s) ->
        LLC_If at ca t' f'
      where
        t' = iters rets ts k
        f' = iters rets fs k
    DLS_Transfer at who aa -> LLC_Transfer at who aa k
    DLS_FromConsensus at cons ->
      case k of
        LLC_Com (LL_Return ret_at) ->
          LLC_FromConsensus at ret_at $ back cons
        _ ->
          impossible $ "consensus cannot fromconsensus w/ non-empty k"
    _ ->
      lin_com_s "consensus" iters LLC_Com rets s k
  where
    iters = lin_ss (lin_con_s back)

lin_con :: SrcLoc -> (DLStmts -> LLStep) -> DLStmts -> LLConsensus
lin_con at back ss = lin_ss (lin_con_s back) mempty ss $ LLC_Com $ LL_Return at

lin_step_s :: LLRets -> DLStmt -> LLStep -> LLStep
lin_step_s rets s k =
  case s of
    DLS_If {}
      | not (stmt_local s) ->
        impossible $ "step cannot unlocal if, must occur in consensus"
    DLS_Only at who ss ->
      LLS_Only at who ls k
      where
        ls = lin_local at ss
    DLS_ToConsensus at who as ms mamt mtime cons ->
      LLS_ToConsensus at who as ms mamt' mtime' cons'
      where
        cons' = lin_con at back cons
        back more = iters rets more k
        mamt' = do
          DLBlock amt_at amt_ss amt_da <- mamt
          return $ (lin_local amt_at amt_ss, amt_da)
        mtime' = do
          (delay_da, DLBlock time_at time_ss time_da) <- mtime
          --- XXX maybe k is needed here?
          let time_ll = lin_ss lin_step_s rets time_ss (LLS_Stop time_at time_da)
          return $ (delay_da, time_ll)
    _ ->
      lin_com_s "step" iters LLS_Com rets s k
  where
    iters = lin_ss lin_step_s

linearize :: DLProg -> LLProg
linearize (DLProg at sps (DLBlock bat ss da)) = LLProg at sps step
  where step = lin_ss lin_step_s mempty ss (LLS_Stop bat da)
