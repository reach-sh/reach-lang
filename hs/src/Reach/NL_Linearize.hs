module Reach.NL_Linearize (linearize) where

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
        t' = lin_local_rets at rets ts
        f' = lin_local_rets at rets fs
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
      impossible $ who ++ " cannot fromconsensus"
    DLS_While {} ->
      impossible $ who ++ " cannot while"
    DLS_Continue {} ->
      impossible $ who ++ " cannot while"

lin_local_s :: LLRets -> DLStmt -> LLLocal -> LLLocal
lin_local_s rets s k =
  lin_com_s "local" (lin_ss lin_local_s) LLL_Com rets s k

lin_local_rets :: SrcLoc -> LLRets -> DLStmts -> LLLocal
lin_local_rets at rets ss = lin_ss lin_local_s rets ss $ LLL_Com $ LL_Return at

lin_local :: SrcLoc -> DLStmts -> LLLocal
lin_local at ss = lin_local_rets at mempty ss

lin_con_s :: (DLStmts -> LLStep) -> LLRets -> DLStmt -> LLConsensus -> LLConsensus
lin_con_s back rets s k =
  case s of
    DLS_If at ca ts fs
      | not (stmt_local s) ->
        LLC_If at ca t' f'
      where
        t' = iters rets ts k
        f' = iters rets fs k
    DLS_Transfer at fs who aa ->
      LLC_Transfer at fs who aa k
    DLS_FromConsensus at cons ->
      case k of
        LLC_Com (LL_Return ret_at) ->
          LLC_FromConsensus at ret_at $ back cons
        _ ->
          impossible $ "consensus cannot fromconsensus w/ non-empty k"
    DLS_While at asn inv_b cond_b body ->
      LLC_While at asn (block inv_b) (block cond_b) body' k
      where
        body' = iters rets body $ LLC_Com $ LL_Return at
        --- Note: The invariant and condition can't return
        block (DLBlock ba fs ss a) =
          LLBlock ba fs (lin_local ba ss) a
    DLS_Continue at update ->
      case k of
        LLC_Com (LL_Return _ret_at) ->
          LLC_Continue at update
        _ ->
          impossible $ "consensus cannot continue w/ non-empty k"
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
    DLS_ToConsensus at who fs as ms amt mtime cons ->
      LLS_ToConsensus at who fs as ms amt mtime' cons'
      where
        cons' = lin_con at back cons
        back more = iters rets more k
        mtime' = do
          (delay_da, DLBlock time_at time_fs time_ss time_da) <- mtime
          {-
          --- XXX Timeouts always "exit"
          --- XXX Not checking the result types
          let time_ll = lin_ss lin_step_s rets time_ss (LLS_Stop time_at time_fs time_da)
          return $ (delay_da, time_ll)
          -}
          case k of
            LLS_Stop fin_at _ fin_da -> do
              let time_ll = lin_ss lin_step_s rets time_ss (LLS_Stop time_at time_fs time_da)
              case typeMeet at (fin_at, argTypeOf fin_da) (time_at, argTypeOf time_da) of
                _ -> return $ (delay_da, time_ll)
            _ ->
              impossible $ "lin toconsensus w/ non-stop k"
    _ ->
      lin_com_s "step" iters LLS_Com rets s k
  where
    iters = lin_ss lin_step_s

linearize :: DLProg -> LLProg
linearize (DLProg at sps (DLBlock bat fs ss da)) = LLProg at sps step
  where
    step = lin_ss lin_step_s mempty ss (LLS_Stop bat fs da)
