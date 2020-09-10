module Reach.Linearize (linearize) where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Reach.AST
import Reach.Type
import Reach.Util

type LLRets = M.Map Int DLVar

lin_com :: String -> (SrcLoc -> LLRets -> DLStmts -> a) -> (LLCommon a -> a) -> LLRets -> DLStmt -> DLStmts -> a
lin_com who back mkk rets s ks =
  case s of
    DLS_Let at dv de -> mkk $ LL_Let at dv de $ back at rets ks
    DLS_ArrayMap at ans x a f r ->
      mkk $ LL_ArrayMap at ans x a f' r $ back at rets ks
      where f' = lin_local at f
    DLS_ArrayReduce at ans x z b a f r -> 
      mkk $ LL_ArrayReduce at ans x z b a f' r $ back at rets ks
      where f' = lin_local at f
    DLS_If at ca _ ts fs | isLocal s ->
      mkk $ LL_LocalIf at ca t' f' $ back at rets ks
      where
        t' = lin_local_rets at rets ts
        f' = lin_local_rets at rets fs
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> back at rets ks
        Just dv -> mkk $ LL_Set at dv da $ back at rets ks
          where
            (_, da) = typeOf at sv
    DLS_Prompt at (Left _) ss -> back at rets (ss <> ks)
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      mkk $ LL_Var at dv $ back at rets' (ss <> ks)
      where
        rets' = M.insert ret dv rets
    DLS_If {} ->
      impossible $ who ++ " cannot non-local if"
    DLS_Stop {} ->
      impossible $ who ++ " cannot stop"
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

lin_local_rets :: SrcLoc -> LLRets -> DLStmts -> LLLocal
lin_local_rets at _ Seq.Empty =
  LLL_Com $ LL_Return at
lin_local_rets _ rets (s Seq.:<| ks) =
  lin_com "local" lin_local_rets LLL_Com rets s ks

lin_local :: SrcLoc -> DLStmts -> LLLocal
lin_local at ks = lin_local_rets at mempty ks

lin_con :: (DLStmts -> LLStep) -> SrcLoc -> LLRets -> DLStmts -> LLConsensus
lin_con _ at _ Seq.Empty =
  LLC_Com $ LL_Return at
lin_con back at_top rets (s Seq.:<| ks) =
  case s of
    DLS_If at ca _ ts fs | not (isLocal s) ->
        LLC_If at ca t' f'
      where
        t' = lin_con back at rets (ts <> ks)
        f' = lin_con back at rets (fs <> ks)
    DLS_FromConsensus at cons ->
      LLC_FromConsensus at at_top $ back (cons <> ks)
    DLS_While at asn inv_b cond_b body ->
      LLC_While at asn (block inv_b) (block cond_b) body' $ lin_con back at rets ks
      where
        body' = lin_con back at rets body
        --- Note: The invariant and condition can't return
        block (DLBlock ba fs ss a) =
          LLBlock ba fs (lin_local ba ss) a
    DLS_Continue at update ->
      case ks of
        Seq.Empty ->
          LLC_Continue at update
        _ ->
          impossible $ "consensus cannot continue w/ non-empty k"
    _ ->
      lin_com "consensus" (lin_con back) LLC_Com rets s ks

lin_step :: SrcLoc -> LLRets -> DLStmts -> LLStep
lin_step at _ Seq.Empty =
  LLS_Stop at []
lin_step _ rets (s Seq.:<| ks) =
  case s of
    DLS_If {} | not (isLocal s) ->
        impossible $ "step cannot unlocal if, must occur in consensus"
    DLS_Stop at fs ->
      LLS_Stop at fs
    DLS_Only at who ss ->
      LLS_Only at who ls $ lin_step at rets ks
      where
        ls = lin_local at ss
    DLS_ToConsensus at who fs as ms amt mtime cons ->
      LLS_ToConsensus at who fs as ms amt mtime' cons'
      where
        cons' = lin_con back at mempty (cons <> ks)
        back = lin_step at rets
        mtime' = do
          (delay_da, time_ss) <- mtime
          return $ (delay_da, lin_step at rets (time_ss <> ks))
    _ ->
      lin_com "step" lin_step LLS_Com rets s ks

linearize :: DLProg -> LLProg
linearize (DLProg at (DLOpts {..}) sps ss) =
  LLProg at opts' sps $ lin_step at mempty ss
  where
    opts' = LLOpts {..}
    llo_deployMode = dlo_deployMode
