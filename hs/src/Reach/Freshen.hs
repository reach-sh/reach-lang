module Reach.Freshen
  ( freshen
  , freshen_
  , freshen_top
  , Freshen
  )
where

import Control.Monad.Reader
import qualified Data.Aeson as AS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.EP
import Reach.Counter
import Reach.Util

type App = ReaderT Env IO

type AppT a = a -> App a

data Env = Env
  { fCounter :: Counter
  , fRho :: IORef (M.Map DLVar DLVar)
  }

instance HasCounter Env where
  getCounter = fCounter

newScope :: App x -> App x
newScope m = do
  Env {..} <- ask
  fRho' <- liftIO $ dupeIORef fRho
  local (\e -> e {fRho = fRho'}) m

class Freshen a where
  fu :: AppT a

class FreshenV a where
  fu_v :: AppT a

instance FreshenV DLVar where
  fu_v v = do
    Env {..} <- ask
    v' <- freshenVar v
    liftIO $ modifyIORef fRho (M.insert v v')
    return $ v'

instance FreshenV DLVarLet where
  fu_v (DLVarLet c v) = DLVarLet c <$> fu_v v

instance FreshenV DLLetVar where
  fu_v = \case
    DLV_Eff -> return $ DLV_Eff
    DLV_Let vc v -> DLV_Let vc <$> fu_v v

instance (Freshen a, Freshen b) => Freshen (a, b) where
  fu (x, y) = (,) <$> fu x <*> fu y

instance (Freshen a, Freshen b, Freshen c) => Freshen (a, b, c) where
  fu (x, y, z) = (,,) <$> fu x <*> fu y <*> fu z

instance (Freshen a, Freshen b) => Freshen (Either a b) where
  fu = \case
    Left x -> Left <$> fu x
    Right x -> Right <$> fu x

instance {-# OVERLAPPABLE #-} (Freshen b) => Freshen (M.Map k b) where
  fu = traverse fu

instance Freshen a => Freshen (Maybe a) where
  fu = traverse fu

instance FreshenV a => FreshenV (Maybe a) where
  fu_v = traverse fu_v

instance Freshen a => Freshen [a] where
  fu = traverse fu

instance FreshenV a => FreshenV [a] where
  fu_v = traverse fu_v

instance Freshen DLVar where
  fu v = do
    Env {..} <- ask
    rho <- liftIO $ readIORef fRho
    let mv' = M.lookup v rho
    -- Often, an expression will be assigned to a DLVar with no label, and
    -- only subsequent uses of the DLVar will have a label associated with it.
    -- If we use the DLVar from the assignment everywhere, we will lose labels, which is needed for nice SMT errors.
    -- If we can ever recover the label information, replace the key.
    mv'' <- case (mv', v) of
      (Just (DLVar at Nothing t i), DLVar _ lab@(Just {}) _ _) -> do
        let v' = DLVar at lab t i
        liftIO $ modifyIORef fRho $ M.insert v v'
        return $ Just v'
      _ -> return $ mv'
    return $ fromMaybe v mv''

instance Freshen DLArg where
  fu = \case
    DLA_Var v -> DLA_Var <$> fu v
    x -> return x

instance Freshen DLLargeArg where
  fu = \case
    DLLA_Array t as -> DLLA_Array t <$> fu as
    DLLA_Tuple as -> DLLA_Tuple <$> fu as
    DLLA_Obj m -> DLLA_Obj <$> fu m
    DLLA_Data t v a -> DLLA_Data t v <$> fu a
    DLLA_Struct kvs -> DLLA_Struct <$> mapM go kvs
    DLLA_Bytes b -> return $ DLLA_Bytes b
    DLLA_BytesDyn b -> return $ DLLA_BytesDyn b
    DLLA_StringDyn t -> return $ DLLA_StringDyn t
    where
      go (k, v) = (,) k <$> fu v

instance Freshen DLTokenNew where
  fu (DLTokenNew {..}) =
    DLTokenNew
      <$> fu dtn_name
      <*> fu dtn_sym
      <*> fu dtn_url
      <*> fu dtn_metadata
      <*> fu dtn_supply
      <*> fu dtn_decimals

instance Freshen ClaimType where
  fu = \case
    CT_Unknowable x y -> CT_Unknowable x <$> fu y
    x -> return x

instance Freshen DLWithBill where
  fu (DLWithBill x y z) = DLWithBill x <$> fu y <*> fu z

instance Freshen Bool where
  fu = return

instance Freshen a => Freshen (DLInvariant a) where
  fu (DLInvariant inv lab) = DLInvariant <$> fu inv <*> pure lab

instance Freshen DLRemoteALGOOC where
  fu = return

instance Freshen DLRemoteALGOSTR where
  fu = \case
    RA_Unset -> return RA_Unset
    RA_List at l -> RA_List at <$> fu l
    RA_Tuple t -> RA_Tuple <$> fu t

instance Freshen DLRemoteALGO where
  fu (DLRemoteALGO {..}) =
    DLRemoteALGO <$> fu ra_fees <*> fu ra_accounts <*> fu ra_assets <*> fu ra_addr2acc <*> fu ra_apps <*> fu ra_boxes <*> fu ra_onCompletion <*> fu ra_strictPay <*> fu ra_rawCall <*> fu ra_simNetRecv <*> fu ra_simTokensRecv <*> fu ra_simReturnVal <*> fu ra_txnOrderForward

instance Freshen AS.Value where
  fu = return

instance Freshen DLContractNew where
  fu (DLContractNew x y) = DLContractNew <$> fu x <*> fu y

instance Freshen DLRemote where
  fu (DLRemote m pamt as wbill malgo) = DLRemote <$> pure m <*> fu pamt <*> fu as <*> fu wbill <*> fu malgo

instance Freshen DLExpr where
  fu = \case
    DLE_Arg at a -> DLE_Arg at <$> fu a
    DLE_LArg at a -> DLE_LArg at <$> fu a
    e@(DLE_Impossible {}) -> return $ e
    DLE_VerifyMuldiv at f cl as err -> DLE_VerifyMuldiv at f cl <$> fu as <*> pure err
    DLE_PrimOp at p as -> DLE_PrimOp at p <$> fu as
    DLE_ArrayRef at a b -> DLE_ArrayRef at <$> fu a <*> fu b
    DLE_ArraySet at a b c -> DLE_ArraySet at <$> fu a <*> fu b <*> fu c
    DLE_ArrayConcat at a b -> DLE_ArrayConcat at <$> fu a <*> fu b
    DLE_BytesDynCast at a -> DLE_BytesDynCast at <$> fu a
    DLE_TupleRef at x y -> DLE_TupleRef at <$> fu x <*> pure y
    DLE_ObjectRef at x y -> DLE_ObjectRef at <$> fu x <*> pure y
    DLE_Interact a b c d e f -> DLE_Interact a b c d e <$> fu f
    DLE_Digest at as -> DLE_Digest at <$> fu as
    DLE_Claim a b c d e -> DLE_Claim a b <$> fu c <*> fu d <*> pure e
    DLE_Transfer at x y z -> DLE_Transfer at <$> fu x <*> fu y <*> fu z
    DLE_TokenInit at x -> DLE_TokenInit at <$> fu x
    DLE_TokenAccepted at x y -> DLE_TokenAccepted at <$> fu x <*> fu y
    DLE_CheckPay at x y z -> DLE_CheckPay at x <$> fu y <*> fu z
    DLE_Wait at x -> DLE_Wait at <$> fu x
    DLE_PartSet at x y -> DLE_PartSet at x <$> fu y
    DLE_MapRef at mv fa vt -> DLE_MapRef at mv <$> fu fa <*> pure vt
    DLE_MapSet at mv fa vt na -> DLE_MapSet at mv <$> fu fa <*> pure vt <*> fu na
    DLE_Remote at fs av rt dr -> DLE_Remote at fs <$> fu av <*> pure rt <*> fu dr
    DLE_TokenNew at tns -> DLE_TokenNew at <$> fu tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> fu tok <*> fu amt
    DLE_TokenDestroy at tok -> DLE_TokenDestroy at <$> fu tok
    DLE_TimeOrder at op a b -> DLE_TimeOrder at op <$> fu a <*> fu b
    DLE_EmitLog at k a -> DLE_EmitLog at k <$> fu a
    DLE_setApiDetails s p ts mc f -> return $ DLE_setApiDetails s p ts mc f
    DLE_GetUntrackedFunds at mt tb -> DLE_GetUntrackedFunds at <$> fu mt <*> fu tb
    DLE_DataTag at d -> DLE_DataTag at <$> fu d
    DLE_FromSome at mo da -> DLE_FromSome at <$> fu mo <*> fu da
    DLE_ContractNew at tns dr -> DLE_ContractNew at <$> fu tns <*> fu dr
    DLE_ObjectSet at a b c -> DLE_ObjectSet at <$> fu a <*> pure b <*> fu c
    DLE_TupleSet at a b c -> DLE_TupleSet at <$> fu a <*> pure b <*> fu c
    DLE_ContractFromAddress at a -> DLE_ContractFromAddress at <$> fu a

instance Freshen k => Freshen (SwitchCase k) where
  fu (SwitchCase {..}) = SwitchCase <$> fu_v sc_vl <*> (newScope $ fu sc_k)

instance Freshen k => Freshen (SwitchCases k) where
  fu (SwitchCases m) = SwitchCases <$> fu m

instance Freshen DLStmt where
  fu = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at v e -> do
      f' <- fu_v v
      DL_Let at f' <$> fu e
    DL_Var at v -> DL_Var at <$> fu_v v
    DL_Set at v a -> DL_Set at <$> fu v <*> fu a
    DL_LocalIf at mans c t f -> DL_LocalIf at <$> fu mans <*> fu c <*> fu t <*> fu f
    DL_LocalSwitch at ov csm ->
      DL_LocalSwitch at <$> fu ov <*> fu csm
    DL_Only at who b -> DL_Only at who <$> fu b
    DL_ArrayMap at ans x a i fb -> do
      x' <- fu x
      a' <- fu_v a
      i' <- fu_v i
      fb' <- fu fb
      ans' <- fu_v ans
      return $ DL_ArrayMap at ans' x' a' i' fb'
    DL_ArrayReduce at ans x z b a i fb -> do
      ans' <- fu_v ans
      x' <- fu x
      z' <- fu z
      b' <- fu_v b
      a' <- fu_v a
      i' <- fu_v i
      fb' <- fu fb
      return $ DL_ArrayReduce at ans' x' z' b' a' i' fb'
    DL_MapReduce at mri ans x z b k a fb -> do
      ans' <- fu_v ans
      z' <- fu z
      b' <- fu_v b
      k' <- fu_v k
      a' <- fu_v a
      fb' <- fu fb
      return $ DL_MapReduce at mri ans' x z' b' k' a' fb'
    DL_LocalDo at mans t -> DL_LocalDo at <$> fu mans <*> fu t

instance Freshen DLPayAmt where
  fu = \case
    DLPayAmt net ks ->
      DLPayAmt <$> fu net <*> mapM (\(amt, ty) -> (,) <$> fu amt <*> fu ty) ks

instance Freshen DLTail where
  fu = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> DT_Com <$> fu m <*> fu k

instance Freshen DLBlock where
  fu (DLBlock at fs t a) = newScope $
    DLBlock at fs <$> fu t <*> fu a

instance Freshen DLAssignment where
  fu (DLAssignment m) =
    DLAssignment <$> (M.fromList <$> (mapM go $ M.toList m))
    where
      go (v, a) = (,) <$> fu v <*> fu a

instance FreshenV DLAssignment where
  fu_v (DLAssignment m) =
    DLAssignment <$> (M.fromList <$> (mapM go $ M.toList m))
    where
      go (v, a) = (,) <$> fu_v v <*> fu a

instance Freshen k => Freshen (DLinExportBlock k) where
  fu (DLinExportBlock at mvs k) =
    DLinExportBlock at <$> fu_v mvs <*> fu k

instance Freshen LLConsensus where
  fu = \case
    LLC_Com s k -> LLC_Com <$> fu s <*> fu k
    LLC_If at c t f -> LLC_If at <$> fu c <*> fu t <*> fu f
    LLC_Switch at v csm -> LLC_Switch at <$> fu v <*> fu csm
    LLC_FromConsensus x y fs k -> LLC_FromConsensus x y fs <$> fu k
    LLC_While at asn inv cond body k ->
      LLC_While at <$> fu_v asn <*> fu inv <*> fu cond <*> fu body <*> fu k
    LLC_Continue at asn -> LLC_Continue at <$> fu asn
    LLC_ViewIs at v vn deb k ->
      LLC_ViewIs at v vn <$> fu deb <*> fu k

instance Freshen DLSend where
  fu (DLSend {..}) =
    DLSend ds_isClass <$> fu ds_msg <*> fu ds_pay <*> fu ds_when

instance Freshen a => Freshen (DLRecv a) where
  fu (DLRecv {..}) =
    DLRecv <$> fu_v dr_from <*> fu_v dr_msg <*> fu_v dr_time <*> fu_v dr_secs <*> fu dr_didSend <*> (newScope $ fu dr_k)

instance Freshen LLStep where
  fu = \case
    LLS_Com s k -> LLS_Com <$> fu s <*> fu k
    LLS_Stop at -> return $ LLS_Stop at
    LLS_ToConsensus at lct send recv mtime ->
      LLS_ToConsensus at <$> fu lct <*> fu send <*> fu recv <*> (newScope $ fu mtime)

instance Freshen SvsPut where
  fu (SvsPut {..}) = SvsPut svsp_svs <$> fu svsp_val

instance Freshen FromInfo where
  fu = \case
    FI_Continue vs -> FI_Continue <$> mapM fu vs
    FI_Halt toks -> FI_Halt <$> mapM fu toks

instance Freshen ([DLArg], DLPayAmt, DLArg, [DLVar], Bool) where
  fu (a, b, c, d, e) = (,,,,) <$> fu a <*> fu b <*> fu c <*> fu d <*> pure e

instance Freshen ETail where
  fu = \case
    ET_Com c k -> ET_Com <$> fu c <*> fu k
    ET_Stop at -> return $ ET_Stop at
    ET_If a c t f -> ET_If a <$> fu c <*> fu t <*> fu f
    ET_Switch a x csm -> ET_Switch a <$> fu x <*> fu csm
    ET_FromConsensus a w f k -> ET_FromConsensus a w <$> fu f <*> fu k
    ET_ToConsensus at from prev lct w me msg out timev secsv didSendv mtime cons ->
      ET_ToConsensus at <$> fu_v from <*> pure prev <*> fu lct <*> pure w <*> fu me <*> fu_v msg <*> fu_v out <*> fu_v timev <*> fu_v secsv <*> fu_v didSendv <*> fu mtime <*> fu cons
    ET_While at asn cond body k -> ET_While at <$> fu_v asn <*> fu cond <*> fu body <*> fu k
    ET_Continue at asn -> ET_Continue at <$> fu asn

instance Freshen LLProg where
  fu (LLProg llp_at llp_opts llp_parts llp_init llp_exports llp_views llp_apis llp_aliases llp_events llp_step) =
    LLProg llp_at llp_opts llp_parts llp_init llp_exports llp_views llp_apis llp_aliases llp_events <$> fu llp_step

freshen_ :: Freshen a => Counter -> a -> [DLVar] -> IO (a, [DLVar])
freshen_ fCounter x vs = do
  fRho <- newIORef mempty
  flip runReaderT (Env {..}) $ do
    vs' <- mapM fu_v vs
    x' <- fu x
    return $ (x', vs')

freshen :: Freshen a => Counter -> a -> IO a
freshen fCounter x = fst <$> freshen_ fCounter x []

freshen_top :: (Freshen a, HasCounter a) => a -> IO a
freshen_top p = freshen (getCounter p) p
