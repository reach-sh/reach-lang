module Reach.Subst
  ( subst_
  , Subst
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Aeson as AS
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AST.DLBase
import Reach.AST.CP

type SubstEnv = M.Map DLVar DLVar

type SubstApp = ReaderT SubstEnv Identity

type SubstT a = a -> SubstApp a

class Subst a where
  subst :: SubstT a

instance (Traversable f, Subst a) => Subst (f a) where
  subst = traverse subst

instance {-# OVERLAPS #-} (Subst a, Subst b) => Subst (a, b) where
  subst (x, y) = (,) <$> subst x <*> subst y

instance {-# OVERLAPS #-} (Subst a, Subst b, Subst c) => Subst (a, b, c) where
  subst (x, y, z) = (,,) <$> subst x <*> subst y <*> subst z

instance {-# OVERLAPS #-} Subst a => Subst (SwitchCases a) where
  subst (SwitchCases csm) = SwitchCases <$> mapM subst csm

instance {-# OVERLAPS #-} Subst a => Subst (SwitchCase a) where
  subst (SwitchCase {..}) = SwitchCase sc_vl <$> subst sc_k

instance Subst Bool where
  subst = return

instance Subst DLVar where
  subst v = do
    m <- ask
    return $ fromMaybe v $ M.lookup v m

instance Subst DLArg where
  subst = \case
    DLA_Var v -> DLA_Var <$> subst v
    x -> return x

instance Subst DLLargeArg where
  subst = \case
    DLLA_Array t as -> DLLA_Array t <$> subst as
    DLLA_Tuple as -> DLLA_Tuple <$> subst as
    DLLA_Obj m -> DLLA_Obj <$> subst m
    DLLA_Data t v a -> DLLA_Data t v <$> subst a
    DLLA_Struct kvs -> DLLA_Struct <$> mapM go kvs
    DLLA_Bytes b -> return $ DLLA_Bytes b
    DLLA_BytesDyn b -> return $ DLLA_BytesDyn b
    DLLA_StringDyn t -> return $ DLLA_StringDyn t
    where
      go (k, v) = (,) k <$> subst v

instance Subst DLPayAmt where
  subst = \case
    DLPayAmt net ks ->
      DLPayAmt <$> subst net
        <*> mapM (\(amt, ty) -> (,) <$> subst amt <*> subst ty) ks

instance Subst DLTokenNew where
  subst (DLTokenNew {..}) =
    DLTokenNew
      <$> subst dtn_name
      <*> subst dtn_sym
      <*> subst dtn_url
      <*> subst dtn_metadata
      <*> subst dtn_supply
      <*> subst dtn_decimals

instance Subst DLWithBill where
  subst (DLWithBill x y z) =
    DLWithBill x <$> subst y <*> subst z

instance Subst DLRemoteALGOOC where
  subst = return

instance Subst DLRemoteALGOSTR where
  subst = \case
    RA_Unset -> return RA_Unset
    RA_List at l -> RA_List at <$> subst l
    RA_Tuple t -> RA_Tuple <$> subst t

instance Subst DLRemoteALGO where
  subst (DLRemoteALGO {..}) =
    DLRemoteALGO <$> subst ra_fees <*> subst ra_accounts <*> subst ra_assets <*> subst ra_addr2acc <*> subst ra_apps <*> subst ra_boxes <*> subst ra_onCompletion <*> subst ra_strictPay <*> subst ra_rawCall <*> subst ra_simNetRecv <*> subst ra_simTokensRecv <*> subst ra_simReturnVal <*> subst ra_txnOrderForward

instance Subst AS.Value where
  subst = return

instance Subst DLContractNew where
  subst (DLContractNew a b) = DLContractNew <$> subst a <*> subst b

instance Subst DLRemote where
  subst (DLRemote m pamt as wbill malgo) = DLRemote <$> pure m <*> subst pamt <*> subst as <*> subst wbill <*> subst malgo

instance Subst DLExpr where
  subst = \case
    DLE_Arg at a -> DLE_Arg at <$> subst a
    DLE_LArg at a -> DLE_LArg at <$> subst a
    e@(DLE_Impossible {}) -> return $ e
    DLE_VerifyMuldiv at f cl as err -> DLE_VerifyMuldiv at f cl <$> subst as <*> pure err
    DLE_PrimOp at p as -> DLE_PrimOp at p <$> subst as
    DLE_ArrayRef at a b -> DLE_ArrayRef at <$> subst a <*> subst b
    DLE_ArraySet at a b c -> DLE_ArraySet at <$> subst a <*> subst b <*> subst c
    DLE_ArrayConcat at a b -> DLE_ArrayConcat at <$> subst a <*> subst b
    DLE_BytesDynCast at a -> DLE_BytesDynCast at <$> subst a
    DLE_TupleRef at x y -> DLE_TupleRef at <$> subst x <*> pure y
    DLE_ObjectRef at x y -> DLE_ObjectRef at <$> subst x <*> pure y
    DLE_Interact a b c d e f -> DLE_Interact a b c d e <$> subst f
    DLE_Digest at as -> DLE_Digest at <$> subst as
    DLE_Claim a b c d e -> DLE_Claim a b c <$> subst d <*> pure e
    DLE_Transfer at x y z -> DLE_Transfer at <$> subst x <*> subst y <*> subst z
    DLE_TokenInit at x -> DLE_TokenInit at <$> subst x
    DLE_TokenAccepted at addr tok -> DLE_TokenAccepted at <$> subst addr <*> subst tok
    DLE_CheckPay at x y z -> DLE_CheckPay at x <$> subst y <*> subst z
    DLE_Wait at x -> DLE_Wait at <$> subst x
    DLE_PartSet at x y -> DLE_PartSet at x <$> subst y
    DLE_MapRef at mv fa vt -> DLE_MapRef at mv <$> subst fa <*> pure vt
    DLE_MapSet at mv fa vt na -> DLE_MapSet at mv <$> subst fa <*> pure vt <*> subst na
    DLE_Remote at fs av rt dr -> DLE_Remote at fs <$> subst av <*> pure rt <*> subst dr
    DLE_TokenNew at tns -> DLE_TokenNew at <$> subst tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> subst tok <*> subst amt
    DLE_TokenDestroy at tok -> DLE_TokenDestroy at <$> subst tok
    DLE_TimeOrder at op a b -> DLE_TimeOrder at op <$> subst a <*> subst b
    DLE_EmitLog at lk x -> DLE_EmitLog at lk <$> subst x
    DLE_setApiDetails s p ts mc f -> return $ DLE_setApiDetails s p ts mc f
    DLE_GetUntrackedFunds at mtok tb -> DLE_GetUntrackedFunds at <$> subst mtok <*> subst tb
    DLE_DataTag at d -> DLE_DataTag at <$> subst d
    DLE_FromSome at mo da -> DLE_FromSome at <$> subst mo <*> subst da
    DLE_ContractNew at cns dr -> DLE_ContractNew at <$> subst cns <*> subst dr
    DLE_ObjectSet at a b c -> DLE_ObjectSet at <$> subst a <*> pure b <*> subst c
    DLE_TupleSet at a b c -> DLE_TupleSet at <$> subst a <*> pure b <*> subst c
    DLE_ContractFromAddress at a -> DLE_ContractFromAddress at <$> subst a

instance Subst DLStmt where
  subst = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at v de -> DL_Let at v <$> subst de
    DL_ArrayMap at ans x a i f ->
      DL_ArrayMap at ans <$> subst x <*> pure a <*> pure i <*> subst f
    DL_ArrayReduce at ans x z b a i f ->
      DL_ArrayReduce at ans <$> subst x <*> subst z <*> pure b <*> pure a <*> pure i <*> subst f
    DL_Var at v -> return $ DL_Var at v
    DL_Set at v a -> DL_Set at v <$> subst a
    DL_LocalIf at mans c t f -> DL_LocalIf at <$> subst mans <*> subst c <*> subst t <*> subst f
    DL_LocalSwitch at v csm -> DL_LocalSwitch at <$> subst v <*> subst csm
    DL_Only at who b -> DL_Only at who <$> subst b
    DL_MapReduce at mri x a b u k v bl ->
      DL_MapReduce at mri x a <$> subst b <*> pure u <*> pure k <*> pure v <*> subst bl
    DL_LocalDo at mans t -> DL_LocalDo at <$> subst mans <*> subst t

instance Subst DLTail where
  subst = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> DT_Com <$> subst m <*> subst k

instance Subst DLBlock where
  subst (DLBlock at fs t a) = DLBlock at fs <$> subst t <*> subst a

instance {-# OVERLAPS #-} (Subst a) => Subst (DLinExportBlock a) where
  subst (DLinExportBlock at mdom x) = DLinExportBlock at mdom <$> subst x

instance Subst DLAssignment where
  subst (DLAssignment m) = DLAssignment <$> subst m

instance Subst SvsPut where
  subst (SvsPut {..}) = SvsPut svsp_svs <$> subst svsp_val

instance Subst SvsGet where
  subst = return

instance Subst FromInfo where
  subst = \case
    FI_Continue svs -> FI_Continue <$> subst svs
    FI_Halt toks -> FI_Halt <$> subst toks

instance Subst CTail where
  subst = \case
    CT_Com m k -> CT_Com <$> subst m <*> subst k
    CT_If at c t f -> CT_If at <$> subst c <*> subst t <*> subst f
    CT_Switch at v csm -> CT_Switch at <$> subst v <*> subst csm
    CT_From at which msvs -> CT_From at which <$> subst msvs
    CT_Jump at which svs asn -> CT_Jump at which <$> subst svs <*> subst asn

subst_ :: Subst a => SubstEnv -> a -> a
subst_ rho x = runIdentity $ flip runReaderT rho $ subst x
