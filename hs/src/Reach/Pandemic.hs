module Reach.Pandemic (Infections, pan_) where

import Control.Monad.Reader
import qualified Data.Aeson as AS
import qualified Data.Map.Strict as M
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase

type App = ReaderT Env IO
type Infections = M.Map Int (SrcLoc, SLVar)

data Env = Env
  { eInfections :: Infections
  }

pan_ :: Pandemic a => Infections -> a -> IO a
pan_ eInfections = flip runReaderT (Env {..}) . pan

class Pandemic a where
  pan :: a -> App a

instance Pandemic Char where
  pan = return

instance Pandemic DLType where
  pan = return

instance Pandemic a => Pandemic [a] where
  pan = mapM pan

instance Pandemic a => Pandemic (Maybe a) where
  pan = mapM pan

instance (Pandemic b) => Pandemic (Either a b) where
  pan = mapM pan

instance Pandemic DLStmts where
  pan = mapM pan

instance Pandemic AS.Value where
  pan = return

instance Pandemic DLContractNew where
  pan (DLContractNew {..}) =
    DLContractNew <$> pan dcn_code <*> pan dcn_opts

instance Pandemic DLRemote where
  pan (DLRemote s amt as bill malgo) =
    DLRemote <$> pan s <*> pan amt <*> pan as <*> pan bill <*> pan malgo

instance Pandemic DLExpr where
  pan = \case
    DLE_Arg at a -> DLE_Arg at <$> pan a
    DLE_LArg at a  -> DLE_LArg at <$> pan a
    DLE_Impossible at i err -> return $ DLE_Impossible at i err
    DLE_VerifyMuldiv at cxt ct as err -> DLE_VerifyMuldiv at cxt ct <$> pan as <*> pure err
    DLE_PrimOp at primop as -> DLE_PrimOp at primop <$> pan as
    DLE_ArrayRef at a i -> DLE_ArrayRef at <$> pan a <*> pan i
    DLE_ArraySet at a i v -> DLE_ArraySet at <$> pan a <*> pan i <*> pan v
    DLE_ArrayConcat at x y -> DLE_ArrayConcat at <$> pan x <*> pan y
    DLE_BytesDynCast at x -> DLE_BytesDynCast at <$> pan x
    DLE_TupleRef at a i -> DLE_TupleRef at <$> pan a <*> pure i
    DLE_ObjectRef at a f -> DLE_ObjectRef at <$> pan a <*> pure f
    DLE_Interact at cxt slp s ty as -> DLE_Interact at cxt slp s ty <$> pan as
    DLE_Digest at as -> DLE_Digest at <$> pan as
    DLE_Claim at cxt ct a mbbs -> DLE_Claim at cxt ct <$> pan a <*> pure mbbs
    DLE_Transfer at who da mtok -> DLE_Transfer at <$> pan who <*> pan da <*> pan mtok
    DLE_TokenInit at a -> DLE_TokenInit at <$> pan a
    DLE_TokenAccepted at addr tok -> DLE_TokenAccepted at <$> pan addr <*> pan tok
    DLE_CheckPay at cxt a mtok -> DLE_CheckPay at cxt <$> pan a <*> pan mtok
    DLE_Wait at a -> DLE_Wait at <$> pan a
    DLE_PartSet at slp a -> DLE_PartSet at slp <$> pan a
    DLE_MapRef at mv a -> DLE_MapRef at mv <$> pan a
    DLE_MapSet at mv a marg -> DLE_MapSet at mv <$> pan a <*> pan marg
    DLE_Remote at cxt a ty dr -> DLE_Remote at cxt <$> pan a <*> pan ty <*> pan dr
    DLE_TokenNew at tns -> DLE_TokenNew at <$> pan tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> pan tok <*> pan amt
    DLE_TokenDestroy at a -> DLE_TokenDestroy at <$> pan a
    DLE_TimeOrder at op a b -> DLE_TimeOrder at op <$> pan a <*> pan b
    DLE_EmitLog at lk vars -> DLE_EmitLog at lk <$> pan vars
    DLE_setApiDetails at who dom mc info -> return $ DLE_setApiDetails at who dom mc info
    DLE_GetUntrackedFunds at marg a -> DLE_GetUntrackedFunds at <$> pan marg <*> pan a
    DLE_DataTag at d -> DLE_DataTag at <$> pan d
    DLE_FromSome at mo da -> DLE_FromSome at <$> pan mo <*> pan da
    DLE_ContractNew at cns dr -> DLE_ContractNew at <$> pan cns <*> pan dr
    DLE_ContractFromAddress at a -> DLE_ContractFromAddress at <$> pan a
    DLE_ObjectSet at o k v -> DLE_ObjectSet at <$> pan o <*> pan k <*> pan v
    DLE_TupleSet at t i v -> DLE_TupleSet at <$> pan t <*> pure i <*> pan v

instance Pandemic DLVar where
  pan (DLVar at m_locvar t i) = do
    case m_locvar of
      Nothing -> do
        r <- asks eInfections
        return $ DLVar at (M.lookup i r) t i
      Just _ -> return $ DLVar at m_locvar t i

instance Pandemic DLLetVar where
  pan = \case
    DLV_Eff -> return DLV_Eff
    DLV_Let vc v -> DLV_Let vc <$> pan v

instance Pandemic DLSBlock where
  pan (DLSBlock at cxt sts a) = DLSBlock at cxt <$> pan sts <*> pan a

instance Pandemic DLWithBill where
  pan (DLWithBill nr b nb) = DLWithBill nr <$> pan b <*> pan nb

instance Pandemic Bool where
  pan = return

instance Pandemic DLRemoteALGOOC where
  pan = return

instance Pandemic DLRemoteALGOSTR where
  pan = \case
    RA_Unset -> return RA_Unset
    RA_List at l -> RA_List at <$> pan l
    RA_Tuple t -> RA_Tuple <$> pan t

instance Pandemic DLRemoteALGO where
  pan (DLRemoteALGO a b c d e f g h i j k) =
    DLRemoteALGO <$> pan a <*> pan b <*> pan c <*> pan d <*> pan e <*> pan f <*> pan g <*> pan h <*>
                     pan i <*> pan j <*> pan k

instance Pandemic DLPayAmt where
  pan (DLPayAmt net ks) = do
    let f (a,b) = (,) <$> pan a <*> pan b
    DLPayAmt <$> pan net <*> mapM f ks

instance Pandemic DLTokenNew where
  pan (DLTokenNew r s t u v w) = do
    DLTokenNew <$> pan r <*> pan s <*> pan t <*> pan u <*> pan v <*> pan w

instance Pandemic DLArg where
  pan = \case
    DLA_Var v -> DLA_Var <$> pan v
    DLA_Constant c -> return $ DLA_Constant c
    DLA_Literal l -> return $ DLA_Literal l
    DLA_Interact sl s t -> return $ DLA_Interact sl s t

instance Pandemic b => Pandemic (a, b) where
  pan (s,a) = (,) s <$> pan a

instance (Pandemic a, Pandemic b, Pandemic c) => Pandemic (a, b, c) where
  pan (x, y, z) = (,,) <$> pan x <*> pan y <*> pan z

instance Pandemic DLLargeArg where
  pan = \case
    DLLA_Array t as -> DLLA_Array t <$> pan as
    DLLA_Tuple as -> DLLA_Tuple <$> pan as
    DLLA_Obj strs_args -> DLLA_Obj <$> pan strs_args
    DLLA_Data m s a -> DLLA_Data m s <$> pan a
    DLLA_Struct vars_args -> DLLA_Struct <$> pan vars_args
    DLLA_Bytes s -> return $ DLLA_Bytes s
    DLLA_BytesDyn s -> return $ DLLA_BytesDyn s
    DLLA_StringDyn s -> return $ DLLA_StringDyn s

instance Pandemic DLSend where
  pan (DLSend b r s t) =
    DLSend b <$> pan r <*> pan s <*> pan t

instance Pandemic DLAssignment where
  pan (DLAssignment mvargs) = do
    let f (a,b) = (,) <$> pan a <*> pan b
    r <- mapM f $ M.toList mvargs
    return $ DLAssignment $ M.fromList r

instance Pandemic b => Pandemic (M.Map a b) where
  pan = mapM pan

instance Pandemic a => Pandemic (DLRecv a) where
  pan (DLRecv r s t u v w) =
    DLRecv <$> pan r <*> pan s <*> pan t <*> pan u <*> pan v <*> pan w

instance Pandemic a => Pandemic (DLInvariant a) where
  pan (DLInvariant inv lab) = DLInvariant <$> pan inv <*> pure lab

instance Pandemic DLSStmt where
  pan = \case
    DLS_Let at v e -> do
      DLS_Let at <$> pan v <*> pan e
    DLS_ArrayMap at v1 a1 v2 v3 bl -> do
      DLS_ArrayMap at <$> pan v1 <*> pan a1 <*> pan v2 <*> pan v3 <*> pan bl
    DLS_ArrayReduce at v1 a1 a2 v2 v3 v4 bl -> do
      DLS_ArrayReduce at <$> pan v1 <*> pan a1 <*> pan a2 <*> pan v2 <*> pan v3 <*> pan v4 <*> pan bl
    DLS_If at ans a ann sts1 sts2 -> DLS_If at <$> pan ans <*> pan a <*> pure ann <*> pan sts1 <*> pan sts2
    DLS_Switch at v sa sw -> DLS_Switch at <$> pan v <*> pure sa <*> pan sw
    DLS_Return at i a -> DLS_Return at i <$> pan a
    DLS_Prompt at v ann sts -> DLS_Prompt at <$> pan v <*> pure ann <*> pan sts
    DLS_Stop at -> return $ DLS_Stop at
    DLS_Unreachable at ctx s -> return $ DLS_Unreachable at ctx s
    DLS_Only at sl sts -> DLS_Only at sl <$> pan sts
    DLS_ToConsensus at s r m -> DLS_ToConsensus at <$> pan s <*> pan r <*> pan m
    DLS_FromConsensus at cxt sts -> DLS_FromConsensus at cxt <$> pan sts
    DLS_While at agn bl1 bl2 sts -> do
      DLS_While at <$> pan agn <*> pan bl1 <*> pan bl2 <*> pan sts
    DLS_Continue at agn -> DLS_Continue at <$> pan agn
    DLS_FluidSet at flv a -> DLS_FluidSet at flv <$> pan a
    DLS_FluidRef at v flv -> DLS_FluidRef at <$> pan v <*> pure flv
    DLS_MapReduce at i v1 mv a v2 v3 bl -> do
      DLS_MapReduce at i <$> pan v1 <*> pure mv <*> pan a <*> pan v2 <*> pan v3 <*> pan bl
    DLS_Throw at a b -> DLS_Throw at <$> pan a <*> pure b
    DLS_Try at sts1 v sts2 -> DLS_Try at <$> pan sts1 <*> pan v <*> pan sts2
    DLS_ViewIs at sl1 sl2 expo -> return $ DLS_ViewIs at sl1 sl2 expo
    DLS_TokenMetaGet tm at v a i -> DLS_TokenMetaGet tm at <$> pan v <*> pan a <*> pure i
    DLS_TokenMetaSet tm at a1 a2 i b -> DLS_TokenMetaSet tm at <$> pan a1 <*> pan a2 <*> pure i <*> pure b

