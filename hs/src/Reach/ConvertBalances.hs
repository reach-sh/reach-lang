module Reach.ConvertBalances (convertBalances) where

import Reach.AST.DL
import Control.Monad.Reader
import Reach.AST.DLBase
import Reach.Util
-- import Reach.Texty
import Reach.Counter
-- import Reach.AST.Base


type App = ReaderT Env IO

type AppT a = a -> App a

data Env = Env
  { eBals :: Integer
  , eId :: Counter
  }

class ConvertBalances a where
  cb :: AppT a

getBalanceType :: App DLType
getBalanceType = do
  eBals <- asks eBals
  return $ T_Array balanceElemTy eBals

instance {-# OVERLAPPABLE #-} (Traversable m, ConvertBalances a) => ConvertBalances (m a) where
  cb = traverse cb

instance (ConvertBalances a, ConvertBalances b) => ConvertBalances (a, b) where
  cb (x, y) = (,) <$> cb x <*> cb y

instance (ConvertBalances a, ConvertBalances b) => ConvertBalances (Either a b) where
  cb = \case
    Left x  -> Left <$> cb x
    Right x -> Right <$> cb x

instance ConvertBalances DLLargeArg where
  cb = \case
    DLLA_Array dt das -> DLLA_Array dt <$> cb das
    DLLA_Tuple das -> DLLA_Tuple <$> cb das
    DLLA_Obj m -> DLLA_Obj <$> cb m
    DLLA_Data m s da -> DLLA_Data m s <$> cb da
    DLLA_Struct xs -> DLLA_Struct <$> mapM go xs
    DLLA_Bytes bs -> return $ DLLA_Bytes bs
    where
      go (k, v) = (,) k <$> cb v

instance ConvertBalances DLTokenNew where
  cb = \case
    DLTokenNew {..} ->
      DLTokenNew
      <$> cb dtn_name
      <*> cb dtn_sym
      <*> cb dtn_url
      <*> cb dtn_metadata
      <*> cb dtn_supply
      <*> cb dtn_decimals

instance ConvertBalances DLPayAmt where
  cb (DLPayAmt {..}) =
    DLPayAmt <$> cb pa_net <*> cb pa_ks

instance ConvertBalances DLWithBill where
  cb (DLWithBill {..}) =
    DLWithBill <$> cb dwb_tok_billed <*> cb dwb_tok_not_billed

instance ConvertBalances DLExpr where
  cb = \case
    DLE_Arg sl da ->
      DLE_Arg sl <$> cb da
    DLE_LArg sl dla ->
      DLE_LArg sl <$> cb dla
    v@(DLE_Impossible {}) -> return v
    DLE_VerifyMuldiv sl f ct das ie ->
      DLE_VerifyMuldiv sl f ct <$> cb das <*> pure ie
    DLE_PrimOp sl po das ->
      DLE_PrimOp sl po <$> cb das
    DLE_ArrayRef sl a b ->
      DLE_ArrayRef sl <$> cb a <*> cb b
    DLE_ArraySet sl a b c ->
      DLE_ArraySet sl <$> cb a <*> cb b <*> cb c
    DLE_ArrayConcat sl a b ->
      DLE_ArrayConcat sl <$> cb a <*> cb b
    DLE_ArrayZip sl a b ->
      DLE_ArrayZip sl <$> cb a <*> cb b
    DLE_TupleRef sl da n ->
      DLE_TupleRef sl <$> cb da <*> pure n
    DLE_ObjectRef sl da s ->
      DLE_ObjectRef sl <$> cb da <*> pure s
    DLE_Interact sl f w s dt das ->
      DLE_Interact sl f w s dt <$> cb das
    DLE_Digest sl das ->
      DLE_Digest sl <$> cb das
    DLE_Claim sl f ct da mb ->
      DLE_Claim sl f ct <$> cb da <*> pure mb
    DLE_Transfer sl a b mc ->
      DLE_Transfer sl <$> cb a <*> cb b <*> cb mc
    DLE_TokenInit sl da ->
      DLE_TokenInit sl <$> cb da
    DLE_CheckPay sl f da ma ->
      DLE_CheckPay sl f <$> cb da <*> cb ma
    DLE_Wait sl e ->
      DLE_Wait sl <$> cb e
    DLE_PartSet sl bs da ->
      DLE_PartSet sl bs <$> cb da
    DLE_MapRef sl mv da ->
      DLE_MapRef sl <$> pure mv <*> cb da
    DLE_MapSet sl mv da ma ->
      DLE_MapSet sl mv <$> cb da <*> cb ma
    DLE_Remote sl f da s dpa das dwb -> do
      DLE_Remote sl f <$> cb da <*> pure s <*> cb dpa <*> cb das <*> cb dwb
    DLE_TokenNew sl dtn ->
      DLE_TokenNew sl <$> cb dtn
    DLE_TokenBurn sl a b ->
      DLE_TokenBurn sl <$> cb a <*> cb b
    DLE_TokenDestroy sl da ->
      DLE_TokenDestroy sl <$> cb da
    DLE_TimeOrder sl a ->
      DLE_TimeOrder sl <$> cb a
    DLE_GetContract sl ->
      return $ DLE_GetContract sl
    DLE_GetAddress sl ->
      return $ DLE_GetAddress sl
    DLE_EmitLog sl lk dvs ->
      DLE_EmitLog sl lk <$> cb dvs
    DLE_setApiDetails sl bs dts ms aic ->
      return $ DLE_setApiDetails sl bs dts ms aic
    DLE_GetUntrackedFunds sl ma da ->
      DLE_GetUntrackedFunds sl <$> cb ma <*> cb da
    DLE_FromSome sl a b ->
      DLE_FromSome sl <$> cb a <*> cb b
    DLE_BalanceInit dv ->
      DLE_BalanceInit <$> cb dv

instance ConvertBalances DLLetVar where
  cb = \case
    DLV_Let dvc dv -> DLV_Let dvc <$> cb dv
    ow -> return ow

instance ConvertBalances DLSBlock where
  cb = \case
    DLSBlock at f ss da -> DLSBlock at f <$> cb ss <*> cb da

instance ConvertBalances DLArg where
  cb = \case
    DLA_Var dv -> DLA_Var <$> cb dv
    ow -> return ow

instance ConvertBalances DLVar where
  cb = \case
    DLVar a bo T_Balances i -> DLVar a bo <$> getBalanceType <*> pure i
    ow -> return ow

instance ConvertBalances DLAssignment where
  cb (DLAssignment m) = DLAssignment <$> cb m

instance ConvertBalances a => ConvertBalances (DLinExportBlock a) where
  cb (DLinExportBlock a b c) = DLinExportBlock a <$> cb b <*> cb c

-- mkVar :: SrcLoc -> DLType -> App DLVar
-- mkVar at t = do
--   eId <- asks eId
--   idx <- liftIO $ incCounter eId
--   return $ DLVar at Nothing t idx

instance ConvertBalances DLSStmt where
  cb = \case
    _ -> impossible ""
    -- DLS_TokenMetaGet TM_Balance at dv k mp -> do
    --   let dv' = cb dv
    --   let k' = cb k
    --   let idx = case mp of
    --           Just i -> i
    --           Nothing -> impossible "DLS_TokenMetaGet: Requires dynamic computation"
    --   dv2 <- mkVar at T_UInt
    --   return $ DLS_Let at (DLV_Let DVC_Many dv2) $ DLS_

    -- DLS_TokenMetaSet TM_Balance at dv k v mp -> do
    --   let dv' = cb dv
    --   let k' = cb k
    --   let v' = cb v
    --   let idx = case mp of
    --           Just i -> i
    --           Nothing -> impossible "DLS_TokenMetaSet: Requires dynamic computation"
    --   impossible $ show $ pretty v
    -- DLS_Let sl dlv de ->
    --   DLS_Let sl <$> cb dlv <*> cb de
    -- DLS_ArrayMap sl dv1 da dv2 dv3 db ->
    --   DLS_ArrayMap sl <$> cb dv1 <*> cb da <*> cb dv2 <*> cb dv3 <*> cb db
    -- DLS_ArrayReduce sl dv1 da1 da2 dv2 dv3 db ->
    --   DLS_ArrayReduce sl <$> cb dv1 <*> cb da1 <*> cb da2 <*> cb dv2 <*> cb dv3 <*> cb db
    -- DLS_If sl da sa seq1 seq2 ->
    --   DLS_If sl <$> cb da <*> pure sa <*> cb seq1 <*> cb seq2
    -- DLS_Switch sl dv sa sc -> do
    --   sc' <- mapM (\(a, b, c) -> (a,b,) <$> cb c) sc
    --   DLS_Switch sl <$> cb dv <*> pure sa <*> pure sc'
    -- DLS_Return sl n da ->
    --   DLS_Return sl n <$> cb da
    -- DLS_Prompt sl dv sa ss ->
    --   DLS_Prompt sl <$> cb dv <*> pure sa <*> cb ss
    -- DLS_Stop sl ->
    --   return $ DLS_Stop sl
    -- DLS_Unreachable sl scfs s ->
    --   return $ DLS_Unreachable sl scfs s
    -- DLS_Only sl w ss ->
    --   DLS_Only sl w <$> cb ss
    -- DLS_ToConsensus sl m dr ma -> do
    --   k'  <- cb $ dr_k dr
    --   let dr'= dr { dr_k = k' }
    --   ma' <- cb ma
    --   return $ DLS_ToConsensus sl m dr' ma'
    -- DLS_FromConsensus sl ss ->
    --   DLS_FromConsensus sl <$> cb ss
    -- DLS_While sl asn b1 b2 ss ->
    --   DLS_While sl <$> cb asn <*> cb b1 <*> cb b2 <*> cb ss
    -- DLS_Continue sl asn ->
    --   DLS_Continue sl <$> cb asn
    -- DLS_FluidSet sl fv da -> DLS_FluidSet sl fv <$> cb da
    -- DLS_FluidRef sl dv fv -> DLS_FluidRef sl <$> cb dv <*> pure fv
    -- DLS_MapReduce sl n dv1 mv da dv3 dv4 db ->
    --   DLS_MapReduce sl n <$> cb dv1 <*> pure mv <*> cb da <*> cb dv3 <*> cb dv4 <*> cb db
    -- DLS_Throw sl da b -> DLS_Throw sl <$> cb da <*> pure b
    -- DLS_Try sl ss1 dv ss2 -> DLS_Try sl <$> cb ss1 <*> cb dv <*> cb ss2
    -- DLS_ViewIs sl mv s eb -> do
    --   DLS_ViewIs sl mv s <$> cb eb

convertBalances :: DLProg -> IO DLProg
convertBalances (DLProg at opts sps dli dex dvs das devts ss) = do
  let eBals = fromIntegral $ dlo_bals opts
  let eId = dlo_counter opts
  flip runReaderT (Env {..}) $ do
    ss' <- cb ss
    return $ DLProg at opts sps dli dex dvs das devts ss'
