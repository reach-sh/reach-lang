module Reach.Subst
  ( subst_
  , Subst
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Maybe
import Reach.AST.DLBase
import Reach.AST.PL

type SubstEnv = M.Map DLVar DLVar

type SubstApp = ReaderT SubstEnv Identity

type SubstT a = a -> SubstApp a

class Subst a where
  subst :: SubstT a

instance (Traversable f, Subst a) => Subst (f a) where
  subst = traverse subst

instance {-# OVERLAPS #-} (Subst a, Subst b) => Subst (a, b) where
  subst (x, y) = (,) <$> subst x <*> subst y

instance {-# OVERLAPS #-} Subst a => Subst (SwitchCases a) where
  subst csm = mapM go csm
    where
      go (a, b, c) = (,,) a b <$> subst c

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
      where
        go (k, v) = (,) k <$> subst v

instance Subst DLPayAmt where
  subst = \case
    DLPayAmt net ks ->
      DLPayAmt <$> subst net
        <*> mapM (\(amt, ty) -> (,) <$> subst amt <*> subst ty) ks

instance Subst DLTokenNew where
  subst (DLTokenNew {..}) = DLTokenNew
    <$> subst dtn_name
    <*> subst dtn_sym
    <*> subst dtn_url
    <*> subst dtn_metadata
    <*> subst dtn_supply

instance Subst DLExpr where
  subst = \case
    DLE_Arg at a -> DLE_Arg at <$> subst a
    DLE_LArg at a -> DLE_LArg at <$> subst a
    e@(DLE_Impossible {}) -> return $ e
    DLE_PrimOp at p as -> DLE_PrimOp at p <$> subst as
    DLE_ArrayRef at a b -> DLE_ArrayRef at <$> subst a <*> subst b
    DLE_ArraySet at a b c -> DLE_ArraySet at <$> subst a <*> subst b <*> subst c
    DLE_ArrayConcat at a b -> DLE_ArrayConcat at <$> subst a <*> subst b
    DLE_ArrayZip at a b -> DLE_ArrayZip at <$> subst a <*> subst b
    DLE_TupleRef at x y -> DLE_TupleRef at <$> subst x <*> pure y
    DLE_ObjectRef at x y -> DLE_ObjectRef at <$> subst x <*> pure y
    DLE_Interact a b c d e f -> DLE_Interact a b c d e <$> subst f
    DLE_Digest at as -> DLE_Digest at <$> subst as
    DLE_Claim a b c d e -> DLE_Claim a b c <$> subst d <*> pure e
    DLE_Transfer at x y z -> DLE_Transfer at <$> subst x <*> subst y <*> subst z
    DLE_TokenInit at x -> DLE_TokenInit at <$> subst x
    DLE_CheckPay at x y z -> DLE_CheckPay at x <$> subst y <*> subst z
    DLE_Wait at x -> DLE_Wait at <$> subst x
    DLE_PartSet at x y -> DLE_PartSet at x <$> subst y
    DLE_MapRef at mv fa -> DLE_MapRef at mv <$> subst fa
    DLE_MapSet at mv fa na -> DLE_MapSet at mv <$> subst fa <*> subst na
    DLE_Remote at fs av m pamt as wbill -> DLE_Remote at fs <$> subst av <*> pure m <*> subst pamt <*> subst as <*> pure wbill
    DLE_TokenNew at tns -> DLE_TokenNew at <$> subst tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> subst tok <*> subst amt
    DLE_TokenDestroy at tok -> DLE_TokenDestroy at <$> subst tok
    DLE_TimeOrder at tos -> DLE_TimeOrder at <$> subst tos

instance Subst DLStmt where
  subst = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at v de -> DL_Let at v <$> subst de
    DL_ArrayMap at x a v bl ->
      DL_ArrayMap at x <$> subst a <*> pure v <*> subst bl
    DL_ArrayReduce at x a b u v bl ->
      DL_ArrayReduce at x <$> subst a <*> subst b <*> pure u <*> pure v <*> subst bl
    DL_Var at v -> return $ DL_Var at v
    DL_Set at v a -> DL_Set at v <$> subst a
    DL_LocalIf at c t f -> DL_LocalIf at <$> subst c <*> subst t <*> subst f
    DL_LocalSwitch at v csm -> DL_LocalSwitch at <$> subst v <*> subst csm
    DL_Only at who b -> DL_Only at who <$> subst b
    DL_MapReduce at mri x a b u v bl ->
      DL_MapReduce at mri x a <$> subst b <*> pure u <*> pure v <*> subst bl
    DL_LocalDo at t -> DL_LocalDo at <$> subst t

instance Subst DLTail where
  subst = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> DT_Com <$> subst m <*> subst k

instance Subst DLBlock where
  subst (DLBlock at fs t a) = DLBlock at fs <$> subst t <*> subst a

instance Subst DLAssignment where
  subst (DLAssignment m) = DLAssignment <$> subst m

instance Subst FromInfo where
  subst = \case
    FI_Continue vis svs -> FI_Continue <$> subst vis <*> subst svs
    FI_Halt toks -> FI_Halt <$> subst toks

instance Subst ViewSave where
  subst = \case
    ViewSave i svs -> ViewSave i <$> subst svs

instance Subst CTail where
  subst = \case
    CT_Com m k -> CT_Com <$> subst m <*> subst k
    CT_If at c t f -> CT_If at <$> subst c <*> subst t <*> subst f
    CT_Switch at v csm -> CT_Switch at <$> subst v <*> subst csm
    CT_From at which msvs -> CT_From at which <$> subst msvs
    CT_Jump at which svs asn -> CT_Jump at which <$> subst svs <*> subst asn

subst_ :: Subst a => SubstEnv -> a -> a
subst_ rho x = runIdentity $ flip runReaderT rho $ subst x
