{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DLBase where

import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Generics
import Reach.AST.Base

data DeployMode
  = DM_constructor
  | DM_firstMsg
  deriving (Eq, Generic, Show)

-- DL types only describe data, and explicitly do not describe functions
data DLType
  = T_Null
  | T_Bool
  | T_UInt
  | T_Bytes Integer
  | T_Digest
  | T_Address
  | T_Array DLType Integer
  | T_Tuple [DLType]
  | T_Object (M.Map SLVar DLType)
  | T_Data (M.Map SLVar DLType)
  deriving (Eq, Generic, Ord)

showTys :: Show a => [a] -> String
showTys = intercalate ", " . map show

showTyMap :: Show a => M.Map SLVar a -> String
showTyMap = intercalate ", " . map showPair . M.toList
  where
    showPair (name, ty) = show name <> ": " <> show ty

instance Show DLType where
  show T_Null = "Null"
  show T_Bool = "Bool"
  show T_UInt = "UInt"
  show (T_Bytes sz) = "Bytes(" <> show sz <> ")"
  show T_Digest = "Digest"
  show T_Address = "Address"
  show (T_Array ty i) = "Array(" <> show ty <> ", " <> show i <> ")"
  show (T_Tuple tys) = "Tuple(" <> showTys tys <> ")"
  show (T_Object tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (T_Data tyMap) = "Object({" <> showTyMap tyMap <> "})"

-- Interact types can only be value types or first-order function types
data IType
  = IT_Val DLType
  | IT_Fun [DLType] DLType
  deriving (Eq, Ord, Generic, Show)

newtype InteractEnv
  = InteractEnv (M.Map SLVar IType)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

newtype SLParts
  = SLParts (M.Map SLPart InteractEnv)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

data DLInit = DLInit
  {dli_ctimem :: Maybe DLVar}
  deriving (Eq, Generic, Show, Ord)

data DLConstant
  = DLC_UInt_max
  deriving (Eq, Generic, Show, Ord)

conTypeOf :: DLConstant -> DLType
conTypeOf = \case
  DLC_UInt_max -> T_UInt

data DLLiteral
  = DLL_Null
  | DLL_Bool Bool
  | DLL_Int SrcLoc Integer
  | DLL_Bytes B.ByteString
  deriving (Eq, Generic, Show, Ord)

litTypeOf :: DLLiteral -> DLType
litTypeOf = \case
  DLL_Null -> T_Null
  DLL_Bool _ -> T_Bool
  DLL_Int {} -> T_UInt
  DLL_Bytes bs -> T_Bytes $ fromIntegral $ B.length bs

data DLVar = DLVar SrcLoc String DLType Int
  deriving (Generic, Show, Ord)

instance Eq DLVar where
  (DLVar _ _ _ x) == (DLVar _ _ _ y) = x == y

dvdelete :: DLVar -> [DLVar] -> [DLVar]
dvdelete x = filter (x /=)

dvdeletem :: Maybe DLVar -> [DLVar] -> [DLVar]
dvdeletem = \case
  Nothing -> id
  Just x -> dvdelete x

dvdeletep :: DLVar -> [(DLVar, a)] -> [(DLVar, a)]
dvdeletep x = filter ((x /=) . fst)

varType :: DLVar -> DLType
varType (DLVar _ _ t _) = t

data DLArg
  = DLA_Var DLVar
  | DLA_Constant DLConstant
  | DLA_Literal DLLiteral
  | DLA_Interact SLPart String DLType
  deriving (Eq, Ord, Generic, Show)

argTypeOf :: DLArg -> DLType
argTypeOf = \case
  DLA_Var (DLVar _ _ t _) -> t
  DLA_Constant c -> conTypeOf c
  DLA_Literal c -> litTypeOf c
  DLA_Interact _ _ t -> t

data DLLargeArg
  = DLLA_Array DLType [DLArg]
  | DLLA_Tuple [DLArg]
  | DLLA_Obj (M.Map String DLArg)
  | DLLA_Data (M.Map SLVar DLType) String DLArg
  deriving (Eq, Ord, Generic, Show)

data DLArgExpr
  = DLAE_Arg DLArg
  | DLAE_Array DLType [DLArgExpr]
  | DLAE_Tuple [DLArgExpr]
  | DLAE_Obj (M.Map SLVar DLArgExpr)
  | DLAE_Data (M.Map SLVar DLType) String DLArgExpr

largeArgToArgExpr :: DLLargeArg -> DLArgExpr
largeArgToArgExpr = \case
  DLLA_Array sz as -> DLAE_Array sz $ map DLAE_Arg as
  DLLA_Tuple as -> DLAE_Tuple $ map DLAE_Arg as
  DLLA_Obj m -> DLAE_Obj $ M.map DLAE_Arg m
  DLLA_Data m v a -> DLAE_Data m v $ DLAE_Arg a

largeArgTypeOf :: DLLargeArg -> DLType
largeArgTypeOf = argExprTypeOf . largeArgToArgExpr

argExprTypeOf :: DLArgExpr -> DLType
argExprTypeOf = \case
  DLAE_Arg a -> argTypeOf a
  DLAE_Array t as -> T_Array t $ fromIntegral (length as)
  DLAE_Tuple as -> T_Tuple $ map argExprTypeOf as
  DLAE_Obj senv -> T_Object $ M.map argExprTypeOf senv
  DLAE_Data t _ _ -> T_Data t

data ClaimType
  = --- Verified on all paths
    CT_Assert
  | --- Always assumed true
    CT_Assume
  | --- Verified in honest, assumed in dishonest. (This may sound
    --- backwards, but by verifying it in honest mode, then we are
    --- checking that the other participants fulfill the promise when
    --- acting honestly.)
    CT_Require
  | --- Check if an assignment of variables exists to make
    --- this true.
    CT_Possible
  | --- Check if one part can't know what another party does know
    CT_Unknowable SLPart [DLArg]
  deriving (Eq, Ord, Generic, Show)

class IsPure a where
  isPure :: a -> Bool

class IsLocal a where
  isLocal :: a -> Bool

instance IsPure a => IsPure (Seq.Seq a) where
  isPure = all isPure

instance IsLocal a => IsLocal (Seq.Seq a) where
  isLocal = all isLocal

data DLExpr
  = DLE_Arg SrcLoc DLArg
  | DLE_LArg SrcLoc DLLargeArg
  | DLE_Impossible SrcLoc String
  | DLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLE_ArrayRef SrcLoc DLArg DLArg
  | DLE_ArraySet SrcLoc DLArg DLArg DLArg
  | DLE_ArrayConcat SrcLoc DLArg DLArg
  | DLE_ArrayZip SrcLoc DLArg DLArg
  | DLE_TupleRef SrcLoc DLArg Integer
  | DLE_ObjectRef SrcLoc DLArg String
  | DLE_Interact SrcLoc [SLCtxtFrame] SLPart String DLType [DLArg]
  | DLE_Digest SrcLoc [DLArg]
  | DLE_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg (Maybe B.ByteString)
  | DLE_Transfer SrcLoc DLArg DLArg
  | DLE_Wait SrcLoc DLArg
  | DLE_PartSet SrcLoc SLPart DLArg
  deriving (Eq, Ord, Generic, Show)

instance IsPure DLExpr where
  isPure = \case
    DLE_Arg {} -> True
    DLE_LArg {} -> True
    DLE_Impossible {} -> True
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_ArraySet {} -> True
    DLE_ArrayConcat {} -> True
    DLE_ArrayZip {} -> True
    DLE_TupleRef {} -> True
    DLE_ObjectRef {} -> True
    DLE_Interact {} -> False
    DLE_Digest {} -> True
    DLE_Claim _ _ ct _ _ ->
      case ct of
        CT_Assert -> True
        CT_Possible -> True
        CT_Assume -> False
        CT_Require -> False
        CT_Unknowable {} -> True
    DLE_Transfer {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> False

instance IsLocal DLExpr where
  isLocal = \case
    DLE_Arg {} -> True
    DLE_LArg {} -> True
    DLE_Impossible {} -> True
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_ArraySet {} -> True
    DLE_ArrayConcat {} -> True
    DLE_ArrayZip {} -> True
    DLE_TupleRef {} -> True
    DLE_ObjectRef {} -> True
    DLE_Interact {} -> True
    DLE_Digest {} -> True
    DLE_Claim {} -> True
    DLE_Transfer {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> True

newtype DLAssignment
  = DLAssignment (M.Map DLVar DLArg)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

assignment_vars :: DLAssignment -> [DLVar]
assignment_vars (DLAssignment m) = M.keys m

type SwitchCases a =
  --- FIXME at the SrcLoc of the case
  M.Map SLVar (Maybe DLVar, a)

data DLinStmt a
  = DL_Nop SrcLoc
  | DL_Let SrcLoc a DLExpr
  | DL_ArrayMap SrcLoc DLVar DLArg DLVar (DLinBlock a)
  | DL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar (DLinBlock a)
  | DL_Var SrcLoc DLVar
  | DL_Set SrcLoc DLVar DLArg
  | DL_LocalIf SrcLoc DLArg (DLinTail a) (DLinTail a)
  | DL_LocalSwitch SrcLoc DLVar (SwitchCases (DLinTail a))
  deriving (Eq, Show)

data DLinTail a
  = DT_Return SrcLoc
  | DT_Com (DLinStmt a) (DLinTail a)
  deriving (Eq, Show)

data DLinBlock a
  = DLinBlock SrcLoc [SLCtxtFrame] (DLinTail a) DLArg
  deriving (Eq, Show)

data FluidVar
  = FV_balance
  | FV_thisConsensusTime
  | FV_lastConsensusTime
  deriving (Eq, Generic, Ord, Show, Bounded, Enum)

fluidVarType :: FluidVar -> DLType
fluidVarType = \case
  FV_balance -> T_UInt
  FV_thisConsensusTime -> T_UInt
  FV_lastConsensusTime -> T_UInt

allFluidVars :: [FluidVar]
allFluidVars = enumFrom minBound
