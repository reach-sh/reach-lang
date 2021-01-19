{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DLBase where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Generics
import GHC.Stack (HasCallStack)
import Reach.AST.Base

data DeployMode
  = DM_constructor
  | DM_firstMsg
  deriving (Eq, Generic, NFData, Show)

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
  deriving (Eq, Generic, NFData, Ord)

instance Show DLType where
  show = show . dt2st

-- Interact types can only be value types or first-order function types
data IType
  = IT_Val DLType
  | IT_Fun [DLType] DLType
  deriving (Eq, Ord, Generic, NFData, Show)

newtype InteractEnv
  = InteractEnv (M.Map SLVar IType)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, NFData, Semigroup)

newtype SLParts
  = SLParts (M.Map SLPart InteractEnv)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, NFData, Semigroup)

data DLInit = DLInit
  {dli_ctimem :: Maybe DLVar}
  deriving (Eq, Generic, NFData, Show, Ord)

data DLConstant
  = DLC_UInt_max
  deriving (Eq, Generic, NFData, Show, Ord)

data DLLiteral
  = DLL_Null
  | DLL_Bool Bool
  | DLL_Int SrcLoc Integer
  | DLL_Bytes B.ByteString
  deriving (Eq, Generic, NFData, Show, Ord)

data DLVar = DLVar SrcLoc String DLType Int
  deriving (Generic, NFData, Show, Ord)

instance Eq DLVar where
  (DLVar _ _ _ x) == (DLVar _ _ _ y) = x == y

-- XXX better error message for stuff like Array<Fun> or Array<Forall>
-- that can't exist in DL-land
st2dt :: HasCallStack => SLType -> DLType
st2dt = \case
  ST_Null -> T_Null
  ST_Bool -> T_Bool
  ST_UInt -> T_UInt
  ST_Bytes i -> T_Bytes i
  ST_Digest -> T_Digest
  ST_Address -> T_Address
  ST_Array ty i -> T_Array (st2dt ty) i
  ST_Tuple tys -> T_Tuple (map st2dt tys)
  ST_Object tyMap -> T_Object (M.map st2dt tyMap)
  ST_Data tyMap -> T_Data (M.map st2dt tyMap)
  -- XXX consider using Maybe so that callers have to handle the error case
  t@(ST_Fun {}) -> error $ "ST_Fun not a dt: " <> show t
  t@(ST_Forall {}) -> error $ "ST_Forall not a dt: " <> show t
  t@(ST_Var {}) -> error $ "ST_Var not a dt: " <> show t
  t@(ST_Type {}) -> error $ "ST_Type not a dt: " <> show t

dt2st :: DLType -> SLType
dt2st = \case
  T_Null -> ST_Null
  T_Bool -> ST_Bool
  T_UInt -> ST_UInt
  T_Bytes i -> ST_Bytes i
  T_Digest -> ST_Digest
  T_Address -> ST_Address
  T_Array ty i -> ST_Array (dt2st ty) i
  T_Tuple tys -> ST_Tuple (map dt2st tys)
  T_Object tyMap -> ST_Object (M.map dt2st tyMap)
  T_Data tyMap -> ST_Data (M.map dt2st tyMap)

-- XXX improve error messages
st2it :: SLType -> IType
st2it t = case t of
  ST_Fun dom rng -> IT_Fun (map st2dt dom) (st2dt rng)
  _ -> IT_Val (st2dt t)

dvdelete :: DLVar -> [DLVar] -> [DLVar]
dvdelete x = filter (x /=)

dvdeletem :: Maybe DLVar -> [DLVar] -> [DLVar]
dvdeletem = \case
  Nothing -> id
  Just x -> dvdelete x

varType :: DLVar -> DLType
varType (DLVar _ _ t _) = t

data DLArg
  = DLA_Var DLVar
  | DLA_Constant DLConstant
  | DLA_Literal DLLiteral
  | DLA_Interact SLPart String DLType
  deriving (Eq, Ord, Generic, NFData, Show)

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
  deriving (Eq, Ord, Generic, NFData, Show)

class IsPure a where
  isPure :: a -> Bool

class IsLocal a where
  isLocal :: a -> Bool

data StmtAnnot = StmtAnnot
  { sa_pure :: Bool
  , sa_local :: Bool
  }
  deriving (Eq, Generic, NFData, Show)

instance Semigroup StmtAnnot where
  (StmtAnnot xp xl) <> (StmtAnnot yp yl) = (StmtAnnot (xp && yp) (xl && yl))

instance Monoid StmtAnnot where
  mempty = StmtAnnot True True

instance IsPure StmtAnnot where
  isPure = sa_pure

instance IsLocal StmtAnnot where
  isLocal = sa_local

instance IsPure a => IsPure (Seq.Seq a) where
  isPure = all isPure

instance IsLocal a => IsLocal (Seq.Seq a) where
  isLocal = all isLocal

mkAnnot :: IsPure a => IsLocal a => a -> StmtAnnot
mkAnnot a = StmtAnnot {..}
  where
    sa_pure = isPure a
    sa_local = isLocal a

data DLLargeArg
  = DLLA_Array DLType [DLArg]
  | DLLA_Tuple [DLArg]
  | DLLA_Obj (M.Map String DLArg)
  | DLLA_Data (M.Map SLVar DLType) String DLArg
  deriving (Eq, Ord, Generic, NFData, Show)

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
  deriving (Eq, Ord, Generic, NFData, Show)

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
  deriving newtype (Monoid, NFData, Semigroup)

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
  deriving (Eq, Generic, NFData, Ord, Show, Bounded, Enum)

fluidVarType :: FluidVar -> DLType
fluidVarType = \case
  FV_balance -> T_UInt
  FV_thisConsensusTime -> T_UInt
  FV_lastConsensusTime -> T_UInt

allFluidVars :: [FluidVar]
allFluidVars = enumFrom minBound
