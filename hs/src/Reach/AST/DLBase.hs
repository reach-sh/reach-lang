{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DLBase where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Generics
import Reach.AST.Base

data DeployMode
  = DM_constructor
  | DM_firstMsg
  deriving (Eq, Generic, NFData, Show)

newtype InteractEnv
  = InteractEnv (M.Map SLVar SLType)
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

data DLVar = DLVar SrcLoc String SLType Int
  deriving (Generic, NFData, Show, Ord)

instance Eq DLVar where
  (DLVar _ _ _ x) == (DLVar _ _ _ y) = x == y

dvdelete :: DLVar -> [DLVar] -> [DLVar]
dvdelete x = filter (x /=)

dvdeletem :: Maybe DLVar -> [DLVar] -> [DLVar]
dvdeletem = \case
  Nothing -> id
  Just x -> dvdelete x

varType :: DLVar -> SLType
varType (DLVar _ _ t _) = t

data DLArg
  = DLA_Var DLVar
  | DLA_Constant DLConstant
  | DLA_Literal DLLiteral
  | DLA_Interact SLPart String SLType
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
  = DLLA_Array SLType [DLArg]
  | DLLA_Tuple [DLArg]
  | DLLA_Obj (M.Map String DLArg)
  | DLLA_Data (M.Map SLVar SLType) String DLArg
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
  | DLE_Interact SrcLoc [SLCtxtFrame] SLPart String SLType [DLArg]
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
