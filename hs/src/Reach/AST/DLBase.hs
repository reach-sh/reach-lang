{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DLBase where

import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Generics
import Reach.AST.Base
import Reach.Pretty
import Reach.Texty
import Reach.Util

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

arrType :: DLType -> DLType
arrType = \case
  T_Array d _ -> d
  _ -> impossible "no array"

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

instance Pretty DLType where
  pretty = viaShow

-- Interact types can only be value types or first-order function types
data IType
  = IT_Val DLType
  | IT_Fun [DLType] DLType
  deriving (Eq, Ord, Generic, Show)

instance Pretty IType where
  pretty = viaShow

newtype InteractEnv
  = InteractEnv (M.Map SLVar IType)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty InteractEnv where
  pretty (InteractEnv m) = "interact" <+> render_obj m

newtype SLParts
  = SLParts (M.Map SLPart InteractEnv)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty SLParts where
  pretty (SLParts m) = "parts" <+> render_obj m <> semi

data DLInit = DLInit
  {dli_ctimem :: Maybe DLVar}
  deriving (Eq, Generic, Show, Ord)

instance Pretty DLInit where
  pretty (DLInit ctimem) =
    "// initialization" <> hardline
      <> ctimem'
    where
      ctimem' = case ctimem of
        Nothing -> "// no ctime" <> hardline
        Just x -> "const" <+> pretty x <+> "=" <+> "creationTime();" <> hardline

data DLConstant
  = DLC_UInt_max
  deriving (Eq, Generic, Show, Ord)

instance Pretty DLConstant where
  pretty = \case
    DLC_UInt_max -> "UInt.max"

conTypeOf :: DLConstant -> DLType
conTypeOf = \case
  DLC_UInt_max -> T_UInt

data DLLiteral
  = DLL_Null
  | DLL_Bool Bool
  | DLL_Int SrcLoc Integer
  | DLL_Bytes B.ByteString
  deriving (Eq, Generic, Show, Ord)

instance Pretty DLLiteral where
  pretty = \case
    DLL_Null -> "null"
    DLL_Bool b -> if b then "#t" else "#f"
    DLL_Int _ i -> viaShow i
    DLL_Bytes bs -> dquotes (viaShow bs)

litTypeOf :: DLLiteral -> DLType
litTypeOf = \case
  DLL_Null -> T_Null
  DLL_Bool _ -> T_Bool
  DLL_Int {} -> T_UInt
  DLL_Bytes bs -> T_Bytes $ fromIntegral $ B.length bs

data DLVar = DLVar SrcLoc String DLType Int
  deriving (Generic, Show)

instance Eq DLVar where
  (DLVar _ _ _ x) == (DLVar _ _ _ y) = x == y

instance Ord DLVar where
  (DLVar _ _ _ x) <= (DLVar _ _ _ y) = x <= y

instance Pretty DLVar where
  pretty (DLVar _ _ _ i) = "v" <> viaShow i

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

newtype DLMVar = DLMVar Int
  deriving (Eq, Ord, Generic)

instance Pretty DLMVar where
  pretty (DLMVar i) = "map" <> pretty i

data DLMapInfo = DLMapInfo
  { dlmi_ty :: DLType
  , dlmi_at :: SrcLoc }
  deriving (Eq, Generic)

data DLArg
  = DLA_Var DLVar
  | DLA_Constant DLConstant
  | DLA_Literal DLLiteral
  | DLA_Interact SLPart String DLType
  deriving (Eq, Ord, Generic, Show)

instance Pretty DLArg where
  pretty = \case
    DLA_Var v -> pretty v
    DLA_Constant c -> pretty c
    DLA_Literal c -> pretty c
    DLA_Interact who m t ->
      "interact(" <> render_sp who <> ")." <> viaShow m <> parens (pretty t)

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

instance Pretty DLLargeArg where
  pretty = \case
    DLLA_Array t as -> "array" <> parens (pretty t <> comma <+> pretty (DLLA_Tuple as))
    DLLA_Tuple as -> brackets $ render_das as
    DLLA_Obj env -> render_obj env
    DLLA_Data _ vn vv -> "<" <> pretty vn <> " " <> pretty vv <> ">"

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

instance Pretty ClaimType where
  pretty = \case
    CT_Assert -> "assert"
    CT_Assume -> "assume"
    CT_Require -> "require"
    CT_Possible -> "possible"
    CT_Unknowable p as -> "unknowable" <> parens (pretty p <> render_das as)

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
  | DLE_MapRef SrcLoc DLMVar DLArg
  | DLE_MapSet SrcLoc DLMVar DLArg DLArg
  | DLE_MapDel SrcLoc DLMVar DLArg
  deriving (Eq, Ord, Generic)

instance Pretty DLExpr where
  pretty e =
    case e of
      DLE_Arg _ a -> pretty a
      DLE_LArg _ a -> pretty a
      DLE_Impossible _ msg -> "impossible" <> parens (pretty msg)
      DLE_PrimOp _ IF_THEN_ELSE [c, t, el] -> pretty c <> " ? " <> pretty t <> " : " <> pretty el
      DLE_PrimOp _ o [a] -> pretty o <> pretty a
      DLE_PrimOp _ o [a, b] -> hsep [pretty a, pretty o, pretty b]
      DLE_PrimOp _ o as -> pretty o <> parens (render_das as)
      DLE_ArrayRef _ a o -> pretty a <> brackets (pretty o)
      DLE_ArraySet _ a i v -> "array_set" <> (parens $ render_das [a, i, v])
      DLE_ArrayConcat _ x y -> "array_concat" <> (parens $ render_das [x, y])
      DLE_ArrayZip _ x y -> "array_zip" <> (parens $ render_das [x, y])
      DLE_TupleRef _ a i -> pretty a <> brackets (pretty i)
      DLE_ObjectRef _ a f -> pretty a <> "." <> pretty f
      DLE_Interact _ _ who m _ as -> "interact(" <> render_sp who <> ")." <> viaShow m <> parens (render_das as)
      DLE_Digest _ as -> "digest" <> parens (render_das as)
      DLE_Claim _ _ ct a m -> prettyClaim ct a m
      DLE_Transfer _ who da ->
        prettyTransfer who da
      DLE_Wait _ a -> "wait" <> parens (pretty a)
      DLE_PartSet _ who a -> render_sp who <> ".set" <> parens (pretty a)
      DLE_MapRef _ mv i -> pretty mv <> brackets (pretty i)
      DLE_MapSet _ mv kv nv ->
        pretty mv <> "[" <> pretty kv <> "]" <+> "=" <+> pretty nv
      DLE_MapDel _ mv i -> "delete" <+> pretty mv <> brackets (pretty i)

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
    DLE_MapRef {} -> True
    DLE_MapSet {} -> False
    DLE_MapDel {} -> False

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
    DLE_MapRef {} -> True
    DLE_MapSet {} -> False
    DLE_MapDel {} -> False

newtype DLAssignment
  = DLAssignment (M.Map DLVar DLArg)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty DLAssignment where
  pretty (DLAssignment m) = render_obj m

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
  deriving (Eq)

instance Pretty a => Pretty (DLinStmt a) where
  pretty = \case
    DL_Nop _ -> mempty
    DL_Let _at x de -> "const" <+> pretty x <+> "=" <+> pretty de <> semi
    DL_ArrayMap _ ans x a f -> prettyMap ans x a f
    DL_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
    DL_Var _at dv -> "let" <+> pretty dv <> semi
    DL_Set _at dv da -> pretty dv <+> "=" <+> pretty da <> semi
    DL_LocalIf _at ca t f -> prettyIfp ca t f
    DL_LocalSwitch _at ov csm -> prettySwitch ov csm

data DLinTail a
  = DT_Return SrcLoc
  | DT_Com (DLinStmt a) (DLinTail a)
  deriving (Eq)

instance Pretty a => Pretty (DLinTail a) where
  pretty = \case
    DT_Return _at -> mempty
    DT_Com x k -> prettyCom x k

data DLinBlock a
  = DLinBlock SrcLoc [SLCtxtFrame] (DLinTail a) DLArg
  deriving (Eq)

instance Pretty a => Pretty (DLinBlock a) where
  pretty (DLinBlock _ _ ts ta) = prettyBlockP ts ta

data FluidVar
  = FV_balance
  | FV_thisConsensusTime
  | FV_lastConsensusTime
  deriving (Eq, Generic, Ord, Show, Bounded, Enum)

instance Pretty FluidVar where
  pretty = \case
    FV_balance -> "balance"
    FV_thisConsensusTime -> "thisConsensusTime"
    FV_lastConsensusTime -> "lastConsensusTime"

fluidVarType :: FluidVar -> DLType
fluidVarType = \case
  FV_balance -> T_UInt
  FV_thisConsensusTime -> T_UInt
  FV_lastConsensusTime -> T_UInt

allFluidVars :: [FluidVar]
allFluidVars = enumFrom minBound
