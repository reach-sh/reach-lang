{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.DLBase where

import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import GHC.Generics
import Reach.AST.Base
import Reach.Counter
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
  | T_Token
  | T_Array DLType Integer
  | T_Tuple [DLType]
  | T_Object (M.Map SLVar DLType)
  | T_Data (M.Map SLVar DLType)
  | T_Struct [(SLVar, DLType)]
  deriving (Eq, Generic, Ord)

maybeT :: DLType -> DLType
maybeT t = T_Data $ M.fromList $ [("None", T_Null), ("Some", t)]

dataTypeMap :: DLType -> M.Map SLVar DLType
dataTypeMap = \case
  T_Data m -> m
  _ -> impossible "no data"

arrTypeLen :: DLType -> (DLType, Integer)
arrTypeLen = \case
  T_Array d l -> (d, l)
  _ -> impossible "no array"

arrType :: DLType -> DLType
arrType = fst . arrTypeLen

showTys :: Show a => [a] -> String
showTys = List.intercalate ", " . map show

showTyMap :: Show a => M.Map SLVar a -> String
showTyMap = List.intercalate ", " . map showPair . M.toList
  where
    showPair (name, ty) = show name <> ": " <> show ty

showTyList :: Show a => [(SLVar, a)] -> String
showTyList = List.intercalate ", " . map showPair
  where
    showPair (name, ty) = "['" <> show name <> "', " <> show ty <> "]"

instance Show DLType where
  show T_Null = "Null"
  show T_Bool = "Bool"
  show T_UInt = "UInt"
  show (T_Bytes sz) = "Bytes(" <> show sz <> ")"
  show T_Digest = "Digest"
  show T_Address = "Address"
  show T_Token = "Token"
  show (T_Array ty i) = "Array(" <> show ty <> ", " <> show i <> ")"
  show (T_Tuple tys) = "Tuple(" <> showTys tys <> ")"
  show (T_Object tyMap) = "Object({" <> showTyMap tyMap <> "})"
  show (T_Data tyMap) = "Data({" <> showTyMap tyMap <> "})"
  show (T_Struct tys) = "Struct([" <> showTyList tys <> "])"

instance Pretty DLType where
  pretty = viaShow

-- Interact types can only be value types or first-order function types
data IType
  = IT_Val DLType
  | IT_Fun [DLType] DLType
  | IT_UDFun DLType
  deriving (Eq, Ord, Generic, Show)

itype2arr :: IType -> ([DLType], DLType)
itype2arr = \case
  IT_Val t -> ([], t)
  IT_Fun dom rng -> (dom, rng)
  IT_UDFun rng -> ([], rng)

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

type DLMapInfos = M.Map DLMVar DLMapInfo

data DLInit = DLInit
  { dli_ctimem :: Maybe DLVar
  , dli_maps :: DLMapInfos
  }
  deriving (Eq, Generic)

instance Pretty DLInit where
  pretty (DLInit {..}) =
    "// maps" <> hardline
      <> render_obj dli_maps
      <> hardline
      <> "// initialization"
      <> hardline
      <> ctimem'
    where
      ctimem' = case dli_ctimem of
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
    DLL_Bool b -> if b then "true" else "false"
    DLL_Int _ i -> viaShow i
    DLL_Bytes bs -> dquotes (viaShow bs)

litTypeOf :: DLLiteral -> DLType
litTypeOf = \case
  DLL_Null -> T_Null
  DLL_Bool _ -> T_Bool
  DLL_Int {} -> T_UInt
  DLL_Bytes bs -> T_Bytes $ fromIntegral $ B.length bs

data DLVar = DLVar SrcLoc (Maybe (SrcLoc, SLVar)) DLType Int
  deriving (Generic)

instance SrcLocOf DLVar where
  srclocOf (DLVar a _ _ _) = a

instance Eq DLVar where
  (DLVar _ _ _ x) == (DLVar _ _ _ y) = x == y

instance Ord DLVar where
  (DLVar _ _ _ x) <= (DLVar _ _ _ y) = x <= y

instance Pretty DLVar where
  pretty (DLVar _ _ _ i) = "v" <> viaShow i

instance Show DLVar where
  show (DLVar _ b _ i) =
    case b of
      Nothing -> "v" <> show i
      Just (_, v) -> v <> "/" <> show i

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
  , dlmi_at :: SrcLoc
  }
  deriving (Eq, Generic)

instance Pretty DLMapInfo where
  pretty (DLMapInfo {..}) = pretty dlmi_ty

dlmi_tym :: DLMapInfo -> DLType
dlmi_tym = maybeT . dlmi_ty

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

class CanDupe a where
  canDupe :: a -> Bool

instance CanDupe DLArg where
  canDupe = \case
    DLA_Var {} -> True
    DLA_Constant {} -> True
    DLA_Literal {} -> True
    DLA_Interact {} -> False

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
  | DLLA_Struct [(SLVar, DLArg)]
  deriving (Eq, Ord, Generic, Show)

instance Pretty DLLargeArg where
  pretty = \case
    DLLA_Array t as -> "array" <> parens (pretty t <> comma <+> pretty (DLLA_Tuple as))
    DLLA_Tuple as -> brackets $ render_das as
    DLLA_Obj env -> render_obj env
    DLLA_Data _ vn vv -> "<" <> pretty vn <> " " <> pretty vv <> ">"
    DLLA_Struct kvs -> "struct" <> brackets (render_das kvs)

mdaToMaybeLA :: DLType -> Maybe DLArg -> DLLargeArg
mdaToMaybeLA t = \case
  Nothing -> f "None" $ DLA_Literal $ DLL_Null
  Just a -> f "Some" a
  where
    f = DLLA_Data (dataTypeMap $ maybeT t)

data DLArgExpr
  = DLAE_Arg DLArg
  | DLAE_Array DLType [DLArgExpr]
  | DLAE_Tuple [DLArgExpr]
  | DLAE_Obj (M.Map SLVar DLArgExpr)
  | DLAE_Data (M.Map SLVar DLType) String DLArgExpr
  | DLAE_Struct [(SLVar, DLArgExpr)]

argExprToArgs :: DLArgExpr -> [DLArg]
argExprToArgs = \case
  DLAE_Arg a -> [a]
  DLAE_Array _ aes -> many aes
  DLAE_Tuple aes -> many aes
  DLAE_Obj m -> many $ M.elems m
  DLAE_Data _ _ ae -> one ae
  DLAE_Struct aes -> many $ map snd aes
  where
    one = argExprToArgs
    many = concatMap one

largeArgToArgExpr :: DLLargeArg -> DLArgExpr
largeArgToArgExpr = \case
  DLLA_Array sz as -> DLAE_Array sz $ map DLAE_Arg as
  DLLA_Tuple as -> DLAE_Tuple $ map DLAE_Arg as
  DLLA_Obj m -> DLAE_Obj $ M.map DLAE_Arg m
  DLLA_Data m v a -> DLAE_Data m v $ DLAE_Arg a
  DLLA_Struct kvs -> DLAE_Struct $ map (\(k, v) -> (,) k $ DLAE_Arg v) kvs

largeArgTypeOf :: DLLargeArg -> DLType
largeArgTypeOf = argExprTypeOf . largeArgToArgExpr

argExprTypeOf :: DLArgExpr -> DLType
argExprTypeOf = \case
  DLAE_Arg a -> argTypeOf a
  DLAE_Array t as -> T_Array t $ fromIntegral (length as)
  DLAE_Tuple as -> T_Tuple $ map argExprTypeOf as
  DLAE_Obj senv -> T_Object $ M.map argExprTypeOf senv
  DLAE_Data t _ _ -> T_Data t
  DLAE_Struct kvs -> T_Struct $ map (\(k, v) -> (,) k $ argExprTypeOf v) kvs

data ClaimType
  = --- Verified on all paths
    CT_Assert
  | --- Assume true in verification, but check at runtime
    --- Bool means it is internally generated, rather than specified by user
    CT_Assume Bool
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
    CT_Assume _ -> "assume"
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

data DLWithBill = DLWithBill
  { amts_recv :: DLVar
  , tok_billed :: [DLArg]
  , tok_not_billed :: [DLArg]
  }
  deriving (Eq, Ord, Show)

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
  | DLE_Transfer SrcLoc DLArg DLArg (Maybe DLArg)
  | DLE_TokenInit SrcLoc DLArg
  | DLE_CheckPay SrcLoc [SLCtxtFrame] DLArg (Maybe DLArg)
  | DLE_Wait SrcLoc DLArg
  | DLE_PartSet SrcLoc SLPart DLArg
  | DLE_MapRef SrcLoc DLMVar DLArg
  | DLE_MapSet SrcLoc DLMVar DLArg (Maybe DLArg)
  | DLE_Remote SrcLoc [SLCtxtFrame] DLArg String DLPayAmt [DLArg] DLWithBill
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
      DLE_Transfer _ who da mtok -> prettyTransfer who da mtok
      DLE_TokenInit _ tok -> "tokenInit" <> parens (pretty tok)
      DLE_CheckPay _ _ da mtok -> "checkPay" <> parens (pretty da <> ", " <> pretty mtok)
      DLE_Wait _ a -> "wait" <> parens (pretty a)
      DLE_PartSet _ who a -> render_sp who <> ".set" <> parens (pretty a)
      DLE_MapRef _ mv i -> pretty mv <> brackets (pretty i)
      DLE_MapSet _ mv kv (Just nv) ->
        pretty mv <> "[" <> pretty kv <> "]" <+> "=" <+> pretty nv
      DLE_MapSet _ mv i Nothing ->
        "delete" <+> pretty mv <> brackets (pretty i)
      DLE_Remote _ _ av m amta as (DLWithBill _ nonNetTokRecv _) ->
        "remote(" <> pretty av <> ")." <> viaShow m <> ".pay" <> parens (pretty amta)
          <> parens (render_das as)
          <> ".withBill"
          <> parens (render_das nonNetTokRecv)

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
    DLE_Claim {} ->
      -- These are all false, because we use purity to determine if we can
      -- reorder things and an assert can not be ordered outside of an IF to
      -- turn it into an ITE
      False
    DLE_Transfer {} -> False
    DLE_TokenInit {} -> False
    DLE_CheckPay {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> False
    DLE_MapRef {} -> True
    DLE_MapSet {} -> False
    DLE_Remote {} -> False

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
    DLE_TokenInit {} -> False
    DLE_CheckPay {} -> False
    DLE_Wait {} -> False
    DLE_PartSet {} -> True
    DLE_MapRef {} -> True
    DLE_MapSet {} -> False
    DLE_Remote {} -> False

instance CanDupe DLExpr where
  canDupe e =
    isPure e && x
    where
      x =
        case e of
          DLE_MapRef {} -> False
          DLE_Remote {} -> False
          _ -> True

newtype DLAssignment
  = DLAssignment (M.Map DLVar DLArg)
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance Pretty DLAssignment where
  pretty (DLAssignment m) = render_obj m

assignment_vars :: DLAssignment -> [DLVar]
assignment_vars (DLAssignment m) = M.keys m

data DLVarCat
  = DVC_Once
  | DVC_Many
  deriving (Eq, Show)

instance Semigroup DLVarCat where
  _ <> _ = DVC_Many

instance Pretty DLVarCat where
  pretty = \case
    DVC_Many -> "*"
    DVC_Once -> "!"

data DLLetVar
  = DLV_Eff
  | DLV_Let DLVarCat DLVar
  deriving (Eq, Show)

instance Pretty DLLetVar where
  pretty = \case
    DLV_Eff -> "eff"
    DLV_Let lc x -> pretty x <> pretty lc

lv2mdv :: DLLetVar -> Maybe DLVar
lv2mdv = \case
  DLV_Eff -> Nothing
  DLV_Let _ v -> Just v

type SwitchCases a =
  --- FIXME at the SrcLoc of the case
  M.Map SLVar (Maybe DLVar, a)

data DLStmt
  = DL_Nop SrcLoc
  | DL_Let SrcLoc DLLetVar DLExpr
  | DL_ArrayMap SrcLoc DLVar DLArg DLVar DLBlock
  | DL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar DLBlock
  | DL_Var SrcLoc DLVar
  | DL_Set SrcLoc DLVar DLArg
  | DL_LocalDo SrcLoc DLTail
  | DL_LocalIf SrcLoc DLArg DLTail DLTail
  | DL_LocalSwitch SrcLoc DLVar (SwitchCases DLTail)
  | DL_Only SrcLoc (Either SLPart Bool) DLTail
  | DL_MapReduce SrcLoc Int DLVar DLMVar DLArg DLVar DLVar DLBlock
  deriving (Eq)

instance SrcLocOf DLStmt where
  srclocOf = \case
    DL_Nop a -> a
    DL_Let a _ _ -> a
    DL_ArrayMap a _ _ _ _ -> a
    DL_ArrayReduce a _ _ _ _ _ _ -> a
    DL_Var a _ -> a
    DL_Set a _ _ -> a
    DL_LocalDo a _ -> a
    DL_LocalIf a _ _ _ -> a
    DL_LocalSwitch a _ _ -> a
    DL_Only a _ _ -> a
    DL_MapReduce a _ _ _ _ _ _ _ -> a

instance Pretty DLStmt where
  pretty = \case
    DL_Nop _ -> mempty
    DL_Let _ DLV_Eff de -> pretty de <> semi
    DL_Let _ x de -> "const" <+> pretty x <+> "=" <+> pretty de <> semi
    DL_ArrayMap _ ans x a f -> prettyMap ans x a f
    DL_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
    DL_Var _at dv -> "let" <+> pretty dv <> semi
    DL_Set _at dv da -> pretty dv <+> "=" <+> pretty da <> semi
    DL_LocalDo _at k -> "do" <+> braces (pretty k) <> semi
    DL_LocalIf _at ca t f -> prettyIfp ca t f
    DL_LocalSwitch _at ov csm -> prettySwitch ov csm
    DL_Only _at who b -> prettyOnly who b
    DL_MapReduce _ _mri ans x z b a f -> prettyReduce ans x z b a f

mkCom :: (DLStmt -> k -> k) -> DLStmt -> k -> k
mkCom mk m k =
  case m of
    DL_Nop _ -> k
    DL_LocalDo _ k' ->
      dtReplace mk k k'
    _ -> mk m k

data DLTail
  = DT_Return SrcLoc
  | DT_Com DLStmt DLTail
  deriving (Eq)

instance Pretty DLTail where
  pretty = \case
    DT_Return _at -> mempty
    DT_Com x k -> prettyCom x k

dtReplace :: (DLStmt -> b -> b) -> b -> DLTail -> b
dtReplace mkk nk = \case
  DT_Return _ -> nk
  DT_Com m k -> (mkCom mkk) m $ dtReplace mkk nk k

data DLBlock
  = DLBlock SrcLoc [SLCtxtFrame] DLTail DLArg
  deriving (Eq)

instance Pretty DLBlock where
  pretty (DLBlock _ _ ts ta) = prettyBlockP ts ta

data DLinExportBlock a
  = DLinExportBlock SrcLoc (Maybe [DLVar]) a
  deriving (Eq)

instance SrcLocOf (DLinExportBlock a) where
  srclocOf = \case
    DLinExportBlock a _ _ -> a

instance Pretty a => Pretty (DLinExportBlock a) where
  pretty = \case
    DLinExportBlock _ args b ->
      "export" <+> parens (pretty args) <+> "=>" <+> braces (pretty b)

dlebEnsureFun :: DLinExportBlock a -> DLinExportBlock a
dlebEnsureFun (DLinExportBlock at mvs a) =
  DLinExportBlock at (Just $ fromMaybe [] mvs) a

type DLExportBlock = DLinExportBlock DLBlock

type DLExports = M.Map SLVar DLExportBlock

data DLPayAmt = DLPayAmt
  { pa_net :: DLArg
  , pa_ks :: [(DLArg, DLArg)]
  }
  deriving (Eq, Generic, Ord)

instance Pretty DLPayAmt where
  pretty (DLPayAmt {..}) =
    brackets $ pretty pa_net <> ", " <> render_das pa_ks

data DLSend = DLSend
  { ds_isClass :: Bool
  , ds_msg :: [DLArg]
  , ds_pay :: DLPayAmt
  , ds_when :: DLArg
  }
  deriving (Eq, Generic)

instance Pretty DLSend where
  pretty (DLSend {..}) =
    ".send"
      <> parens
        (render_obj $
           M.fromList $
             [ ("isClass" :: String, pretty ds_isClass)
             , ("msg", pretty ds_msg)
             , ("pay", pretty ds_pay)
             , ("when", pretty ds_when)
             ])

data DLRecv a = DLRecv
  { dr_from :: DLVar
  , dr_msg :: [DLVar]
  , dr_time :: DLVar
  , dr_k :: a
  }
  deriving (Eq, Generic)

instance Pretty a => Pretty (DLRecv a) where
  pretty (DLRecv {..}) =
    ".recv"
      <> parens
        (render_obj $
           M.fromList $
             [ ("from" :: String, pretty dr_from)
             , ("msg", pretty dr_msg)
             , ("time", pretty dr_time)
             ])
      <> render_nest (pretty dr_k)

data FluidVar
  = FV_balance Int
  | FV_thisConsensusTime
  | FV_lastConsensusTime
  deriving (Eq, Generic, Ord, Show)

instance Pretty FluidVar where
  pretty = \case
    FV_balance i -> "balance" <> parens (pretty i)
    FV_thisConsensusTime -> "thisConsensusTime"
    FV_lastConsensusTime -> "lastConsensusTime"

fluidVarType :: FluidVar -> DLType
fluidVarType = \case
  FV_balance _ -> T_UInt
  FV_thisConsensusTime -> T_UInt
  FV_lastConsensusTime -> T_UInt

allFluidVars :: Int -> [FluidVar]
allFluidVars bals =
  [ FV_thisConsensusTime
  , FV_lastConsensusTime
  ]
    <> map FV_balance [0 .. (bals + 1)]

class HasCounter a where
  getCounter :: a -> Counter

type DLViews = M.Map SLPart (M.Map SLVar IType)
