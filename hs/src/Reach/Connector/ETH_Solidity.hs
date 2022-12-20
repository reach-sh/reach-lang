{-# LANGUAGE QuasiQuotes #-}

module Reach.Connector.ETH_Solidity (connect_eth) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ETH_EVM.hs.dead

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Crypto.Hash (hash, SHA1)
import Data.Aeson as Aeson
import qualified Data.Aeson as AS
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.IORef
import Data.List (intersperse)
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.CL
import Reach.CLike
import Reach.Connector
import Reach.Connector.ETH_solc
import Reach.Counter
import Reach.EmbeddedFiles
import Reach.InterferenceGraph
import Reach.OutputUtil
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version
import System.FilePath
import Text.Printf
import Safe.Foldable (maximumMay)

--- Debugging tools

--- You can turn this to True and manually change the Solidity file
dontWriteSol :: Bool
dontWriteSol = False

maxDepth :: Int
maxDepth = 12

apiMaxArgs :: Int
apiMaxArgs = 12

reachEthBackendVersion :: Int
reachEthBackendVersion = 9

contractId :: String
contractId = "ReachContract"

--- Solidity errors

data EthError
  = Err_SolTooManyArgs String String Int
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions, Generic)

instance HasErrorCode EthError where
  errPrefix = const "RETH"
  errIndex = \case
    Err_SolTooManyArgs {} -> 0

instance Show EthError where
  show = \case
    Err_SolTooManyArgs viewsOrApis name num ->
      "The ETH connector supports " <> viewsOrApis <> " that have up to " <>
      show apiMaxArgs <> " arguments, but " <> name <> " uses " <> show num <> "."

--- Solidity helpers

solString :: String -> Doc
solString s = squotes $ pretty s

solNum :: Show n => n -> Doc
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Docs -> Doc
solBraces body = braces (nest $ hardline <> vsep body)

data SolFunctionLike
  = SFLCtor
  | SFLFun Bool Bool Doc (Maybe Doc)

solFunctionLike :: SolFunctionLike -> [Doc] -> Docs -> Docs
solFunctionLike sfl args body = [ sflp <+> solBraces body ]
  where
    sflp =
      case sfl of
        SFLCtor -> solApply "constructor" args <+> "payable"
        SFLFun ext mut name mret ->
          "function" <+> solApply name args <+> ext' <+> mut' <> ret'
          where
            ret' = case mret of
                     Nothing -> ""
                     Just ret -> " returns" <+> parens ret
            ext' = if ext then "external" else "internal"
            mut' = if mut then (if ext then "payable" else "") else "view"

solFunction :: Doc -> [Doc] -> Maybe Doc -> Docs -> Docs
solFunction name args mret body =
  solFunctionLike (SFLFun False True name mret) args body

solVersion :: Docs
solVersion = ["pragma solidity ^" <> pretty solcVersionStr <> ";"]

solStdLib :: Docs
solStdLib = [pretty $ B.unpack stdlib_sol]

solCommas :: [Doc] -> Doc
solCommas args = hcat $ intersperse (comma <> space) args

solApply :: Doc -> [Doc] -> Doc
solApply f args = f <> parens (solCommas args)

solRequireMsg :: String -> App Doc
solRequireMsg umsg = do
  let smsg = unsafeRedactAbsStr umsg
  idx <- (liftIO . incCounter) =<< (ctxt_requireMsg <$> ask)
  -- XXX save the full string in a dictionary and emit it
  return $ solNum idx <+> "/*" <> solString smsg <> "*/"

solRequire :: String -> Doc -> App Doc
solRequire umsg a = do
  mmsg <- solRequireMsg umsg
  return $ solApply "reachRequire" $ [parens a, mmsg]

solRequireS :: String -> Doc -> App Docs
solRequireS x y = do
  z <- solRequire x y
  return [ z <> semi ]

solBinOp :: String -> Doc -> Doc -> Doc
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc -> Doc -> App Doc
solEq x y = solPrimApply (PEQ UI_Word) [x, y]

solSet :: Doc -> Doc -> Doc
solSet x y = solBinOp "=" x y <> semi

solWhen :: Doc -> Docs -> Doc
solWhen c t = "if" <+> parens c <+> solBraces t

solIf_ :: Doc -> Docs -> Docs -> Docs
solIf_ c t f = [ solWhen c t <> hardline <> "else" <+> solBraces f ]

solIf :: (SolFrag a, SolStmts k) => a -> k -> k -> App Docs
solIf c t f = solIf_ <$> solF c <*> solS t <*> solS f

solIfs_ :: [(Doc, Docs)] -> Docs
solIfs_ = \case
  [] -> []
  ((c, t) : more) ->
    [ "if" <+> parens c <+> "{" ] <> t <>
    case solIfs_ more of
      [] -> [ "}" ]
      rec_hd : rec_tl -> [ "} else " <> rec_hd ] <> rec_tl

solDecl :: Doc -> Doc -> Doc
solDecl n ty = ty <+> n

solStruct :: Doc -> [(Doc, Doc)] -> Maybe Docs
solStruct name = \case
  [] -> Nothing
  fields -> Just $ ["struct" <+> name <+> solBraces (map (<> semi) $ map (uncurry solDecl) fields)]

solEnum :: Doc -> [Doc] -> Docs
solEnum name opts = ["enum" <+> name <+> braces (hcat $ intersperse (comma <> space) opts)]

reachPre :: Doc
reachPre = "_reach_"

solOutput_evt :: DLVar -> Doc
solOutput_evt dv = reachPre <> "oe_" <> solRawVar dv

solMsg_evt :: Pretty i => i -> Doc
solMsg_evt i = reachPre <> "e" <> pretty i

solMapVar :: DLMVar -> Doc
solMapVar (DLMVar i) = pclv $ nameMap i

solMapRefExt_ :: Doc -> Doc
solMapRefExt_ x = x <> "Ref"

solMapRefInt :: DLMVar -> Doc
solMapRefInt = solMapRefInt_ . solMapVar

solMapRefInt_ :: Doc -> Doc
solMapRefInt_ x = "_" <> solMapRefExt_ x

solBlockTime :: Doc
solBlockTime = "uint256(block.number)"

solBlockSecs :: Doc
solBlockSecs = "uint256(block.timestamp)"

solEncode :: [Doc] -> Doc
solEncode = solApply "abi.encode"

solHash :: [Doc] -> Doc
solHash a = solApply "uint256" [solApply "keccak256" [solEncode a]]

solArraySet :: Int -> Doc
solArraySet i = "array_set" <> pretty i

solArrayRef :: Doc -> Doc -> Doc
solArrayRef arr idx = arr <> brackets idx

solVariant :: Doc -> SLVar -> Doc
solVariant t vn = "_enum_" <> t <> "." <> pretty vn

solRawVar :: DLVar -> Doc
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc
solMemVar dv = "_f." <> solRawVar dv

memVar :: Doc
memVar = "_Memory"

memVarF :: String -> Doc
memVarF f = memVar <> "." <> pretty f

memTy :: Doc
memTy = "Memory"

memVarDecl :: Doc
memVarDecl = solDecl ("memory" <+> memVar) memTy

solSVSVar :: DLVar -> Doc
solSVSVar dv = "_svs." <> solRawVar dv

vsToType :: [DLVar] -> DLType
vsToType vs = T_Struct $ map go_ty vs
  where
    go_ty v = (show (solRawVar v), varType v)

objPrefix :: (Semigroup a, IsString a) => a -> a
objPrefix = ("_" <>)

--- Compiler

type VarMap = M.Map DLVar Doc

type AesonRib = [(T.Text, Aeson.Value)]

data SolCtxt = SolCtxt
  { ctxt_varm :: IORef VarMap
  , ctxt_mvars :: IORef (S.Set DLVar)
  , ctxt_depths :: IORef (M.Map DLVar Int)
  , ctxt_typei :: IORef (M.Map DLType Int)
  , ctxt_typem :: IORef (M.Map DLType Doc)
  , ctxt_typed :: IORef (M.Map Int Docs)
  , ctxt_typef :: IORef (M.Map Int Docs)
  , ctxt_typeidx :: Counter
  , ctxt_varidx :: Counter
  , ctxt_ints :: IORef (M.Map String Docs)
  , ctxt_outputs :: IORef (M.Map String Docs)
  , ctxt_requireMsg :: Counter
  , ctxt_which_msg :: IORef (M.Map Int [DLVar])
  , ctxt_view_json :: IORef AesonRib
  }

instance HasCounter SolCtxt where
  getCounter = ctxt_varidx

readCtxtIO :: (SolCtxt -> IORef a) -> App a
readCtxtIO f = (liftIO . readIORef) =<< (f <$> ask)

modifyCtxtIO :: (SolCtxt -> IORef a) -> (a -> a) -> App ()
modifyCtxtIO f m = (liftIO . (flip modifyIORef m)) =<< (f <$> ask)

type App = ReaderT SolCtxt IO

type Docs = [Doc]

class SolStmts a where
  solS :: a -> App Docs

instance (SolStmts a) => SolStmts [a] where
  solS = concatMapM solS

class SolFrag a where
  solF :: a -> App Doc

addInterface :: Doc -> [Doc] -> Doc -> App Doc
addInterface f dom rng = do
  let body = "function" <+> solApply f dom <+> "external payable returns" <+> parens rng <> semi
  let body' = show $ hash @BS.ByteString @SHA1 $ bpack $ show body
  let ip = "I" <> body'
  let ip' = pretty ip
  let idef = ["interface" <+> ip' <+> solBraces [body]]
  modifyCtxtIO ctxt_ints $ M.insert ip idef
  return $ ip' <> "." <> f <> ".selector"

allocRawVar :: App Doc
allocRawVar = (pretty . (++) "v" . show) <$> allocVarIdx

extendVarMap :: VarMap -> App ()
extendVarMap vm1 = do
  varmr <- ctxt_varm <$> ask
  liftIO $ modifyIORef varmr $ (<>) vm1

addVar :: DLVar -> Doc -> App ()
addVar v x = extendVarMap $ M.singleton v x

addVar' :: DLVar -> App Doc
addVar' v = do
  let v' = solRawVar v
  addVar v v'
  return v'

addVars_ :: (DLVar -> Doc) -> [DLVar] -> App ()
addVars_ f l = extendVarMap $ M.fromList $ map (\v -> (v, f v)) l

addVars :: (DLVar -> Doc) -> [DLVarLet] -> App ()
addVars f = addVars_ f . map varLetVar

freshVarMap :: App a -> App a
freshVarMap m = do
  let d f = liftIO . dupeIORef . f =<< ask
  varmr' <- d ctxt_varm
  mvars' <- d ctxt_mvars
  depths' <- d ctxt_depths
  local
    (\e ->
       e
         { ctxt_varm = varmr'
         , ctxt_mvars = mvars'
         , ctxt_depths = depths'
         })
    m

readMemVars :: App (S.Set DLVar)
readMemVars = readCtxtIO ctxt_mvars

addMemVar :: DLVar -> App ()
addMemVar v = do
  addVar v $ solMemVar v
  mvars <- ctxt_mvars <$> ask
  liftIO $ modifyIORef mvars $ S.insert v

allocMemVar :: SrcLoc -> DLType -> App Doc
allocMemVar at t = do
  dv <- allocVar at t
  addMemVar dv
  return $ solMemVar dv

addMemVars :: [DLVar] -> App ()
addMemVars = mapM_ addMemVar

recordDepth :: DLVar -> Int -> App ()
recordDepth v d = modifyCtxtIO ctxt_depths $ M.insert v d

readDepth :: DLVar -> App Int
readDepth v = do
  mvars <- readCtxtIO ctxt_depths
  return $ fromMaybe 0 $ M.lookup v mvars

uint8ArrayToString :: Integer -> Doc -> App Doc
uint8ArrayToString len x = do
  let xs =
        case len > byteChunkSize of
          True ->
            solApply "bytes.concat" $
              solBytesSplit len (const $ impossible "uint8ArrayToString") $
                \ i -> const $ x <> ".elem" <> pretty i
          False -> solApply "abi.encodePacked" [x]
  return $ solApply "string" [xs]

class DepthOf a where
  depthOf :: a -> App Int

instance (Traversable t, DepthOf a) => DepthOf (t a) where
  depthOf o = (fromMaybe 0 . maximumMay) <$> mapM depthOf o

instance {-# OVERLAPS #-} (DepthOf a, DepthOf b) => DepthOf (a, b) where
  depthOf (x, y) = max <$> depthOf x <*> depthOf y

instance DepthOf DLVar where
  depthOf = readDepth

instance DepthOf DLArg where
  depthOf = \case
    DLA_Var v -> depthOf v
    DLA_Constant {} -> return 0
    DLA_Literal {} -> return 0
    DLA_Interact {} -> return 0

instance DepthOf DLLargeArg where
  depthOf = \case
    DLLA_Array _ as -> depthOf as
    DLLA_Tuple as -> depthOf as
    DLLA_Obj m -> depthOf m
    DLLA_Data _ _ x -> depthOf x
    DLLA_Struct kvs -> depthOf $ map snd kvs
    DLLA_Bytes {} -> return 1
    DLLA_BytesDyn {} -> return 1
    DLLA_StringDyn {} -> return 1

instance DepthOf DLTokenNew where
  depthOf (DLTokenNew {..}) =
    depthOf [dtn_name, dtn_sym, dtn_url, dtn_metadata, dtn_supply]

instance DepthOf DLContractNew where
  depthOf (DLContractNew {}) = return 1

instance DepthOf DLRemote where
  depthOf (DLRemote _ (DLPayAmt net ks) as (DLWithBill _ nr nz) _) =
    add1 $
      depthOf $ net : pairList ks <> as <> nr <> nz
    where
      add1 = addN 1
      addN n m = (+) n <$> m
      pairList = concatMap (\(a, b) -> [a, b])

instance DepthOf PrimOp where
  depthOf = \case
    ADD {} -> return 1
    SUB {} -> return 1
    MUL {} -> return 1
    DIV {} -> return 1
    MOD {} -> return 1
    _ -> return 0

instance DepthOf DLExpr where
  depthOf = \case
    DLE_Arg _ a -> depthOf a
    DLE_LArg _ a -> depthOf a
    DLE_Impossible {} -> return 0
    DLE_VerifyMuldiv {} -> return 0
    DLE_PrimOp _ op as -> add1 $ max <$> depthOf as <*> depthOf op
    DLE_ArrayRef _ x y -> add1 $ depthOf [x, y]
    DLE_ArraySet _ x y z -> depthOf [x, y, z]
    DLE_ArrayConcat _ x y -> add1 $ depthOf [x, y]
    DLE_BytesDynCast _ x -> add1 $ depthOf x
    DLE_TupleRef _ x _ -> add1 $ depthOf x
    DLE_TupleSet _ t _ v -> add1 $ depthOf [t, v]
    DLE_ObjectRef _ x _ -> add1 $ depthOf x
    DLE_ObjectSet _ a _ b -> add1 $ depthOf [a, b]
    DLE_Interact _ _ _ _ _ as -> depthOf as
    DLE_Digest _ as -> add1 $ depthOf as
    DLE_Claim _ _ _ a _ -> depthOf a
    DLE_Transfer _ x y z -> max <$> depthOf [x, y] <*> depthOf z
    DLE_TokenInit _ x -> depthOf x
    DLE_TokenAccepted _ x y -> max <$> depthOf x <*> depthOf y
    DLE_CheckPay _ _ y z -> max <$> depthOf y <*> depthOf z
    DLE_Wait _ x -> depthOf x
    DLE_PartSet _ _ x -> depthOf x
    DLE_MapRef _ _ x _ -> add1 $ depthOf x
    DLE_MapSet _ _ x _ y -> max <$> depthOf x <*> depthOf y
    DLE_Remote _ _ av _ dr ->
      add1 $ max <$> depthOf av <*> depthOf dr
    DLE_TokenNew _ tns -> add1 $ depthOf tns
    DLE_TokenBurn _ t a -> add1 $ depthOf [t, a]
    DLE_TokenDestroy _ t -> add1 $ depthOf t
    DLE_TimeOrder {} -> impossible "timeorder"
    DLE_EmitLog _ _ a -> add1 $ depthOf a
    DLE_setApiDetails {} -> return 0
    DLE_GetUntrackedFunds _ mt tb -> max <$> depthOf mt <*> depthOf tb
    DLE_DataTag _ d -> add1 $ depthOf d
    DLE_FromSome _ mo da -> add1 $ depthOf [mo, da]
    DLE_ContractNew _ cns dr -> add1 $ max <$> depthOf cns <*> depthOf dr
    DLE_ContractFromAddress _ a -> add1 $ depthOf a
    where
      add1 = addN 1
      addN n m = (+) n <$> m

instance SolFrag DLVar where
  solF v@(DLVar _ _ _ i) = do
    varm <- readCtxtIO ctxt_varm
    case M.lookup v varm of
      Just x -> return $ x
      Nothing -> return $ "_unbound_v" <> pretty i

mkSolType :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a Doc)) -> a -> App Doc
mkSolType ensure f t = do
  ensure t
  (fromMaybe (impossible "solType") . M.lookup t) <$> readCtxtIO f

solType_ :: DLType -> App Doc
solType_ = mkSolType ensureTypeDefined ctxt_typem

solType :: DLType -> App Doc
solType t = do
  t' <- solType_ t
  case t' == "address" of
    True -> return $ t' <+> "payable"
    False -> return $ t'

solTypeI :: DLType -> App Int
solTypeI t = do
  ensureTypeDefined t
  (fromMaybe (impossible "solTypeI") . M.lookup t) <$> readCtxtIO ctxt_typei

mustBeMem :: DLType -> Bool
mustBeMem = \case
  T_Null -> False
  T_Bool -> False
  T_UInt _ -> False
  T_Bytes sz -> sz > byteChunkSize
  T_BytesDyn -> True
  T_StringDyn -> True
  T_Digest -> False
  T_Address -> False
  T_Contract -> False
  T_Token -> False
  T_Array _ sz -> sz /= 0
  T_Tuple l -> not $ null l
  T_Object m -> not $ M.null m
  T_Data {} -> True
  T_Struct l -> not $ null l

mayMemSol :: Doc -> Doc
mayMemSol x =
  case x' of
    "bool" -> no
    _ -> yes
  where
    x' = show x
    yes = x <> " memory"
    no = x

data ArgMode
  = AM_Call
  | AM_Memory
  | AM_Event

instance SolFrag ArgMode where
  solF = \case
    AM_Call -> return $ " calldata"
    AM_Memory -> return $ " memory"
    AM_Event -> return $ ""

instance SolFrag DLLiteral where
  solF = \case
    DLL_Null -> return $ "false"
    DLL_Bool True -> return $ "true"
    DLL_Bool False -> return $ "false"
    DLL_Int at _ i -> return $ solNum $ checkIntLiteralC at conName' conCons' i
    DLL_TokenZero -> return $ "payable(address(0))"

instance SolFrag DLArg where
  solF = \case
    DLA_Var v -> solF v
    DLA_Constant c -> solF $ conCons' c
    DLA_Literal c -> solF c
    DLA_Interact {} -> impossible "consensus interact"

solPrimApply :: PrimOp -> [Doc] -> App Doc
solPrimApply = \case
  SELF_ADDRESS {} -> impossible "self address"
  ADD _ pv -> safeOp pv "unsafeAdd" "safeAdd"
  SUB _ pv -> safeOp pv "unsafeSub" "safeSub"
  MUL _ pv -> safeOp pv "unsafeMul" "safeMul"
  DIV _ pv -> safeOp pv "unsafeDiv" "safeDiv"
  MOD _ pv -> safeOp pv "unsafeMod" "safeMod"
  PLT _ -> binOp "<"
  PLE _ -> binOp "<="
  PEQ _ -> binOp "=="
  PGE _ -> binOp ">="
  PGT _ -> binOp ">"
  SQRT _ -> app "safeSqrt"
  UCAST _ _ _ _ -> \case
    [x] -> return x
    _ -> impossible "ucast"
  MUL_DIV pv -> \case
    [x, y, den] -> do
      mul <- safeOp pv "unsafeMul" "safeMul" [x, y]
      binOp "/" [mul, den]
    _ -> impossible "solPrimApply: MUL_DIV args"
  LSH -> binOp "<<"
  RSH -> binOp ">>"
  BAND _ -> binOp "&"
  BIOR _ -> binOp "|"
  BXOR _ -> binOp "^"
  DIGEST_XOR -> binOp "^"
  BYTES_XOR -> binOp "^"
  IF_THEN_ELSE -> \case
    -- XXX Copy the simplifications from ALGO.hs
    [c, t, f] -> return $ c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitSol: ITE wrong args"
  DIGEST_EQ -> binOp "=="
  ADDRESS_EQ -> binOp "=="
  TOKEN_EQ -> binOp "=="
  BYTES_ZPAD {} -> impossible "bytes concat"
  BTOI_LAST8 {} -> impossible "btoiLast8"
  STRINGDYN_CONCAT -> app "string.concat"
  UINT_TO_STRINGDYN _ -> app "uintToStringDyn"
  CTC_ADDR_EQ -> binOp "=="
  GET_CONTRACT -> constr "payable(address(this))"
  GET_ADDRESS -> constr "payable(address(this))"
  GET_COMPANION -> impossible "GET_COMPANION"
  ALGO_BLOCK _ -> impossible "ALGO_BLOCK"
  where
    app f args = return $ solApply f args
    constr = const . return
    safeOp pv veriFun safeFun args = do
      return $ flip solApply args $
        case pv of
          PV_Safe -> safeFun
          _ -> veriFun
    binOp op = \case
      [l, r] -> return $ solBinOp op l r
      _ -> impossible $ "emitSol: bin op args"

-- Sol cannot copy struct array to storage
canAssignToStorage :: DLType -> Bool
canAssignToStorage = \case
  T_Array dt _ -> not $ mustBeMem dt
  T_Tuple dts -> all (canAssignToStorage) dts
  T_Object m -> all (canAssignToStorage . snd) $ M.toList m
  T_Data m -> all (canAssignToStorage . snd) $ M.toList m
  T_Struct m -> all (canAssignToStorage . snd) m
  T_Bytes _ -> True
  T_BytesDyn -> True
  T_StringDyn -> True
  T_Null {} -> True
  T_Bool {} -> True
  T_UInt {} -> True
  T_Digest -> True
  T_Address -> True
  T_Contract -> True
  T_Token -> True

-- Assigns `x = y;` using `x.f = y.f; ...` pattern, if necessary
asnArg' :: Bool -> Doc -> DLType -> Doc -> App Docs
asnArg' usesStorage dv dt av = do
  case usesStorage && not (canAssignToStorage dt) of
    True -> do
      let go (field, ty, access) = do
            let lhs = dv <> access field
            let rhs = av <> access field
            asnArg' usesStorage lhs ty rhs
      let asnMapLike field_x_types = do
            map (\(f, ty) -> ( pretty f, ty, ("." <>) )) field_x_types
      let fieldAssigns = case dt of
            T_Object m -> asnMapLike $ map (first objPrefix) $ M.toList m
            T_Data m   -> asnMapLike $ M.toList m
            T_Struct m -> asnMapLike m
            T_Array t i -> do
              let go' i' = (pretty i', t, brackets)
              map go' [ 0 .. i - 1 ]
            T_Bytes i -> do
              let (howMany, lastLen) = solBytesInfo i
              let arrOfLengths = take (fromIntegral $ howMany - 1) (repeat byteChunkSize) <> [lastLen]
              let go' (i', sz) = (pretty i', T_Bytes sz, brackets)
              map go' $ zip [0 :: Int ..] arrOfLengths
            _ -> []
      concatMapM go fieldAssigns
    False ->
      return $ [ dv <+> "=" <+> av <> semi ]

asnArg :: Bool -> Doc -> DLArg -> App Docs
asnArg usesStorage dv a = do
  let dt = argTypeOf a
  av <- solF a
  asnArg' usesStorage dv dt av

concatZipWithM :: (Monad m) => (a -> b -> m [c]) -> [a] -> [b] -> m [c]
concatZipWithM f as bs = concatMapM (uncurry f) $ zip as bs

solLargeArg' :: Bool -> Doc -> DLLargeArg -> App Docs
solLargeArg' usesStorage dv la =
  case la of
    DLLA_Array _ as -> concatZipWithM go ([0 ..] :: [Int]) as
      where
        go i a = do
          let name = dv <> "[" <> pretty i <> "]"
          asnArg usesStorage name a
    DLLA_Tuple as -> concatZipWithM go ([0 ..] :: [Int]) as
      where
        go i a = do
          let name = dv <> ".elem" <> pretty i
          asnArg usesStorage name a
    DLLA_Obj m ->
      solLargeArg' usesStorage dv $ DLLA_Struct $ map (first objPrefix) $ M.toAscList m
    DLLA_Data _ vn vv -> do
      t <- solType $ largeArgTypeOf la
      asnFields <- asnArg usesStorage (dv <> "._" <> pretty vn) vv
      return $ one ".which" (solVariant t vn) <> asnFields
    DLLA_Struct kvs -> concatMapM go kvs
      where
        go (k, a) = do
          let name = dv <> "." <> pretty k
          asnArg usesStorage name a
    DLLA_Bytes s -> doBytes s
    DLLA_BytesDyn s -> doBytes s
    DLLA_StringDyn s ->
      return $ one "" $ solString $ T.unpack s
  where
    doBytes s = do
      let g3 :: Char -> String -> String
          g3 a b = (printf "%02x" a) <> b
      let g2 x = "hex" <> solString (B.foldr g3 "" x)
      let bcs = fromIntegral byteChunkSize
      case B.length s <= bcs of
        True -> do
          return $ one "" (g2 s)
        False -> do
          let chunks :: Int -> B.ByteString -> [B.ByteString]
              chunks n xs =
                case B.length xs > n of
                  False -> [xs]
                  True -> ys : chunks n zs
                    where
                      (ys, zs) = B.splitAt n xs
          let cs = chunks bcs s
          let go i x = return $ one (".elem" <> pretty i) (g2 x)
          concatZipWithM go ([0 ..] :: [Int]) cs
    one :: Doc -> Doc -> Docs
    one f v = [dv <> f <+> "=" <+> v <> semi]

solLargeArg :: Bool -> DLVar -> DLLargeArg -> App Docs
solLargeArg usesStorage dv la = flip (solLargeArg' usesStorage) la =<< solF dv

mapRefArg :: DLArg -> App Doc
mapRefArg a = do
  let kt = argTypeOf a
  a' <- solF a
  case M.lookup kt baseTypes of
    Just _ -> return $ a'
    Nothing -> return $ solHash [a']

solExpr :: Doc -> DLExpr -> App Doc
solExpr sp = \case
  DLE_Arg _ a -> spa $ solF a
  DLE_LArg {} ->
    impossible "large arg"
  DLE_Impossible at _ (Err_Impossible_Case s) ->
    impossible $ "solExpr: impossible case `" <> s <> "` encountered at: " <> show at
  DLE_Impossible at _ err ->
    expect_thrown at err
  DLE_VerifyMuldiv at _ _ _ err ->
    expect_thrown at err
  DLE_PrimOp _ p args -> do
    args' <- mapM solF args
    spa $ solPrimApply p args'
  DLE_ArrayRef _ ae ie ->
    spa $ (solArrayRef <$> solF ae <*> solF ie)
  DLE_ArraySet _ ae ie ve -> do
    args' <- mapM solF [ae, ie, ve]
    ti <- solTypeI (argTypeOf ae)
    spa $ return $ solApply (solArraySet ti) args'
  DLE_ArrayConcat {} ->
    impossible "array concat"
  DLE_BytesDynCast _ ae -> do
    ae' <- solF ae
    return $ "bytes.concat" <> parens ae'
  DLE_TupleRef _ ae i -> do
    ae' <- solF ae
    return $ ae' <> ".elem" <> pretty i <> sp
  DLE_TupleSet _ tup_a index val_a -> do
    let tupFields = tupleTypes $ argTypeOf tup_a
    let tupLen = fromIntegral $ length tupFields
    tup_t <- solType $ argTypeOf tup_a
    tup' <- solF tup_a
    val' <- solF val_a
    let newField = "elem" <> pretty index <> ": " <> val'
    let copiedFields = map (\n -> "elem" <> n <> ": " <> tup' <> "." <> "elem" <> n) $
                         map pretty $ filter (/= index) [0..tupLen-1]
    let tupLiteral = braces $ comma_sep $ newField : copiedFields
    return $ tup_t <> parens tupLiteral
  DLE_ObjectRef _ oe f -> do
    oe' <- solF oe
    let p = case argTypeOf oe of
          T_Struct {} -> id
          T_Object {} -> objPrefix
          _ -> impossible "objectref"
    return $ oe' <> "." <> p (pretty f) <> sp
  DLE_ObjectSet _ obj_a fieldName val_a -> do
    let objFields = M.fromList $ argObjstrTypes obj_a
    obj_t <- solType $ argTypeOf obj_a
    obj' <- solF obj_a
    val' <- solF val_a
    let newField = objPrefix $ pretty fieldName <> ": " <> val'
    let copiedFields = map (\fn -> objPrefix $ fn <> ": " <> obj' <> "." <> (objPrefix fn)) $
                         map (pretty . fst) $ M.toList $ M.delete fieldName objFields
    let objLiteral = braces $ comma_sep $ newField : copiedFields
    return $ obj_t <> parens objLiteral
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> do
    args' <- mapM solF args
    return $ (solHash $ args') <> sp
  DLE_Claim at fs ct a mmsg -> spa check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Enforce -> require
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = solRequire (show (at, fs, mmsg)) =<< solF a
  DLE_Transfer _ who amt mtok ->
    spa $ solTransfer who amt mtok
  DLE_TokenInit {} -> return emptyDoc
  DLE_TokenAccepted _ _ _ -> do
    return "(true)"
  DLE_CheckPay at fs amt mtok -> do
    let require :: String -> Doc -> App Doc
        require msg e = spa $ solRequire (show (at, fs, msg)) e
    amt' <- solF amt
    case mtok of
      Nothing -> do
        cmp <- solEq "msg.value" amt'
        require "verify network token pay amount" cmp
      Just tok -> do
        tok' <- solF tok
        require "verify non-network token pay amount" $
          solApply "checkPayAmt" ["msg.sender", tok', amt']
  DLE_Wait {} -> return emptyDoc
  DLE_PartSet _ _ a -> spa $ solF a
  DLE_MapRef _ mpv fa _ -> do
    fa' <- mapRefArg fa
    return $ solApply (solMapRefInt mpv) [fa'] <> sp
  DLE_MapSet _ mpv fa _ (Just na) -> do
    fa' <- mapRefArg fa
    vsep <$> solLargeArg' True (solArrayRef (solMapVar mpv) fa') nla
    where
      nla = mdaToMaybeLA na_t (Just na)
      na_t = argTypeOf na
  DLE_MapSet _ mpv fa _ Nothing -> do
    fa' <- mapRefArg fa
    return $ "delete" <+> solArrayRef (solMapVar mpv) fa' <> sp
  DLE_Remote {} -> impossible "remote"
  DLE_TokenNew _ (DLTokenNew {..}) -> do
    let go a = do
          a' <- solF a
          uint8ArrayToString (bytesTypeLen $ argTypeOf a) a'
    n' <- go dtn_name
    s' <- go dtn_sym
    u' <- go dtn_url
    m' <- go dtn_metadata
    p' <- solF dtn_supply
    d' <- maybe (solF $ DLL_Int sb UI_Word 18) solF dtn_decimals
    return $ solApply "payable" [solApply "address" ["new" <+> solApply "ReachToken" [n', s', u', m', p', d']]]
  DLE_TokenBurn _ ta aa -> do
    ta' <- solF ta
    aa' <- solF aa
    return $ solApply "safeReachTokenBurn" [ta', aa'] <> sp
  DLE_TokenDestroy _ ta -> do
    ta' <- solF ta
    return $ solApply "safeReachTokenDestroy" [ta'] <> sp
  DLE_TimeOrder {} -> impossible "timeorder"
  DLE_EmitLog {} -> impossible "emitLog"
  DLE_setApiDetails {} -> impossible "setApiDetails"
  DLE_DataTag _ d -> do
    d' <- solF d
    return $ solApply "uint256" [d' <> ".which"]
  DLE_FromSome _ mo da -> do
    mo' <- solF mo
    da' <- solF da
    t <- solType $ argTypeOf mo
    let vn = "Some"
    c <- solEq (mo' <> ".which") (solVariant t vn)
    return $ parens $ c <+> "?" <+> (mo' <> "._" <> pretty vn) <+> ":" <+> da'
  DLE_GetUntrackedFunds {} -> impossible "getUntrackedFunds"
  DLE_ContractNew {} -> impossible "contractNew"
  DLE_ContractFromAddress _at _addr -> impossible "ContractFromAddress"
  where
    spa m = (<> sp) <$> m

solTransfer :: DLArg -> DLArg -> Maybe DLArg -> App Doc
solTransfer who amt mtok = do
  who' <- solF who
  amt' <- solF amt
  case mtok of
    Nothing ->
      return $ who' <> "." <> solApply "transfer" [amt']
    Just tok -> do
      tok' <- solF tok
      return $ solApply "safeTokenTransfer" [tok', who', amt']

solAsnType :: [DLVar] -> App Doc
solAsnType = solType . vsToType

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
    T_Bytes sz -> sz
    T_Array _ sz -> sz
    _ -> impossible "arraySize"

solSwitch :: SolStmts k => SrcLoc -> DLVar -> SwitchCases k -> App [Doc]
solSwitch _at ov (SwitchCases csm) = do
  ovp <- solF ov
  t <- solType $ argTypeOf (DLA_Var ov)
  let cm1 (vn, SwitchCase {..}) = do
        c <- solEq (ovp <> ".which") (solVariant t vn)
        set' <-
          case sc_vl of
            DLVarLet (Just _) ov' -> do
              addMemVar ov'
              return $ [solSet (solMemVar ov') (ovp <> "._" <> pretty vn)]
            _ -> return $ []
        body' <- solS sc_k
        let set_and_body' = set' <> body'
        return (c, set_and_body')
  solIfs_ <$> (mapM cm1 $ M.toAscList csm)

withArgLoc :: DLType -> App Doc
withArgLoc t =
  solF $
    case mustBeMem t of
      True -> AM_Memory
      False -> AM_Event

solType_withArgLoc :: DLType -> App Doc
solType_withArgLoc t =
  (<>) <$> solType t <*> withArgLoc t

doConcat :: DLVar -> DLArg -> DLArg -> App Docs
doConcat dv x y = do
  addMemVar dv
  dv' <- solF dv
  let copy src (off :: Integer) = do
        let sz = arraySize src
        src' <- solF src
        add <- solPrimApply (ADD UI_Word PV_Veri) ["i", solNum off]
        let ref = solArrayRef src' "i"
        return $ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++") <> solBraces [solArrayRef dv' add <+> "=" <+> ref <> semi]
  x' <- copy x 0
  y' <- copy y (arraySize x)
  return $ [x', y']

getBalance :: Doc -> Doc
getBalance tok = solApply "tokenBalanceOf" [tok, "address(this)"]

instance SolStmts DLStmt where
  solS = \case
    DL_Nop _ -> return $ mempty
    DL_Let _ pv (DLE_Remote at fs av rng_ty dr) -> do
      let DLRemote mf (DLPayAmt net ks) as (DLWithBill _nRecv nonNetTokRecv nnTokRecvZero) _ = dr
      let f = fromMaybe (impossible "remote no fun") mf
      -- XXX make this not rely on pv
      av' <- solF av
      as' <- mapM solF as
      dom'mem <- mapM (solType_withArgLoc . argTypeOf) as
      let dv =
            case pv of
              DLV_Eff -> impossible "remote result unbound"
              DLV_Let _ x -> x
      rng_ty'_ <- solType_ rng_ty
      rng_ty'mem <- solType_withArgLoc rng_ty
      f' <- addInterface (pretty f) dom'mem rng_ty'mem
      let eargs = f' : as'
      v_succ <- allocRawVar
      v_return <- allocRawVar
      v_before <- allocMemVar at $ T_UInt UI_Word
      -- Note: Not checking that the address is really a contract and not doing
      -- exactly what OpenZeppelin does
      netTokPaid <- solF net
      ks' <- mapM (\(amt, ty) -> (,) <$> solF amt <*> solF ty) ks
      nonNetTokApprovals <-
        concatMapM
          (\(amt, ty) -> do
             let approve = solApply "tokenApprove" [ty, av', amt]
             solRequireS "Approving remote ctc to transfer tokens" approve)
          ks'
      checkNonNetTokAllowances <-
        concatMapM
          (\(_, ty) -> do
             -- Ensure that the remote ctc transfered the approved funds
             let allowance = solApply "tokenAllowance" [ty, "address(this)", av']
             eq <- solEq allowance "0"
             solRequireS "Ensure remote ctc transferred approved funds" eq)
          ks'
      -- This is for when we don't know how much non-net tokens we will receive. i.e. `withBill`
      -- The amount of non-network tokens received will be stored in this tuple: nnTokRecvVar
      (getDynamicNonNetTokBals, setDynamicNonNetTokBals) <-
        unzip
          <$> mapM
            (\(tok, i :: Int) -> do
               tv_before <- allocMemVar at $ T_UInt UI_Word
               tokArg <- solF tok
               -- Get balances of non-network tokens before call
               let s1 = solSet tv_before $ getBalance tokArg
               -- Get balances of non-network tokens after call
               tokRecv <- solPrimApply (SUB UI_Word PV_Safe) [getBalance tokArg, tv_before]
               let s2 = solSet ((solMemVar dv <> ".elem1") <> ".elem" <> pretty i) tokRecv
               return (s1, s2))
            (zip nonNetTokRecv [0 ..])
      let nonNetToksPayAmt = foldr' (\(a, t) acc -> M.insert t a acc) M.empty ks
      -- Ensure that the non-net tokens we are NOT expecting to receive
      -- do not have a change in balance
      (getUnexpectedNonNetTokBals, checkUnexpectedNonNetTokBals) <-
        unzip
          <$> mapM
            (\tok -> do
               tv_before <- allocMemVar at $ T_UInt UI_Word
               tokArg <- solF tok
               paid <- maybe (return "0") solF $ M.lookup tok nonNetToksPayAmt
               sub <- solPrimApply (SUB UI_Word PV_Veri) [getBalance tokArg, paid]
               let s1 = solSet tv_before sub
               tv_after <- allocMemVar at $ T_UInt UI_Word
               tokRecv <- solPrimApply (SUB UI_Word PV_Veri) [getBalance tokArg, tv_before]
               let s2 = [ solSet tv_after tokRecv ]
               s3 <- solRequireS "remote did not transfer unexpected non-network tokens" =<< solEq tv_after "0"
               return (s1, s2 <> s3))
            nnTokRecvZero
      let call' = ".call{value:" <+> netTokPaid <> "}"
      let meBalance = "address(this).balance"
      addMemVar dv
      sub' <- solPrimApply (SUB UI_Word PV_Veri) [meBalance, v_before]
      let sub'l = [solSet (solMemVar dv <> ".elem0") sub']
      -- Non-network tokens received from remote call
      let billOffset :: Int -> Doc
          billOffset i = viaShow $ if null nonNetTokRecv then i else i + 1
      let pv' =
            case rng_ty of
              T_Null -> []
              _ -> [ solSet (solMemVar dv <> ".elem" <> billOffset 1) $ solApply "abi.decode" [v_return, parens rng_ty'_] ]
      let e_data_e = solApply "abi.encodeWithSelector" eargs
      e_data <- allocRawVar
      e_before <- solPrimApply (SUB UI_Word PV_Veri) [meBalance, netTokPaid]
      err_msg <- solRequireMsg $ show (at, fs, ("remote " <> f <> " failed"))
      -- XXX we could assert that the balances of all our tokens is the same as
      -- it was before
      return $ [ solBraces $
        nonNetTokApprovals
          <> getDynamicNonNetTokBals
          <> getUnexpectedNonNetTokBals
          <> [ solSet v_before e_before
             , solSet "locked" "true"
             , solSet ("bytes memory" <+> e_data) e_data_e
             , "(bool " <> v_succ <> ", bytes memory " <> v_return <> ")" <+> "=" <+> av' <> solApply call' [e_data] <> semi
             , solApply "checkFunReturn" [v_succ, v_return, err_msg] <> semi
             , solSet "locked" "false"
             ]
          <> concat checkUnexpectedNonNetTokBals
          <> setDynamicNonNetTokBals
          <> checkNonNetTokAllowances
          <> sub'l
          <> pv'
        ]
    DL_Let _ pv (DLE_EmitLog _ lk lvs) -> do
      lvs' <- mapM solF lvs
      let lv_tys = map varType lvs
      lv_tys' <- mapM solType lv_tys
      -- Get event label or use variable name from internal log
      let oe = case (lk, lvs) of
            (L_Event ml l, _) -> pretty $ maybe l (\l' -> bunpack l' <> "_" <> l) ml
            (_, [h]) -> solOutput_evt h
            (_, _) -> impossible "Expecting one value to emit"
      let go sv ls = solApply oe (map (\(l, v) -> l <+> sv v) $ ls) <> semi
      let eventVars = do
            -- Name doesn't matter in event definition just needs to be unique
            let fvs = map (\(i, DLVar at ml t _) -> DLVar at ml t i) $ zip [0 ..] lvs
            zip lv_tys' fvs
      let ed = ["event" <+> go solRawVar eventVars]
      modifyCtxtIO ctxt_outputs $ M.insert (show oe) ed
      let emitVars = map (mempty,) lvs'
      let emitl = "emit" <+> go id emitVars
      asn <- case (lk, lvs') of
        (L_Api p, [v]) -> do
          return $ [ solSet (memVarF $ bunpack $ nameReturn p) v ]
        (_, _) -> return []
      case pv of
        DLV_Eff -> do
          return $ [emitl] <> asn
        DLV_Let _ dv -> do
          addMemVar dv
          v' <- case lvs of
            [h] -> solF h
            _ -> impossible "solCom: emitLog expected one value"
          return $ [solSet (solMemVar dv) v', emitl] <> asn
    DL_Let _ (DLV_Let _ dv) (DLE_LArg _ la) -> do
      addMemVar dv
      solLargeArg False dv la
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ GET_COMPANION []) -> do
      addMemVar dv
      solLargeArg False dv $ mdaToMaybeLA T_Contract Nothing
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (ALGO_BLOCK bf) _) -> do
      addMemVar dv
      solLargeArg False dv $ mdaToMaybeLA (abfType bf) Nothing
    DL_Let _ (DLV_Eff) (DLE_GetUntrackedFunds {}) ->
      return $ mempty
    DL_Let _ (DLV_Let _ dv) (DLE_GetUntrackedFunds at mtok tb) -> do
      addMemVar dv
      actBalV <- allocRawVar
      tb' <- solF tb
      bal <- case mtok of
        Nothing -> return "address(this).balance"
        Just tok -> getBalance <$> solF tok
      sub <- solPrimApply (SUB UI_Word PV_Veri) [actBalV, tb']
      zero <- solF $ DLA_Literal $ DLL_Int at UI_Word 0
      cnd <- solPrimApply (PLT UI_Word) [actBalV, tb']
      ite <- solPrimApply IF_THEN_ELSE [cnd, zero, sub]
      return $ [ solBraces $
        [ solSet ("uint256" <+> actBalV) bal
        , solSet (solMemVar dv) ite
        ] ]
    DL_Let _ (DLV_Let _ dv) (DLE_ContractFromAddress _at addr) -> do
      addr' <- solF addr
      let isContract = parens $ addr' <> ".code.length > 0"
      addMemVar dv
      dv' <- solF dv
      trueCase <- solLargeArg' False dv' $ mdaToMaybeLA T_Contract $ Just addr
      falseCase <- solLargeArg' False dv' $ mdaToMaybeLA T_Contract Nothing
      return $ solIf_ isContract trueCase falseCase
    DL_Let _ (DLV_Eff) (DLE_ContractFromAddress _at _addr) -> do
      return $ []
    DL_Let _ (DLV_Eff) (DLE_ContractNew {}) ->
      return $ []
    DL_Let _ (DLV_Let _ dv) (DLE_ContractNew _at cns dr) -> do
      let DLRemote _ _ as _ _ = dr
      let DLContractNew {..} = cns M.! conName'
      let (bc :: String) = either impossible id $ aesonParse dcn_code
      addMemVar dv
      bc' <- allocRawVar
      as'bs <- allocRawVar
      bc'' <- allocRawVar
      ctc' <- allocRawVar
      let pay' = "0"
      let p' = solApply "add" [ bc'', "0x20" ]
      let len' = solApply "mload" [ bc'' ]
      let asm = [ ctc' <+> ":=" <+> solApply "create" [ pay', p', len' ] ]
      --- XXX support payment and bills
      as' <- mapM solF as
      chk' <- solRequireS "new contract not zero" $ ctc' <+> "!= address(0)"
      return $ [ solBraces $
        [ solSet ("bytes memory" <+> bc') (pretty $ "hex\"" <> bc <> "\"")
        , solSet ("bytes memory" <+> as'bs) (solApply "abi.encode" as')
        , solSet ("bytes memory" <+> bc'') (solApply "bytes.concat" [ bc', as'bs ])
        , "address payable" <+> ctc' <> semi
        , "assembly" <+> solBraces asm
        , solSet (solMemVar dv) ctc'
        ] <> chk'
        ]
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BYTES_ZPAD _) [x]) -> do
      addMemVar dv
      dv' <- solF dv
      x' <- solF x
      let sz = arraySize $ DLA_Var dv
      let xHowMany = fst $ solBytesInfo $ arraySize x
      let xIsStruct = xHowMany > 1
      let goSmall _ = dv' <+> "=" <> x' <> semi
      let goBig i _ = case i of
            _
              -- When x is Bytes(<= 32), assign all of x to the first element of dv
              | i == 0 && 1 == xHowMany -> dv' <> ".elem0" <+> "=" <> x' <> semi
              -- When x is Bytes(> 32) and we have yet to assign all of x to dv
              | xIsStruct && i <= xHowMany -> dv' <> ei <+> "=" <> x' <> ei <> semi
              -- Nothing left to take from x, no more assignments, rest of bytes will be null
              | otherwise -> ""
            where ei = ".elem" <> pretty i
      return $ solBytesSplit sz goSmall goBig
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ BYTES_XOR [x, y]) -> do
      addMemVar dv
      dv' <- solF dv
      let bl = bytesTypeLen $ argTypeOf x
      x' <- solF x
      y' <- solF y
      let goSmall _ = dv' <+> "=" <+> x' <+> "^" <+> y' <> semi
      let goBig i _ =  dv' <> ei <+> "=" <+> x' <> ei <+> "^" <+> y' <+> ei <> semi
              where ei = ".elem" <> pretty i
      return $ solBytesSplit bl goSmall goBig
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BTOI_LAST8 True) [x]) -> do
      addMemVar dv
      dv' <- solF dv
      x' <- solF x
      return $ [dv' <+> "=" <+> "uint64" <> parens x' <> semi]
    DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BTOI_LAST8 {}) [x]) -> do
      addMemVar dv
      dv' <- solF dv
      x' <- solF x
      let (howMany, lastLen) = solBytesInfo $ bytesTypeLen $ argTypeOf x
      let go :: Integer -> Integer -> Integer -> Doc
          go elemIdx from to =  "for(uint i = " <> pretty from <> "; i < " <> pretty to <> "; i++) {" <> hardline <>
                                    dv' <+> "=" <+> parens (dv' <+> "* 256") <+> "+" <+> "uint8" <> parens ("bytes1" <> parens (x'' <> brackets "i")) <> semi <>
                                    hardline <> "}"
            where
              x'' = case howMany == 1 of
                    True -> x'
                    False -> x' <> ".elem" <> pretty elemIdx
      let (res:: Docs) = case (lastLen < 8, howMany == 1) of
                -- The last chunk is >= 8 bytes: take 8 bytes
                (False, _) -> [go lastChunk (lastLen - 8) lastLen]
                -- One byte chunk that's < 8 Bytes: take as many as you can
                (True, True)  -> [go lastChunk 0 lastLen]
                -- Multiple byte chunks with last chunk < 8 Bytes: take all from last chunk and rest from previous
                (True, False) -> [go (howMany - 2) (byteChunkSize - lastLen) byteChunkSize, go lastChunk 0 lastLen]
                where
                  lastChunk = howMany - 1
      return $ res
    DL_Let _ (DLV_Let _ dv) (DLE_ArrayConcat _ x y) -> do
      doConcat dv x y
    DL_Let _ (DLV_Let pu dv) de ->
      case simple de of
        True -> no_def
        False ->
          case pu of
            DVC_Once -> no_def
            DVC_Many -> def
      where
        simple = \case
          DLE_Arg {} -> True
          DLE_ArrayRef {} -> True
          DLE_TupleRef {} -> True
          DLE_ObjectRef {} -> True
          _ -> False
        no_def = do
          dp <- depthOf de
          case dp > maxDepth of
            True -> def
            False -> do
              de' <- parens <$> solExpr emptyDoc de
              addVar dv de'
              recordDepth dv dp
              return []
        def = do
          addMemVar dv
          de' <- solExpr emptyDoc de
          return $ [solSet (solMemVar dv) de']
    DL_Let _ _ (DLE_setApiDetails {}) -> return []
    DL_Let _ DLV_Eff de -> return <$> solExpr semi de
    DL_Var _ dv -> do
      addMemVar dv
      return []
    DL_Set _ dv da -> return <$> (solSet (solMemVar dv) <$> solF da)
    DL_LocalIf _ _ ca t f -> solIf ca t f
    DL_LocalSwitch at ov csm -> solSwitch at ov csm
    DL_Only {} -> impossible $ "only in CT"
    DL_ArrayMap _ ans_lv xs as i (DLBlock _ _ f r) -> do
      addMemVars $ map vl2v as
      let sz = arraysLength xs
      xs' <- mapM solF xs
      as' <- mapM solF $ map vl2v as
      i' <- addVar' $ vl2v i
      recv_ans <-
        case ans_lv of
          DLV_Let _ ans -> do
            addMemVars [ans]
            ans' <- solF ans
            return $ \r' ->
              [ (solArrayRef ans' i') <+> "=" <+> r' <> semi ]
          _ ->
            return $ const []
      f' <- solS f
      r' <- solF r
      return $
        [ "for" <+> parens ("uint256 " <> i' <> " = 0" <> semi <+> i' <> " <" <+> (pretty sz) <> semi <+> i' <> "++")
            <> solBraces
              (zipWith (\a x -> a <+> "=" <+> (solArrayRef x i') <> semi) as' xs'
               <> f' <> recv_ans r')
        ]
    DL_ArrayReduce _ ans_lv xs z b as i (DLBlock _ _ f r) -> do
      addMemVars $ [vl2v b] <> (map vl2v as)
      let sz = arraysLength xs
      xs' <- mapM solF xs
      z' <- solF z
      as' <- mapM solF $ map vl2v as
      b' <- solF $ vl2v b
      i' <- addVar' $ vl2v i
      f' <- solS f
      r' <- solF r
      let start =
            [ b' <+> "=" <+> z' <> semi
            , "for" <+> parens ("uint256 " <> i' <> " = 0" <> semi <+> i' <> " <" <+> (pretty sz) <> semi <+> i' <> "++")
                <> solBraces
                  (zipWith (\a x -> a <+> "=" <+> (solArrayRef x i') <> semi) as' xs'
                   <> f' <> [ b' <+> "=" <+> r' <> semi ])
            ]
      case ans_lv of
        DLV_Eff -> return start
        DLV_Let _ ans -> do
          addMemVars [ans]
          ans' <- solF ans
          return $ start <> [ ans' <+> "=" <+> b' <> semi ]
    DL_MapReduce {} ->
      impossible $ "cannot inspect maps at runtime"
    DL_LocalDo _ _ t -> solS t

solScat :: (SolStmts a, SolStmts b) => a -> b -> App Docs
solScat m k = (<>) <$> solS m <*> solS k

instance SolStmts DLTail where
  solS = \case
    DT_Return _ -> return []
    DT_Com m k -> solScat m k

solFrame :: S.Set DLVar -> App (Docs, Docs)
solFrame sim = do
  let mk_field dv@(DLVar _ _ t _) = (,) (solRawVar dv) <$> (solType t)
  i <- allocVarIdx
  fs <- mapM mk_field $ S.elems sim
  let framei = "_F" <> pretty i
  case solStruct framei fs of
    Nothing -> return $ ([], [])
    Just frame_defp -> do
      let frame_declp = [(framei <+> "memory _f") <> semi]
      return $ (frame_defp, frame_declp)

solSF :: (SolStmts a) => a -> App (Docs, Docs)
solSF x = do
  x' <- solS x
  mvars <- readMemVars
  (frameDefn, frameDecl) <- solFrame mvars
  return (frameDefn, frameDecl <> x')

divup :: Integer -> Integer -> Integer
divup x y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y)

byteChunkSize :: Integer
byteChunkSize = 32

solBytesInfo :: Integer -> (Integer, Integer)
solBytesInfo sz = (howMany, lastLen)
  where
    howMany = divup sz byteChunkSize
    szRem = sz `rem` byteChunkSize
    lastLen =
      case szRem == 0 of
        True -> byteChunkSize
        False -> szRem

-- `goSmall` handles the case where `bytes` is not a struct
solBytesSplit :: Integer -> (Integer -> a) -> (Integer -> Integer -> a) -> [a]
solBytesSplit sz goSmall goBig =
  case lastOne of
    0 -> [goSmall lastLen]
    _ -> map go [0 .. lastOne]
  where
    (howMany, lastLen) = solBytesInfo sz
    lastOne = howMany - 1
    go i = goBig i len
      where
        len = case i == lastOne of
          True -> lastLen
          False -> byteChunkSize

solDefineType :: DLType -> App ()
solDefineType t = case t of
  T_Null -> base
  T_Bool -> base
  T_UInt _ -> base
  T_Bytes sz
    | sz <= byteChunkSize -> base
    | otherwise -> do
    -- NOTE: Get rid of this stupidity when
    -- https://github.com/ethereum/solidity/issues/8772
    let atsn = solBytesSplit sz (const $ impossible "atsn") $
                  \ i n -> ("elem" <> pretty i, "bytes" <> pretty n)
    (name, i) <- addName
    let x = fromMaybe (impossible "bytes") $ solStruct name atsn
    addDef i x
  T_BytesDyn -> base
  T_StringDyn -> base
  T_Digest -> base
  T_Address -> base
  T_Contract -> base
  T_Token -> base
  T_Array _ 0 -> do
    addMap "bool"
  T_Array et sz -> do
    tn <- solType et
    let me = tn <> brackets (pretty sz)
    inmem <- solF AM_Memory
    let memem = me <> inmem
    let tnmem = tn <> (if mustBeMem et then inmem else "")
    let args =
          [ solDecl "arr" memem
          , solDecl "idx" "uint256"
          , solDecl "val" tnmem
          ]
    let ret = Just $ solDecl "arrp" memem
    let assign idx val = (solArrayRef "arrp" idx) <+> "=" <+> val <> semi
    let body =
          [ ("for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
                 <> solBraces [assign "i" (solArrayRef "arr" "i")])
            , assign "idx" "val"
          ]
    addMap me
    i <- addId
    -- XXX This is pure, but we can't say that
    addFun i $ solFunction (solArraySet i) args ret body
  T_Tuple ats -> do
    let ats' = (flip zip) ats $ map (("elem" ++) . show) ([0 ..] :: [Int])
    addMap =<< doStruct ats'
  T_Object tm -> do
    addMap =<< (doStruct $ map (first objPrefix) $ M.toAscList tm)
  T_Data tm -> do
    tmn <- mapM solType tm
    --- XXX Try to use bytes and abi.decode; Why not right away? The
    --- length of the bytes would not be predictable, which means the
    --- gas cost would be arbitrary.
    (name, i) <- addName
    let enumn = "_enum_" <> name
    let enump = solEnum enumn $ map (pretty . fst) $ M.toAscList tmn
    let structp = fromMaybe (impossible "T_Data") $ solStruct name $ ("which", enumn) : map (\(k, kt) -> (pretty ("_" <> k), kt)) (M.toAscList tmn)
    addDef i $ enump <> structp
  T_Struct ats -> void $ doStruct ats
  where
    base = impossible "base"
    addMap d = modifyCtxtIO ctxt_typem $ M.insert t d
    addId = do
      i <- (liftIO . incCounter) =<< (ctxt_typeidx <$> ask)
      modifyCtxtIO ctxt_typei $ M.insert t i
      return $ i
    addName = do
      i <- addId
      let name = "T" <> pretty i
      addMap name
      return $ (name, i)
    addDef i d = modifyCtxtIO ctxt_typed $ M.insert i d
    addFun i f = modifyCtxtIO ctxt_typef $ M.insert i f
    doStruct ats = do
      atsn <- mapM (\(k, v) -> (,) (pretty k) <$> solType v) ats
      (name, i) <- addName
      let sp = solStruct name atsn
      case sp of
        -- XXX This really stinks
        Nothing -> do
          addMap "bool"
          return "bool"
        Just x -> do
          addDef i x
          return name

mkEnsureTypeDefined :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a b)) -> a -> App ()
mkEnsureTypeDefined define f t =
  (M.lookup t <$> readCtxtIO f) >>= \case
    Nothing -> define t
    Just _ -> return ()

ensureTypeDefined :: DLType -> App ()
ensureTypeDefined = mkEnsureTypeDefined solDefineType ctxt_typem

newtype ExportBlockCall = ExportBlockCall ([DLVar], DLExportBlock)

instance SolStmts ExportBlockCall where
  solS (ExportBlockCall (args, (DLinExportBlock _ mfargls (DLBlock _ _ t r)))) = do
    let fargs = map varLetVar $ fromMaybe mempty mfargls
    let go fa a = do
          a' <- solF a
          return $ (fa, a')
    extendVarMap =<< (M.fromList <$> zipWithM go fargs args)
    t' <- solS t
    r' <- solF r
    return $ t' <> ["return" <+> r' <> semi]

baseTypes :: M.Map DLType Doc
baseTypes =
  M.fromList $
    [ (T_Null, "bool")
    , (T_Bool, "bool")
    , (T_UInt UI_Word, "uint256")
    , (T_UInt UI_256, "uint256")
    , (T_BytesDyn, "bytes")
    , (T_StringDyn, "string")
    , (T_Digest, "uint256")
    , (T_Address, "address")
    , (T_Contract, "address")
    , (T_Token, "address")
    ] <> map (\ sz -> (T_Bytes sz, "bytes" <> pretty sz)) [0..byteChunkSize]

data SMapY = SMapY Doc DLType DLType

newtype SMapX = SMapX (DLMVar, DLMapInfo)

instance SolStmts SMapX where
  solS (SMapX (mpv, (DLMapInfo {..}))) =
    solS $ SMapY (solMapVar mpv) dlmi_kt dlmi_ty

instance SolStmts SMapY where
  solS (SMapY name mk ty) = do
    let mt = maybeT ty
    let kt = maybe (T_UInt UI_Word) (const mk) $ M.lookup mk baseTypes
    keyTy <- solType_ kt
    valTy <- solType mt
    let args = [solDecl "addr" keyTy]
    let ret = Just $ valTy <> " memory res"
    let ref = (solArrayRef name "addr")
    do_none <- solLargeArg' False "res" $ mdaToMaybeLA ty Nothing
    let do_some = [solSet "res" ref]
    eq <- solEq (ref <> ".which") (solVariant valTy "Some")
    let int_defn =
          solFunctionLike (SFLFun False False (solMapRefInt_ name) ret) args $
            solIf_ eq do_some do_none
    let ext_defn =
          solFunctionLike (SFLFun True False (solMapRefExt_ name) ret) args $
            [solSet "res" (solApply (solMapRefInt_ name) ["addr"])]
    return $
      [ "mapping (" <> keyTy <> " => " <> valTy <> ") " <> name <> semi ]
      <> int_defn
      <> ext_defn

data SolMap t k v = SolMap ((k, v) -> t) (M.Map k v)

instance (SolStmts t) => SolStmts (SolMap t k v) where
  solS (SolMap f m) = solS $ map f $ M.toAscList m

solSM :: (SolStmts t) => ((k, v) -> t) -> M.Map k v -> App Docs
solSM f = solS . SolMap f

newtype DefX = DefX (CLVar, CLDef)

instance SolStmts DefX where
  solS (DefX (name, d)) =
    case d of
      CLD_Mem {} -> return []
      CLD_Map {..} -> solS (SMapY (pclv name) cldm_kt cldm_ty)
      CLD_Evt _ ->
        -- XXX Ignored, because they are part of ctxt_outputs
        return []

createMem :: CLDefs -> App Docs
createMem defs = do
  fs <- forM (M.toAscList $ M.mapMaybe viewCLD_Mem defs) $ \(k, t) -> do
    let n = pclv k
    t' <- solType t
    return (n, t')
  let fs' = ("nil", "bool") : fs
  return $ fromMaybe (impossible "cm") $ solStruct memTy fs'

instance SolStmts CLStmt where
  solS = \case
    CLDL m -> solS m
    CLBindSpecial _ lv sp ->
      case lv of
        DLV_Eff -> return []
        DLV_Let _ v -> do
          let rhs =
                case sp of
                  CLS_TxnFrom -> "payable(msg.sender)"
                  CLS_TxnTime -> solBlockTime
                  CLS_TxnSecs -> solBlockSecs
                  CLS_StorageState -> "current_step"
          extendVarMap $ M.singleton v rhs
          return []
    CLEmitPublish _at which vars -> do
      let msg_ty = T_Tuple $ map varType vars
      -- XXX This should be in CLike, but how can we force the inclusion of
      -- _who?
      msg_ty' <- solType msg_ty
      let e = solMsg_evt which
      let ed = [ "event" <+> solApply e ["address _who", msg_ty' <+> "_a"] <> semi ]
      modifyCtxtIO ctxt_outputs $ M.insert (show e) ed
      -- We are relying on knowing that this is always used for effectful funs
      s <- solRequireS "locked" "! locked"
      return $ s <> [ "emit" <+> solApply e ["msg.sender", "_a"] <> semi ]
    CLTimeCheck at given -> do
      let actual' = "current_time"
      given' <- solF given
      zeq <- solEq given' $ solNum (0 :: Int)
      teq <- solEq actual' given'
      solRequireS ("time check at " <> show at) $
        solBinOp "||" (parens zeq) (parens teq)
    CLStateBind at isSafe svs_vl prev -> do
      s' <-
        case isSafe of
          True -> return []
          False -> do
            s <- solEq "current_step" (solNum prev)
            solRequireS ("state check at " <> show at) s
      let svs = map varLetVar svs_vl
      svs_ty' <- solAsnType svs
      addVars_ solSVSVar svs
      let svs' = [solSet (parens $ solDecl "_svs" (mayMemSol svs_ty')) $ solApply "abi.decode" ["current_svbs", parens svs_ty']]
      return $ s' <> svs'
    CLIntervalCheck at timev secsv (CBetween ifrom ito) -> do
      let checkTime1 op ta = do
            let (v, a) = case ta of
                  Left x -> (timev, x)
                  Right x -> (secsv, x)
            v' <- solF v
            a' <- solF a
            t <- solPrimApply op [v', a']
            solRequireS ("timeout check at " <> show at) t
      let checkTime op = \case
            Nothing -> return []
            Just x -> checkTime1 op x
      ((<>) <$> checkTime (PGE UI_Word) ifrom <*> checkTime (PLT UI_Word) ito)
    CLStateSet _at which svs -> do
      let asnv = "nsvs"
      let vs = svs
      let go (SvsPut v a) = solSet (asnv <> "." <> solRawVar v) <$> solF a
      vs' <- mapM go vs
      vs_ty' <- solAsnType $ map svsp_svs vs
      return $
        [solDecl asnv (mayMemSol vs_ty') <> semi]
        <> vs'
        <> [ solSet "current_step" (solNum which)
           , solSet "current_time" solBlockTime
           , solSet "current_svbs" (solEncode ["nsvs"])
           ]
    CLTokenUntrack _at _tok -> do
      -- We could "selfdestruct" our token holdings but this is not a norm on
      -- ETH, so we won't spend the gas to do so
      return []
    CLMemorySet _at v a -> do
      let v' = memVarF $ bunpack v
      a' <- solF a
      return $ [ solSet v' a' ]

vsToInternalArg :: [DLArg] -> DLType
vsToInternalArg = T_Tuple . map typeOf
makeInternalArg :: Doc -> [DLArg] -> App Docs
makeInternalArg ia args = solLargeArg' False ia $ DLLA_Tuple args
bindInternalArg :: [DLVar] -> Doc -> App ()
bindInternalArg vs ia = extendVarMap $ M.fromList $ zipWith go vs ([0 ..] :: [Int])
  where
    go v i = (v, ia <> ".elem" <> pretty i)

instance SolStmts CLTail where
  solS = \case
    CL_Com m k -> solScat m k
    CL_If _ ca t f -> solIf ca t f
    CL_Switch at ov csm -> solSwitch at ov csm
    CL_Jump _at f args_ _isApi mmret -> do
      let f' = pclv f
      call <- case args_ of
        [ arg ] -> do
          arg' <- solF arg
          return $ [ solApply f' [ arg', memVar ] <> semi ]
        args -> do
          -- Turn the arguments into a single object and call w/ memory
          let args_ty = vsToInternalArg args
          args_ty' <- solType args_ty
          am' <- withArgLoc args_ty
          let defn = [ solDecl (am' <+> "_ja") args_ty' <> semi ]
          asn <- makeInternalArg "_ja" args
          return $ defn <> asn <> [ solApply f' [ "_ja", memVar ] <> semi ]
      case mmret of
        Nothing -> do
          -- internal to internal call
          return $ call
        Just mret -> do
          -- external to internal call, we must allocate
          let alloc = [ memVarDecl <> semi ]
          let ret =
                case mret of
                  Nothing ->
                    -- No return
                    []
                  Just retv ->
                    [ "return" <+> memVarF (bunpack retv) <> semi ]
          return $ alloc <> call <> ret
    CL_Halt _ hm ->
      case hm of
        HM_Forever -> do
          return $
            [ solSet "current_step" "0x0"
            , solSet "current_time" "0x0"
            , "delete current_svbs;"
            --
            -- , solApply "selfdestruct" ["payable(msg.sender)"] <> semi
            --
            -- We don't do this, because selfdestruct-ing is dangerous, because
            -- although the contract is gone, that means the messages sent to it
            -- are like no-ops, so they can take funds from clients that think they
            -- are calling real contracts. Ideally, we'd be able to get back our
            -- funds (e.g. on Conflux where there are storage costs), we would
            -- rather not get those, than have users lose funds to dead contracts.
            ]
        _ -> return []

data FunX = FunX
  { fx_sfl :: SolFunctionLike
  , fx_args :: [Doc]
  , fx_extra :: Docs
  , fx_tail :: CLTail
  }
newtype IntX = IntX (CLVar, CLIntFun)
newtype ExtX = ExtX (CLSym, CLExtFun)

instance SolStmts FunX where
  solS (FunX {..}) = freshVarMap $ do
    (frameDefn, body) <- solSF fx_tail
    return $ frameDefn <> solFunctionLike fx_sfl fx_args (fx_extra <> body)

instance SolStmts IntX where
  solS (IntX (name, CLIntFun {..})) = do
    let CLFun {..} = cif_fun
    let fx_tail = clf_tail
    let name' = pclv name
    let mut = not clf_view
    let fx_sfl = SFLFun False mut name' Nothing
    let fx_extra = mempty
    let vs_ = map varLetVar clf_dom
    argTy <- case vs_ of
      [ v ] -> do
        addVar v "_a"
        return $ varType v
      vs -> do
        bindInternalArg vs "_a"
        return $ vsToInternalArg $ map DLA_Var vs
    argTyl <- solType_withArgLoc argTy
    let fx_args = [ solDecl "_a" argTyl, memVarDecl ]
    solS $ FunX {..}

instance SolStmts ExtX where
  solS (ExtX ((CLSym name _ _), CLExtFun {..})) = do
    let CLFun {..} = cef_fun
    let fx_tail = clf_tail
    let name' = pclv name
    let mut = not clf_view
    mret <- Just <$> solType_withArgLoc cef_rng
    (am, fx_sfl, fx_extra) <-
      case cef_kind of
        CE_Publish 0 -> do
          let extra =
                [ "current_step = 0x0;"
                , "creation_time = uint256(block.number);"
                ]
          return (AM_Memory, SFLCtor, extra)
        _ -> do
          return (AM_Call, SFLFun True mut name' mret, mempty)
    am' <- solF am
    let howMany = length clf_dom
    when (howMany > apiMaxArgs) $
      expect_throw Nothing clf_at $ Err_SolTooManyArgs "externally visible functions" (bunpack name) howMany
    addVars solRawVar clf_dom
    fx_args <- forM clf_dom $ \vl -> do
      let v = varLetVar vl
      let ty = varType v
      ty' <- solType ty
      let v' = solRawVar v
      let am'' = if mustBeMem ty then am' else ""
      return $ solDecl v' $ ty' <> am''
    solS $ FunX {..}

instance SolStmts CLProg where
  solS (CLProg {..}) = do
    defs <- solSM DefX clp_defs
    mem <- createMem clp_defs
    funs <- solSM IntX clp_funs
    apis <- solSM ExtX clp_api
    return $ defs <> mem <> funs <> apis

instance (SolStmts a) => SolStmts (IGd a) where
  solS (IGd x _) = solS x

solProg :: (HasCounter a, SolStmts a) => a -> IO (ConnectorObject, Doc)
solProg p = do
  ctxt_varm <- newIORef mempty
  ctxt_mvars <- newIORef mempty
  ctxt_depths <- newIORef mempty
  ctxt_typei <- newIORef mempty
  ctxt_typem <- newIORef baseTypes
  ctxt_typef <- newIORef mempty
  ctxt_typed <- newIORef mempty
  ctxt_view_json <- newIORef mempty
  ctxt_typeidx <- newCounter 0
  ctxt_requireMsg <- newCounter 7 -- +1 the num used in stdlib_reach.sol
  ctxt_ints <- newIORef mempty
  ctxt_outputs <- newIORef mempty
  ctxt_which_msg <- newIORef mempty
  let ctxt_varidx = getCounter p
  p' <- flip runReaderT (SolCtxt {..}) $ solS p
  -- XXX current_time should be part of CLike (AVM uses it too)
  let state_defn :: Docs =
        [ "uint256 current_step;"
        , "uint256 current_time;"
        , "  bytes current_svbs;"
        , "uint256 creation_time;"
        , "   bool locked;"
        , "function _reachCreationTime() external view returns (uint256) { return creation_time; }"
        , "function _reachCurrentTime() external view returns (uint256) { return current_time; }"
        , "function _reachCurrentState() external view returns (uint256, bytes memory) { return (current_step, current_svbs); }"
        ]
  let getm ctxt_f = (concat . map snd . M.toAscList) <$> (readIORef ctxt_f)
  typedsp <- getm ctxt_typed
  typefsp <- getm ctxt_typef
  intsp <- getm ctxt_ints
  outputsp <- getm ctxt_outputs
  let defp =
        [ "receive () external payable {}"
        , "fallback () external payable {}"
        ]
  let ctcbody = state_defn <> typefsp <> outputsp <> defp <> p'
  let ctcp = ["contract" <+> pretty contractId <+> "is Stdlib" <+> solBraces ctcbody]
  view_json <- readIORef ctxt_view_json
  let cinfo =
        M.fromList $
          [ ("views", aesonObject view_json)
          ]
  let preamble =
        [ "// Automatically generated with Reach" <+> (pretty versionHashStr)
        , "pragma abicoder v2" <> semi
        ]
  let fin = preamble <> solVersion <> solStdLib <> typedsp <> intsp <> ctcp
  return $ (cinfo, vsep fin)

compile_sol :: ConnectorObject -> FilePath -> IO ConnectorInfo
compile_sol cinfo solf = compile_sol_ solf contractId >>= \case
  Left x -> impossible x
  Right (CompiledSolRec {..}) ->
    return $
      Aeson.Object $
        mToKM $
          M.union cinfo $
            M.fromList $
              [ ("ABI", Aeson.String csrAbi)
              , ("Bytecode", Aeson.String $ "0x" <> csrCode)
              , ("BytecodeLen", Aeson.Number $ (fromIntegral $ T.length csrCode) / 2)
              , ("version", Aeson.Number $ fromIntegral reachEthBackendVersion)
              ]

-- Connector

ccBin :: BS.ByteString -> String
ccBin = B.unpack

ccJson :: String -> String -> BS.ByteString -> CCApp String
ccJson x y z = do
  CompiledSolRec {..} <- except (compile_sol_extract True x y z)
  return $ t2s $ csrCode

ccSol :: String -> FilePath -> CCApp String
ccSol cn solf = do
  CompiledSolRec {..} <- ExceptT (compile_sol_ solf cn)
  return $ T.unpack csrCode

ccPath :: String -> CCApp String
ccPath fp = do
  case splitOn ":" fp of
    [ x ] | takeExtension x == ".bin" ->
      ccBin <$> ccRead x
    [ x, y, cn ] | takeExtension x == ".json" ->
      ccJson y cn =<< ccRead x
    [ x, cn ] | takeExtension x == ".sol" ->
      ccSol cn x
    _ -> throwE $ "Invalid code path: " <> show fp

data ETHConnectorInfo = ETHConnectorInfo
  { eci_bytecode :: String
  } deriving (Show)

instance AS.ToJSON ETHConnectorInfo where
  toJSON (ETHConnectorInfo {..}) = AS.String $ T.pack $ drop0x $ eci_bytecode
    where
      drop0x = \case
        '0':'x':s -> s
        ow -> ow

instance AS.FromJSON ETHConnectorInfo where
  parseJSON = AS.withObject "ETHConnectorInfo" $ \obj -> do
    eci_bytecode <- obj .: "Bytecode"
    return $ ETHConnectorInfo {..}

solReservedNames :: S.Set SLVar
solReservedNames = S.fromList $
  [ "address"
  , "after"
  , "alias"
  , "anonymous"
  , "apply"
  , "auto"
  , "callStatic"
  , "case"
  , "constant"
  , "copyof"
  , "default"
  , "define"
  , "delete"
  , "estimateGas"
  , "external"
  , "filters"
  , "final"
  , "functions"
  , "immutable"
  , "implements"
  , "in"
  , "indexed"
  , "inline"
  , "interface"
  , "internal"
  , "let"
  , "macro"
  , "match"
  , "mutable"
  , "null"
  , "of"
  , "override"
  , "partial"
  , "payable"
  , "populateTransaction"
  , "private"
  , "promise"
  , "provider"
  , "public"
  , "pure"
  , "reference"
  , "relocatable"
  , "resolvedAddress"
  , "sealed"
  , "sizeof"
  , "signer"
  , "static"
  , "super"
  , "supports"
  , "switch"
  , "this"
  , "typedef"
  , "typeof"
  , "unchecked"
  , "view"
  , "virtual"
  ]

conName' :: T.Text
conName' = "ETH"

conCons' :: DLConstant -> DLLiteral
conCons' = \case
  DLC_UInt_max  -> DLL_Int sb UI_Word $ 2 ^ (256 :: Integer) - 1
  DLC_Token_zero -> DLL_TokenZero

connect_eth :: Connector
connect_eth = Connector {..}
  where
    conName = conName'
    conCons = conCons'
    conReserved = flip S.member solReservedNames
    conGen (ConGenConfig {..}) cl = do
      (cinfo, sol) <- solProg cl
      let o@(_, solf) = cgOutput (not dontWriteSol) "sol"
      mayOutput o $ flip LTIO.writeFile (render sol)
      compile_sol cinfo solf
    conCompileCode v = runExceptT $ do
      (c::String) <- ccPath =<< aesonParse' v
      return $ toJSON c
    conContractNewOpts :: Maybe AS.Value -> Either String AS.Value
    conContractNewOpts mv = do
      (x :: ()) <- aesonParse $ fromMaybe AS.Null mv
      return $ AS.toJSON x
    conCompileConnectorInfo :: Maybe AS.Value -> Either String AS.Value
    conCompileConnectorInfo v = do
      ETHConnectorInfo {..} <- aesonParse $ fromMaybe (AS.object mempty) v
      return $ AS.toJSON $ ETHConnectorInfo {..}
