{-# LANGUAGE QuasiQuotes #-}

module Reach.Connector.ETH_Solidity (connect_eth) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ETH_EVM.hs.dead

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Aeson as Aeson
import qualified Data.Aeson as AS
import Data.Aeson.Encode.Pretty
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
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
import Reach.AST.PL
import Reach.CommandLine
import Reach.Connector
import Reach.Counter
import Reach.EmbeddedFiles
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version
import Reach.Warning
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process.ByteString
import Text.Printf
import Safe (headMay)
import Safe.Foldable (maximumMay)

--- Debugging tools

--- You can turn this to True and manually change the Solidity file
dontWriteSol :: Bool
dontWriteSol = False

maxDepth :: Int
maxDepth = 13

apiMaxArgs :: Int
apiMaxArgs = 12

maxContractLen :: Int
maxContractLen = 24576

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

conName' :: T.Text
conName' = "ETH"

conCons' :: DLConstant -> DLLiteral
conCons' = \case
  DLC_UInt_max  -> DLL_Int sb UI_Word $ 2 ^ (256 :: Integer) - 1
  DLC_Token_zero -> DLL_TokenZero

solString :: String -> Doc
solString s = squotes $ pretty s

solNum :: Show n => n -> Doc
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc -> Doc
solBraces body = braces (nest $ hardline <> body)

data SolFunctionLike
  = SFL_Constructor
  | SFL_Function Bool (Doc)

solFunctionLike :: SolFunctionLike -> [Doc] -> Doc -> Doc -> Doc
solFunctionLike sfl args ret body =
  sflp <+> ret' <+> solBraces body
  where
    ret' = ext' <> ret
    (ext', sflp) =
      case sfl of
        SFL_Constructor ->
          (emptyDoc, solApply "constructor" args)
        SFL_Function ext name ->
          (ext'', "function" <+> solApply name args)
          where
            ext'' = if ext then "external " else " "

solFunction :: Doc -> [Doc] -> Doc -> Doc -> Doc
solFunction name =
  solFunctionLike (SFL_Function False name)

solContract :: String -> Doc -> Doc
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc
solVersion = "pragma solidity ^" <> pretty solcVersionStr <> ";"

solStdLib :: Doc
solStdLib = pretty $ B.unpack stdlib_sol

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

solBinOp :: String -> Doc -> Doc -> Doc
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc -> Doc -> App Doc
solEq x y = solPrimApply (PEQ UI_Word) [x, y]

solSet :: Doc -> Doc -> Doc
solSet x y = solBinOp "=" x y <> semi

solWhen :: Doc -> Doc -> Doc
solWhen c t = "if" <+> parens c <+> solBraces t

solIf :: Doc -> Doc -> Doc -> Doc
solIf c t f = solWhen c t <> hardline <> "else" <+> solBraces f

--- FIXME don't nest
solIfs :: [(Doc, Doc)] -> Doc
solIfs [] = emptyDoc
solIfs ((c, t) : more) = solIf c t $ solIfs more

solDecl :: Doc -> Doc -> Doc
solDecl n ty = ty <+> n

solStruct :: Doc -> [(Doc, Doc)] -> Maybe Doc
solStruct name = \case
  [] -> Nothing
  fields -> Just $ "struct" <+> name <+> solBraces (vsep $ map (<> semi) $ map (uncurry solDecl) fields)

solEnum :: Doc -> [Doc] -> Doc
solEnum name opts = "enum" <+> name <+> braces (hcat $ intersperse (comma <> space) opts)

--- Runtime helpers
reachPre :: Doc
reachPre = "_reach_"

solOutput_evt :: DLVar -> Doc
solOutput_evt dv = reachPre <> "oe_" <> solRawVar dv

solMsg_evt :: Pretty i => i -> Doc
solMsg_evt i = reachPre <> "e" <> pretty i

solMsg_fun :: Pretty i => i -> Doc
solMsg_fun i = reachPre <> "m" <> pretty i

solLoop_fun :: Pretty i => i -> Doc
solLoop_fun i = "l" <> pretty i

solMapVar :: DLMVar -> Doc
solMapVar mpv = pretty mpv

solMapRefExt :: DLMVar -> Doc
solMapRefExt (DLMVar i) = "_reachMap" <> pretty i <> "Ref"

solMapRefInt :: DLMVar -> Doc
solMapRefInt mv = "_" <> solMapRefExt mv

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

apiRetVar :: Doc
apiRetVar = "_apiRet"

apiRetMemVar :: String -> Doc
apiRetMemVar f = apiRetVar <> "." <> pretty f

apiRngTy :: Doc
apiRngTy = "ApiRng"

solArgSVSVar :: DLVar -> Doc
solArgSVSVar dv = "_a.svs." <> solRawVar dv

solSVSVar :: DLVar -> Doc
solSVSVar dv = "_svs." <> solRawVar dv

solArgMsgVar :: DLVar -> Doc
solArgMsgVar dv = "_a.msg." <> solRawVar dv

vsToType :: [DLVar] -> DLType
vsToType vs = T_Struct $ map go_ty vs
  where
    go_ty v = (show (solRawVar v), varType v)

objPrefix :: (Semigroup a, IsString a) => a -> a
objPrefix = ("_" <>)

--- Compiler

type VarMap = M.Map DLVar Doc

data SolCtxt = SolCtxt
  { ctxt_handler_num :: Int
  , ctxt_varm :: IORef VarMap
  , ctxt_mvars :: IORef (S.Set DLVar)
  , ctxt_depths :: IORef (M.Map DLVar Int)
  , ctxt_typei :: IORef (M.Map DLType Int)
  , ctxt_typem :: IORef (M.Map DLType Doc)
  , ctxt_typed :: IORef (M.Map Int Doc)
  , ctxt_typef :: IORef (M.Map Int Doc)
  , ctxt_typeidx :: Counter
  , ctxt_plo :: PLOpts
  , ctxt_intidx :: Counter
  , ctxt_ints :: IORef (M.Map Int Doc)
  , ctxt_outputs :: IORef (M.Map String Doc)
  , ctxt_tlfuns :: IORef (M.Map String Doc)
  , ctxt_requireMsg :: Counter
  , ctxt_which_msg :: IORef (M.Map Int [DLVar])
  , ctxt_uses_apis :: Bool
  }

readCtxtIO :: (SolCtxt -> IORef a) -> App a
readCtxtIO f = (liftIO . readIORef) =<< (f <$> ask)

modifyCtxtIO :: (SolCtxt -> IORef a) -> (a -> a) -> App ()
modifyCtxtIO f m = (liftIO . (flip modifyIORef m)) =<< (f <$> ask)

type App = ReaderT SolCtxt IO

type AppT a = a -> App Doc

instance Semigroup a => Semigroup (App a) where
  mx <> my = (<>) <$> mx <*> my

instance Monoid a => Monoid (App a) where
  mempty = return mempty

addInterface :: Doc -> [Doc] -> Doc -> App Doc
addInterface f dom rng = do
  idx <- (liftIO . incCounter) =<< (ctxt_intidx <$> ask)
  let ip = "I" <> pretty idx
  let idef = "interface" <+> ip <+> solBraces ("function" <+> solApply f dom <+> "external returns" <+> parens rng <> semi)
  modifyCtxtIO ctxt_ints $ M.insert idx idef
  return $ ip <> "." <> f <> ".selector"

allocVarIdx :: App Int
allocVarIdx = do
  PLOpts {..} <- ctxt_plo <$> ask
  liftIO $ incCounter plo_counter

allocVar :: App Doc
allocVar = (pretty . (++) "v" . show) <$> allocVarIdx

extendVarMap :: VarMap -> App ()
extendVarMap vm1 = do
  varmr <- ctxt_varm <$> ask
  liftIO $ modifyIORef varmr $ (<>) vm1

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
  extendVarMap $ M.singleton v (solMemVar v)
  mvars <- ctxt_mvars <$> ask
  liftIO $ modifyIORef mvars $ S.insert v

allocDLVar :: SrcLoc -> DLType -> App DLVar
allocDLVar at t = DLVar at Nothing t <$> allocVarIdx

allocMemVar :: SrcLoc -> DLType -> App Doc
allocMemVar at t = do
  dv <- allocDLVar at t
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
    DLE_MapRef _ _ x -> add1 $ depthOf x
    DLE_MapSet _ _ x y -> max <$> depthOf x <*> depthOf y
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

solVar :: AppT DLVar
solVar v = do
  varm <- readCtxtIO ctxt_varm
  case M.lookup v varm of
    Just x -> return $ x
    Nothing -> impossible $ "unbound var " ++ show v

mkSolType :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a Doc)) -> AppT a
mkSolType ensure f t = do
  ensure t
  (fromMaybe (impossible "solType") . M.lookup t) <$> readCtxtIO f

solType_ :: AppT DLType
solType_ = mkSolType ensureTypeDefined ctxt_typem

solType :: AppT DLType
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

solArgLoc :: ArgMode -> Doc
solArgLoc = \case
  AM_Call -> " calldata"
  AM_Memory -> " memory"
  AM_Event -> ""

solLit :: DLLiteral -> Doc
solLit = \case
  DLL_Null -> "false"
  DLL_Bool True -> "true"
  DLL_Bool False -> "false"
  DLL_Int at _ i -> solNum $ checkIntLiteralC at conName' conCons' i
  DLL_TokenZero -> "payable(address(0))"

solArg :: AppT DLArg
solArg = \case
  DLA_Var v -> solVar v
  DLA_Constant c -> return $ solLit $ conCons' c
  DLA_Literal c -> return $ solLit c
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
asnArg' :: Bool -> Doc -> DLType -> Doc -> App Doc
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
      vsep <$> mapM go fieldAssigns
    False ->
      return $ dv <+> "=" <+> av <> semi

asnArg :: Bool -> Doc -> DLArg -> App Doc
asnArg usesStorage dv a = do
  let dt = argTypeOf a
  av <- solArg a
  asnArg' usesStorage dv dt av

solLargeArg' :: Bool -> Doc -> DLLargeArg -> App Doc
solLargeArg' usesStorage dv la =
  case la of
    DLLA_Array _ as -> c <$> (zipWithM go ([0 ..] :: [Int]) as)
      where
        go i a = do
          let name = dv <> "[" <> pretty i <> "]"
          asnArg usesStorage name a
    DLLA_Tuple as -> c <$> (zipWithM go ([0 ..] :: [Int]) as)
      where
        go i a = do
          let name = dv <> ".elem" <> pretty i
          asnArg usesStorage name a
    DLLA_Obj m ->
      solLargeArg' usesStorage dv $ DLLA_Struct $ map (first objPrefix) $ M.toAscList m
    DLLA_Data _ vn vv -> do
      t <- solType $ largeArgTypeOf la
      asnFields <- asnArg usesStorage (dv <> "._" <> pretty vn) vv
      return $ c [ one ".which" (solVariant t vn), asnFields ]
    DLLA_Struct kvs -> c <$> (mapM go kvs)
      where
        go (k, a) = do
          let name = dv <> "." <> pretty k
          asnArg usesStorage name a
    DLLA_Bytes s -> do
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
          let go i x = one (".elem" <> pretty i) (g2 x)
          return $ c $ zipWith go ([0 ..] :: [Int]) cs
    DLLA_StringDyn s ->
      return $ one "" $ solString $ T.unpack s
  where
    one :: Doc -> Doc -> Doc
    one f v = dv <> f <+> "=" <+> v <> semi
    c = vsep

solLargeArg :: Bool -> DLVar -> DLLargeArg -> App Doc
solLargeArg usesStorage dv la = flip (solLargeArg' usesStorage) la =<< solVar dv

mapRefArg :: DLArg -> App Doc
mapRefArg a = do
  let kt = argTypeOf a
  a' <- solArg a
  case M.lookup kt baseTypes of
    Just _ -> return $ a'
    Nothing -> return $ solHash [a']

solExpr :: Doc -> DLExpr -> App Doc
solExpr sp = \case
  DLE_Arg _ a -> spa $ solArg a
  DLE_LArg {} ->
    impossible "large arg"
  DLE_Impossible at _ (Err_Impossible_Case s) ->
    impossible $ "solExpr: impossible case `" <> s <> "` encountered at: " <> show at
  DLE_Impossible at _ err ->
    expect_thrown at err
  DLE_VerifyMuldiv at _ _ _ err ->
    expect_thrown at err
  DLE_PrimOp _ p args -> do
    args' <- mapM solArg args
    spa $ solPrimApply p args'
  DLE_ArrayRef _ ae ie ->
    spa $ (solArrayRef <$> solArg ae <*> solArg ie)
  DLE_ArraySet _ ae ie ve -> do
    args' <- mapM solArg [ae, ie, ve]
    ti <- solTypeI (argTypeOf ae)
    spa $ return $ solApply (solArraySet ti) args'
  DLE_ArrayConcat {} ->
    impossible "array concat"
  DLE_TupleRef _ ae i -> do
    ae' <- solArg ae
    return $ ae' <> ".elem" <> pretty i <> sp
  DLE_TupleSet _ tup_a index val_a -> do
    let tupFields = tupleTypes $ argTypeOf tup_a
    let tupLen = fromIntegral $ length tupFields
    tup_t <- solType $ argTypeOf tup_a
    tup' <- solArg tup_a
    val' <- solArg val_a
    let newField = "elem" <> pretty index <> ": " <> val'
    let copiedFields = map (\n -> "elem" <> n <> ": " <> tup' <> "." <> "elem" <> n) $
                         map pretty $ filter (/= index) [0..tupLen-1]
    let tupLiteral = braces $ comma_sep $ newField : copiedFields
    return $ tup_t <> parens tupLiteral
  DLE_ObjectRef _ oe f -> do
    oe' <- solArg oe
    let p = case argTypeOf oe of
          T_Struct {} -> id
          T_Object {} -> objPrefix
          _ -> impossible "objectref"
    return $ oe' <> "." <> p (pretty f) <> sp
  DLE_ObjectSet _ obj_a fieldName val_a -> do
    let objFields = M.fromList $ argObjstrTypes obj_a
    obj_t <- solType $ argTypeOf obj_a
    obj' <- solArg obj_a
    val' <- solArg val_a
    let newField = objPrefix $ pretty fieldName <> ": " <> val'
    let copiedFields = map (\fn -> objPrefix $ fn <> ": " <> obj' <> "." <> (objPrefix fn)) $
                         map (pretty . fst) $ M.toList $ M.delete fieldName objFields
    let objLiteral = braces $ comma_sep $ newField : copiedFields
    return $ obj_t <> parens objLiteral
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> do
    args' <- mapM solArg args
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
      require = solRequire (show (at, fs, mmsg)) =<< solArg a
  DLE_Transfer _ who amt mtok ->
    spa $ solTransfer who amt mtok
  DLE_TokenInit {} -> return emptyDoc
  DLE_TokenAccepted _ _ _ -> do
    return "(true)"
  DLE_CheckPay at fs amt mtok -> do
    let require :: String -> Doc -> App Doc
        require msg e = spa $ solRequire (show (at, fs, msg)) e
    amt' <- solArg amt
    case mtok of
      Nothing -> do
        cmp <- solEq "msg.value" amt'
        require "verify network token pay amount" cmp
      Just tok -> do
        tok' <- solArg tok
        require "verify non-network token pay amount" $
          solApply "checkPayAmt" ["msg.sender", tok', amt']
  DLE_Wait {} -> return emptyDoc
  DLE_PartSet _ _ a -> spa $ solArg a
  DLE_MapRef _ mpv fa -> do
    fa' <- mapRefArg fa
    return $ solApply (solMapRefInt mpv) [fa'] <> sp
  DLE_MapSet _ mpv fa (Just na) -> do
    fa' <- mapRefArg fa
    solLargeArg' True (solArrayRef (solMapVar mpv) fa') nla
    where
      nla = mdaToMaybeLA na_t (Just na)
      na_t = argTypeOf na
  DLE_MapSet _ mpv fa Nothing -> do
    fa' <- mapRefArg fa
    return $ "delete" <+> solArrayRef (solMapVar mpv) fa' <> sp
  DLE_Remote {} -> impossible "remote"
  DLE_TokenNew _ (DLTokenNew {..}) -> do
    let go a = do
          a' <- solArg a
          uint8ArrayToString (bytesTypeLen $ argTypeOf a) a'
    n' <- go dtn_name
    s' <- go dtn_sym
    u' <- go dtn_url
    m' <- go dtn_metadata
    p' <- solArg dtn_supply
    d' <- maybe (return $ solLit $ DLL_Int sb UI_Word 18) solArg dtn_decimals
    return $ solApply "payable" [solApply "address" ["new" <+> solApply "ReachToken" [n', s', u', m', p', d']]]
  DLE_TokenBurn _ ta aa -> do
    ta' <- solArg ta
    aa' <- solArg aa
    return $ solApply "safeReachTokenBurn" [ta', aa'] <> sp
  DLE_TokenDestroy _ ta -> do
    ta' <- solArg ta
    return $ solApply "safeReachTokenDestroy" [ta'] <> sp
  DLE_TimeOrder {} -> impossible "timeorder"
  DLE_EmitLog {} -> impossible "emitLog"
  DLE_setApiDetails {} -> impossible "setApiDetails"
  DLE_DataTag _ d -> do
    d' <- solArg d
    return $ solApply "uint256" [d' <> ".which"]
  DLE_FromSome _ mo da -> do
    mo' <- solArg mo
    da' <- solArg da
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
  who' <- solArg who
  amt' <- solArg amt
  case mtok of
    Nothing ->
      return $ who' <> "." <> solApply "transfer" [amt']
    Just tok -> do
      tok' <- solArg tok
      return $ solApply "safeTokenTransfer" [tok', who', amt']

solEvent :: Int -> [DLVar] -> App Doc
solEvent which msg = do
  arg_ty' <- solArgType Nothing msg
  return $ "event" <+> solApply (solMsg_evt which) ["address _who", arg_ty' <+> "_a"] <> semi

solEventEmit :: Int -> Doc
solEventEmit which =
  "emit" <+> solApply (solMsg_evt which) ["msg.sender", "_a"] <> semi

solAsnType :: [DLVar] -> App Doc
solAsnType = solType . vsToType

solAsnSet :: Doc -> [(DLVar, DLArg)] -> App [Doc]
solAsnSet asnv vs = do
  let go (v, a) = solSet (asnv <> "." <> solRawVar v) <$> solArg a
  vs' <- mapM go vs
  vs_ty' <- solAsnType $ map fst vs
  return $ [solDecl asnv (mayMemSol vs_ty') <> semi] <> vs'

solStateSet :: Int -> [(DLVar, DLArg)] -> App [Doc]
solStateSet which svs = do
  let asnv = "nsvs"
  setl <- solAsnSet asnv svs
  return $
    setl
      <> [ solSet "current_step" (solNum which)
         , solSet "current_time" solBlockTime
         , solSet "current_svbs" (solEncode ["nsvs"])
         ]

solStateCheck :: Int -> App [(String, Doc)]
solStateCheck prev = do
  s <- solEq "current_step" (solNum prev)
  zeq <- solEq "_a.time" $ solNum (0 :: Int)
  teq <- solEq "current_time" "_a.time"
  let t = solBinOp "||" (parens zeq) (parens teq)
  return [("step", s), ("time", t)]

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
    T_Bytes sz -> sz
    T_Array _ sz -> sz
    _ -> impossible "arraySize"

solSwitch :: AppT k -> SrcLoc -> DLVar -> SwitchCases k -> App Doc
solSwitch iter _at ov csm = do
  ovp <- solVar ov
  t <- solType $ argTypeOf (DLA_Var ov)
  let cm1 (vn, (ov', vu, body)) = do
        c <- solEq (ovp <> ".which") (solVariant t vn)
        set' <-
          case vu of
            True -> do
              addMemVar ov'
              return $ solSet (solMemVar ov') (ovp <> "._" <> pretty vn)
            False -> return $ emptyDoc
        body' <- iter body
        let set_and_body' = vsep [set', body']
        return (c, set_and_body')
  solIfs <$> (mapM cm1 $ M.toAscList csm)

withArgLoc :: DLType -> Doc
withArgLoc t =
  solArgLoc $
    case mustBeMem t of
      True -> AM_Memory
      False -> AM_Event

solType_withArgLoc :: DLType -> App Doc
solType_withArgLoc t =
  (<> (withArgLoc t)) <$> solType t

doConcat :: DLVar -> DLArg -> DLArg -> App Doc
doConcat dv x y = do
  addMemVar dv
  dv' <- solVar dv
  let copy src (off :: Integer) = do
        let sz = arraySize src
        src' <- solArg src
        add <- solPrimApply (ADD UI_Word PV_Veri) ["i", solNum off]
        let ref = solArrayRef src' "i"
        return $ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++") <> solBraces (solArrayRef dv' add <+> "=" <+> ref <> semi)
  x' <- copy x 0
  y' <- copy y (arraySize x)
  return $ vsep [x', y']

getBalance :: Doc -> Doc
getBalance tok = solApply "tokenBalanceOf" [tok, "address(this)"]

solCom :: AppT DLStmt
solCom = \case
  DL_Nop _ -> mempty
  DL_Let _ pv (DLE_Remote at fs av rng_ty dr) -> do
    let DLRemote mf (DLPayAmt net ks) as (DLWithBill _nRecv nonNetTokRecv nnTokRecvZero) _ = dr
    let f = fromMaybe (impossible "remote no fun") mf
    -- XXX make this not rely on pv
    av' <- solArg av
    as' <- mapM solArg as
    dom'mem <- mapM (solType_withArgLoc . argTypeOf) as
    let dv =
          case pv of
            DLV_Eff -> impossible "remote result unbound"
            DLV_Let _ x -> x
    rng_ty'_ <- solType_ rng_ty
    rng_ty' <- solType rng_ty
    let rng_ty'mem = rng_ty' <> withArgLoc rng_ty
    f' <- addInterface (pretty f) dom'mem rng_ty'mem
    let eargs = f' : as'
    v_succ <- allocVar
    v_return <- allocVar
    v_before <- allocMemVar at $ T_UInt UI_Word
    -- Note: Not checking that the address is really a contract and not doing
    -- exactly what OpenZeppelin does
    netTokPaid <- solArg net
    ks' <- mapM (\(amt, ty) -> (,) <$> solArg amt <*> solArg ty) ks
    nonNetTokApprovals <-
      mapM
        (\(amt, ty) -> do
           let approve = solApply "tokenApprove" [ty, av', amt]
           flip (<>) semi <$> solRequire "Approving remote ctc to transfer tokens" approve)
        ks'
    checkNonNetTokAllowances <-
      mapM
        (\(_, ty) -> do
           -- Ensure that the remote ctc transfered the approved funds
           let allowance = solApply "tokenAllowance" [ty, "address(this)", av']
           eq <- solEq allowance "0"
           req <- solRequire "Ensure remote ctc transferred approved funds" eq
           return $ vsep [req <> semi])
        ks'
    -- This is for when we don't know how much non-net tokens we will receive. i.e. `withBill`
    -- The amount of non-network tokens received will be stored in this tuple: nnTokRecvVar
    (getDynamicNonNetTokBals, setDynamicNonNetTokBals) <-
      unzip
        <$> mapM
          (\(tok, i :: Int) -> do
             tv_before <- allocMemVar at $ T_UInt UI_Word
             tokArg <- solArg tok
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
             tokArg <- solArg tok
             paid <- maybe (return "0") solArg $ M.lookup tok nonNetToksPayAmt
             sub <- solPrimApply (SUB UI_Word PV_Veri) [getBalance tokArg, paid]
             let s1 = solSet tv_before sub
             tv_after <- allocMemVar at $ T_UInt UI_Word
             tokRecv <- solPrimApply (SUB UI_Word PV_Veri) [getBalance tokArg, tv_before]
             let s2 = solSet tv_after tokRecv
             s3 <- solRequire "remote did not transfer unexpected non-network tokens" =<< solEq tv_after "0"
             return (s1, s2 <> s3 <> semi))
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
    e_data <- allocVar
    e_before <- solPrimApply (SUB UI_Word PV_Veri) [meBalance, netTokPaid]
    err_msg <- solRequireMsg $ show (at, fs, ("remote " <> f <> " failed"))
    -- XXX we could assert that the balances of all our tokens is the same as
    -- it was before
    return $ solBraces $
      vsep $
        nonNetTokApprovals
          <> getDynamicNonNetTokBals
          <> getUnexpectedNonNetTokBals
          <> [ solSet v_before e_before
             , solSet ("bytes memory" <+> e_data) e_data_e
             , "(bool " <> v_succ <> ", bytes memory " <> v_return <> ")" <+> "=" <+> av' <> solApply call' [e_data] <> semi
             , solApply "checkFunReturn" [v_succ, v_return, err_msg] <> semi
             ]
          <> checkUnexpectedNonNetTokBals
          <> setDynamicNonNetTokBals
          <> checkNonNetTokAllowances
          <> sub'l
          <> pv'
  DL_Let _ pv (DLE_EmitLog _ lk lvs) -> do
    lvs' <- mapM solVar lvs
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
    let ed = "event" <+> go solRawVar eventVars
    modifyCtxtIO ctxt_outputs $ M.insert (show oe) ed
    let emitVars = map (mempty,) lvs'
    let emitl = "emit" <+> go id emitVars
    asn <- case (lk, lvs') of
      (L_Api p, [v]) -> do
        return $ solSet (apiRetMemVar $ bunpack p) v
      (_, _) -> return ""
    case pv of
      DLV_Eff -> do
        return $ vsep [emitl, asn]
      DLV_Let _ dv -> do
        addMemVar dv
        v' <- case lvs of
          [h] -> solVar h
          _ -> impossible "solCom: emitLog expected one value"
        return $ vsep [solSet (solMemVar dv) v', emitl, asn]
  DL_Let _ (DLV_Let _ dv) (DLE_LArg _ la) -> do
    addMemVar dv
    solLargeArg False dv la
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ GET_COMPANION []) -> do
    addMemVar dv
    solLargeArg False dv $ mdaToMaybeLA T_Contract Nothing
  DL_Let _ (DLV_Eff) (DLE_GetUntrackedFunds {}) ->
    return ""
  DL_Let _ (DLV_Let _ dv) (DLE_GetUntrackedFunds at mtok tb) -> do
    addMemVar dv
    actBalV <- allocVar
    tb' <- solArg tb
    bal <- case mtok of
      Nothing -> return "address(this).balance"
      Just tok -> getBalance <$> solArg tok
    sub <- solPrimApply (SUB UI_Word PV_Veri) [actBalV, tb']
    zero <- solArg $ DLA_Literal $ DLL_Int at UI_Word 0
    cnd <- solPrimApply (PLT UI_Word) [actBalV, tb']
    ite <- solPrimApply IF_THEN_ELSE [cnd, zero, sub]
    return $ solBraces $ vsep $
      [ solSet ("uint256" <+> actBalV) bal
      , solSet (solMemVar dv) ite
      ]
  DL_Let _ (DLV_Let _ dv) (DLE_ContractFromAddress _at addr) -> do
    addr' <- solArg addr
    let isContract = parens $ addr' <> ".code.length > 0"
    addMemVar dv
    dv' <- solVar dv
    trueCase <- solLargeArg' False dv' $ mdaToMaybeLA T_Contract $ Just addr
    falseCase <- solLargeArg' False dv' $ mdaToMaybeLA T_Contract Nothing
    return $ solIf isContract trueCase falseCase
  DL_Let _ (DLV_Eff) (DLE_ContractFromAddress _at _addr) -> do
    return ""
  DL_Let _ (DLV_Eff) (DLE_ContractNew {}) ->
    return ""
  DL_Let _ (DLV_Let _ dv) (DLE_ContractNew _at cns dr) -> do
    let DLRemote _ _ as _ _ = dr
    let DLContractNew {..} = cns M.! conName'
    let (bc :: String) = either impossible id $ aesonParse dcn_code
    addMemVar dv
    bc' <- allocVar
    as'bs <- allocVar
    bc'' <- allocVar
    ctc' <- allocVar
    let pay' = "0"
    let p' = solApply "add" [ bc'', "0x20" ]
    let len' = solApply "mload" [ bc'' ]
    let asm = vsep $
          [ ctc' <+> ":=" <+> solApply "create" [ pay', p', len' ]
          ]
    --- XXX support payment and bills
    as' <- mapM solArg as
    chk' <- solRequire "new contract not zero" $ ctc' <+> "!= address(0)"
    return $ solBraces $ vsep $
      [ solSet ("bytes memory" <+> bc') (pretty $ "hex\"" <> bc <> "\"")
      , solSet ("bytes memory" <+> as'bs) (solApply "abi.encode" as')
      , solSet ("bytes memory" <+> bc'') (solApply "bytes.concat" [ bc', as'bs ])
      , "address payable" <+> ctc' <> semi
      , "assembly" <+> solBraces asm
      , solSet (solMemVar dv) ctc'
      , chk' <> semi
      ]
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BYTES_ZPAD _) [x]) -> do
    addMemVar dv
    dv' <- solVar dv
    x' <- solArg x
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
    return $ vsep $ solBytesSplit sz goSmall goBig
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ BYTES_XOR [x, y]) -> do
    addMemVar dv
    dv' <- solVar dv
    let bl = bytesTypeLen $ argTypeOf x
    x' <- solArg x
    y' <- solArg y
    let goSmall _ = dv' <+> "=" <+> x' <+> "^" <+> y' <> semi
    let goBig i _ =  dv' <> ei <+> "=" <+> x' <> ei <+> "^" <+> y' <+> ei <> semi
            where ei = ".elem" <> pretty i
    return $ vsep $ solBytesSplit bl goSmall goBig
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BTOI_LAST8 True) [x]) -> do
    addMemVar dv
    dv' <- solVar dv
    x' <- solArg x
    return $ dv' <+> "=" <+> "uint64" <> parens x' <> semi
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ (BTOI_LAST8 {}) [x]) -> do
    addMemVar dv
    dv' <- solVar dv
    x' <- solArg x
    let (howMany, lastLen) = solBytesInfo $ bytesTypeLen $ argTypeOf x
    let go :: Integer -> Integer -> Integer -> Doc
        go elemIdx from to =  "for(uint i = " <> pretty from <> "; i < " <> pretty to <> "; i++) {" <> hardline <>
                                  dv' <+> "=" <+> parens (dv' <+> "* 256") <+> "+" <+> "uint8" <> parens ("bytes1" <> parens (x'' <> brackets "i")) <> semi <>
                                  hardline <> "}"
          where
            x'' = case howMany == 1 of
                  True -> x'
                  False -> x' <> ".elem" <> pretty elemIdx
    let (res:: [Doc]) = case (lastLen < 8, howMany == 1) of
              -- The last chunk is >= 8 bytes: take 8 bytes
              (False, _) -> [go lastChunk (lastLen - 8) lastLen]
              -- One byte chunk that's < 8 Bytes: take as many as you can
              (True, True)  -> [go lastChunk 0 lastLen]
              -- Multiple byte chunks with last chunk < 8 Bytes: take all from last chunk and rest from previous
              (True, False) -> [go (howMany - 2) (byteChunkSize - lastLen) byteChunkSize, go lastChunk 0 lastLen]
              where
                lastChunk = howMany - 1
    return $ vsep res
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
            extendVarMap $ M.singleton dv de'
            recordDepth dv dp
            mempty
      def = do
        addMemVar dv
        de' <- solExpr emptyDoc de
        return $ solSet (solMemVar dv) de'
  DL_Let _ _ (DLE_setApiDetails {}) -> mempty
  DL_Let _ DLV_Eff de -> solExpr semi de
  DL_Var _ dv -> do
    addMemVar dv
    mempty
  DL_Set _ dv da -> solSet (solMemVar dv) <$> solArg da
  DL_LocalIf _ _ ca t f ->
    solIf <$> solArg ca <*> solPLTail t <*> solPLTail f
  DL_LocalSwitch at ov csm -> solSwitch solPLTail at ov csm
  DL_Only {} -> impossible $ "only in CT"
  DL_ArrayMap _ ans xs as i (DLBlock _ _ f r) -> do
    addMemVars $ [ans] <> as
    let sz = arraysLength xs
    ans' <- solVar ans
    xs' <- mapM solArg xs
    as' <- mapM solVar as
    let i' = solRawVar i
    extendVarMap $ M.singleton i i'
    f' <- solPLTail f
    r' <- solArg r
    return $
      vsep
        [ "for" <+> parens ("uint256 " <> i' <> " = 0" <> semi <+> i' <> " <" <+> (pretty sz) <> semi <+> i' <> "++")
            <> solBraces
              (vsep $
                 zipWith (\a x -> a <+> "=" <+> (solArrayRef x i') <> semi) as' xs'
                 <>
                 [ f'
                 , (solArrayRef ans' i') <+> "=" <+> r' <> semi
                 ])
        ]
  DL_ArrayReduce _ ans xs z b as i (DLBlock _ _ f r) -> do
    addMemVars $ [ans, b] <> as
    let sz = arraysLength xs
    ans' <- solVar ans
    xs' <- mapM solArg xs
    z' <- solArg z
    as' <- mapM solVar as
    b' <- solVar b
    let i' = solRawVar i
    extendVarMap $ M.singleton i i'
    f' <- solPLTail f
    r' <- solArg r
    return $
      vsep
        [ b' <+> "=" <+> z' <> semi
        , "for" <+> parens ("uint256 " <> i' <> " = 0" <> semi <+> i' <> " <" <+> (pretty sz) <> semi <+> i' <> "++")
            <> solBraces
              (vsep $
                 zipWith (\a x -> a <+> "=" <+> (solArrayRef x i') <> semi) as' xs'
                 <>
                 [ f'
                 , b' <+> "=" <+> r' <> semi
                 ])
        , ans' <+> "=" <+> b' <> semi
        ]
  DL_MapReduce {} ->
    impossible $ "cannot inspect maps at runtime"
  DL_LocalDo _ _ t -> solPLTail t

solCom_ :: AppT a -> DLStmt -> AppT a
solCom_ iter m k = do
  m' <- solCom m
  k' <- iter k
  return $
    case m' == emptyDoc of
      True -> k'
      False -> m' <> hardline <> k'

solPLTail :: AppT DLTail
solPLTail = \case
  DT_Return _ -> mempty
  DT_Com m k -> solCom_ solPLTail m k

solCTail :: AppT CTail
solCTail = \case
  CT_Com m k -> solCom_ solCTail m k
  CT_If _ ca t f -> solIf <$> solArg ca <*> solCTail t <*> solCTail f
  CT_Switch at ov csm -> solSwitch solCTail at ov csm
  CT_Jump _ which svs (DLAssignment asnm) -> do
    let go_svs v = solSet ("la.svs." <> solRawVar v) <$> solVar v
    svs' <- mapM go_svs svs
    let go_asn (v, a) = solSet ("la.msg." <> solRawVar v) <$> solArg a
    asn' <- mapM go_asn (M.toAscList asnm)
    argDefn <- solArgDefn_ "la" AM_Memory (Just svs) (map fst $ M.toAscList asnm)
    return $
      vsep $
        [argDefn <> semi]
          <> svs'
          <> asn'
          <> [solApply (solLoop_fun which) ["la"] <> semi]
  CT_From _ which (FI_Continue svs) -> do
    vsep <$> solStateSet which svs
  CT_From _ _ (FI_Halt _toks) -> do
    return $
      vsep $
        [ solSet "current_step" "0x0"
        , solSet "current_time" "0x0"
        , "delete current_svbs;"
        -- We could "selfdestruct" our token holdings, based on _toks
        --
        -- , solApply "selfdestruct" ["payable(msg.sender)"] <> semi
        --
        -- However, we don't do either of these, because selfdestruct-ing is
        -- dangerous, because although the contract is gone, that means the
        -- messages sent to it are like no-ops, so they can take funds from
        -- clients that think they are calling real contracts. Ideally, we'd be
        -- able to get back our funds (e.g. on Conflux where there are storage
        -- costs), we would rather not get those, than have users lose funds to
        -- dead contracts.
        ]

solFrame :: Int -> S.Set DLVar -> App (Doc, Doc)
solFrame i sim = do
  let mk_field dv@(DLVar _ _ t _) = (,) (solRawVar dv) <$> (solType t)
  fs <- mapM mk_field $ S.elems sim
  let framei = pretty $ "_F" ++ show i
  case solStruct framei fs of
    Nothing -> return $ (emptyDoc, emptyDoc)
    Just frame_defp -> do
      let frame_declp = (framei <+> "memory _f") <> semi
      return $ (frame_defp, frame_declp)

solCTail_top :: Int -> (DLVar -> Doc) -> [DLVarLet] -> [DLVarLet] -> Bool -> CTail -> App (Doc, Doc, Doc)
solCTail_top which svar svs msg shouldEmit ct = do
  let svsm = M.fromList $ map (\v -> (v, svar v)) $ map varLetVar svs
  let msgm = M.fromList $ map (\v -> (v, solArgMsgVar v)) $ map varLetVar msg
  let emitp = case shouldEmit of
        True -> solEventEmit which
        False -> emptyDoc
  extendVarMap $ svsm <> msgm
  ct' <- local (\e -> e {ctxt_handler_num = which}) $ do
    solCTail ct
  mvars <- readMemVars
  (frameDefn, frameDecl) <- solFrame which mvars
  return (frameDefn, frameDecl, vsep [emitp, ct'])

solArgType :: Maybe [DLVar] -> [DLVar] -> App Doc
solArgType msvs msg = do
  let fst_part =
        case msvs of
          Nothing -> ("time", T_UInt UI_Word)
          Just svs -> ("svs", vsToType svs)
  let msg_ty = vsToType msg
  let arg_ty = T_Struct $ [fst_part, ("msg", msg_ty)]
  solType arg_ty

solArgDefn_ :: Doc -> ArgMode -> Maybe [DLVar] -> [DLVar] -> App Doc
solArgDefn_ name am msvs msg = do
  arg_ty' <- solArgType msvs msg
  return $ solDecl name (arg_ty' <> solArgLoc am)

solArgDefn :: ArgMode -> Maybe [DLVar] -> [DLVar] -> App Doc
solArgDefn = solArgDefn_ "_a"

solHandler :: Int -> CHandler -> App Doc
solHandler which h = freshVarMap $
  case h of
    C_Handler at interval from prev svsl msgl timev secsv ct -> do
      let svs = map varLetVar svsl
      let msg = map varLetVar msgl
      which_msg_r <- asks ctxt_which_msg
      liftIO $ modifyIORef which_msg_r $ M.insert which msg
      let checkMsg s = s <> " check at " <> show at
      let fromm = M.singleton from "payable(msg.sender)"
      let given_mm = M.fromList [(timev, solBlockTime), (secsv, solBlockSecs)]
      extendVarMap $ given_mm <> fromm
      (frameDefn, frameDecl, ctp) <- solCTail_top which solSVSVar svsl msgl True ct
      evtDefn <- solEvent which msg
      let ret = "payable"
      (hc_reqs, svs_init, am, sfl) <-
        case which == 0 of
          True -> do
            let inits = [solSet "creation_time" solBlockTime]
            return (mempty, inits, AM_Memory, SFL_Constructor)
          False -> do
            csv <- solStateCheck prev
            svs_ty' <- solAsnType svs
            let csvs = [solSet (parens $ solDecl "_svs" (mayMemSol svs_ty')) $ solApply "abi.decode" ["current_svbs", parens svs_ty']]
            return (csv, csvs, AM_Call, SFL_Function True (solMsg_fun which))
      let hc_go lab chk =
            (<> semi) <$> solRequire (checkMsg $ "state " <> lab) chk
      hashCheck <- mapM (uncurry hc_go) hc_reqs
      -- This is a re-entrancy lock, because we know that
      -- all methods start by checking the current state.  When we
      -- implement on-chain state, we need to do this differently
      let lock = [solSet "current_step" "0x0"]
      argDefn <- solArgDefn am Nothing msg
      let req x = flip (<>) semi <$> solRequire (checkMsg "timeout") x
      let checkTime1 op ta = do
            let (v, a) = case ta of
                  Left x -> (timev, x)
                  Right x -> (secsv, x)
            v' <- solVar v
            a' <- solArg a
            req =<< solPrimApply op [v', a']
      let checkTime op = \case
            Nothing -> return []
            Just x -> (\y -> [y]) <$> checkTime1 op x
      let CBetween ifrom ito = interval
      timeoutCheck <- vsep <$> ((<>) <$> checkTime (PGE UI_Word) ifrom <*> checkTime (PLT UI_Word) ito)
      let body = vsep $ hashCheck <> lock <> svs_init <> [frameDecl, timeoutCheck, ctp]
      let mkFun args b = solFunctionLike sfl args ret b
      uses_apis <- asks ctxt_uses_apis
      funDefs <-
        case (uses_apis, sfl) of
          (True, SFL_Function _ name) -> do
            let createStruct = solDecl "_r" (apiRngTy <> solArgLoc AM_Memory) <> semi
            let callFun = solApply name ["_a", "_r"] <> semi
            let callBody = vsep [createStruct, callFun]
            let apiRetDefn = solDecl apiRetVar (apiRngTy <> solArgLoc AM_Memory)
            intArg <- solArgDefn AM_Memory Nothing msg
            return
              [ mkFun [argDefn] callBody
              , solFunction name [intArg, apiRetDefn] "internal " body
              ]
          _ ->
            return [mkFun [argDefn] body]
      return $ vsep $ [evtDefn, frameDefn] <> funDefs
    C_Loop _at svsl msgl ct -> do
      let svs = map varLetVar svsl
      let msg = map varLetVar msgl
      (frameDefn, frameDecl, ctp) <- solCTail_top which solArgSVSVar svsl msgl False ct
      argDefn <- solArgDefn AM_Memory (Just $ svs) msg
      let ret = "internal"
      let body = vsep [frameDecl, ctp]
      let funDefn = solFunction (solLoop_fun which) [argDefn] ret body
      return $ vsep [frameDefn, funDefn]

solHandlers :: CHandlers -> App Doc
solHandlers (CHandlers hs) =
  vsep <$> (mapM (uncurry solHandler) $ M.toList hs)

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

funRetSig :: DLType -> Bool -> App Doc
funRetSig ret_ty ext = do
  ret_ty' <- solType_ ret_ty
  let ret_ty'' = ret_ty' <+> withArgLoc ret_ty
  let external = case ext of
        True -> "external payable returns"
        False -> "internal returns"
  return $ external <+> parens ret_ty''

apiArgs :: Doc -> ApiInfo -> App ([Doc], [Doc], [Doc], Doc)
apiArgs tyMsg (ApiInfo {..}) = do
  which_msg <- (liftIO . readIORef) =<< asks ctxt_which_msg
  ai_msg_vs <- case M.lookup ai_which which_msg of
    Just vs -> return vs
    Nothing -> impossible "apiDef: no which"
  m_arg_ty <- solArgType Nothing ai_msg_vs
  let mkArgDefns ts = do
        let indexedTypes = zip ts [0 ..]
        unzip <$> mapM (\(ty, i :: Int) -> do
                      let name = pretty $ "_a" <> show i
                      sol_ty <- solType ty
                      let decl = solDecl name (sol_ty <> withArgLoc ty)
                      return (name, decl))
                  indexedTypes
  let makeT1 :: Doc -> (Int, Doc) -> App [ Doc ]
      makeT1 n (i, a) = return $ [ n <> ".elem" <> pretty i <> " = " <> a <> ";" ]
      makeT :: Doc -> [Doc] -> App [Doc]
      makeT n as = concatMapM (makeT1 n) $ zip ([0 ..]::[Int]) as
      makeTV :: DLVar -> Doc
      makeTV tv = tyMsg <> "." <> solRawVar tv
  let go = \case
        AIC_Case -> do
          let c_id_s = fromMaybe (impossible "Expected case id") ai_mcase_id
          let c_id = pretty c_id_s
          -- Construct product of data variant
          (tv, data_t, argDefns, args) <-
            case (ai_msg_vs, ai_msg_tys) of
              ([v], [dt@(T_Data env)]) ->
                case M.lookup c_id_s env of
                  Just (T_Tuple []) -> return (v, dt, [], [])
                  Just (T_Tuple ts) -> do
                    (args, argDefns) <- mkArgDefns ts
                    return $ (v, dt, argDefns, args)
                  _ -> impossible "apiDef: Constructor not in Data"
              _ -> impossible "apiDef: Expected one `Data` arg"
          dt <- solType_ data_t
          let lifts1 = [ makeTV tv <> ".which = _enum_" <> dt <> "." <> c_id <> semi ]
          lifts2 <- makeT (makeTV tv <> "._" <> c_id) args
          let lifts = lifts1 <> lifts2
          return $ (argDefns, lifts, args, m_arg_ty)
        AIC_SpreadArg -> do
          case (ai_msg_vs, ai_msg_tys) of
            ([tv], [T_Tuple ts]) -> do
              (args, argDefns) <- mkArgDefns ts
              lifts <-
                case ts of
                  [] -> return [ (makeTV tv) <> " = false;" ]
                  _ -> makeT (makeTV tv) args
              return $ (argDefns, lifts, args, m_arg_ty)
            _ -> impossible "apiDef: Expected one tuple arg"
  go ai_compile

apiDef :: SLPart -> Bool -> ApiInfo -> App Doc
apiDef who qualify ApiInfo {..} = do
  let who_orig = (bunpack who)
  let who_s = adjustApiName who_orig ai_which qualify
  let mf = solMsg_fun ai_which
  (argDefns, tyLifts, args, m_arg_ty) <- apiArgs "_t.msg" $ ApiInfo {..}
  when (length args > apiMaxArgs) $
    expect_throw Nothing ai_at $ Err_SolTooManyArgs "APIs" (bunpack who) (length args)
  let body =
        vsep $
          [ m_arg_ty <+> "memory _t;"
          ] <>
          tyLifts <>
          [ solBraces (vsep $
            [ "ApiRng memory _r;"
            , solApply mf ["_t", "_r"] <> semi
            , pretty ("return _r." <> who_orig) <> semi
            ])
          ]
  retExt <- funRetSig ai_ret_ty True
  retInt <- funRetSig ai_ret_ty False
  let internalName = "_reach_internal_" <> pretty who_s
  let mk w ret = solFunction (pretty w) argDefns ret
  let extBody = "return " <> solApply internalName args <> semi
  let alias = case bunpack <$> ai_alias of
              Just ai -> do
                [mk ai retExt extBody]
              Nothing -> []
  return $ vsep $ mk internalName retInt body : (mk who_s retExt extBody) : alias

genApiJump :: SLPart -> M.Map Int ApiInfo -> App Doc
genApiJump p ms = do
  let who = pretty $ bunpack p
  let ai = fromMaybe (impossible "genApiJump") $ headMay $ M.elems ms
  (argDefns, _, args, _) <- apiArgs (impossible "ty") ai
  let whichs = map ai_which $ M.elems ms
  let chk_which w = solBinOp "==" "current_step" $ pretty w
  let chk_st = concatWith (\ l r -> l <> " || " <> r) $ map chk_which whichs
  let require = solApply "require" [chk_st] <> semi
  let mk w = solWhen (chk_which w) thn
        where
          thn = "return " <> solApply ("_reach_internal_" <> inst) args <> semi <> hardline
          inst = "_" <> who <> pretty w
  let go = vsep $ map (mk . fst) $ M.toAscList ms
  ret <- funRetSig (ai_ret_ty ai) True
  let body = vsep $ require : [go]
  return $ solFunction who argDefns ret $ body

apiDefs :: ApiInfos -> App Doc
apiDefs defs = do
  let defL = M.toList defs
  defs' <- forM defL $ \ (p, ms) -> do
            let qualify = M.size ms > 1
            ds <- mapM (apiDef p qualify . snd) $ M.toAscList ms
            case qualify of
              True  -> do
                d <- genApiJump p ms
                return $ d : ds
              False -> return ds
  return $ vsep $ concat defs'

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
    let inmem = solArgLoc AM_Memory
    let memem = me <> inmem
    let tnmem = tn <> (if mustBeMem et then inmem else "")
    let args =
          [ solDecl "arr" memem
          , solDecl "idx" "uint256"
          , solDecl "val" tnmem
          ]
    let ret = "pure internal" <+> "returns" <+> parens (solDecl "arrp" memem)
    let assign idx val = (solArrayRef "arrp" idx) <+> "=" <+> val <> semi
    let body =
          vsep
            [ ("for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
                 <> solBraces (assign "i" (solArrayRef "arr" "i")))
            , assign "idx" "val"
            ]
    addMap me
    i <- addId
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
    addDef i $ vsep [enump, structp]
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

solEB :: [DLVar] -> AppT DLExportBlock
solEB args (DLinExportBlock _ mfargls (DLBlock _ _ t r)) = do
  let fargs = map varLetVar $ fromMaybe mempty mfargls
  let go a fa = do
        a' <- solVar a
        return $ (fa, a')
  extendVarMap =<< (M.fromList <$> zipWithM go args fargs)
  t' <- solPLTail t
  r' <- solArg r
  return $ vsep [t', "return" <+> r' <> semi]

createAPIRng :: ApiInfos -> App Doc
createAPIRng env =
  case M.null env of
    True -> return ""
    False -> do
      fields <- fmap concat $ forM (M.toAscList env) $ \(k, ms) -> do
          let qualify = M.size ms > 1
          let k' = bunpack k
          fs <- mapM (\ (_w, ai) -> do
                  let n = pretty k'
                  t <- solType_ $ ai_ret_ty ai
                  return (n, t)
                ) $ M.toAscList ms
          case qualify of
            True  -> do
              let one = fromMaybe (impossible "createApiRng: empty list") $ headMay fs
              return $ [one]
            False -> return fs

      return $ fromMaybe (impossible "createAPIRng") $ solStruct "ApiRng" fields

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

contractId :: String
contractId = "ReachContract"

solPLProg :: PLProg -> IO (ConnectorObject, Doc)
solPLProg PLProg {plp_opts = plo, plp_init = dli, plp_cpprog = CPProg { cpp_at = at, cpp_views = (vs, vi), cpp_apis = ai, cpp_handlers = hs} } = do
  let DLInit {..} = dli
  let ctxt_handler_num = 0
  ctxt_varm <- newIORef mempty
  ctxt_mvars <- newIORef mempty
  ctxt_depths <- newIORef mempty
  ctxt_typei <- newIORef mempty
  ctxt_typem <- newIORef baseTypes
  ctxt_typef <- newIORef mempty
  ctxt_typed <- newIORef mempty
  ctxt_typeidx <- newCounter 0
  ctxt_intidx <- newCounter 0
  ctxt_requireMsg <- newCounter 7
  ctxt_ints <- newIORef mempty
  ctxt_outputs <- newIORef mempty
  ctxt_tlfuns <- newIORef mempty
  ctxt_which_msg <- newIORef mempty
  let ctxt_plo = plo
  let ctxt_uses_apis = not $ M.null ai
  flip runReaderT (SolCtxt {..}) $ do
    let map_defn (mpv, mi) = do
          let mk = dlmi_kt mi
          let kt = maybe (T_UInt UI_Word) (const mk) $ M.lookup mk baseTypes
          keyTy <- solType_ kt
          let mt = dlmi_tym mi
          valTy <- solType mt
          let args = [solDecl "addr" keyTy]
          let ret = "view returns (" <> valTy <> " memory res)"
          let ext_ret = "external " <> ret
          let int_ret = "internal " <> ret
          let ref = (solArrayRef (solMapVar mpv) "addr")
          do_none <- solLargeArg' False "res" $ mdaToMaybeLA (dlmi_ty mi) Nothing
          let do_some = solSet "res" ref
          eq <- solEq (ref <> ".which") (solVariant valTy "Some")
          let int_defn =
                solFunction (solMapRefInt mpv) args int_ret $
                  solIf eq do_some do_none
          let ext_defn =
                solFunction (solMapRefExt mpv) args ext_ret $
                  solSet "res" (solApply (solMapRefInt mpv) ["addr"])
          return $
            vsep $
              [ "mapping (" <> keyTy <> " => " <> valTy <> ") " <> solMapVar mpv <> semi
              , int_defn
              , ext_defn
              ]
    map_defns <- mapM map_defn (M.toList dli_maps)
    let tgo :: Maybe SLPart -> (SLVar, DLView) -> App ([(T.Text, Aeson.Value)], Doc)
        tgo v (k, (t, aliases)) = do
          let mk kv = maybe kv (\v' -> bunpack v' <> "_" <> kv) v
          let vk_ = mk k
          let vk = pretty $ vk_
          let (dom, rng) = itype2arr t
          args <- mapM (allocDLVar at) dom
          when (length args > apiMaxArgs) $
            expect_throw Nothing at $ Err_SolTooManyArgs "Views" vk_ (length args)
          let mkargvm arg = (arg, solRawVar arg)
          extendVarMap $ M.fromList $ map mkargvm args
          let solType_p am ty = do
                ty' <- solType ty
                let loc = solArgLoc $ if mustBeMem ty then am else AM_Event
                return $ ty' <> loc
          let mkdom arg argt = do
                argt' <- solType_p AM_Call argt
                return $ solDecl (solRawVar arg) argt'
          wrappedDomTy <- solType $ T_Tuple dom
          dom' <- zipWithM mkdom args dom
          rng' <- solType_p AM_Memory rng
          let ret = "external view returns" <+> parens rng'
          let retWrapped = "internal view returns" <+> parens (rng' <> " _viewRet")
          let vkWrapped = vk <> "_wrapped"
          let wrapperBody =
                vsep
                $ [(mayMemSol wrappedDomTy) <> " _t" <> semi]
                <> (map (\(a, i) -> "_t.elem" <> pretty i <> " = "
                          <> solRawVar a <> semi) $ zip args ([0 ..] :: [Int]))
                <> ["return " <> solApply vkWrapped ["_t"] <> semi]
          let mkWrapper name = solFunction name dom' ret wrapperBody
          let wrapper = mkWrapper vk
          let view_defns = map (mkWrapper . pretty . mk . bunpack) aliases
          illegal <- flip (<>) semi <$> solRequire "invalid view_i" "false"
          let igo (i, ViewInfo vvs vim) = freshVarMap $ do
                c' <- solEq "current_step" $ solNum i
                let asnv = "vvs"
                vvs_ty' <- solAsnType vvs
                let de' = solSet (parens $ solDecl asnv (mayMemSol vvs_ty')) $ solApply "abi.decode" ["current_svbs", parens vvs_ty']
                extendVarMap $ M.fromList $ map (\vv -> (vv, asnv <> "." <> solRawVar vv)) $ vvs
                extendVarMap $ M.fromList $ map (\(a, argI) -> (a, "_a.elem" <> pretty argI)) $ zip args ([0 ..] :: [Int])
                (defn, ret') <-
                  case M.lookup k (fromMaybe mempty $ M.lookup v vim) of
                    Just eb -> do
                      eb' <- solEB args eb
                      mvars <- readMemVars
                      which <- allocVarIdx
                      (frameDefn, frameDecl) <- solFrame which mvars
                      return (frameDefn, vsep [frameDecl, eb'])
                    Nothing ->
                      return $ (emptyDoc, illegal)
                return $ (defn, solWhen c' $ vsep [de', ret'])
          (defns, body') <- unzip <$> (mapM igo $ M.toAscList vi)
          let body'' = vsep $ body' <> [illegal]
          let keys = (s2t k, Aeson.String $ s2t vk_) :
                      map (\ a -> let a' = bunpack a in (s2t a', Aeson.String $ s2t $ mk a')) aliases
          return $
            (,) keys $
              vsep $ defns <> [solFunction vkWrapped [mayMemSol wrappedDomTy <> " _a"] retWrapped body''] <> [wrapper] <> view_defns
    let vgo (v, tm) = do
          (o_kss, bs) <- unzip <$> (mapM (tgo v) $ M.toAscList tm)
          let o_ks = concat o_kss
          -- Lift untagged views
          let keys = case v of
                Just v' -> [(b2t v', aesonObject $ o_ks)]
                Nothing -> o_ks
          return (keys, vsep bs)
    (view_jsons, view_defns) <- unzip <$> (mapM vgo $ M.toAscList vs)
    let view_json = concat view_jsons
    let state_defn =
          vsep $
            [ "uint256 current_step;"
            , "uint256 current_time;"
            , "  bytes current_svbs;"
            , "uint256 creation_time;"
            , "function _reachCreationTime() external view returns (uint256) { return creation_time; }"
            , "function _reachCurrentTime() external view returns (uint256) { return current_time; }"
            , "function _reachCurrentState() external view returns (uint256, bytes memory) { return (current_step, current_svbs); }"
            ]
              <> map_defns
              <> view_defns
    hs' <- solHandlers hs
    apidefs <- apiDefs ai
    let getm ctxt_f = (vsep . map snd . M.toAscList) <$> (liftIO $ readIORef ctxt_f)
    typedsp <- getm ctxt_typed
    typefsp <- getm ctxt_typef
    intsp <- getm ctxt_ints
    outputsp <- getm ctxt_outputs
    tlfunsp <- getm ctxt_tlfuns
    api_rng <- createAPIRng ai
    let defp =
          vsep $
            [ "receive () external payable {}"
            , "fallback () external payable {}"
            ]
    let ctcbody = vsep $ [state_defn, typefsp, api_rng, outputsp, tlfunsp, hs', apidefs, defp]
    let ctcp = solContract (contractId <> " is Stdlib") $ ctcbody
    let cinfo =
          M.fromList $
            [ ("views", aesonObject view_json)
            ]
    let preamble =
          vsep
            [ "// Automatically generated with Reach" <+> (pretty versionHashStr)
            , "pragma abicoder v2" <> semi
            ]
    return $ (cinfo, vsep $ [preamble, solVersion, solStdLib, typedsp, intsp, ctcp])

newtype CompiledSolRecs = CompiledSolRecs CompiledSolRecsM

type CompiledSolRecsM = M.Map T.Text CompiledSolRec

instance FromJSON CompiledSolRecs where
  parseJSON = withObject "CompiledSolRecs" $ \o -> do
    ctcs <- o .: "contracts"
    let ctcs' = kmToM ctcs
    CompiledSolRecs <$> mapM parseJSON ctcs'

data CompiledSolRec = CompiledSolRec
  { csrAbi :: T.Text
  , csrCode :: T.Text
  }

instance FromJSON CompiledSolRec where
  parseJSON = withObject "CompiledSolRec" $ \ctc -> do
    (abio :: Value) <- ctc .: "abi"
    -- Why are we re-encoding? ethers takes the ABI as a string, not an object.
    let cfg = defConfig {confIndent = Spaces 2, confCompare = compare}
    let csrAbi = T.pack $ LB.unpack $ encodePretty' cfg abio
    csrCode <- ctc .: "bin"
    return $ CompiledSolRec {..}

try_compile_sol :: FilePath -> String -> Maybe (Maybe Int) -> IO (Either String (String, CompiledSolRec))
try_compile_sol solf cn opt = do
  let o = (<>) ["--optimize"]
  let (me, oargs) =
        case opt of
          Nothing -> ("o0", [])
          Just mo ->
            case mo of
              Nothing -> ("oD", o [])
              Just r -> ("o" <> show r, o ["--optimize-runs=" <> show r])
  let fmts = "abi,bin"
  let args = oargs <> ["--combined-json", fmts, solf]
  -- putStrLn $ "solc " <> (show args)
  (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode "solc" args mempty
  let show_output =
        case stdout == "" of
          True -> stderr
          False -> "STDOUT:\n" <> stdout <> "\nSTDERR:\n" <> stderr
  case ec of
    ExitFailure _ -> return $ Left $ bunpack show_output
    ExitSuccess -> return $ fmap ((,) me) $ compile_sol_extract solf cn stdout

compile_sol_parse :: BS.ByteString -> Either String CompiledSolRecsM
compile_sol_parse stdout =
  case eitherDecodeStrict stdout of
    Left m -> Left $ "It produced invalid JSON output, which failed to decode with the message:\n" <> m
    Right (CompiledSolRecs xs) ->
      return xs

compile_sol_extract :: String -> String -> BS.ByteString -> Either String CompiledSolRec
compile_sol_extract solf cn stdout = do
  xs <- compile_sol_parse stdout
  let k = s2t $ solf <> ":" <> cn
  let ks = M.keys xs
  let xs' = M.filterWithKey (\k' _ -> T.isSuffixOf k' k) xs
  case M.toAscList xs' of
    [ (_, x) ] -> Right x
    _ -> Left $ "Expected contracts object to have unique key " <> show k <> " but had " <> show ks

reachEthBackendVersion :: Int
reachEthBackendVersion = 8

compile_sol_ :: FilePath -> String -> IO (Either String (String, CompiledSolRec))
compile_sol_ solf cn = do
  let shortEnough (_, CompiledSolRec {..}) =
        case len <= (2 * maxContractLen) of
          True -> Nothing
          False -> Just len
        where
          len = T.length csrCode
  let try = try_compile_sol solf cn
  let merr = \case
        Left e -> emitWarning Nothing $ W_SolidityOptimizeFailure e
        Right _ -> return ()
  let desperate = \case
        Right x -> \_ _ -> return $ Right x
        e@(Left bado) -> \case
          Right y -> \_ -> merr e >> return (Right y)
          Left _ -> \case
            Right z -> merr e >> return (Right z)
            Left _ -> return $ Left $ "The Solidity compiler failed with the message:\n" <> bado
  let tryN eA e1 =
        try Nothing >>= \case
          Right oN -> do
            let roN = Right oN
            case shortEnough oN of
              Nothing -> merr eA >> return roN
              Just _lenN -> desperate eA e1 roN
          Left rN -> desperate eA e1 (Left rN)
  let try1 eA =
        try (Just $ Just 1) >>= \case
          Right o1 -> do
            let ro1 = Right o1
            case shortEnough o1 of
              Nothing -> merr eA >> return ro1
              Just _len1 -> tryN eA ro1
          Left r1 -> tryN eA (Left r1)
  let tryA =
        try (Just Nothing) >>= \case
          Right oA -> do
            let roA = Right oA
            case shortEnough oA of
              Nothing -> return roA
              Just _lenA -> try1 roA
          Left rA -> try1 $ Left rA
  tryA

compile_sol :: ConnectorObject -> FilePath -> IO ConnectorInfo
compile_sol cinfo solf = compile_sol_ solf contractId >>= \case
  Left x -> impossible x
  Right (which, CompiledSolRec {..}) ->
    return $
      Aeson.Object $
        mToKM $
          M.union cinfo $
            M.fromList $
              [ ("ABI", Aeson.String csrAbi)
              , ("Bytecode", Aeson.String $ "0x" <> csrCode)
              , ("Which", Aeson.String $ T.pack which)
              , ("BytecodeLen", Aeson.Number $ (fromIntegral $ T.length csrCode) / 2)
              , ("version", Aeson.Number $ fromIntegral reachEthBackendVersion)
              ]

ccBin :: BS.ByteString -> String
ccBin = B.unpack

ccJson :: String -> String -> BS.ByteString -> CCApp String
ccJson x y z = do
  CompiledSolRec {..} <- except (compile_sol_extract x y z)
  return $ t2s $ csrCode

ccSol :: String -> FilePath -> CCApp String
ccSol cn solf = do
  (_, CompiledSolRec {..}) <- ExceptT (compile_sol_ solf cn)
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

connect_eth :: CompilerToolEnv -> Connector
connect_eth _ = Connector {..}
  where
    conName = conName'
    conCons = conCons'
    conReserved = flip S.member solReservedNames
    conGen moutn pl = case moutn of
      Just outn -> go (outn "sol")
      Nothing -> withSystemTempDirectory "reachc-sol" $ \dir ->
        go (dir </> "compiled.sol")
      where
        go :: FilePath -> IO ConnectorInfo
        go solf = do
          (cinfo, sol) <- solPLProg pl
          unless dontWriteSol $ do
            LTIO.writeFile solf $ render sol
          compile_sol cinfo solf
    conCompileCode v = runExceptT $ do
      (c::String) <- ccPath =<< aesonParse' v
      return $ toJSON c
    conContractNewOpts :: Maybe AS.Value -> Either String AS.Value
    conContractNewOpts mv = do
      (x :: ()) <- aesonParse $ fromMaybe AS.Null mv
      return $ AS.toJSON x
