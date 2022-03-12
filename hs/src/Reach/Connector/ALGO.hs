module Reach.Connector.ALGO (connect_algo, AlgoError (..)) where

import Control.Monad.Extra
import Control.Monad.Reader
import Crypto.Hash
import qualified Data.Aeson as Aeson
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encodeBase64')
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (intercalate, foldl')
import qualified Data.List as List
import Data.List.Extra (enumerate, mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as Vector
import Data.Word
import Generics.Deriving (Generic)
import Reach.AddCounts
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.BinaryLeafTree
import Reach.CommandLine
import Reach.Connector
import Reach.Counter
import Reach.Dotty
import Reach.FixedPoint
import qualified Reach.Texty as T
import Reach.Texty (pretty)
import Reach.UnsafeUtil
import Reach.Util
import Reach.Warning
import Safe (atMay)
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process.ByteString
import Text.Read

-- Errors for ALGO

data AlgoError
  = Err_TransferNewToken
  | Err_PayNewToken
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions, Generic)

instance HasErrorCode AlgoError where
  errPrefix = const "RA"
  errIndex = \case
    Err_TransferNewToken {} -> 0
    Err_PayNewToken {} -> 1

instance Show AlgoError where
  show = \case
    Err_TransferNewToken ->
      "Token cannot be transferred within the same consensus step it was created in on Algorand"
    Err_PayNewToken ->
      "Token cannot be paid within the same consensus step it was shared with the contract on Algorand"

type NotifyF = LT.Text -> IO ()
type Notify = Bool -> NotifyF

-- General tools that could be elsewhere

type LPGraph1 a b = M.Map a b
type LPGraph a b = M.Map a (LPGraph1 a b)

type LPPath a = ([a], Integer)

longestPathBetween :: forall b . LPGraph String b -> String -> String -> (b -> Integer) -> IO (LPPath String)
longestPathBetween g f d getc = do
  a2d <- fixedPoint $ \_ (i :: LPGraph1 String Integer) -> do
    flip mapM g $ \tom -> do
      let ext to c =
            case to == d of
              True -> c
              False ->
                case M.lookup to i of
                  Nothing -> 0
                  Just c' -> c + c'
      let tom' = map (uncurry ext) $ M.toAscList $ M.map getc tom
      return $ foldl' max 0 tom'
  let r2d x = fromMaybe 0 $ M.lookup x a2d
  let pc = r2d f
  let getMaxPath' x =
        case x == d of
          True -> []
          False -> getMaxPath $ List.maximumBy (compare `on` r2d) $ M.keys $ fromMaybe mempty $ M.lookup x g
      getMaxPath x = x : getMaxPath' x
  let p = getMaxPath f
  return $ (p, pc)

restrictGraph :: forall a b . (Ord a) => LPGraph a b -> a -> IO (LPGraph a b)
restrictGraph g n = do
  (from, to) <- fixedPoint $ \_ ((from :: S.Set a), (to_ :: S.Set a)) -> do
    let to = S.insert n to_
    let incl1 x cs = x == n || S.member x cs
    let inclFrom (x, es) =
          case incl1 x from of
            True -> S.insert x $ M.keysSet es
            False -> mempty
    let inclTo (x, es) =
          case S.null $ S.intersection (M.keysSet es) to of
            False -> S.singleton x
            True -> mempty
    let from' = mconcatMap inclFrom $ M.toAscList g
    let to' = mconcatMap inclTo $ M.toAscList g
    return (from', to')
  let cs = S.union from to
  let isConnected = flip S.member cs
  let onlyConnected x _ = isConnected x
  let removeDisconnected = M.filterWithKey onlyConnected
  return $ M.map removeDisconnected $ removeDisconnected g

aarray :: [Aeson.Value] -> Aeson.Value
aarray = Aeson.Array . Vector.fromList

aobject :: M.Map T.Text Aeson.Value -> Aeson.Value
aobject = Aeson.Object . HM.fromList . M.toList

-- Algorand constants

conName' :: T.Text
conName' = "ALGO"

conCons' :: DLConstant -> DLLiteral
conCons' = \case
  DLC_UInt_max  -> DLL_Int sb $ 2 ^ (64 :: Integer) - 1
  DLC_Token_zero -> DLL_Int sb $ 0

algoMinTxnFee :: Integer
algoMinTxnFee = 1000

algoMaxLocalSchemaEntries :: Integer
algoMaxLocalSchemaEntries = 16

algoMaxLocalSchemaEntries_usable :: Integer
algoMaxLocalSchemaEntries_usable = algoMaxLocalSchemaEntries

algoMaxGlobalSchemaEntries :: Integer
algoMaxGlobalSchemaEntries = 64

algoMaxGlobalSchemaEntries_usable :: Integer
algoMaxGlobalSchemaEntries_usable = algoMaxGlobalSchemaEntries - 1

algoMaxAppBytesValueLen :: Integer
algoMaxAppBytesValueLen = 128

algoMaxAppBytesValueLen_usable :: Integer
algoMaxAppBytesValueLen_usable =
  -- We guarantee that every key is exactly one byte, so all the rest of the
  -- space goes to the value
  algoMaxAppBytesValueLen - 1

algoMaxAppTotalArgLen :: Integer
algoMaxAppTotalArgLen = 2048

algoMinimumBalance :: Integer
algoMinimumBalance = 100000

algoMaxTxGroupSize :: Integer
algoMaxTxGroupSize = 16

-- not actually the limit, but this is the name of the variable in the
-- consensus configuration
algoMaxInnerTransactions :: Integer
algoMaxInnerTransactions = 16

algoMaxAppTxnAccounts :: Integer
algoMaxAppTxnAccounts = 4

algoMaxAppTxnForeignAssets :: Integer
algoMaxAppTxnForeignAssets = 8

algoMaxAppProgramCost :: Integer
algoMaxAppProgramCost = 700

-- We're making up this name. It is not in consensus.go, but only in the docs
algoMaxLogLen :: Integer
algoMaxLogLen = 1024

algoMaxAppProgramLen :: Integer
algoMaxAppProgramLen = 2048

algoMaxExtraAppProgramPages :: Integer
algoMaxExtraAppProgramPages = 3

algoMaxAppProgramLen_really :: Integer
algoMaxAppProgramLen_really = (1 + algoMaxExtraAppProgramPages) * algoMaxAppProgramLen

minimumBalance_l :: DLLiteral
minimumBalance_l = DLL_Int sb algoMinimumBalance

tealVersionPragma :: LT.Text
tealVersionPragma = "#pragma version 6"

-- Algo specific stuff

maxTypeSize :: M.Map a DLType -> Integer
maxTypeSize m =
  case M.null m of
    True -> 0
    False -> maximum $ map typeSizeOf $ M.elems m

typeSig :: DLType -> String
typeSig x =
  case x of
    T_Null -> "byte[0]"
    T_Bool -> "byte" -- "bool"
    T_UInt -> "uint64"
    T_Bytes sz -> "byte" <> array sz
    T_Digest -> "digest"
    T_Address -> "address"
    T_Contract -> typeSig T_UInt
    T_Token -> typeSig T_UInt
    T_Array t sz -> typeSig t <> array sz
    T_Tuple ts -> "(" <> intercalate "," (map typeSig ts) <> ")"
    T_Object m -> typeSig $ T_Tuple $ M.elems m
    T_Data m -> "(byte,byte" <> array (maxTypeSize m) <> ")"
    T_Struct ts -> typeSig $ T_Tuple $ map snd ts
  where
    array sz = "[" <> show sz <> "]"

typeSizeOf :: DLType -> Integer
typeSizeOf = \case
  T_Null -> 0
  T_Bool -> 1
  T_UInt -> word
  T_Bytes sz -> sz
  T_Digest -> 32
  T_Address -> 32
  T_Contract -> typeSizeOf $ T_UInt
  T_Token -> typeSizeOf $ T_UInt
  T_Array t sz -> sz * typeSizeOf t
  T_Tuple ts -> sum $ map typeSizeOf ts
  T_Object m -> sum $ map typeSizeOf $ M.elems m
  T_Data m -> 1 + maxTypeSize m
  T_Struct ts -> sum $ map (typeSizeOf . snd) ts
  where
    word = 8

encodeBase64 :: B.ByteString -> LT.Text
encodeBase64 bs = LT.pack $ B.unpack $ encodeBase64' bs

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

textyt :: Show a => a -> DLType -> LT.Text
textyt x ty = texty x <> " :: " <> texty ty

textyv :: DLVar -> LT.Text
textyv v = textyt v (varType v)

type ScratchSlot = Word8

type TealOp = LT.Text

type TealArg = LT.Text

type Label = LT.Text

data IndentDir
  = INo
  | IUp
  | IDo

data TEAL
  = TCode TealOp [TealArg]
  | Titob Bool
  | TInt Integer
  | TConst LT.Text
  | TBytes B.ByteString
  | TExtract Word8 Word8
  | TSubstring Word8 Word8
  | TComment IndentDir LT.Text
  | TLabel Label
  | TFor_top Integer
  | TFor_bnz Label Integer Label
  | TLog Integer
  | TStore ScratchSlot LT.Text
  | TLoad ScratchSlot LT.Text
  | TResource Resource

type TEALt = [LT.Text]

type TEALs = DL.DList TEAL

builtin :: S.Set TealOp
builtin = S.fromList ["byte", "int", "substring", "extract", "log", "store", "load", "itob"]

render :: IORef Int -> TEAL -> IO TEALt
render ilvlr = \case
  TInt x -> r ["int", texty x]
  TConst x -> r ["int", x]
  TBytes bs -> r ["byte", "base64(" <> encodeBase64 bs <> ")"]
  TExtract x y -> r ["extract", texty x, texty y]
  TSubstring x y -> r ["substring", texty x, texty y]
  Titob _ -> r ["itob"]
  TCode f args ->
    case S.member f builtin of
      True -> impossible $ show $ "cannot use " <> f <> " directly"
      False -> r $ f : args
  TComment il t -> do
    case il of
      INo -> return ()
      IUp -> modifyIORef ilvlr $ \x -> x + 1
      IDo -> modifyIORef ilvlr $ \x -> x - 1
    case t of
      "" -> return []
      _ -> r ["//", t]
  TLabel lab -> r [lab <> ":"]
  TFor_top maxi ->
    r [("// for runs " <> texty maxi <> " times")]
  TFor_bnz top_lab maxi _ ->
    r ["bnz", top_lab, ("// for runs " <> texty maxi <> " times")]
  TLog sz ->
    r ["log", ("// up to " <> texty sz <> " bytes")]
  TStore sl lab -> r ["store", texty sl, ("// " <> lab)]
  TLoad sl lab -> r ["load", texty sl, ("// " <> lab)]
  TResource rs -> r [("// resource: " <> texty rs)]
  where
    r l = do
      i <- readIORef ilvlr
      let i' = replicate i " "
      return $ i' <> l

renderOut :: [TEAL] -> IO T.Text
renderOut tscl' = do
  ilvlr <- newIORef $ 0
  tsl' <- mapM (render ilvlr) tscl'
  let lts = tealVersionPragma : (map LT.unwords tsl')
  let lt = LT.unlines lts
  let t = LT.toStrict lt
  return t

optimize :: [TEAL] -> [TEAL]
optimize ts0 = tsN
  where
    ts1 = opt_b ts0
    ts2 = opt_bs ts1
    tsN = ts2

opt_bs :: [TEAL] -> [TEAL]
opt_bs = \case
  [] -> []
  x@(TBytes bs) : l | B.all (== '\0') bs ->
    case B.length bs of
      0 -> (TBytes mempty) : opt_bs l
      32 -> opt_bs $ (TCode "global" ["ZeroAddress"]) : l
      -- Cost is more important than space
      -- len -> opt_bs $ (TInt $ fromIntegral len) : (TCode "bzero" []) : l
      _ -> x : opt_bs l
  x : l -> x : opt_bs l

opt_b :: [TEAL] -> [TEAL]
opt_b = foldr (\a b -> opt_b1 $ a : b) mempty

opt_b1 :: [TEAL] -> [TEAL]
opt_b1 = \case
  [] -> []
  [(TCode "return" [])] -> []
  -- This relies on knowing what "done" is
  (TCode "assert" []) : (TCode "b" ["done"]) : x -> (TCode "return" []) : x
  (TBytes "") : (TCode "concat" []) : l -> l
  (TBytes "") : b@(TLoad {}) : (TCode "concat" []) : l -> opt_b1 $ b : l
  (TBytes x) : (TBytes y) : (TCode "concat" []) : l ->
    opt_b1 $ (TBytes $ x <> y) : l
  (TCode "b" [x]) : b@(TLabel y) : l | x == y -> b : l
  (TCode "btoi" []) : (Titob True) : (TSubstring 7 8) : l -> l
  (TCode "btoi" []) : (Titob _) : l -> l
  (TInt 0) : (TCode "-" []) : l -> l
  (TInt 0) : (TCode "+" []) : l -> l
  (Titob _) : (TCode "btoi" []) : l -> l
  (TCode "==" []) : (TCode "!" []) : l -> (TCode "!=" []) : l
  (TInt 0) : (TCode "!=" []) : (TCode "assert" []) : l ->
    (TCode "assert" []) : l
  (TExtract x 8) : (TCode "btoi" []) : l ->
    (TInt $ fromIntegral x) : (TCode "extract_uint64" []) : l
  a@(TLoad x _) : (TLoad y _) : l
    | x == y ->
      -- This misses if there is ANOTHER load of the same thing
      a : (TCode "dup" []) : l
  a@(TStore x _) : (TLoad y _) : l
    | x == y ->
      (TCode "dup" []) : a : l
  a@(TSubstring s0w _) : b@(TInt xn) : c@(TCode "getbyte" []) : l ->
    case xn < 256 && s0xnp1 < 256 of
      True -> opt_b1 $ (TSubstring (fromIntegral s0xn) (fromIntegral s0xnp1)) : (TCode "btoi" []) : l
      False -> a : b : c : l
    where
      s0xn :: Integer
      s0xn = (fromIntegral s0w) + xn
      s0xnp1 :: Integer
      s0xnp1 = s0xn + 1
  a@(TSubstring s0w _) : b@(TSubstring s1w e1w) : l ->
    case s2n < 256 && e2n < 256 of
      True -> opt_b1 $ (TSubstring (fromIntegral s2n) (fromIntegral e2n)) : l
      False -> a : b : l
    where
      s0n = fromIntegral s0w
      s2n :: Integer
      s2n = s0n + (fromIntegral s1w)
      e2n :: Integer
      e2n = s0n + (fromIntegral e1w)
  (TInt x) : (Titob _) : l ->
    opt_b1 $ (TBytes $ itob x) : l
  (TBytes xbs) : (TCode "btoi" []) : l ->
    opt_b1 $ (TInt $ btoi xbs) : l
  (TBytes xbs) : (TCode "sha256" []) : l ->
    opt_b1 $ (TBytes $ sha256bs xbs) : l
  (TBytes xbs) : (TCode "sha512_256" []) : l ->
    opt_b1 $ (TBytes $ sha512_256bs xbs) : l
  (TBytes xbs) : (TSubstring s e) : l ->
    opt_b1 $ (TBytes $ bsSubstring xbs (fromIntegral s) (fromIntegral e)) : l
  x : l -> x : l

sha256bs :: BS.ByteString -> BS.ByteString
sha256bs = BA.convert . hashWith SHA256
sha512_256bs :: BS.ByteString -> BS.ByteString
sha512_256bs = BA.convert . hashWith SHA512t_256

bsSubstring :: BS.ByteString -> Int -> Int -> BS.ByteString
bsSubstring bs s e = BS.take e $ BS.drop s bs

itob :: Integral a => a -> BS.ByteString
itob x = LB.toStrict $ toLazyByteString $ word64BE $ fromIntegral x

btoi :: BS.ByteString -> Integer
btoi bs = BS.foldl' (\i b -> (i `shiftL` 8) .|. fromIntegral b) 0 $ bs

type RestrictCFG = Label -> IO (DotGraph, AnalyzeCFG)
type AnalyzeCFG = Resource -> IO (LPPath String)
type ResourceCost = M.Map Resource Integer

buildCFG :: [TEAL] -> IO (DotGraph, RestrictCFG)
buildCFG ts = do
  res_gr :: IORef (LPGraph String ResourceCost) <- newIORef mempty
  let lTop = "TOP"
  let lBot = "BOT"
  (labr :: IORef String) <- newIORef $ lTop
  (k_r :: IORef Integer) <- newIORef $ 1
  (res_r :: IORef ResourceCost) <- newIORef $ mempty
  let modK = modifyIORef k_r
  let l2s = LT.unpack
  let recResource rs c = do
        k <- readIORef k_r
        let f old = Just $ (k * c) + fromMaybe 0 old
        modifyIORef res_r $ M.alter f rs
  let recCost = recResource R_Cost
  let jump_ :: String -> IO ()
      jump_ t = do
        lab <- readIORef labr
        c <- readIORef res_r
        let ff = M.unionWith max c
        let fg = Just . ff . fromMaybe mempty
        let f = M.alter fg t
        let g = Just . f . fromMaybe mempty
        modifyIORef res_gr $ M.alter g lab
  let switch t = do
        writeIORef labr t
        writeIORef res_r mempty
  let jump t = recCost 1 >> jump_ (l2s t)
  forM_ ts $ \case
    TFor_top cnt -> do
      modK (\x -> x * cnt)
    TFor_bnz _ cnt lab' -> do
      recCost 1
      modK (\x -> x `div` cnt)
      jump lab'
    TCode "bnz" [lab'] -> jump lab'
    TCode "bz" [lab'] -> jump lab'
    TCode "b" [lab'] -> do
      jump lab'
      switch ""
    TCode "return" [] -> do
      jump lBot
      switch ""
    TCode "callsub" [_lab'] ->
      impossible "callsub"
    TLog len -> do
      -- Note: We don't check MaxLogCalls, because it is not actually checked
      -- by the implementation
      recResource R_Log len
      recCost 1
    TComment {} -> return ()
    TLabel lab' -> do
      let lab'' = l2s lab'
      jump_ lab''
      switch lab''
    TBytes _ -> recCost 1
    TConst _ -> recCost 1
    TStore {} -> recCost 1
    TLoad {} -> recCost 1
    TInt _ -> recCost 1
    TExtract {} -> recCost 1
    TSubstring {} -> recCost 1
    Titob {} -> recCost 1
    TResource r -> recResource r 1
    TCode f _ ->
      case f of
        "sha256" -> recCost 35
        "keccak256" -> recCost 130
        "sha512_256" -> recCost 45
        "ed25519verify" -> recCost 1900
        "ecdsa_verify" -> recCost 1700
        "ecdsa_pk_decompress" -> recCost 650
        "ecdsa_pk_recover" -> recCost 2000
        "divmodw" -> recCost 20
        "sqrt" -> recCost 4
        "expw" -> recCost 10
        "b+" -> recCost 10
        "b-" -> recCost 10
        "b/" -> recCost 20
        "b*" -> recCost 20
        "b%" -> recCost 20
        "b|" -> recCost 6
        "b&" -> recCost 6
        "b^" -> recCost 6
        "b~" -> recCost 4
        "bsqrt" -> recCost 40
        "itxn_begin" -> do
          recResource R_ITxn 1
          recCost 1
        "itxn_next" -> do
          recResource R_ITxn 1
          recCost 1
        _ -> recCost 1
  let renderRc m = intercalate "/" $ map f allResources
        where f r = show $ fromMaybe 0 $ M.lookup r m
  let gs :: LPGraph String ResourceCost -> DotGraph
      gs g =
        flip concatMap (M.toAscList g) $ \(from, cs) ->
          flip concatMap (M.toAscList cs) $ \(to, c) ->
            case (from == mempty) of
              True -> []
              False -> [(from, to, (M.fromList $ [("label", renderRc c)]))]
  g <- readIORef res_gr
  let getc rs c = fromMaybe 0 $ M.lookup rs c
  let restrict mustLab = do
        g' <- restrictGraph g $ l2s mustLab
        return $ (gs g', longestPathBetween g' lTop (l2s lBot) . getc)
  return (gs g, restrict)

data LabelRec = LabelRec
  { lr_lab :: Label
  , lr_at :: SrcLoc
  , lr_what :: String
  }

type CompanionCalls = M.Map Label Integer
type CompanionInfo = Maybe CompanionCalls

checkCost :: Notify -> Disp -> Bool -> [TEAL] -> [LabelRec] -> IO CompanionCalls
checkCost notify disp alwaysShow ts ls = do
  xtraR <- newIORef mempty
  let rgs lab gs = void $ disp ("." <> lab <> "dot") $ LT.toStrict $ T.render $ dotty gs
  (gs, restrictCFG) <- buildCFG ts
  rgs "" gs
  when alwaysShow $ do
    putStrLn $ "Conservative analysis on Algorand found:"
  forM_ ls $ \LabelRec {..} -> do
    let starts_at = " starts at " <> show lr_at <> "."
    when alwaysShow $ do
      putStrLn $ " * " <> lr_what <> ", which" <> starts_at
    (gs', analyzeCFG) <- restrictCFG lr_lab
    when False $
      rgs (LT.unpack lr_lab <> ".") gs'
    feesR <- newIORef 0
    forM_ allResources $ \rs -> do
      let algoMax = maxOf rs
      (_p, c) <- analyzeCFG rs
      let tooMuch = c > algoMax
      when (rs == R_Cost && tooMuch) $ do
        modifyIORef xtraR $ M.insertWith (+) lr_lab 1
      when (rHasFee rs) $ do
        modifyIORef feesR $ (+) c
      let units = rLabel (c /= 1) rs
      let pre = "uses " <> show c <> " " <> units
      let post = if tooMuch then ", but the limit is " <> show algoMax else ""
      let msg = pre <> post <> "."
      when tooMuch $ do
        notify (rPrecise rs) $ LT.pack $ lr_what <> " " <> msg <> " " <> lr_what <> starts_at
      when alwaysShow $ do
        putStrLn $ "   + " <> msg
    when alwaysShow $ do
      fees <- readIORef feesR
      putStrLn $ "   + costs " <> show fees <> " " <> plural (fees /= 1) "fee" <> "."
  readIORef xtraR

type Lets = M.Map DLVar (App ())

data Env = Env
  { eFailuresR :: IORef (S.Set LT.Text)
  , eWarningsR :: IORef (S.Set LT.Text)
  , eCounter :: Counter
  , eStateSizeR :: IORef Integer
  , eMaps :: DLMapInfos
  , eMapDataTy :: DLType
  , eMapDataSize :: Integer
  , eMapKeysl :: [Word8]
  , eWhich :: Maybe Int
  , eLabel :: Counter
  , eOutputR :: IORef TEALs
  , eHP :: ScratchSlot
  , eSP :: ScratchSlot
  , eVars :: M.Map DLVar ScratchSlot
  , eLets :: Lets
  , eLetSmalls :: M.Map DLVar Bool
  , eResources :: ResourceSets
  , eNewToks :: IORef (S.Set DLArg)
  , eInitToks :: IORef (S.Set DLArg)
  }

type App = ReaderT Env IO

separateResources :: App a -> App a
separateResources = dupeResources . resetToks

recordWhich :: Int -> App a -> App a
recordWhich n = local (\e -> e {eWhich = Just n}) . separateResources

data Resource
  = R_Asset
  | R_Account
  | R_Log
  | R_Cost
  | R_ITxn
  | R_Txn
  deriving (Eq, Ord, Enum, Bounded, Show)

allResources :: [Resource]
allResources = enumerate

allResourcesM :: M.Map Resource ()
allResourcesM = M.fromList $ map (flip (,) ()) allResources

useResource :: Resource -> App ()
useResource = output . TResource

type ResourceSet = S.Set DLArg

type ResourceSets = IORef (M.Map Resource ResourceSet)

plural :: Bool -> String -> String
plural ph x = x <> if ph then "s" else ""

rLabel :: Bool -> Resource -> String
rLabel ph = \case
  R_Txn -> p "transaction"
  R_ITxn -> "inner " <> p "transaction"
  R_Asset -> p "asset"
  R_Account -> p "account"
  R_Cost -> p "unit" <> " of cost"
  R_Log -> p "byte" <> " of logs"
  where
    p = plural ph

rHasFee :: Resource -> Bool
rHasFee = \case
  R_Txn -> True
  R_ITxn -> True
  R_Asset -> False
  R_Account -> False
  R_Cost -> False
  R_Log -> False

rPrecise :: Resource -> Bool
rPrecise = \case
  R_Txn -> True
  R_ITxn -> True
  R_Asset -> False
  R_Account -> False
  R_Cost -> True
  R_Log -> True

maxOf :: Resource -> Integer
maxOf = \case
  R_Asset -> algoMaxAppTxnForeignAssets
  R_Account -> algoMaxAppTxnAccounts
  R_Txn -> algoMaxTxGroupSize
  R_ITxn -> algoMaxInnerTransactions * algoMaxTxGroupSize
  R_Cost -> algoMaxAppProgramCost
  R_Log -> algoMaxLogLen

newResources :: IO ResourceSets
newResources = newIORef $ M.map (const mempty) allResourcesM

dupeResources :: App a -> App a
dupeResources m = do
  c' <- (liftIO . dupeIORef) =<< asks eResources
  local (\e -> e {eResources = c'}) m

readResource :: Resource -> App ResourceSet
readResource r = do
  rsr <- asks eResources
  m <- liftIO $ readIORef rsr
  return $ fromMaybe mempty $ M.lookup r m

freeResource :: Resource -> DLArg -> App ()
freeResource r a = do
  vs <- readResource r
  let vs' = S.insert a vs
  rsr <- asks eResources
  liftIO $ modifyIORef rsr $ M.insert r vs'

incResource :: Resource -> DLArg -> App ()
incResource r a = do
  vs <- readResource r
  case S.member a vs of
    True -> return ()
    False -> do
      useResource r
      freeResource r a

resetToks :: App a -> App a
resetToks m = do
  ntoks <- liftIO $ newIORef mempty
  itoks <- liftIO $ newIORef mempty
  local (\e -> e {eNewToks = ntoks, eInitToks = itoks}) m

addTok :: (Env -> IORef (S.Set DLArg)) -> DLArg -> App ()
addTok ef tok = do
  r <- asks ef
  liftIO $ modifyIORef r (S.insert tok)

addNewTok :: DLArg -> App ()
addNewTok = addTok eNewToks

addInitTok :: DLArg -> App ()
addInitTok = addTok eInitToks

isTok :: (Env -> IORef (S.Set DLArg)) -> DLArg -> App Bool
isTok ef tok = do
  ts <- (liftIO . readIORef) =<< asks ef
  return $ tok `S.member` ts

isNewTok :: DLArg -> App Bool
isNewTok = isTok eNewToks

isInitTok :: DLArg -> App Bool
isInitTok = isTok eInitToks

output :: TEAL -> App ()
output t = do
  Env {..} <- ask
  liftIO $ modifyIORef eOutputR (flip DL.snoc t)

code :: LT.Text -> [LT.Text] -> App ()
code f args = output $ TCode f args

label :: LT.Text -> App ()
label = output . TLabel

comment :: LT.Text -> App ()
comment = output . TComment INo

block_ :: LT.Text -> App a -> App a
block_ lab m = do
  output $ TComment IUp $ ""
  output $ TComment INo $ "{ " <> lab
  x <- m
  output $ TComment INo $ lab <> " }"
  output $ TComment IDo $ ""
  return x

block :: Label -> App a -> App a
block lab m = block_ lab $ label lab >> m

assert :: App ()
assert = op "assert"

asserteq :: App ()
asserteq = op "==" >> assert

op :: TealOp -> App ()
op = flip code []

nop :: App ()
nop = return ()

dont_concat_first :: [App ()]
dont_concat_first = nop : repeat (op "concat")

padding :: Integer -> App ()
padding = cla . bytesZeroLit

czaddr :: App ()
czaddr = padding $ typeSizeOf T_Address

bad_io :: IORef (S.Set LT.Text) -> NotifyF
bad_io x = modifyIORef x . S.insert

badlike :: (Env -> IORef (S.Set LT.Text)) -> LT.Text -> App ()
badlike eGet lab = do
  r <- asks eGet
  liftIO $ bad_io r lab

bad :: LT.Text -> App ()
bad lab = do
  badlike eFailuresR lab
  mapM_ comment $ LT.lines $ "BAD " <> lab

warn :: LT.Text -> App ()
warn = badlike eWarningsR

freshLabel :: String -> App LT.Text
freshLabel d = do
  i <- (liftIO . incCounter) =<< (eLabel <$> ask)
  return $ "l" <> LT.pack (show i) <> "_" <> LT.pack d

loopLabel :: Int -> LT.Text
loopLabel w = "loopBody" <> LT.pack (show w)

store_let :: DLVar -> Bool -> App () -> App a -> App a
store_let dv small cgen m = do
  Env {..} <- ask
  local
    (\e ->
       e
         { eLets = M.insert dv cgen eLets
         , eLetSmalls = M.insert dv small eLetSmalls
         })
    $ m

letSmall :: DLVar -> App Bool
letSmall dv = do
  Env {..} <- ask
  return $ fromMaybe False (M.lookup dv eLetSmalls)

lookup_let :: DLVar -> App ()
lookup_let dv = do
  Env {..} <- ask
  case M.lookup dv eLets of
    Just m -> m
    Nothing ->
      impossible $ show eWhich <> " lookup_let " <> show (pretty dv) <> " not in " <> (List.intercalate ", " $ map (show . pretty) $ M.keys eLets)

store_var :: DLVar -> ScratchSlot -> App a -> App a
store_var dv ss m = do
  Env {..} <- ask
  local (\e -> e {eVars = M.insert dv ss eVars}) $
    m

lookup_var :: DLVar -> App ScratchSlot
lookup_var dv = do
  Env {..} <- ask
  case M.lookup dv eVars of
    Just x -> return $ x
    Nothing -> impossible $ "lookup_var " <> show dv

salloc :: (ScratchSlot -> App a) -> App a
salloc fm = do
  Env {..} <- ask
  let eSP' = eSP - 1
  when (eSP' == eHP) $ do
    bad "Too many scratch slots"
  local (\e -> e {eSP = eSP'}) $
    fm eSP

salloc_ :: LT.Text -> (App () -> App () -> App a) -> App a
salloc_ lab fm =
  salloc $ \loc -> do
    fm (output $ TStore loc lab) (output $ TLoad loc lab)

sallocLet :: DLVar -> App () -> App a -> App a
sallocLet dv cgen km = do
  salloc_ (textyv dv) $ \cstore cload -> do
    cgen
    cstore
    store_let dv True cload km

sallocVarLet :: DLVarLet -> Bool -> App () -> App a -> App a
sallocVarLet (DLVarLet mvc dv) sm cgen km = do
  let once = store_let dv sm cgen km
  case mvc of
    Nothing -> km
    Just DVC_Once -> once
    Just DVC_Many ->
      case sm of
        True -> once
        False -> sallocLet dv cgen km

ctobs :: DLType -> App ()
ctobs = \case
  T_UInt -> output (Titob False)
  T_Bool -> output (Titob True) >> output (TSubstring 7 8)
  T_Null -> nop
  T_Bytes _ -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Contract -> ctobs T_UInt
  T_Token -> ctobs T_UInt
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Struct {} -> nop

cfrombs :: DLType -> App ()
cfrombs = \case
  T_UInt -> op "btoi"
  T_Bool -> op "btoi"
  T_Null -> nop
  T_Bytes _ -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Contract -> cfrombs T_UInt
  T_Token -> cfrombs T_UInt
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Struct {} -> nop

ctzero :: DLType -> App ()
ctzero = \case
  T_UInt -> cint 0
  t -> do
    padding $ typeSizeOf t
    cfrombs t

chkint :: SrcLoc -> Integer -> Integer
chkint at i = checkIntLiteralC at conName' conCons' i

cint_ :: SrcLoc -> Integer -> App ()
cint_ at i = output $ TInt $ chkint at i

cint :: Integer -> App ()
cint = cint_ sb

cl :: DLLiteral -> App ()
cl = \case
  DLL_Null -> cbs ""
  DLL_Bool b -> cint $ if b then 1 else 0
  DLL_Int at i -> cint_ at i
  DLL_TokenZero -> cint 0

cbool :: Bool -> App ()
cbool = cl . DLL_Bool

ca_boolb :: DLArg -> Maybe B.ByteString
ca_boolb = \case
  DLA_Literal (DLL_Bool b) ->
    Just $ B.singleton $ toEnum $ if b then 1 else 0
  _ -> Nothing

cas_boolbs :: [DLArg] -> Maybe B.ByteString
cas_boolbs = mconcat . map ca_boolb

cv :: DLVar -> App ()
cv = lookup_let

ca :: DLArg -> App ()
ca = \case
  DLA_Var v -> cv v
  DLA_Constant c -> cl $ conCons' c
  DLA_Literal c -> cl c
  DLA_Interact {} -> impossible "consensus interact"

argSmall :: DLArg -> App Bool
argSmall = \case
  DLA_Var v -> letSmall v
  DLA_Constant {} -> return True
  DLA_Literal {} -> return True
  DLA_Interact {} -> impossible "consensus interact"

exprSmall :: DLExpr -> App Bool
exprSmall = \case
  DLE_Arg _ a -> argSmall a
  _ -> return False

czpad :: Integer -> App ()
czpad xtra = do
  padding xtra
  op "concat"

cprim :: PrimOp -> [DLArg] -> App ()
cprim = \case
  SELF_ADDRESS {} -> impossible "self address"
  ADD -> call "+"
  SUB -> call "-"
  MUL -> call "*"
  DIV -> call "/"
  MUL_DIV -> \case
    [x, y, z] -> do
      ca x
      ca y
      op "mulw"
      ca z
      op "divw"
    _ -> impossible "cprim: MUL_DIV args"
  MOD -> call "%"
  PLT -> call "<"
  PLE -> call "<="
  PEQ -> call "=="
  PGT -> call ">"
  PGE -> call ">="
  LSH -> call "<<"
  RSH -> call ">>"
  BAND -> call "&"
  BIOR -> call "|"
  BXOR -> call "^"
  DIGEST_XOR -> call "b^"
  BYTES_XOR -> call "b^"
  DIGEST_EQ -> call "=="
  ADDRESS_EQ -> call "=="
  TOKEN_EQ -> call "=="
  BYTES_ZPAD xtra -> \case
    [x] -> do
      ca x
      czpad xtra
    _ -> impossible $ "zpad"
  IF_THEN_ELSE -> \case
    [be, DLA_Literal (DLL_Bool True), DLA_Literal (DLL_Bool False)] -> do
      ca be
    [be, DLA_Literal (DLL_Bool False), DLA_Literal (DLL_Bool True)] -> do
      ca be
      op "!"
    [be, DLA_Literal (DLL_Bool True), fe] -> do
      ca be
      ca fe
      op "||"
    [be, DLA_Literal (DLL_Bool False), fe] -> do
      -- be \ fe |  T  | F
      --    T    |  F  | F
      --    F    |  T  | F
      ca be
      op "!"
      ca fe
      op "&&"
    [be, te, DLA_Literal (DLL_Bool False)] -> do
      ca be
      ca te
      op "&&"
    [be, te, DLA_Literal (DLL_Bool True)] -> do
      -- be \ te |  T  | F
      --    T    |  T  | F
      --    F    |  T  | T
      ca be
      op "!"
      ca te
      op "||"
    [be, te, fe] -> do
      ca fe
      ca te
      ca be
      op "select"
    _ -> impossible "ite args"
  where
    call o = \args -> do
      forM_ args ca
      op o

cconcatbs_ :: (DLType -> App ()) -> [(DLType, App ())] -> App ()
cconcatbs_ f l = do
  let totlen = typeSizeOf $ T_Tuple $ map fst l
  check_concat_len totlen
  case l of
    [] -> padding 0
    _ -> do
      forM_ (zip l dont_concat_first) $ \((t, m), a) ->
        m >> f t >> a

cconcatbs :: [(DLType, App ())] -> App ()
cconcatbs = cconcatbs_ ctobs

check_concat_len :: Integer -> App ()
check_concat_len totlen =
  case totlen <= 4096 of
    True -> nop
    False ->
      bad $
        "Cannot `concat` " <> texty totlen
          <> " bytes; the resulting byte array must be <= 4096 bytes."
          <> " This is caused by a Reach data type being too large."

cdigest :: [(DLType, App ())] -> App ()
cdigest l = cconcatbs l >> op "sha256"

cextract :: Integer -> Integer -> App ()
cextract _s 0 = do
  op "pop"
  padding 0
cextract s l =
  case s < 256 && l < 256 && l /= 0 of
    True -> do
      output $ TExtract (fromIntegral s) (fromIntegral l)
    False -> do
      cint s
      cint l
      op "extract3"

csubstring :: Integer -> Integer -> App ()
csubstring s e = cextract s (e - s)

computeSplice :: Integer -> Integer -> Integer -> (App (), App ())
computeSplice start end tot = (before, after)
  where
    -- XXX If start == 0, then we could remove before and have another version
    -- of the callers of computeSplice
    before = cextract 0 start
    after = cextract end (tot - end)

csplice :: SrcLoc -> Integer -> Integer -> Integer -> App ()
csplice _at b c e = do
  -- [ Bytes  = X b Y c Z e , NewBytes = Y' ]
  let len = c - b
  case len == 1 of
    True -> do
      -- [ Bytes, NewByte ]
      cint b
      -- [ Bytes, NewByte, Offset ]
      op "swap"
      -- [ Bytes, Offset, NewByte ]
      op "setbyte"
    False -> salloc_ "spliceNew" $ \store_new load_new -> do
      let (cbefore, cafter) = computeSplice b c e
      -- [ Big, New ]
      store_new
      -- [ Big ]
      csplice3 Nothing cbefore cafter load_new
  -- [ Big' ]
  -- [ Bytes' = X b Y'c Z e]
  return ()

csplice3 :: Maybe (App ()) -> App () -> App () -> App () -> App ()
csplice3 Nothing cbefore cafter cnew = do
  -- [ Big ]
  op "dup"
  -- [ Big, Big ]
  cbefore
  -- [ Big, Before ]
  cnew
  -- [ Big, Before, New ]
  op "concat"
  -- [ Big, Mid' ]
  op "swap"
  -- [ Mid', Big ]
  cafter
  -- [ Mid', After ]
  op "concat"
  -- [ Big' ]
  return ()
csplice3 (Just cbig) cbefore cafter cnew = do
  cbig
  cbefore
  cnew
  op "concat"
  cbig
  cafter
  op "concat"

cArraySet :: SrcLoc -> (DLType, Integer) -> Maybe (App ()) -> Either Integer (App ()) -> App () -> App ()
cArraySet _at (t, alen) mcbig eidx cnew = do
  let tsz = typeSizeOf t
  let (cbefore, cafter) =
        case eidx of
          Left ii ->
            computeSplice start end tot
            where
              start = ii * tsz
              end = start + tsz
              tot = alen * tsz
          Right cidx -> (b, a)
            where
              b = do
                cint 0
                cint tsz
                cidx
                op "*"
                op "substring3"
              a = do
                cint tsz
                op "dup"
                cidx
                op "*"
                op "+"
                cint $ alen * tsz
                op "substring3"
  csplice3 mcbig cbefore cafter cnew

computeExtract :: [DLType] -> Integer -> (DLType, Integer, Integer)
computeExtract ts idx = (t, start, sz)
  where
    szs = map typeSizeOf ts
    starts = scanl (+) 0 szs
    idx' = fromIntegral idx
    tsz = zip3 ts starts szs
    (t, start, sz) =
      case atMay tsz idx' of
        Nothing -> impossible "bad idx"
        Just x -> x

cfor :: Integer -> (App () -> App ()) -> App ()
cfor 0 _ = return ()
cfor 1 body = body (cint 0)
cfor maxi body = do
  when (maxi < 2) $ impossible "cfor maxi=0"
  top_lab <- freshLabel "forTop"
  end_lab <- freshLabel "forEnd"
  block_ top_lab $ do
    salloc_ (top_lab <> "Idx") $ \store_idx load_idx -> do
      cint 0
      store_idx
      label top_lab
      output $ TFor_top maxi
      body load_idx
      load_idx
      cint 1
      op "+"
      op "dup"
      store_idx
      cint maxi
      op "<"
      output $ TFor_bnz top_lab maxi end_lab
    label end_lab
    return ()

doArrayRef :: SrcLoc -> DLArg -> Bool -> Either DLArg (App ()) -> App ()
doArrayRef at aa frombs ie = do
  let (t, _) = argArrTypeLen aa
  ca aa
  cArrayRef at t frombs ie

cArrayRef :: SrcLoc -> DLType -> Bool -> Either DLArg (App ()) -> App ()
cArrayRef _at t frombs ie = do
  let tsz = typeSizeOf t
  let ie' =
        case ie of
          Left ia -> ca ia
          Right x -> x
  case t of
    T_Bool -> do
      ie'
      op "getbyte"
      case frombs of
        True -> nop
        False -> ctobs T_Bool
    _ -> do
      case ie of
        Left (DLA_Literal (DLL_Int _ ii)) -> do
          let start = ii * tsz
          cextract start tsz
        _ -> do
          cint tsz
          ie'
          op "*"
          cint tsz
          op "extract3"
      case frombs of
        True -> cfrombs t
        False -> nop

cla :: DLLargeArg -> App ()
cla = \case
  DLLA_Array t as ->
    case t of
      T_Bool ->
        case cas_boolbs as of
          Nothing -> normal
          Just x -> cbs x
      _ -> normal
    where
      normal = cconcatbs $ map (\a -> (t, ca a)) as
  DLLA_Tuple as ->
    cconcatbs $ map (\a -> (argTypeOf a, ca a)) as
  DLLA_Obj m -> cla $ DLLA_Struct $ M.toAscList m
  DLLA_Data tm vn va -> do
    let h ((k, v), i) = (k, (i, v))
    let tm' = M.fromList $ map h $ zip (M.toAscList tm) [0 ..]
    let (vi, vt) = fromMaybe (impossible $ "dla_data") $ M.lookup vn tm'
    cbs $ B.singleton $ BI.w2c vi
    ca va
    ctobs vt
    let vlen = 1 + typeSizeOf (argTypeOf va)
    op "concat"
    let dlen = typeSizeOf $ T_Data tm
    czpad $ fromIntegral $ dlen - vlen
    check_concat_len dlen
  DLLA_Struct kvs ->
    cconcatbs $ map (\a -> (argTypeOf a, ca a)) $ map snd kvs
  DLLA_Bytes bs -> cbs bs

cbs :: B.ByteString -> App ()
cbs = output . TBytes

cTupleRef :: SrcLoc -> DLType -> Integer -> App ()
cTupleRef _at tt idx = do
  -- [ Tuple ]
  let ts = tupleTypes tt
  let (t, start, sz) = computeExtract ts idx
  case (ts, idx) of
    ([_], 0) ->
      return ()
    _ -> do
      cextract start sz
  -- [ ValueBs ]
  cfrombs t
  -- [ Value ]
  return ()

computeSubstring :: [DLType] -> Integer -> (DLType, Integer, Integer)
computeSubstring ts idx = (t, start, end)
  where
    (t, start, sz) = computeExtract ts idx
    end = start + sz

cTupleSet :: SrcLoc -> DLType -> Integer -> App ()
cTupleSet at tt idx = do
  -- [ Tuple, Value' ]
  let tot = typeSizeOf tt
  let ts = tupleTypes tt
  let (t, start, end) = computeSubstring ts idx
  ctobs t
  -- [ Tuple, Value'Bs ]
  csplice at start end tot
  -- [ Tuple' ]
  return ()

cMapLoad :: App ()
cMapLoad = do
  Env {..} <- ask
  labK <- freshLabel "mapLoadK"
  labReal <- freshLabel "mapLoadDo"
  labDef <- freshLabel "mapLoadDef"
  op "dup"
  code "txn" ["ApplicationID"]
  op "app_opted_in"
  code "bnz" [labReal]
  label labDef
  op "pop"
  padding eMapDataSize
  code "b" [labK]
  label labReal
  let getOne mi = do
        -- [ Address ]
        cbs $ keyVary mi
        -- [ Address, Key ]
        op "app_local_get"
        -- [ MapData ]
        return ()
  case eMapKeysl of
    -- Special case one key:
    [0] -> getOne 0
    _ -> do
      -- [ Address ]
      -- [ Address, MapData_0? ]
      forM_ (zip eMapKeysl $ False : repeat True) $ \(mi, doConcat) -> do
        -- [ Address, MapData_N? ]
        case doConcat of
          True -> code "dig" ["1"]
          False -> op "dup"
        -- [ Address, MapData_N?, Address ]
        getOne mi
        -- [ Address, MapData_N?, NewPiece ]
        case doConcat of
          True -> op "concat"
          False -> nop
        -- [ Address, MapData_N+1 ]
        return ()
      -- [ Address, MapData_k ]
      op "swap"
      op "pop"
      -- [ MapData ]
      return ()
  label labK

cMapStore :: SrcLoc -> App ()
cMapStore _at = do
  Env {..} <- ask
  -- [ Address, MapData' ]
  case eMapKeysl of
    -- Special case one key:
    [0] -> do
      -- [ Address, MapData' ]
      cbs $ keyVary 0
      -- [ Address, MapData', Key ]
      op "swap"
      -- [ Address, Key, Value ]
      op "app_local_put"
    _ -> do
      forM_ eMapKeysl $ \mi -> do
        -- [ Address, MapData' ]
        code "dig" ["1"]
        -- [ Address, MapData', Address ]
        cbs $ keyVary mi
        -- [ Address, MapData', Address, Key ]
        code "dig" ["2"]
        -- [ Address, MapData', Address, Key, MapData' ]
        cStateSlice eMapDataSize mi
        -- [ Address, MapData', Address, Key, Value ]
        op "app_local_put"
        -- [ Address, MapData' ]
        return ()
      -- [ Address, MapData' ]
      op "pop"
      op "pop"
      -- [ ]
      return ()

divup :: Integer -> Integer -> Integer
divup x y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y)

computeStateSizeAndKeys :: Monad m => (LT.Text -> m ()) -> LT.Text -> Integer -> Integer -> m (Integer, [Word8])
computeStateSizeAndKeys badx prefix size limit = do
  let keys = size `divup` algoMaxAppBytesValueLen_usable
  when (keys > limit) $ do
    badx $ "Too many " <> prefix <> " keys, " <> texty keys <> ", but limit is " <> texty limit
  let keysl = take (fromIntegral keys) [0 ..]
  return (keys, keysl)

cSvsLoad :: Integer -> App ()
cSvsLoad size = do
  (_, keysl) <- computeStateSizeAndKeys bad "svs" size algoMaxGlobalSchemaEntries_usable
  unless (null keysl) $ do
    -- [ SvsData_0? ]
    forM_ (zip keysl $ False : repeat True) $ \(mi, doConcat) -> do
      -- [ SvsData_N? ]
      cbs $ keyVary mi
      -- [ SvsData_N?, Key ]
      op "app_global_get"
      -- [ SvsData_N?, NewPiece ]
      case doConcat of
        True -> op "concat"
        False -> nop
      -- [ SvsData_N+1 ]
      return ()
    -- [ SvsData_k ]
    gvStore GV_svs

cSvsSave :: SrcLoc -> [DLArg] -> App ()
cSvsSave _at svs = do
  let la = DLLA_Tuple svs
  let lat = largeArgTypeOf la
  let size = typeSizeOf lat
  cla la
  ctobs lat
  (_, keysl) <- computeStateSizeAndKeys bad "svs" size algoMaxGlobalSchemaEntries_usable
  ssr <- asks eStateSizeR
  liftIO $ modifyIORef ssr $ max size
  -- [ SvsData ]
  forM_ keysl $ \vi -> do
    -- [ SvsData ]
    cbs $ keyVary vi
    -- [ SvsData, Key ]
    code "dig" ["1"]
    -- [ SvsData, Key, SvsData ]
    cStateSlice size vi
    -- [ SvsData, Key, ViewData' ]
    op "app_global_put"
    -- [ SvsData ]
    return ()
  -- [ SvsData ]
  op "pop"
  -- [ ]
  return ()

cGetBalance :: SrcLoc -> Maybe DLArg -> App ()
cGetBalance _at = \case
  Nothing -> do
    -- []
    cContractAddr
    op "balance"
    -- [ bal ]
    cContractAddr
    op "min_balance"
    -- [ bal, min_bal ]
    op "-"
  Just tok -> do
    cContractAddr
    incResource R_Asset tok
    ca tok
    code "asset_holding_get" [ "AssetBalance" ]
    op "pop"

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_LArg _ a -> cla a
  DLE_Impossible at _ (Err_Impossible_Case s) ->
    impossible $ "ce: impossible case `" <> s <> "` encountered at: " <> show at
  DLE_Impossible at _ err -> expect_thrown at err
  DLE_VerifyMuldiv at _ _ _ err ->
    expect_thrown at err
  DLE_PrimOp _ p args -> cprim p args
  DLE_ArrayRef at aa ia -> doArrayRef at aa True (Left ia)
  DLE_ArraySet at aa ia va -> do
    let (t, alen) = argArrTypeLen aa
    case t of
      T_Bool -> do
        ca aa
        ca ia
        ca va
        op "setbyte"
      _ -> do
        let cnew = ca va >> ctobs t
        mcbig <-
          argSmall aa >>= \case
            False -> do
              ca aa
              return $ Nothing
            True -> do
              return $ Just $ ca aa
        let eidx =
              case ia of
                DLA_Literal (DLL_Int _ ii) -> Left ii
                _ -> Right $ ca ia
        cArraySet at (t, alen) mcbig eidx cnew
  DLE_ArrayConcat _ x y -> do
    let (xt, xlen) = argArrTypeLen x
    let (_, ylen) = argArrTypeLen y
    ca x
    ca y
    check_concat_len $ (xlen + ylen) * typeSizeOf xt
    op "concat"
  DLE_TupleRef at ta idx -> do
    ca ta
    cTupleRef at (argTypeOf ta) idx
  DLE_ObjectRef _at oa f -> do
    let fts = argObjstrTypes oa
    let fidx = fromIntegral $ fromMaybe (impossible "field") $ List.findIndex ((== f) . fst) fts
    let (t, start, sz) = computeExtract (map snd fts) fidx
    ca oa
    cextract start sz
    cfrombs t
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> cdigest $ map go args
    where
      go a = (argTypeOf a, ca a)
  DLE_Transfer mt_at who mt_amt mt_mtok -> do
    let mt_always = False
    let mt_mrecv = Just $ Left who
    let mt_mcclose = Nothing
    let mt_next = False
    let mt_submit = True
    void $ makeTxn $ MakeTxn {..}
  DLE_TokenInit mt_at tok -> do
    block_ "TokenInit" $ do
      let mt_always = True
      let mt_mtok = Just tok
      let mt_amt = DLA_Literal $ DLL_Int sb 0
      let mt_mrecv = Nothing
      let mt_next = False
      let mt_submit = True
      let mt_mcclose = Nothing
      let ct_at = mt_at
      let ct_mtok = Nothing
      let ct_amt = DLA_Literal $ minimumBalance_l
      addInitTok tok
      void $ checkTxn $ CheckTxn {..}
      void $ makeTxn $ MakeTxn {..}
  DLE_CheckPay ct_at fs ct_amt ct_mtok -> do
    void $ checkTxn $ CheckTxn {..}
    show_stack "CheckPay" Nothing ct_at fs
  DLE_Claim at fs t a mmsg -> do
    let check = ca a >> assert
    case t of
      CT_Assert -> impossible "assert"
      CT_Assume _ -> check
      CT_Require -> check
      CT_Possible -> impossible "possible"
      CT_Unknowable {} -> impossible "unknowable"
    show_stack "Claim" mmsg at fs
  DLE_Wait {} -> nop
  DLE_PartSet _ _ a -> ca a
  DLE_MapRef _ (DLMVar i) fa -> do
    incResource R_Account fa
    ca fa
    cMapLoad
    mdt <- getMapDataTy
    cTupleRef sb mdt $ fromIntegral i
  DLE_MapSet at mpv@(DLMVar i) fa mva -> do
    incResource R_Account fa
    Env {..} <- ask
    mdt <- getMapDataTy
    mt <- getMapTy mpv
    case (length eMapKeysl) == 1 && (M.size eMaps) == 1 of
      -- Special case one key and one map
      True -> do
        ca fa
        cla $ mdaToMaybeLA mt mva
        cMapStore at
      _ -> do
        ca fa
        op "dup"
        cMapLoad
        cla $ mdaToMaybeLA mt mva
        cTupleSet at mdt $ fromIntegral i
        cMapStore at
  DLE_Remote at fs ro rng_ty rm (DLPayAmt pay_net pay_ks) as (DLWithBill _nRecv nnRecv _nnZero) -> do
    warn_lab <- asks eWhich >>= \case
      Just which -> return $ "Step " <> show which
      Nothing -> return $ "This program"
    warn $ LT.pack $
      warn_lab <> " calls a remote object at " <> show at <> ". This means that Reach's conservative analysis of resource utilization and fees is incorrect, because we cannot take into account the needs of the remote object."
    let ts = map argTypeOf as
    let sig = signatureStr rm ts (Just rng_ty)
    remoteTxns <- liftIO $ newCounter 0
    let mayIncTxn m = do
          b <- m
          when b $
            void $ liftIO $ incCounter remoteTxns
          return b
    -- Figure out what we're calling
    salloc_ "remote address" $ \storeAddr loadAddr -> do
      salloc_ "pre balances" $ \storeBals loadBals -> do
        cbs "appID"
        ca ro
        ctobs T_UInt
        op "concat"
        op "sha512_256"
        storeAddr
        let mtoksBill = Nothing : map Just nnRecv
        let mtoksiAll = zip [0..] mtoksBill
        let (mtoksiBill, mtoksiZero) = splitAt (length mtoksBill) mtoksiAll
        let paid = M.fromList $ (Nothing, pay_net) : (map (\(x, y) -> (Just y, x)) pay_ks)
        let balsT = T_Tuple $ map (const T_UInt) mtoksiAll
        let gb_pre _ mtok = do
              cGetBalance at mtok
              case M.lookup mtok paid of
                Nothing -> return ()
                Just amt -> do
                  ca amt
                  op "-"
        cconcatbs $ map (\(i, mtok) -> (T_UInt, gb_pre i mtok)) mtoksiAll
        storeBals
        -- Start the call
        let mt_at = at
        let mt_mcclose = Nothing
        let mt_mrecv = Just $ Right loadAddr
        let mt_always = False
        hadNet <- (do
          let mt_amt = pay_net
          let mt_mtok = Nothing
          let mt_next = False
          let mt_submit = False
          mayIncTxn $ makeTxn $ MakeTxn {..})
        let foldMy a l f = foldM f a l
        hadSome <- foldMy hadNet pay_ks $ \mt_next (mt_amt, tok) -> do
          let mt_mtok = Just tok
          let mt_submit = False
          x <- mayIncTxn $ makeTxn $ MakeTxn {..}
          return $ hadNet || x
        itxnNextOrBegin hadSome
        output $ TConst "appl"
        makeTxn1 "TypeEnum"
        ca ro
        -- XXX remove this when JJ changes stuff for us
        incResource R_Account ro
        makeTxn1 "ApplicationID"
        let as' = (DLA_Literal $ DLL_Int at $ fromIntegral $ sigStrToInt sig) : as
        forM_ as' $ \a -> do
          ca a
          ctobs $ argTypeOf a
          makeTxn1 "ApplicationArgs"
        -- XXX If we can "inherit" resources, then this needs to be removed and
        -- we need to check that nnZeros actually stay 0
        forM_ nnRecv $ \a -> do
          incResource R_Asset a
          ca a
          makeTxn1 "Assets"
        op "itxn_submit"
        show_stack ("Remote: " <> sig) Nothing at fs
        appl_idx <- liftIO $ readCounter remoteTxns
        let gb_post idx mtok = do
              cGetBalance at mtok
              loadBals
              cTupleRef at balsT idx
              op "-"
        cconcatbs $ map (\(i, mtok) -> (T_UInt, gb_post i mtok)) mtoksiBill
        forM_ mtoksiZero $ \(idx, mtok) -> do
          cGetBalance at mtok
          loadBals
          cTupleRef at balsT idx
          asserteq
        code "gitxn" [ texty appl_idx, "LastLog" ]
        output $ TExtract 4 0 -- (0 = to the end)
        op "concat"
  DLE_TokenNew at (DLTokenNew {..}) -> do
    block_ "TokenNew" $ do
      let ct_at = at
      let ct_mtok = Nothing
      let ct_amt = DLA_Literal $ minimumBalance_l
      void $ checkTxn $ CheckTxn {..}
      itxnNextOrBegin False
      let vTypeEnum = "acfg"
      output $ TConst vTypeEnum
      makeTxn1 "TypeEnum"
      ca dtn_supply >> makeTxn1 "ConfigAssetTotal"
      maybe (cint_ at 6) ca dtn_decimals >> makeTxn1 "ConfigAssetDecimals"
      ca dtn_sym >> makeTxn1 "ConfigAssetUnitName"
      ca dtn_name >> makeTxn1 "ConfigAssetName"
      ca dtn_url >> makeTxn1 "ConfigAssetURL"
      ca dtn_metadata >> makeTxn1 "ConfigAssetMetadataHash"
      cContractAddr >> makeTxn1 "ConfigAssetManager"
      op "itxn_submit"
      code "itxn" ["CreatedAssetID"]
  DLE_TokenBurn {} ->
    -- Burning does nothing on Algorand, because we already own it and we're
    -- the creator, and that's the rule for being able to destroy
    return ()
  DLE_TokenDestroy _at aida -> do
    itxnNextOrBegin False
    let vTypeEnum = "acfg"
    output $ TConst vTypeEnum
    makeTxn1 "TypeEnum"
    incResource R_Asset aida
    ca aida
    makeTxn1 "ConfigAsset"
    op "itxn_submit"
    -- XXX We could give the minimum balance back to the creator
    return ()
  DLE_TimeOrder {} -> impossible "timeorder"
  DLE_GetContract _ -> code "txn" ["ApplicationID"]
  DLE_GetAddress _ -> cContractAddr
  DLE_EmitLog at k vs -> do
    let internal = do
          (v, n) <- case vs of
            [v'@(DLVar _ _ _ n')] -> return (v', n')
            _ -> impossible "algo ce: Expected one value"
          clog $
            [ DLA_Literal (DLL_Int at $ fromIntegral n)
            , DLA_Var v
            ]
          cv v
          return $ v
    case k of
      L_Internal -> void $ internal
      L_Api {} -> do
        v <- internal
        --op "dup" -- API log values are never used
        ctobs $ varType v
        gvStore GV_apiRet
      L_Event ml en -> do
        let name = maybe en (\l -> bunpack l <> "_" <> en) ml
        clogEvent name vs
        --cl DLL_Null -- Event log values are never used
  DLE_setApiDetails _at p _ _ _ -> do
    label $ apiLabel p
  DLE_GetUntrackedFunds at mtok tb -> do
    after_lab <- freshLabel "getActualBalance"
    cGetBalance at mtok
    -- [ bal ]
    ca tb
    -- [ bal, rsh_bal ]
    case mtok of
      Nothing -> do
        -- [ bal, rsh_bal ]
        op "-"
        -- [ extra ]
        return ()
      Just _ -> do
        cb_lab <- freshLabel $ "getUntrackedFunds" <> "_z"
        -- [ bal, rsh_bal ]
        op "dup2"
        -- [ bal, rsh_bal, bal, rsh_bal ]
        op "<"
        -- [ bal, rsh_bal, {0, 1} ]
        -- Branch IF the bal < rsh_bal
        code "bnz" [ cb_lab ]
        -- [ bal, rsh_bal ]
        op "-"
        code "b" [ after_lab ]
        -- This happens because of clawback
        label cb_lab
        -- [ bal, rsh_bal ]
        op "pop"
        -- [ bal ]
        op "pop"
        -- [  ]
        cint 0
        -- [ extra ]
        return ()
    label after_lab
  DLE_FromSome _ mo da -> do
    ca da
    ca mo
    salloc_ "fromSome object" $ \cstore cload -> do
      cstore
      cextractDataOf cload da
      cload
      cint 0
      op "getbyte"
    -- [ Default, Object, Tag ]
    -- [ False, True, Cond ]
    op "select"
  where
    show_stack :: String -> Maybe BS.ByteString -> SrcLoc -> [SLCtxtFrame] -> App ()
    show_stack what msg at fs = do
      let msg' =
            case msg of
              Nothing -> ""
              Just x -> ": " <> x
      comment $ LT.pack $ "^ " <> what <> (bunpack msg')
      comment $ LT.pack $ "at " <> (unsafeRedactAbsStr $ show at)
      forM_ fs $ \f ->
        comment $ LT.pack $ unsafeRedactAbsStr $ show f

signatureStr :: String -> [DLType] -> Maybe DLType -> String
signatureStr f args mret = sig
  where
    rets = fromMaybe "" $ fmap typeSig mret
    sig = f <> "(" <> intercalate "," (map typeSig args) <> ")" <> rets

sigStrToBytes :: String -> BS.ByteString
sigStrToBytes sig = shabs
  where
    sha = hashWith SHA512t_256 $ bpack sig
    shabs = BS.take 4 $ BA.convert sha

sigStrToInt :: String -> Int
sigStrToInt = fromIntegral . btoi . sigStrToBytes

clogEvent :: String -> [DLVar] -> App ()
clogEvent eventName vs = do
  let sigStr = signatureStr eventName (map varType vs) Nothing
  let as = map DLA_Var vs
  let cheader = cbs (bpack sigStr) >> op "sha512_256" >> output (TSubstring 0 4)
  cconcatbs $ (T_Bytes 4, cheader) : map (\a -> (argTypeOf a, ca a)) as
  clog_ $ 4 + (typeSizeOf $ largeArgTypeOf $ DLLA_Tuple as)

clog_ :: Integer -> App ()
clog_ = output . TLog

clog :: [DLArg] -> App ()
clog as = do
  let la = DLLA_Tuple as
  cla la
  clog_ $ typeSizeOf $ largeArgTypeOf la

data CheckTxn = CheckTxn
  { ct_at :: SrcLoc
  , ct_amt :: DLArg
  , ct_mtok :: Maybe DLArg
  }

data MakeTxn = MakeTxn
  { mt_at :: SrcLoc
  , mt_mrecv :: Maybe (Either DLArg (App ()))
  , mt_mcclose :: Maybe (App ())
  , mt_amt :: DLArg
  , mt_always :: Bool
  , mt_mtok :: Maybe DLArg
  , mt_next :: Bool
  , mt_submit :: Bool
  }

checkTxn1 :: LT.Text -> App ()
checkTxn1 f = do
  code "dig" ["1"]
  code "gtxns" [f]
  asserteq

makeTxn1 :: LT.Text -> App ()
makeTxn1 f = code "itxn_field" [f]

checkTxnUsage_ :: (DLArg -> App Bool) -> AlgoError -> SrcLoc -> Maybe DLArg -> App ()
checkTxnUsage_ isXTok err at mtok = do
  case mtok of
    Just tok -> do
      x <- isXTok tok
      when x $ do
        bad $ LT.pack $ getErrorMessage [] at True err
    Nothing -> return ()

makeTxnUsage :: SrcLoc -> Maybe DLArg -> App ()
makeTxnUsage = checkTxnUsage_ isNewTok Err_TransferNewToken

checkTxnUsage :: SrcLoc -> Maybe DLArg -> App ()
checkTxnUsage = checkTxnUsage_ isInitTok Err_PayNewToken

ntokFields :: (LT.Text, LT.Text, LT.Text, LT.Text)
ntokFields = ("pay", "Receiver", "Amount", "CloseRemainderTo")

tokFields :: (LT.Text, LT.Text, LT.Text, LT.Text)
tokFields = ("axfer", "AssetReceiver", "AssetAmount", "AssetCloseTo")

checkTxn :: CheckTxn -> App Bool
checkTxn (CheckTxn {..}) =
  case staticZero ct_amt of
    True -> return False
    False -> block_ "checkTxn" $ do
      let check1 = checkTxn1
      let ((vTypeEnum, fReceiver, fAmount, _fCloseTo), extra) =
            case ct_mtok of
              Nothing ->
                (ntokFields, return ())
              Just tok ->
                (tokFields, textra)
                where
                  textra = ca tok >> check1 "XferAsset"
      checkTxnUsage ct_at ct_mtok
      useResource R_Txn
      gvLoad GV_txnCounter
      op "dup"
      cint 1
      op "+"
      gvStore GV_txnCounter
      ca ct_amt
      check1 fAmount
      extra
      output $ TConst vTypeEnum
      checkTxn1 "TypeEnum"
      when False $ do
        -- NOTE: We don't actually care about these... it's not our problem if the
        -- user does these things.
        cint 0
        checkTxn1 "Fee"
        czaddr
        checkTxn1 "Lease"
        czaddr
        checkTxn1 "RekeyTo"
      cContractAddr
      cfrombs T_Address
      check1 fReceiver
      op "pop" -- pop id
      return True

itxnNextOrBegin :: Bool -> App ()
itxnNextOrBegin isNext = do
  op (if isNext then "itxn_next" else "itxn_begin")
  -- We do this because by default it will inspect the remaining fee and only
  -- set it to zero if there is a surplus, which means that sometimes it means
  -- 0 and sometimes it means "take money from the escrow", which is dangerous,
  -- so we force it to be 0 here. The alternative would be to check that other
  -- fees were set correctly, but I believe that would be more annoying to
  -- track, so I don't.
  cint 0
  makeTxn1 "Fee"

makeTxn :: MakeTxn -> App Bool
makeTxn (MakeTxn {..}) =
  case (mt_always || not (staticZero mt_amt)) of
    False -> return False
    True -> block_ "makeTxn" $ do
      let ((vTypeEnum, fReceiver, fAmount, fCloseTo), extra) =
            case mt_mtok of
              Nothing ->
                (ntokFields, return ())
              Just tok ->
                (tokFields, textra)
                where
                  textra = do
                    incResource R_Asset tok
                    ca tok
                    makeTxn1 "XferAsset"
      makeTxnUsage mt_at mt_mtok
      itxnNextOrBegin mt_next
      ca mt_amt
      makeTxn1 fAmount
      output $ TConst vTypeEnum
      makeTxn1 "TypeEnum"
      whenJust mt_mcclose $ \cclose -> do
        cclose
        cfrombs T_Address
        makeTxn1 fCloseTo
      case mt_mrecv of
        Nothing -> cContractAddr
        Just (Left a) -> do
          incResource R_Account a
          ca a
        Just (Right cr) -> cr
      cfrombs T_Address
      makeTxn1 fReceiver
      extra
      when mt_submit $ op "itxn_submit"
      return True

cextractDataOf :: App () -> DLArg -> App ()
cextractDataOf cd va = do
  let vt = argTypeOf va
  let sz = typeSizeOf vt
  case sz == 0 of
    True -> padding 0
    False -> do
      cd
      cextract 1 sz
      cfrombs vt

doSwitch :: String -> (a -> App ()) -> SrcLoc -> DLVar -> SwitchCases a -> App ()
doSwitch lab ck _at dv csm = do
  let go cload = do
        let cm1 _vi (vn, (vv, vu, k)) = do
              l <- freshLabel $ lab <> "_" <> vn
              block l $
                case vu of
                  False -> ck k
                  True -> do
                    flip (sallocLet vv) (ck k) $ do
                      cextractDataOf cload (DLA_Var vv)
        cload
        cint 0
        op "getbyte"
        let csml = zip [0 ..] (M.toAscList csm)
        case csml of
          [ (0, x), (1, y) ] -> do
            y_lab <- freshLabel $ lab <> "_" <> "nz"
            code "bnz" [ y_lab ]
            cm1 x x
            label y_lab
            cm1 y y
          _ -> cblt lab cm1 $ bltL csml
  letSmall dv >>= \case
    True -> go (ca $ DLA_Var dv)
    False -> do
      salloc_ (textyv dv <> " for switch") $ \cstore cload -> do
        ca $ DLA_Var dv
        cstore
        go cload

cm :: App () -> DLStmt -> App ()
cm km = \case
  DL_Nop _ -> km
  DL_Let _ DLV_Eff de ->
    -- XXX this could leave something on the stack
    ce de >> km
  DL_Let _ (DLV_Let vc dv) de -> do
    sm <- exprSmall de
    recordNew <-
      case de of
        DLE_TokenNew {} -> do
          return True
        DLE_EmitLog _ _ [dv'] -> do
          isNewTok $ DLA_Var dv'
        _ -> do
          return False
    when recordNew $
      addNewTok $ DLA_Var dv
    sallocVarLet (DLVarLet (Just vc) dv) sm (ce de) km
  DL_ArrayMap at ansv as xs iv (DLBlock _ _ body ra) -> do
    let anssz = typeSizeOf $ argTypeOf $ DLA_Var ansv
    let xlen = arraysLength as
    let rt = argTypeOf ra
    check_concat_len anssz
    salloc_ (textyv ansv) $ \store_ans load_ans -> do
      cbs ""
      store_ans
      cfor xlen $ \load_idx -> do
        load_ans
        let finalK = cp (ca ra >> ctobs rt) body
        let bodyF (x, a) k = do
             doArrayRef at a True $ Right load_idx
             sallocLet x (return ()) $
               store_let iv True load_idx $
               k
        foldr bodyF finalK $ zip xs as
        op "concat"
        store_ans
      store_let ansv True load_ans km
  DL_ArrayReduce at ansv as za av xs iv (DLBlock _ _ body ra) -> do
    let xlen = arraysLength as
    salloc_ (textyv ansv) $ \store_ans load_ans -> do
      ca za
      store_ans
      store_let av True load_ans $ do
        cfor xlen $ \load_idx -> do
          let finalK = cp (ca ra) body
          let bodyF (x, a) k = do
               doArrayRef at a True $ Right load_idx
               sallocLet x (return ()) $
                 store_let iv True load_idx $
                 k
          foldr bodyF finalK $ zip xs as
          store_ans
        store_let ansv True load_ans km
  DL_Var _ dv ->
    salloc $ \loc -> do
      store_var dv loc $
        store_let dv True (output $ TLoad loc (textyv dv)) $
          km
  DL_Set _ dv da -> do
    loc <- lookup_var dv
    ca da
    output $ TStore loc (textyv dv)
    km
  DL_LocalIf _ a tp fp -> do
    ca a
    false_lab <- freshLabel "localIfF"
    join_lab <- freshLabel "localIfK"
    code "bz" [false_lab]
    cp (return ()) tp
    code "b" [join_lab]
    label false_lab
    cp (return ()) fp
    label join_lab
    km
  DL_LocalSwitch at dv csm -> do
    end_lab <- freshLabel $ "LocalSwitchK"
    doSwitch "LocalSwitch" (cp (code "b" [end_lab])) at dv csm
    label end_lab
    km
  DL_MapReduce {} ->
    impossible $ "cannot inspect maps at runtime"
  DL_Only {} ->
    impossible $ "only in CP"
  DL_LocalDo _ t -> cp km t

cp :: App () -> DLTail -> App ()
cp km = \case
  DT_Return _ -> km
  DT_Com m k -> cm (cp km k) m

ct :: CTail -> App ()
ct = \case
  CT_Com m k -> cm (ct k) m
  CT_If _ a tt ft -> do
    ca a
    false_lab <- freshLabel "ifF"
    code "bz" [false_lab]
    nct tt
    label false_lab
    nct ft
  CT_Switch at dv csm ->
    doSwitch "Switch" nct at dv csm
  CT_Jump _at which svs (DLAssignment msgm) -> do
    -- NOTE: I considered statically assigning these to heap pointers and
    -- limiting the total memory available (we can't assign them to where they
    -- will end up, because this tail might be using the same things)
    mapM_ ca $ (map DLA_Var svs) <> map snd (M.toAscList msgm)
    code "b" [loopLabel which]
  CT_From at which msvs -> do
    isHalt <- do
      case msvs of
        FI_Halt toks -> do
          forM_ toks close_asset
          close_escrow
          return True
          where
            mt_at = at
            mt_always = True
            mt_mrecv = Nothing
            mt_amt = DLA_Literal $ DLL_Int sb 0
            mt_mcclose = Just $ cDeployer
            mt_submit = True
            mt_next = False
            close_asset tok = void $ makeTxn $ MakeTxn {..}
              where
                mt_mtok = Just tok
            close_escrow = void $ makeTxn $ MakeTxn {..}
              where
                mt_mtok = Nothing
        FI_Continue svs -> do
          cSvsSave at $ map snd svs
          cint $ fromIntegral which
          gvStore GV_currentStep
          cRound
          gvStore GV_currentTime
          return False
    code "txn" ["OnCompletion"]
    output $ TConst $ if isHalt then "DeleteApplication" else "NoOp"
    asserteq
    code "b" ["updateState"]
  where
    nct = dupeResources . ct

-- Reach Constants
reachAlgoBackendVersion :: Int
reachAlgoBackendVersion = 9

-- State:
keyState :: B.ByteString
keyState = ""

keyVary :: Word8 -> B.ByteString
keyVary = B.singleton . BI.w2c

cContractAddr :: App ()
cContractAddr = code "global" ["CurrentApplicationAddress"]

cDeployer :: App ()
cDeployer = code "global" ["CreatorAddress"]

etexty :: Enum a => a -> LT.Text
etexty = texty . fromEnum

data ArgId
  = ArgMethod
  | ArgPublish
  | ArgTime
  | ArgMsg
  deriving (Eq, Ord, Show, Enum, Bounded)

argLoad :: ArgId -> App ()
argLoad ai = code "txna" ["ApplicationArgs", etexty ai]

boundedCount :: forall a. (Enum a, Bounded a) => a -> Integer
boundedCount _ = 1 + (fromIntegral $ fromEnum $ (maxBound :: a))

argCount :: Integer
argCount = boundedCount ArgMethod

data GlobalVar
  = GV_txnCounter
  | GV_currentStep
  | GV_currentTime
  | GV_svs
  | GV_argTime
  | GV_argMsg
  | GV_wasMeth
  | GV_apiRet
  deriving (Eq, Ord, Show, Enum, Bounded)

gvSlot :: GlobalVar -> ScratchSlot
gvSlot ai = fromIntegral $ fromEnum ai

gvOutput :: (ScratchSlot -> LT.Text -> TEAL) -> GlobalVar -> App ()
gvOutput f gv = output $ f (gvSlot gv) (textyt gv (gvType gv))

gvStore :: GlobalVar -> App ()
gvStore = gvOutput TStore

gvLoad :: GlobalVar -> App ()
gvLoad = gvOutput TLoad

gvType :: GlobalVar -> DLType
gvType = \case
  GV_txnCounter -> T_UInt
  GV_currentStep -> T_UInt
  GV_currentTime -> T_UInt
  GV_svs -> T_Null
  GV_argTime -> T_UInt
  GV_argMsg -> T_Null
  GV_wasMeth -> T_Bool
  GV_apiRet -> T_Null

keyState_gvs :: [GlobalVar]
keyState_gvs = [GV_currentStep, GV_currentTime]

keyState_ty :: DLType
keyState_ty = T_Tuple $ map gvType keyState_gvs

defn_fixed :: Label -> Bool -> App ()
defn_fixed l b = do
  label l
  cbool b
  op "return"

defn_done :: App ()
defn_done = defn_fixed "done" True

cRound :: App ()
cRound = code "global" ["Round"]

bindTime :: DLVar -> App a -> App a
bindTime dv = store_let dv True cRound

bindSecs :: DLVar -> App a -> App a
bindSecs dv = store_let dv True (code "global" ["LatestTimestamp"])

allocDLVar :: SrcLoc -> DLType -> App DLVar
allocDLVar at t =
  DLVar at Nothing t <$> ((liftIO . incCounter) =<< (eCounter <$> ask))

bindFromGV :: GlobalVar -> App () -> SrcLoc -> [DLVarLet] -> App a -> App a
bindFromGV gv ensure at vls m = do
  let notNothing = \case
        DLVarLet (Just _) _ -> True
        _ -> False
  case any notNothing vls of
    False -> m
    True -> do
      av <- allocDLVar at $ T_Tuple $ map varLetType vls
      ensure
      let go = \case
            [] -> m
            (dv, i) : more -> sallocVarLet dv False cgen $ go more
              where
                cgen = ce $ DLE_TupleRef at (DLA_Var av) i
      store_let av True (gvLoad gv) $
        go $ zip vls [0 ..]

bindFromStack :: SrcLoc -> [DLVarLet] -> App a -> App a
bindFromStack _at vsl m = do
  -- STACK: [ ...vs ] TOP on right
  let go m' v = sallocLet v (return ()) m'
  -- The 'l' is important here because it means we're nesting the computation
  -- from the left, so the bindings match the (reverse) push order
  foldl' go m $ map varLetVar vsl

cloop :: Int -> CHandler -> App ()
cloop _ (C_Handler {}) = return ()
cloop which (C_Loop at svs vars body) = recordWhich which $ do
  block (loopLabel which) $ do
    -- STACK: [ ...svs, ...vars ] TOP on right
    let bindVars = bindFromStack at $ svs <> vars
    bindVars $ ct body

-- NOTE This could be compiled to a jump table if that were possible with TEAL
cblt :: String -> (Int -> a -> App ()) -> BLT Int a -> App ()
cblt lab go t = do
  -- liftIO $ putStrLn $ show t
  rec 0 Nothing t
  where
    rec low mhi = \case
      Empty -> op "err"
      Branch rv l r -> do
        op "dup"
        cint $ fromIntegral rv
        op "<"
        llab <- freshLabel $ lab <> "_lt_" <> show rv
        code "bnz" [llab]
        rec rv mhi r
        label llab
        rec low (Just $ rv - 1) l
      Leaf which h -> do
        case (which == low && mhi == Just which) of
          True -> op "pop"
          False -> do
            cint $ fromIntegral which
            asserteq
        go which h

handlerLabel :: Int -> Label
handlerLabel w = "publish" <> texty w

apiLabel :: SLPart -> Label
apiLabel w = "api_" <> (LT.pack $ bunpack w)

bindFromSvs_ :: SrcLoc -> [DLVarLet] -> App a -> App a
bindFromSvs_ at svs m = do
  let ensure = cSvsLoad $ typeSizeOf $ T_Tuple $ map varLetType svs
  bindFromGV GV_svs ensure at svs m

ch :: Int -> CHandler -> App ()
ch _ (C_Loop {}) = return ()
ch which (C_Handler at int from prev svsl msgl timev secsv body) = recordWhich which $ do
  freeResource R_Account $ DLA_Var from
  let msg = map varLetVar msgl
  let isCtor = which == 0
  let argSize = 1 + (typeSizeOf $ T_Tuple $ map varType $ msg)
  when (argSize > algoMaxAppTotalArgLen) $
    bad $ LT.pack $
      "Step " <> show which <> "'s argument length is " <> show argSize
      <> ", but the limit is " <> show algoMaxAppTotalArgLen
      <> ". Step " <> show which <> " starts at " <> show at
  let bindFromMsg = bindFromGV GV_argMsg (return ()) at
  let bindFromSvs = bindFromSvs_ at svsl
  block (handlerLabel which) $ do
    comment "check step"
    cint $ fromIntegral prev
    gvLoad GV_currentStep
    asserteq
    comment "check time"
    gvLoad GV_argTime
    op "dup"
    cint 0
    op "=="
    op "swap"
    gvLoad GV_currentTime
    op "=="
    op "||"
    assert
    let bindVars =
          id
            . (store_let from True (code "txn" ["Sender"]))
            . (bindTime timev)
            . (bindSecs secsv)
            . bindFromSvs
            . (bindFromMsg $ map v2vl msg)
    bindVars $ do
      clogEvent ("_reach_e" <> show which) msg
      when isCtor $ do
        ce $ DLE_CheckPay at [] (DLA_Literal $ minimumBalance_l) Nothing
      let checkTime1 :: LT.Text -> App () -> DLArg -> App ()
          checkTime1 cmp clhs rhsa = do
            clhs
            ca rhsa
            op cmp
            assert
      let checkFrom_ = checkTime1 ">="
      let checkTo_ = checkTime1 "<"
      let makeCheck check_ = \case
            Left x -> check_ (cv timev) x
            Right x -> check_ (cv secsv) x
      let checkFrom = makeCheck checkFrom_
      let checkTo = makeCheck checkTo_
      let checkBoth v xx yy = do
            cv v
            checkFrom_ (op "dup") xx
            checkTo_ (return ()) yy
      let CBetween ifrom ito = int
      case (ifrom, ito) of
        (Nothing, Nothing) -> return ()
        (Just x, Nothing) -> checkFrom x
        (Nothing, Just y) -> checkTo y
        (Just x, Just y) ->
          case (x, y) of
            (Left xx, Left yy) -> checkBoth timev xx yy
            (Right xx, Right yy) -> checkBoth secsv xx yy
            (_, _) -> checkFrom x >> checkFrom y
      ct body

getMapTy :: DLMVar -> App DLType
getMapTy mpv = do
  ms <- asks eMaps
  return $
    case M.lookup mpv ms of
      Nothing -> impossible "getMapTy"
      Just mi -> dlmi_ty mi

mapDataTy :: DLMapInfos -> DLType
mapDataTy m = T_Tuple $ map (dlmi_tym . snd) $ M.toAscList m

getMapDataTy :: App DLType
getMapDataTy = asks eMapDataTy

type Disp = String -> T.Text -> IO String

cStateSlice :: Integer -> Word8 -> App ()
cStateSlice size iw = do
  let i = fromIntegral iw
  let k = algoMaxAppBytesValueLen_usable
  csubstring (k * i) (min size $ k * (i + 1))

compileTEAL :: String -> IO BS.ByteString
compileTEAL tealf = do
  (ec, stdout, stderr) <- readProcessWithExitCode "goal" ["clerk", "compile", tealf, "-o", "-"] mempty
  case ec of
    ExitFailure _ -> do
      let failed = impossible $ "The TEAL compiler failed with the message:\n" <> show stderr
      let tooBig = bpack tealf <> ": app program size too large: "
      case BS.isPrefixOf tooBig stderr of
        True -> do
          let notSpace = (32 /=)
          let sz_bs = BS.takeWhile notSpace $ BS.drop (BS.length tooBig) stderr
          let mlen :: Maybe Int = readMaybe $ bunpack sz_bs
          case mlen of
            Nothing -> failed
            Just sz -> return $ BS.replicate sz 0
        False -> failed
    ExitSuccess -> return stdout

data CMeth
  = CApi
    { capi_who :: SLPart
    , capi_sig :: String
    , capi_ret_ty :: DLType
    , capi_which :: Int
    , capi_arg_tys :: [DLType]
    , capi_doWrap :: App ()
    }
  | CView
    { cview_who :: SLPart
    , cview_sig :: String
    , cview_ret_ty :: DLType
    , cview_hs :: VSIHandler
    }

cmethRetTy :: CMeth -> DLType
cmethRetTy = \case
  CApi {..} -> capi_ret_ty
  CView {..} -> cview_ret_ty

cmethIsApi :: CMeth -> Bool
cmethIsApi = \case
  CApi {} -> True
  CView {} -> False

cmethIsView :: CMeth -> Bool
cmethIsView = \case
  CApi {} -> False
  CView {} -> True

capi :: (SLPart, ApiInfo) -> (String, CMeth)
capi (who, (ApiInfo {..})) = (capi_sig, c)
  where
    c = CApi {..}
    capi_who = who
    capi_which = ai_which
    capi_sig = signatureStr f capi_arg_tys mret
    f = bunpack who
    imp = impossible "apiSig"
    (capi_arg_tys, capi_doWrap) =
      case ai_compile of
        AIC_SpreadArg ->
          case ai_msg_tys of
            [T_Tuple ts] -> (ts, return ())
            _ -> imp
        AIC_Case ->
          case ai_msg_tys of
            [T_Data tm] ->
              case M.lookup cid tm of
                Just (T_Tuple ts) -> (ts, doWrapData ts $ cla . DLLA_Data tm cid)
                _ -> imp
            _ -> imp
    cid = fromMaybe imp ai_mcase_id
    capi_ret_ty = ai_ret_ty
    mret = Just $ capi_ret_ty

cview :: (SLPart, VSITopInfo) -> (String, CMeth)
cview (who, VSITopInfo cview_arg_tys cview_ret_ty cview_hs) = (cview_sig, c)
  where
    cview_who = who
    f = bunpack who
    cview_sig = signatureStr f cview_arg_tys $ Just cview_ret_ty
    c = CView {..}

doWrapData :: [DLType] -> (DLArg -> App ()) -> App ()
doWrapData tys mk = do
  -- Tuple of tys is on stack
  av <- allocDLVar sb $ T_Tuple tys
  sallocLet av (return ()) $ mk (DLA_Var av)
  -- Data of tys is on stack
  return ()

cmeth :: Int -> CMeth -> App ()
cmeth sigi = \case
  CApi who sig _ which tys doWrap -> do
    block_ (LT.pack $ bunpack who) $ do
      comment $ LT.pack $ "API: " <> sig
      comment $ LT.pack $ " ui: " <> show sigi
      let f :: DLType -> Integer -> (DLType, App ())
          f t i = (t, code "txna" ["ApplicationArgs", texty i])
      cconcatbs_ (const $ return ()) $ zipWith f tys [1 ..]
      doWrap
      gvStore GV_argMsg
      code "b" [handlerLabel which]
  CView who sig _ hs -> do
    block_ (LT.pack $ bunpack who) $ do
      comment $ LT.pack $ "View: " <> sig
      comment $ LT.pack $ "  ui: " <> show sigi
      gvLoad GV_currentStep
      flip (cblt "viewStep") (bltM hs) $ \hi vbvs -> separateResources $ do
        vbvs' <- liftIO $ add_counts vbvs
        let VSIBlockVS svsl deb = vbvs'
        let (DLinExportBlock _ mfargs (DLBlock at _ t r)) = deb
        let fargs = fromMaybe mempty mfargs
        block_ (LT.pack $ bunpack who <> "_" <> show hi) $
          bindFromArgs fargs $ bindFromSvs_ at svsl $
            flip cp t $ do
              ca r
              ctobs $ argTypeOf r
              gvStore GV_apiRet
              code "b" [ "apiReturn" ]

bindFromArgs :: [DLVarLet] -> App a -> App a
bindFromArgs vs m = do
  -- XXX deal with >15 args
  let go (v, i) = sallocVarLet v False (code "txna" ["ApplicationArgs", texty i] >> cfrombs (varLetType v))
  foldl' (flip go) m (zip vs [(1::Integer) ..])

data VSIBlockVS = VSIBlockVS [DLVarLet] DLExportBlock
type VSIHandler = M.Map Int VSIBlockVS
data VSITopInfo = VSITopInfo [DLType] DLType VSIHandler
type VSITop = M.Map SLPart VSITopInfo

instance AC VSIBlockVS where
  ac (VSIBlockVS svs eb) = do
    eb' <- ac eb
    svs' <- ac_vls svs
    return $ VSIBlockVS svs' eb'

selectFromM :: Ord b => (x -> c -> d) -> b -> M.Map a (x, M.Map b c) -> M.Map a d
selectFromM f k m = M.mapMaybe go m
  where
    go (x, m') = f x <$> M.lookup k m'

analyzeViews :: (CPViews, ViewInfos) -> VSITop
analyzeViews (vs, vis) = vsit
  where
    vsit = M.mapWithKey got $ flattenInterfaceLikeMap vs
    got who it = VSITopInfo args ret hs
      where
        hs = selectFromM VSIBlockVS who vsih
        (args, ret) =
          case it of
            IT_Val t -> ([], t)
            IT_Fun d r -> (d, r)
            IT_UDFun _ -> impossible $ "udview " <> bunpack who
    vsih = M.map goh vis
    goh (ViewInfo svs vi) = (map v2vl svs, flattenInterfaceLikeMap vi)

compile_algo :: CompilerToolEnv -> Disp -> PLProg -> IO ConnectorInfo
compile_algo env disp pl = do
  let PLProg _at plo dli _ _ _ cpp = pl
  let CPProg at vsi ai _ (CHandlers hm) = cpp
  let ai_sm = M.fromList $ map capi $ M.toAscList ai
  let vsiTop = analyzeViews vsi
  let vsi_sm = M.fromList $ map cview $ M.toAscList vsiTop
  let meth_sm = M.union ai_sm vsi_sm
  let maxApiRetSize = maxTypeSize $ M.map cmethRetTy meth_sm
  let meth_im = M.mapKeys sigStrToInt meth_sm
  let eMaps = dli_maps dli
  resr <- newIORef mempty
  eFailuresR <- newIORef mempty
  eWarningsR <- newIORef mempty
  let bad' = bad_io eFailuresR
  let warn' = bad_io eWarningsR
  let eMapDataTy = mapDataTy eMaps
  let eMapDataSize = typeSizeOf eMapDataTy
  let PLOpts {..} = plo
  let eCounter = plo_counter
  let recordSize prefix size = do
        modifyIORef resr $
          M.insert (prefix <> "Size") $
            Aeson.Number $ fromIntegral size
  let recordSizeAndKeys :: T.Text -> Integer -> Integer -> IO [Word8]
      recordSizeAndKeys prefix size limit = do
        (keys, keysl) <- computeStateSizeAndKeys bad' (LT.fromStrict prefix) size limit
        recordSize prefix size
        modifyIORef resr $
          M.insert (prefix <> "Keys") $
            Aeson.Number $ fromIntegral keys
        return $ keysl
  eMapKeysl <- recordSizeAndKeys "mapData" eMapDataSize algoMaxLocalSchemaEntries_usable
  eStateSizeR <- newIORef 0
  let run :: App () -> IO TEALs
      run m = do
        eLabel <- newCounter 0
        eOutputR <- newIORef mempty
        let eHP = fromIntegral $ fromEnum (maxBound :: GlobalVar)
        let eSP = 255
        let eVars = mempty
        let eLets = mempty
        let eLetSmalls = mempty
        let eWhich = Nothing
        eNewToks <- newIORef mempty
        eInitToks <- newIORef mempty
        eResources <- newResources
        flip runReaderT (Env {..}) m
        readIORef eOutputR
  unless (plo_untrustworthyMaps || null eMapKeysl) $ do
    warn' $ "This program was compiled with trustworthy maps, but maps are not trustworthy on Algorand, because they are represented with local state. A user can delete their local state at any time, by sending a ClearState transaction. The only way to use local state properly on Algorand is to ensure that a user doing this can only 'hurt' themselves and not the entire system."
  totalLenR <- newIORef (0 :: Integer)
  let addProg lab ts' = do
        t <- renderOut ts'
        tf <- disp (lab <> ".teal") t
        tbs <- compileTEAL tf
        modifyIORef totalLenR $ (+) (fromIntegral $ BS.length tbs)
        let tc = LT.toStrict $ encodeBase64 tbs
        modifyIORef resr $ M.insert (T.pack lab) $ Aeson.String tc
  let runProg lab showCost ls m = do
        ts <- run m
        let disp' = disp . (lab <>)
        let notify b = if b then bad' else warn'
        let ts' = optimize $ DL.toList ts
        _ <- checkCost notify disp' showCost ts' ls
        addProg lab ts'
  let h2lr = \case
        (i, C_Handler {..}) -> Just $ LabelRec {..}
          where
            lr_lab = handlerLabel i
            lr_at = ch_at
            lr_what = "Step " <> show i
        (_, C_Loop {}) -> Nothing
  let a2lr (p, ApiInfo {..}) = LabelRec {..}
        where
          lr_lab = apiLabel p
          lr_at = ai_at
          lr_what = "API " <> bunpack p
  let progLs = (mapMaybe h2lr $ M.toAscList hm) <> (map a2lr $ M.toAscList ai)
  runProg "appApproval" (cte_REACH_DEBUG env) progLs $ do
    useResource R_Txn
    cint 0
    gvStore GV_txnCounter
    code "txn" ["ApplicationID"]
    code "bz" ["alloc"]
    cbs keyState
    op "app_global_get"
    let nats = [0 ..]
    let shouldDups = reverse $ zipWith (\_ i -> i /= 0) keyState_gvs nats
    forM_ (zip (zip keyState_gvs shouldDups) nats) $ \((gv, shouldDup), i) -> do
      when shouldDup $ op "dup"
      cTupleRef at keyState_ty i
      gvStore gv
    unless (null eMapKeysl) $ do
      -- NOTE We could allow an OptIn if we are not going to halt
      code "txn" ["OnCompletion"]
      output $ TConst "OptIn"
      op "=="
      code "bz" ["normal"]
      code "txn" ["Sender"]
      padding eMapDataSize
      cMapStore at
      code "b" ["checkSize"]
      -- The NON-OptIn case:
      label "normal"
    when False $ do
      -- NOTE: We don't actually care about this, because there will be a
      -- different failure if there are too few and if there are too few, who
      -- cares?
      code "txn" ["NumAppArgs"]
      cint argCount
      asserteq
    argLoad ArgMethod
    cfrombs T_UInt
    label "preamble"
    op "dup"
    code "bz" ["publish"]
    label "api"
    cint 0
    gvStore GV_argTime
    cbool True
    gvStore GV_wasMeth
    cblt "method" cmeth $ bltM meth_im
    label "publish"
    argLoad ArgPublish
    cfrombs T_UInt
    argLoad ArgTime
    cfrombs T_UInt
    gvStore GV_argTime
    argLoad ArgMsg
    gvStore GV_argMsg
    cblt "publish" ch $ bltM hm
    forM_ (M.toAscList hm) $ \(hi, hh) ->
      cloop hi hh
    label "updateState"
    cbs keyState
    forM_ keyState_gvs $ \gv -> do
      gvLoad gv
      ctobs $ gvType gv
    forM_ (tail keyState_gvs) $ const $ op "concat"
    op "app_global_put"
    gvLoad GV_wasMeth
    code "bz" ["checkSize"]
    label "apiReturn"
    -- SHA-512/256("return")[0..4] = 0x151f7c75
    cbs $ BS.pack [0x15, 0x1f, 0x7c, 0x75]
    gvLoad GV_apiRet
    op "concat"
    clog_ $ 4 + maxApiRetSize
    code "b" ["checkSize"]
    label "checkSize"
    gvLoad GV_txnCounter
    op "dup"
    -- The size is correct
    cint 1
    op "+"
    code "global" ["GroupSize"]
    asserteq
    -- We're last
    code "txn" ["GroupIndex"]
    asserteq
    when False $ do
      -- There's no point to checking this, because if the fee is too much,
      -- there's no harm and if it is too low, the network will reject it
      -- anyways
      cint algoMinTxnFee
      op "*"
      code "txn" ["Fee"]
      op "<="
      assert
    code "b" ["done"]
    defn_done
    label "alloc"
    code "txn" ["OnCompletion"]
    output $ TConst "NoOp"
    asserteq
    forM_ keyState_gvs $ \gv -> do
      ctzero $ gvType gv
      gvStore gv
    code "b" ["updateState"]
  -- Clear state is never allowed
  addProg "appClear" []
  stateSize <- readIORef eStateSizeR
  void $ recordSizeAndKeys "state" stateSize algoMaxGlobalSchemaEntries_usable
  totalLen <- readIORef totalLenR
  unless (totalLen <= algoMaxAppProgramLen_really) $ do
    bad' $ LT.pack $ "The program is too long; its length is " <> show totalLen <> ", but the maximum possible length is " <> show algoMaxAppProgramLen_really
  let extraPages :: Integer = ceiling ((fromIntegral totalLen :: Double) / fromIntegral algoMaxAppProgramLen) - 1
  modifyIORef resr $
    M.insert "extraPages" $
      Aeson.Number $ fromIntegral $ extraPages
  sFailures <- readIORef eFailuresR
  sWarnings <- readIORef eWarningsR
  let wss w lab ss = do
        unless (null ss) $
          emitWarning Nothing $ w $ S.toList $ S.map LT.unpack ss
        modifyIORef resr $ M.insert lab $
          aarray $ S.toList $ S.map (Aeson.String . LT.toStrict) ss
  wss W_ALGOConservative "warnings" sWarnings
  wss W_ALGOUnsupported "unsupported" sFailures
  let apiEntry lab f = (lab, aarray $ map (Aeson.String . s2t) $ M.keys $ M.filter f meth_sm)
  modifyIORef resr $
    M.insert "ABI" $
      aobject $
        M.fromList $
          [ apiEntry "sigs" (const True)
          , apiEntry "impure" cmethIsApi
          , apiEntry "pure" cmethIsView
          ]
  modifyIORef resr $
    M.insert "version" $
      Aeson.Number $ fromIntegral $ reachAlgoBackendVersion
  res <- readIORef resr
  return $ aobject res

connect_algo :: CompilerToolEnv -> Connector
connect_algo env = Connector {..}
  where
    conName = conName'
    conCons = conCons'
    conGen moutn pl = case moutn of
      Nothing -> withSystemTempDirectory "reachc-algo" $ \d ->
        go (\w -> d </> T.unpack w) pl
      Just outn -> go outn pl
    go :: (T.Text -> String) -> PLProg -> IO ConnectorInfo
    go outn = compile_algo env disp
      where
        disp :: String -> T.Text -> IO String
        disp which c = do
          let oi = which
          let oit = T.pack oi
          let f = outn oit
          conWrite (Just outn) oit c
          return f
