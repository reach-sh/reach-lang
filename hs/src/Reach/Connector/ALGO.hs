module Reach.Connector.ALGO (connect_algo, AlgoError(..)) where

import Control.Monad.Extra
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.ByteString.Base64 (encodeBase64', decodeBase64)
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL
import Data.Either
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as Vector
import Data.Word
import GHC.Stack (HasCallStack)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Connector
import Reach.Counter
import Reach.FixedPoint
import Reach.Texty (pretty)
import Reach.UnsafeUtil
import Reach.Util
import Reach.Warning
import Safe (atMay)
import Text.Read
import Generics.Deriving ( Generic )
import Reach.CommandLine
import Data.List (intercalate)
import Crypto.Hash

-- import Debug.Trace

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

-- General tools that could be elsewhere

type LPGraph a = M.Map a (M.Map a Int)
type LPPath a = ([(a, Int)], Int)
type LPInt a = M.Map a ([(a, Int)], Int)
longestPathBetween :: forall a . (Ord a, Show a) => LPGraph a -> a -> a -> IO (LPPath a)
longestPathBetween g f _d = do
  when False $ do
    putStrLn $ "digraph {"
    forM_ (M.toAscList g) $ \(from, cs) -> do
      forM_ (M.toAscList cs) $ \(to, c) -> do
        putStrLn $ show from <> " -> " <> show to <> " [label=\"" <> show c <> "\"]"
    putStrLn $ "}"
  let r x y = fromMaybe ([], 0) $ M.lookup x y
  ps <- fixedPoint $ \(m::LPInt a) -> do
    flip mapWithKeyM g $ \n cs -> do
      cs' <- flip mapM (M.toAscList cs) $ \(t, c) -> do
        let (p, pc) = r t m
        let p' = (t, c) : p
        let c' = pc + c
        case n `elem` map fst p' of
          True -> return (p, -1)
          False -> return (p', c')
      return $ List.maximumBy (compare `on` snd) cs'
  return $ r f ps

aarray :: [Aeson.Value] -> Aeson.Value
aarray = Aeson.Array . Vector.fromList

sb :: SrcLoc
sb = srcloc_builtin

typeArray :: HasCallStack => DLArg -> (DLType, Integer)
typeArray a =
  case argTypeOf a of
    T_Array t sz -> (t, sz)
    _ -> impossible $ "should be array"

typeTupleTypes :: HasCallStack => DLType -> [DLType]
typeTupleTypes = \case
  T_Tuple ts -> ts
  _ -> impossible $ "should be tuple"

typeObjectTypes :: HasCallStack => DLArg -> [(SLVar, DLType)]
typeObjectTypes a =
  case argTypeOf a of
    T_Object m -> M.toAscList m
    T_Struct ts -> ts
    _ -> impossible $ "should be obj"

-- Algorand constants

conName' :: T.Text
conName' = "ALGO"

conCons' :: DLConstant -> DLLiteral
conCons' DLC_UInt_max = DLL_Int sb $ 2 ^ (64 :: Integer) - 1

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

minimumBalance_l :: DLLiteral
minimumBalance_l = DLL_Int sb algoMinimumBalance

tealVersionPragma :: LT.Text
tealVersionPragma = "#pragma version 5"

-- Algo specific stuff

_udiv :: Integer -> Integer -> Integer
_udiv x y = z
  where
    (q, d) = quotRem x y
    z = if d == 0 then q else q + 1

typeSig :: DLType -> String
typeSig x =
  case x of
  T_Null -> "null"
  T_Bool -> "bool"
  T_UInt -> "uint64"
  T_Bytes sz -> "byte" <> array sz
  T_Digest -> "digest"
  T_Address -> "address"
  T_Contract -> typeSig T_UInt
  T_Token -> typeSig T_UInt
  T_Array  t sz -> typeSig t <> array sz
  T_Tuple ts -> "(" <> intercalate "," (map typeSig ts) <> ")"
  T_Object m -> typeSig $ T_Tuple $ M.elems m
  T_Data m -> "(byte, byte" <> array (maximum $ map typeSizeOf $ M.elems m) <> ")"
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
  T_Data m -> 1 + (maximum $ map typeSizeOf $ M.elems m)
  T_Struct ts -> sum $ map (typeSizeOf . snd) ts
  where
    word = 8

encodeBase64 :: B.ByteString -> LT.Text
encodeBase64 bs = LT.pack $ B.unpack $ encodeBase64' bs

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

type ScratchSlot = Word8

type TEAL = [LT.Text]

code_ :: LT.Text -> [LT.Text] -> TEAL
code_ fun args = fun : args

label_ :: LT.Text -> TEAL
label_ lab = [lab <> ":"]

comment_ :: LT.Text -> TEAL
comment_ t = ["//", t]

type TEALs = DL.DList TEAL

optimize :: [TEAL] -> [TEAL]
optimize ts0 = tsN
  where
    ts1 = opt_b ts0
    ts2 = opt_bs ts1
    tsN = ts2

opt_bs :: [TEAL] -> [TEAL]
opt_bs = \case
  [] -> []
  ["byte", x] : l | isBase64_zeros x ->
    case B.length $ base64u x of
      0 -> ["byte", x] : opt_bs l
      32 -> opt_bs $ ["global", "ZeroAddress"] : l
      len -> opt_bs $ ["int", texty len] : ["bzero"] : l
  x : l -> x : opt_bs l

opt_b :: [TEAL] -> [TEAL]
opt_b = foldr (\a b -> opt_b1 $ a : b) mempty

opt_b1 :: [TEAL] -> [TEAL]
opt_b1 = \case
  [] -> []
  [["return"]] -> []
  -- This relies on knowing what "done" is
  ["assert"] : ["b", "done"] : x -> ["return"] : x
  ["byte", "base64()"] : ["concat"] : l -> l
  ["byte", "base64()"] : b@["load", _] : ["concat"] : l -> opt_b1 $ b : l
  ["byte", x] : ["byte", y] : ["concat"] : l | isBase64 x && isBase64 y ->
    opt_b1 $ [ "byte", (base64d $ base64u x <> base64u y) ] : l
  ["b", x] : b@[y] : l | y == (x <> ":") -> b : l
  ["btoi"] : ["itob", "// bool"] : ["substring", "7", "8"] : l -> l
  ["btoi"] : ["itob"] : l -> l
  ["itob"] : ["btoi"] : l -> l
  ["extract", x, "8"] : ["btoi"] : l -> ["int",x] : ["extract_uint64"] : l
  -- Not actually an optimization:
  -- ["extract", x, "1"] : l -> ["int", x] : ["getbyte"] : ["itob"] : l
  a@["load", x] : ["load", y] : l
    | x == y ->
      -- This misses if there is ANOTHER load of the same thing
      a : ["dup"] : l
  a@["store", x] : ["load", y] : l
    | x == y ->
      ["dup"] : a : l
  a@["substring", s0, _] : b@["int", x] : c@["getbyte"] : l ->
    case mse of
      Just (s0x, s0xp1) ->
        opt_b1 $ ["substring", s0x, s0xp1] : ["btoi"] : l
      Nothing ->
        a : b : c : l
    where
      mse = do
        s0n <- parse s0
        xn <- parse x
        let s0xn = s0n + xn
        let s0xp1n = s0xn + 1
        case s0xn < 256 && s0xp1n < 256 of
          True -> return (texty s0xn, texty s0xp1n)
          False -> mempty
  a@["substring", s0, _] : b@["substring", s1, e1] : l ->
    case mse of
      Just (s2, e2) ->
        opt_b1 $ ["substring", s2, e2] : l
      Nothing ->
        a : b : l
    where
      mse = do
        s0n <- parse s0
        s1n <- parse s1
        e1n <- parse e1
        let s2n = s0n + s1n
        let e2n = s0n + e1n
        case s2n < 256 && e2n < 256 of
          True -> return $ (texty s2n, texty e2n)
          False -> mempty
  a@["int", x] : b@["itob"] : l ->
    case itob x of
      Nothing ->
        a : b : l
      Just xbs ->
        opt_b1 $ ["byte", xbs] : l
  x : l -> x : l
  where
    parse :: LT.Text -> Maybe Integer
    parse = readMaybe . LT.unpack
    itob :: LT.Text -> Maybe LT.Text
    itob x_lt = do
      x <- parse x_lt
      let x_bs = LB.toStrict $ toLazyByteString $ word64BE $ fromIntegral x
      return $ base64d x_bs

checkCost :: Bool -> [TEAL] -> IO ()
checkCost alwaysShow ts = do
  let mkg :: IO (IORef (LPGraph String))
      mkg = newIORef mempty
  cost_gr <- mkg
  logLen_gr <- mkg
  let lTop = "TOP"
  let lBot = "BOT"
  (labr :: IORef String) <- newIORef $ lTop
  (cost_r :: IORef Int) <- newIORef $ 0
  (logLen_r :: IORef Int) <- newIORef $ 0
  hasForR <- newIORef $ False
  let l2s = LT.unpack
  let rec_ r c = modifyIORef r (c +)
  let recCost = rec_ cost_r
  let recLogLen = rec_ logLen_r
  let jump_ t = do
        lab <- readIORef labr
        let updateGraph cr cgr = do
              c <- readIORef cr
              let ff = max c
              let fg = Just . ff . fromMaybe 0
              let f = M.alter fg t
              let g = Just . f . fromMaybe mempty
              modifyIORef cgr $ M.alter g lab
        updateGraph cost_r cost_gr
        updateGraph logLen_r logLen_gr
  let switch t = do
        writeIORef labr t
        writeIORef cost_r 0
        writeIORef logLen_r 0
  let jump t = recCost 1 >> jump_ (l2s t ++ ":")
  forM_ ts $ \case
    ["sha256"] -> recCost 35
    ["keccak256"] -> recCost 130
    ["sha512_256"] -> recCost 45
    ["ed25519verify"] -> recCost 1900
    ["ecdsa_verify", _] -> recCost 1700
    ["ecdsa_pk_decompress",_] -> recCost 650
    ["ecdsa_pk_recover", _] -> recCost 2000
    ["divmodw"] -> recCost 20
    ["sqrt"] -> recCost 4
    ["expw"] -> recCost 10
    ["b+"] -> recCost 10
    ["b-"] -> recCost 10
    ["b/"] -> recCost 20
    ["b*"] -> recCost 20
    ["b%"] -> recCost 20
    ["b|"] -> recCost 6
    ["b&"] -> recCost 6
    ["b^"] -> recCost 6
    ["b~"] -> recCost 4
    ["b|"] -> recCost 6
    ["bnz", lab'] -> jump lab'
    ["bz", lab'] -> jump lab'
    ["b", lab'] -> do
      jump lab'
      switch ""
    ["return"] -> do
      jump lBot
      switch ""
    ["callsub", _lab'] ->
      impossible "callsub"
    ["log", "//", len'] -> do
      -- Note: We don't check MaxLogCalls, because it is not actually checked
      recLogLen (read $ LT.unpack len')
      recCost 1
    [ "//", com ] -> do
      when (com == "<for>") $ do
        writeIORef hasForR True
      return ()
    [lab'] | LT.isSuffixOf ":" lab' -> do
      let lab'' = l2s lab'
      jump_ lab''
      switch lab''
    _ -> recCost 1
  let whenl x e =
        case x of
          True -> [ e ]
          False -> []
  let analyze cgr units algoMax = do
        cg <- readIORef cgr
        (p, c) <- longestPathBetween cg lTop (l2s lBot)
        let msg = "This program could use " <> show c <> " " <> units
        let tooMuch = fromIntegral c > algoMax
        let cs = (whenl tooMuch $ msg <> ", but the limit is " <> show algoMax <> ": " <> show p <> "\n")
        return (msg, tooMuch, cs)
  hasFor <- readIORef hasForR
  (showCost, exceedsCost, csCost) <- analyze cost_gr "units of cost" algoMaxAppProgramCost
  (showLogLen, exceedsLogLen, csLogLen) <- analyze logLen_gr "bytes of logs" algoMaxLogLen
  let cs = []
        <> (whenl hasFor $
              "This program contains a loop, which cannot be tracked accurately for cost or log bytes.\n")
        <> csCost
        <> csLogLen
  unless (null cs) $
    emitWarning Nothing $ W_ALGOConservative cs
  let exceeds = exceedsCost || exceedsLogLen
  when (alwaysShow && not exceeds) $ do
    putStrLn $ "Conservative analysis on Algorand found:"
    putStrLn $ " * " <> showCost <> "."
    putStrLn $ " * " <> showLogLen <> "."

data Shared = Shared
  { sFailuresR :: IORef (S.Set LT.Text)
  , sCounter :: Counter
  , sStateSizeR :: IORef Integer
  , sMaps :: DLMapInfos
  , sMapDataTy :: DLType
  , sMapDataSize :: Integer
  , sMapKeysl :: [Word8]
  , sResources :: IORef ResourceGraph
  }

type Lets = M.Map DLVar (App ())
data Env = Env
  { eShared :: Shared
  , eWhich :: Int
  , eLabel :: Counter
  , eOutputR :: IORef TEALs
  , eHP :: ScratchSlot
  , eSP :: ScratchSlot
  , eVars :: M.Map DLVar ScratchSlot
  , eLets :: Lets
  , eLetSmalls :: M.Map DLVar Bool
  , eResources :: ResourceCounters
  , eNewToks :: IORef (S.Set DLArg)
  , eInitToks :: IORef (S.Set DLArg)
  }

type App = ReaderT Env IO

recordWhich :: Int -> App a -> App a
recordWhich n = local (\e -> e { eWhich = n }) . dupeResources . resetToks

type CostGraph a = M.Map a (CostRecord a)
data CostRecord a = CostRecord
  { cr_n :: Int
  , cr_max :: S.Set a
  }
  deriving (Show)
type ResourceRec = CostRecord Int

data Resource
  = R_Txn
  | R_Asset
  | R_Account
  | R_InnerTxn
  deriving (Eq, Ord)
type ResourceGraph = M.Map Resource (CostGraph Int)
type ResourceCounter = ((S.Set DLArg), Int)
type ResourceCounters = IORef (M.Map Resource ResourceCounter)

instance Show Resource where
  show = \case
    R_Txn -> "transactions"
    R_Asset -> "assets"
    R_Account -> "accounts"
    R_InnerTxn -> "inner transactions"

maxOf :: Resource -> Integer
maxOf = \case
  R_Txn -> algoMaxTxGroupSize
  R_Asset -> algoMaxAppTxnForeignAssets
  R_Account -> algoMaxAppTxnAccounts + 1 -- XXX could detect the sender as a free account
  R_InnerTxn -> algoMaxInnerTransactions

newResources :: IO ResourceCounters
newResources = do
  newIORef $ M.fromList $
    [ (R_Txn, (mempty, 1))
    , (R_Asset, (mempty, 0))
    , (R_Account, (mempty, 0))
    , (R_InnerTxn, (mempty, 0))
    ]
newResourceGraph :: IO (IORef ResourceGraph)
newResourceGraph = do
  newIORef $ M.fromList $
    [ (R_Txn, mempty)
    , (R_Asset, mempty)
    , (R_Account, mempty)
    , (R_InnerTxn, mempty)
    ]

dupeResources :: App a -> App a
dupeResources m = do
  c' <- (liftIO . dupeIORef) =<< asks eResources
  local (\e -> e { eResources = c' }) m

incResourceM :: Maybe DLArg -> Resource -> App ()
incResourceM ma r = do
  rsr <- asks eResources
  let f (vs, i) =
        case ma of
          Nothing -> (vs, i + 1)
          Just a ->
            case S.member a vs of
              True -> (vs, i)
              False -> (S.insert a vs, i+1)
  liftIO $ modifyIORef rsr $ M.adjust f r
incResource :: Resource -> App ()
incResource = incResourceM Nothing
incResourceL :: DLArg -> Resource -> App ()
incResourceL = incResourceM . Just

updateResources :: (Resource -> ResourceRec -> ResourceRec) -> App ()
updateResources f = do
  Env {..} <- ask
  let Shared {..} = eShared
  let g r = Just . (f r) . fromMaybe (CostRecord 0 mempty)
  liftIO $ modifyIORef sResources $ M.mapWithKey (\r -> M.alter (g r) eWhich)
addResourceEdge :: Int -> App ()
addResourceEdge w' = do
  addResourceCheck
  updateResources (\_ t -> t { cr_max = S.insert w' (cr_max t) })
addResourceCheck :: App ()
addResourceCheck = do
  c <- (liftIO . readIORef) =<< asks eResources
  updateResources $ \r t ->
    t { cr_n = max (snd $ c M.! r) (cr_n t) }
checkResources :: (LT.Text -> IO ()) -> ResourceGraph -> IO ()
checkResources bad' rg = do
  let one emit r = do
        let maxc = maxOf r
        let tcs = rg M.! r
        -- XXX Do this not dumb
        let maximum' :: [Int] -> Int
            maximum' l = maximum $ 0 : l
        let chase i = cr_n + (maximum' $ map chase $ S.toAscList cr_max)
              where CostRecord {..} = tcs M.! i
        forM_ (M.keys tcs) $ \which -> do
          let amt = chase which
          when (fromIntegral amt > maxc) $ do
            emit $ "Step " <> texty which <> " could have too many " <> texty r <> ": could have " <> texty amt <> " but limit is " <> texty maxc
  let warn x = emitWarning Nothing $ W_ALGOConservative [LT.unpack x]
  one bad' R_Txn
  one warn R_Asset
  one warn R_Account
  one bad' R_InnerTxn

resetToks :: App a -> App a
resetToks m = do
  ntoks <- liftIO $ newIORef mempty
  itoks <- liftIO $ newIORef mempty
  local (\e -> e { eNewToks = ntoks, eInitToks = itoks }) m

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
code f args = output $ code_ f args

label :: LT.Text -> App ()
label = output . label_

comment :: LT.Text -> App ()
comment = output . comment_

assert :: App ()
assert = op "assert"

asserteq :: App ()
asserteq = op "==" >> assert

op :: LT.Text -> App ()
op = flip code []

nop :: App ()
nop = return ()

dont_concat_first :: [App ()]
dont_concat_first = nop : repeat (op "concat")

padding :: Integer -> App ()
padding = cla . bytesZeroLit

czaddr :: App ()
czaddr = padding $ typeSizeOf T_Address

checkRekeyTo :: App ()
checkRekeyTo = do
  code "txn" ["RekeyTo"]
  czaddr
  asserteq

checkLease :: App ()
checkLease = do
  code "txn" ["Lease"]
  czaddr
  asserteq

bad_io :: IORef (S.Set LT.Text) -> LT.Text -> IO ()
bad_io x = modifyIORef x . S.insert

bad :: LT.Text -> App ()
bad lab = do
  Env {..} <- ask
  let Shared {..} = eShared
  liftIO $ bad_io sFailuresR lab
  output $ comment_ $ "BAD " <> lab

xxx :: LT.Text -> App ()
xxx lab = bad $ "This program uses " <> lab

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

salloc_ :: (App () -> App () -> App a) -> App a
salloc_ fm =
  salloc $ \loc -> do
    let loct = texty loc
    fm (code "store" [loct]) (code "load" [loct])

sallocLet :: DLVar -> App () -> App a -> App a
sallocLet dv cgen km = do
  salloc_ $ \cstore cload -> do
    cgen
    cstore
    store_let dv True cload km

ctobs :: DLType -> App ()
ctobs = \case
  T_UInt -> op "itob"
  T_Bool -> code "itob" ["// bool"] >> code "substring" ["7", "8"]
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

tint :: SrcLoc -> Integer -> LT.Text
tint at i = texty $ checkIntLiteralC at conName' conCons' i

base64d :: B.ByteString -> LT.Text
base64d bs = "base64(" <> encodeBase64 bs <> ")"

decodeBase64' :: B.ByteString -> B.ByteString
decodeBase64' = fromRight (impossible "isBase64") . decodeBase64

base64u :: LT.Text -> B.ByteString
base64u x0 = x3
  where
    x1 = f $ LT.stripPrefix "base64(" x0
    x2 = f $ LT.stripSuffix ")" x1
    x3 = decodeBase64' $ bpack $ LT.unpack x2
    f = fromMaybe (impossible "isBase64")

isBase64 :: LT.Text -> Bool
isBase64 = LT.isPrefixOf "base64("

isBase64_zeros :: LT.Text -> Bool
isBase64_zeros t = isBase64 t && B.all (== '\0') (base64u t)

cint_ :: SrcLoc -> Integer -> App ()
cint_ at i = code "int" [tint at i]

cint :: Integer -> App ()
cint = cint_ sb

cl :: DLLiteral -> App ()
cl = \case
  DLL_Null -> cbs ""
  DLL_Bool b -> cint $ if b then 1 else 0
  DLL_Int at i -> cint_ at i

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
      cint 0
      ca z
      op "divmodw"
      op "pop"
      op "pop"
      op "swap"
      cint 0
      asserteq
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

cconcatbs :: [(DLType, App ())] -> App ()
cconcatbs l = do
  let totlen = typeSizeOf $ T_Tuple $ map fst l
  check_concat_len totlen
  case l of
    [] -> padding 0
    _ -> do
      forM_ (zip l dont_concat_first) $ \((t, m), a) ->
        m >> ctobs t >> a

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

cextract :: SrcLoc -> Integer -> Integer -> App ()
cextract _at _s 0 = do
  op "pop"
  padding 0
cextract at s l =
  case s < 256 && l < 256 && l /= 0 of
    True -> do
      code "extract" [tint at s, tint at l]
    False -> do
      cint s
      cint l
      op "extract3"

csubstring :: SrcLoc -> Integer -> Integer -> App ()
csubstring at s e = cextract at s (e - s)

computeSplice :: SrcLoc -> Integer -> Integer -> Integer -> (App (), App ())
computeSplice at start end tot = (before, after)
  where
    -- XXX If start == 0, then we could remove before and have another version
    -- of the callers of computeSplice
    before = cextract at 0 start
    after = cextract at end (tot - end)

csplice :: SrcLoc -> Integer -> Integer -> Integer -> App ()
csplice at b c e = do
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
    False -> salloc_ $ \store_new load_new -> do
      let (cbefore, cafter) = computeSplice at b c e
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
csplice3 (Just cbig) cbefore cafter cnew = do
  cbig
  cbefore
  cnew
  op "concat"
  cbig
  cafter
  op "concat"

cArraySet :: SrcLoc -> (DLType, Integer) -> Maybe (App ()) -> Either Integer (App ()) -> App () -> App ()
cArraySet at (t, alen) mcbig eidx cnew = do
  let tsz = typeSizeOf t
  let (cbefore, cafter) =
        case eidx of
          Left ii ->
            computeSplice at start end tot
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
cfor maxi body = do
  top_lab <- freshLabel "forTop"
  end_lab <- freshLabel "forK"
  salloc_ $ \store_idx load_idx -> do
    cint 0
    store_idx
    comment $ "<for>"
    label top_lab
    load_idx
    cint maxi
    op "<"
    code "bz" [end_lab]
    body load_idx
    load_idx
    cint 1
    op "+"
    store_idx
    code "b" [top_lab]
  comment "</for>"
  label end_lab

doArrayRef :: SrcLoc -> DLArg -> Bool -> Either DLArg (App ()) -> App ()
doArrayRef at aa frombs ie = do
  let (t, _) = typeArray aa
  ca aa
  cArrayRef at t frombs ie

cArrayRef :: SrcLoc -> DLType -> Bool -> Either DLArg (App ()) -> App ()
cArrayRef at t frombs ie = do
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
          cextract at start tsz
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
cbs bs = code "byte" [base64d bs]

cTupleRef :: SrcLoc -> DLType -> Integer -> App ()
cTupleRef at tt idx = do
  -- [ Tuple ]
  let ts = typeTupleTypes tt
  let (t, start, sz) = computeExtract ts idx
  case (ts, idx) of
    ([ _ ], 0) ->
      return ()
    _ -> do
      cextract at start sz
  -- [ ValueBs ]
  cfrombs t
  -- [ Value ]
  return ()

computeSubstring :: [DLType] -> Integer -> (DLType, Integer, Integer)
computeSubstring ts idx = (t, start, end)
  where (t, start, sz) = computeExtract ts idx
        end = start + sz

cTupleSet :: SrcLoc -> DLType -> Integer -> App ()
cTupleSet at tt idx = do
  -- [ Tuple, Value' ]
  let tot = typeSizeOf tt
  let ts = typeTupleTypes tt
  let (t, start, end) = computeSubstring ts idx
  ctobs t
  -- [ Tuple, Value'Bs ]
  csplice at start end tot
  -- [ Tuple' ]
  return ()

cMapLoad :: App ()
cMapLoad = do
  Shared {..} <- eShared <$> ask
  -- [ Address ]
  -- [ Address, MapData_0? ]
  forM_ (zip sMapKeysl $ False : repeat True) $ \(mi, doConcat) -> do
    -- [ Address, MapData_N? ]
    case doConcat of
      True -> code "dig" [ "1" ]
      False -> op "dup"
    -- [ Address, MapData_N?, Address ]
    cbs $ keyVary mi
    -- [ Address, MapData_N?, Address, Key ]
    op "app_local_get"
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

cMapStore :: SrcLoc -> App ()
cMapStore at = do
  Shared {..} <- eShared <$> ask
  -- [ Address, MapData' ]
  forM_ sMapKeysl $ \mi -> do
    -- [ Address, MapData' ]
    code "dig" ["1"]
    -- [ Address, MapData', Address ]
    cbs $ keyVary mi
    -- [ Address, MapData', Address, Key ]
    code "dig" ["2"]
    -- [ Address, MapData', Address, Key, MapData' ]
    cStateSlice at sMapDataSize mi
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
  case null keysl of
    True -> do
      padding 0
    False -> do
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
      return ()

cSvsSave :: SrcLoc -> [DLArg] -> App ()
cSvsSave at svs = do
  let la = DLLA_Tuple svs
  let lat = largeArgTypeOf la
  let size = typeSizeOf lat
  cla la
  ctobs lat
  (_, keysl) <- computeStateSizeAndKeys bad "svs" size algoMaxGlobalSchemaEntries_usable
  ssr <- asks $ sStateSizeR . eShared
  liftIO $ modifyIORef ssr $ max size
  -- [ SvsData ]
  forM_ keysl $ \vi -> do
    -- [ SvsData ]
    cbs $ keyVary vi
    -- [ SvsData, Key ]
    code "dig" [ "1" ]
    -- [ SvsData, Key, SvsData ]
    cStateSlice at size vi
    -- [ SvsData, Key, ViewData' ]
    op "app_global_put"
    -- [ SvsData ]
    return ()
  -- [ SvsData ]
  op "pop"
  -- [ ]
  return ()

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_LArg _ a -> cla a
  DLE_Impossible at _ err -> expect_thrown at err
  DLE_PrimOp _ p args -> cprim p args
  DLE_ArrayRef at aa ia -> doArrayRef at aa True (Left ia)
  DLE_ArraySet at aa ia va -> do
    let (t, alen) = typeArray aa
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
    let (xt, xlen) = typeArray x
    let (_, ylen) = typeArray y
    ca x
    ca y
    check_concat_len $ (xlen + ylen) * typeSizeOf xt
    op "concat"
  DLE_ArrayZip at x y -> do
    let xsz = typeSizeOf $ argTypeOf x
    let ysz = typeSizeOf $ argTypeOf y
    let (_, xlen) = typeArray x
    check_concat_len $ xsz + ysz
    salloc_ $ \store_ans load_ans -> do
      cbs ""
      store_ans
      cfor xlen $ \load_idx -> do
        load_ans
        doArrayRef at x False $ Right load_idx
        doArrayRef at y False $ Right load_idx
        op "concat"
        op "concat"
        store_ans
      load_ans
  DLE_TupleRef at ta idx -> do
    ca ta
    cTupleRef at (argTypeOf ta) idx
  DLE_ObjectRef at oa f -> do
    let fts = typeObjectTypes oa
    let fidx = fromIntegral $ fromMaybe (impossible "field") $ List.findIndex ((== f) . fst) fts
    let (t, start, sz) = computeExtract (map snd fts) fidx
    ca oa
    cextract at start sz
    cfrombs t
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> cdigest $ map go args
    where
      go a = (argTypeOf a, ca a)
  DLE_Transfer mt_at who mt_amt mt_mtok -> do
    let mt_always = False
    let mt_mrecv = Just who
    let mt_mcclose = Nothing
    makeTxn $ MakeTxn {..}
  DLE_TokenInit mt_at tok -> do
    comment $ "Initializing token"
    let mt_always = True
    let mt_mtok = Just tok
    let mt_amt = DLA_Literal $ DLL_Int sb 0
    let mt_mrecv = Nothing
    let mt_mcclose = Nothing
    let ct_at = mt_at
    let ct_mtok = Nothing
    let ct_amt = DLA_Literal $ minimumBalance_l
    addInitTok tok
    checkTxn $ CheckTxn {..}
    makeTxn $ MakeTxn {..}
  DLE_CheckPay ct_at fs ct_amt ct_mtok -> do
    show_stack ("CheckPay" :: String) ct_at fs
    checkTxn $ CheckTxn {..}
  DLE_Claim at fs t a mmsg -> do
    show_stack mmsg at fs
    case t of
      CT_Assert -> impossible "assert"
      CT_Assume _ -> check
      CT_Require -> check
      CT_Possible -> impossible "possible"
      CT_Unknowable {} -> impossible "unknowable"
    where
      check = ca a >> assert
  DLE_Wait {} -> nop
  DLE_PartSet _ _ a -> ca a
  DLE_MapRef _ (DLMVar i) fa -> do
    incResourceL fa R_Account
    ca fa
    cMapLoad
    mdt <- getMapDataTy
    cTupleRef sb mdt $ fromIntegral i
  DLE_MapSet at mpv@(DLMVar i) fa mva -> do
    incResourceL fa R_Account
    ca fa
    op "dup"
    cMapLoad
    mdt <- getMapDataTy
    mt <- getMapTy mpv
    cla $ mdaToMaybeLA mt mva
    cTupleSet at mdt $ fromIntegral i
    cMapStore at
  DLE_Remote {} -> xxx "remote objects"
  DLE_TokenNew at (DLTokenNew {..}) -> do
    let ct_at = at
    let ct_mtok = Nothing
    let ct_amt = DLA_Literal $ minimumBalance_l
    checkTxn $ CheckTxn {..}
    op "itxn_begin"
    let vTypeEnum = "acfg"
    code "int" [vTypeEnum]
    makeTxn1 "TypeEnum"
    ca dtn_supply >> makeTxn1 "ConfigAssetTotal"
    maybe (cint_ at 6) ca dtn_decimals >> makeTxn1 "ConfigAssetDecimals"
    ca dtn_sym >> makeTxn1 "ConfigAssetUnitName"
    ca dtn_name >> makeTxn1 "ConfigAssetName"
    ca dtn_url >> makeTxn1 "ConfigAssetURL"
    ca dtn_metadata >> makeTxn1 "ConfigAssetMetadataHash"
    cContractAddr >> makeTxn1 "ConfigAssetManager"
    incResource R_InnerTxn
    op "itxn_submit"
    code "itxn" ["CreatedAssetID"]
  DLE_TokenBurn {} ->
    -- Burning does nothing on Algorand, because we already own it and we're
    -- the creator, and that's the rule for being able to destroy
    return ()
  DLE_TokenDestroy _at aida -> do
    op "itxn_begin"
    let vTypeEnum = "acfg"
    code "int" [vTypeEnum]
    makeTxn1 "TypeEnum"
    incResourceL aida R_Asset
    ca aida
    makeTxn1 "ConfigAsset"
    op "itxn_submit"
    incResource R_InnerTxn
    -- XXX We could give the minimum balance back to the creator
  DLE_TimeOrder {} -> impossible "timeorder"
  DLE_GetContract _ -> code "txn" ["ApplicationID"]
  DLE_GetAddress _ -> cContractAddr
  DLE_EmitLog at k vs
    | isInternalLog k -> do
      (v, n) <- case vs of
          [v'@(DLVar _ _ _ n')] -> return (v', n')
          _ -> impossible "algo ce: Expected one value"
      clog $
        [ DLA_Literal (DLL_Int at $ fromIntegral n)
        , DLA_Var v
        ]
      cv v
    | L_Event ml en <- k -> do
      let name = maybe en (\l -> bunpack l <> "_" <> en) ml
      clogEvent name vs
      cl DLL_Null
    | otherwise -> impossible "algo: emitLog"
    where
      isInternalLog = \case
        L_Internal -> True
        L_Api {} -> True
        _ -> False
  DLE_setApiDetails {} -> return ()
  where
    show_stack msg at fs = do
      comment $ texty msg
      comment $ texty $ unsafeRedactAbsStr $ show at
      comment $ texty $ unsafeRedactAbsStr $ show fs

clogEvent :: String -> [DLVar] -> ReaderT Env IO ()
clogEvent eventName vs = do
  let signature = eventName <> "(" <> intercalate "," (map (typeSig . varType) vs) <> ")"
  let shaString = show . hashWith SHA512t_256 $ bpack signature
  let shaBytes = bpack . take 4 $ shaString
  let as = map DLA_Var vs
  cconcatbs $ (T_Bytes 4, cbs shaBytes) : map (\a -> (argTypeOf a, ca a)) as
  code "log" [ "//", texty $ typeSizeOf $ largeArgTypeOf $ DLLA_Tuple as ]

clog :: [DLArg] -> App ()
clog as = do
  let la = DLLA_Tuple as
  cla la
  code "log" [ "//", texty $ typeSizeOf $ largeArgTypeOf la ]

staticZero :: DLArg -> Bool
staticZero = \case
  DLA_Literal (DLL_Int _ 0) -> True
  _ -> False

data CheckTxn = CheckTxn
  { ct_at :: SrcLoc
  , ct_amt :: DLArg
  , ct_mtok :: Maybe DLArg }

data MakeTxn = MakeTxn
  { mt_at :: SrcLoc
  , mt_mrecv :: Maybe DLArg
  , mt_mcclose :: Maybe (App ())
  , mt_amt :: DLArg
  , mt_always :: Bool
  , mt_mtok :: Maybe DLArg }

checkTxn1 :: LT.Text -> App ()
checkTxn1 f = do
  code "dig" [ "1" ]
  code "gtxns" [f]
  asserteq

makeTxn1 :: LT.Text -> App ()
makeTxn1 f = code "itxn_field" [f]

checkTxnInit :: LT.Text -> App ()
checkTxnInit vTypeEnum = do
  -- [ txn ]
  code "int" [vTypeEnum]
  checkTxn1 "TypeEnum"
  cint 0
  checkTxn1 "Fee"
  czaddr
  checkTxn1 "Lease"
  czaddr
  checkTxn1 "RekeyTo"
  -- [ txn ]
  return ()

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

checkTxn :: CheckTxn -> App ()
checkTxn (CheckTxn {..}) = when (not (staticZero ct_amt) ) $ do
  let check1 = checkTxn1
  let ((vTypeEnum, fReceiver, fAmount, _fCloseTo), extra) =
        case ct_mtok of
          Nothing ->
            (ntokFields, return ())
          Just tok ->
            (tokFields, textra)
            where textra = ca tok >> check1 "XferAsset"
  checkTxnUsage ct_at ct_mtok
  after_lab <- freshLabel "checkTxnK"
  ca ct_amt
  op "dup"
  code "bz" [after_lab]
  incResource R_Txn
  gvLoad GV_txnCounter
  op "dup"
  cint 1
  op "+"
  gvStore GV_txnCounter
  -- [ amt, id ]
  op "swap"
  -- [ id, amt ]
  check1 fAmount
  extra
  checkTxnInit vTypeEnum
  cContractAddr
  cfrombs T_Address
  check1 fReceiver
  label after_lab
  op "pop" -- if !always & zero then pop amt ; else pop id

makeTxn :: MakeTxn -> App ()
makeTxn (MakeTxn {..}) = when (mt_always || not (staticZero mt_amt) ) $ do
  let ((vTypeEnum, fReceiver, fAmount, fCloseTo), extra) =
        case mt_mtok of
          Nothing ->
            (ntokFields, return ())
          Just tok ->
            (tokFields, textra)
            where
              textra = do
                incResourceL tok R_Asset
                ca tok
                makeTxn1 "XferAsset"
  makeTxnUsage mt_at mt_mtok
  after_lab <- freshLabel "makeTxnK"
  ca mt_amt
  unless mt_always $ do
    op "dup"
    code "bz" [after_lab]
  op "itxn_begin"
  makeTxn1 fAmount
  code "int" [vTypeEnum]
  makeTxn1 "TypeEnum"
  whenJust mt_mcclose $ \cclose -> do
    cclose
    cfrombs T_Address
    makeTxn1 fCloseTo
  case mt_mrecv of
    Nothing -> cContractAddr
    Just a -> do
      incResourceL a R_Account
      ca a
  cfrombs T_Address
  makeTxn1 fReceiver
  extra
  op "itxn_submit"
  incResource R_InnerTxn
  cint 0
  label after_lab
  op "pop" -- if !always & zero then pop amt ; else pop 0

doSwitch :: (a -> App ()) -> SrcLoc -> DLVar -> SwitchCases a -> App ()
doSwitch ck at dv csm = do
  end_lab <- freshLabel "switchK"
  let cm1 ((vn, (vv, vu, k)), vi) = do
        next_lab <- freshLabel $ "switchAfter" <> vn
        ca $ DLA_Var dv
        cint 0
        op "getbyte"
        cint vi
        op "=="
        code "bz" [next_lab]
        case vu of
          False -> ck k
          True -> do
            flip (sallocLet vv) (ck k) $ do
              let vt = argTypeOf $ DLA_Var vv
              let sz = typeSizeOf vt
              case sz == 0 of
                True -> padding 0
                False -> do
                  ca $ DLA_Var dv
                  cextract at 1 sz
                  cfrombs vt
        label next_lab
  mapM_ cm1 $ zip (M.toAscList csm) [0 ..]
  label end_lab

cm :: App () -> DLStmt -> App ()
cm km = \case
  DL_Nop _ -> km
  DL_Let _ DLV_Eff de ->
    -- XXX this could leave something on the stack
    ce de >> km
  DL_Let _ (DLV_Let DVC_Once dv) de -> do
    sm <- exprSmall de
    store_let dv sm (ce de) km
  DL_Let _ (DLV_Let DVC_Many dv) de -> do
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
    case sm of
      True ->
        store_let dv True (ce de) km
      False ->
        sallocLet dv (ce de) km
  DL_ArrayMap at ansv aa lv (DLBlock _ _ body ra) -> do
    let anssz = typeSizeOf $ argTypeOf $ DLA_Var ansv
    let (_, xlen) = typeArray aa
    check_concat_len anssz
    salloc_ $ \store_ans load_ans -> do
      cbs ""
      store_ans
      cfor xlen $ \load_idx -> do
        load_ans
        doArrayRef at aa True $ Right load_idx
        sallocLet lv (return ()) $ do
          cp (ca ra) body
        op "concat"
        store_ans
      store_let ansv True load_ans km
  DL_ArrayReduce at ansv aa za av lv (DLBlock _ _ body ra) -> do
    let (_, xlen) = typeArray aa
    salloc_ $ \store_ans load_ans -> do
      ca za
      store_ans
      store_let av True load_ans $ do
        cfor xlen $ \load_idx -> do
          doArrayRef at aa True $ Right load_idx
          sallocLet lv (return ()) $ do
            cp (ca ra) body
          store_ans
        store_let ansv True load_ans km
  DL_Var _ dv ->
    salloc $ \loc -> do
      store_var dv loc $
        store_let dv True (code "load" [texty loc]) $
          km
  DL_Set _ dv da -> do
    loc <- lookup_var dv
    ca da
    code "store" [texty loc]
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
    doSwitch (cp (return ())) at dv csm
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
    doSwitch nct at dv csm
  CT_Jump _at which svs (DLAssignment msgm) -> do
    cla $ DLLA_Tuple $ map DLA_Var svs
    cla $ DLLA_Tuple $ map snd $ M.toAscList msgm
    addResourceEdge which
    code "b" [ loopLabel which ]
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
            close_asset tok = makeTxn $ MakeTxn {..}
              where mt_mtok = Just tok
            close_escrow = makeTxn $ MakeTxn {..}
              where mt_mtok = Nothing
        FI_Continue svs -> do
          cSvsSave at $ map snd svs
          cint $ fromIntegral which
          gvStore GV_currentStep
          cRound
          gvStore GV_currentTime
          return False
    code "txn" ["OnCompletion"]
    code "int" [ if isHalt then "DeleteApplication" else "NoOp" ]
    asserteq
    code "b" ["updateState"]
    addResourceCheck
  where
    nct = dupeResources . ct

-- Reach Constants
reachAlgoBackendVersion :: Int
reachAlgoBackendVersion = 6

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
  | ArgTime
  | ArgMsg
  deriving (Eq, Ord, Show, Enum, Bounded)

argLoad :: ArgId -> App ()
argLoad ai = code "txna" [ "ApplicationArgs", etexty ai ]

boundedCount :: forall a . (Enum a, Bounded a) => a -> Integer
boundedCount _ = 1 + (fromIntegral $ fromEnum $ (maxBound :: a))

argCount :: Integer
argCount = boundedCount ArgMethod

data GlobalVar
  = GV_txnCounter
  | GV_currentStep
  | GV_currentTime
  deriving (Eq, Ord, Show, Enum, Bounded)

gvSlot :: GlobalVar -> ScratchSlot
gvSlot ai = fromIntegral $ fromEnum ai

gvStore :: GlobalVar -> App ()
gvStore gv = code "store" [texty $ gvSlot gv ]

gvLoad :: GlobalVar -> App ()
gvLoad gv = code "load" [texty $ gvSlot gv ]

gvType :: GlobalVar -> DLType
gvType = \case
  GV_txnCounter -> T_UInt
  GV_currentStep -> T_UInt
  GV_currentTime -> T_UInt

keyState_gvs :: [GlobalVar]
keyState_gvs = [GV_currentStep, GV_currentTime]

keyState_ty :: DLType
keyState_ty = T_Tuple $ map gvType keyState_gvs

defn_done :: App ()
defn_done = do
  label "done"
  cint 1
  op "return"

cRound :: App ()
cRound = code "global" ["Round"]

bindTime :: DLVar -> App a -> App a
bindTime dv = store_let dv True cRound
bindSecs :: DLVar -> App a -> App a
bindSecs dv = store_let dv True (code "global" ["LatestTimestamp"])

allocDLVar :: SrcLoc -> DLType -> App DLVar
allocDLVar at t =
  DLVar at Nothing t <$> ((liftIO . incCounter) =<< ((sCounter . eShared) <$> ask))

bindFromTuple :: SrcLoc -> [DLVar] -> App a -> App a
bindFromTuple at vs m = do
  let mkArgVar l = allocDLVar at $ T_Tuple $ map varType l
  av <- mkArgVar vs
  let go = \case
        [] -> op "pop" >> m
        (dv, i) : more -> sallocLet dv cgen $ go more
          where cgen = ce $ DLE_TupleRef at (DLA_Var av) i
  store_let av True (op "dup") $
    go $ zip vs [0..]

cloop :: Int -> CHandler -> App ()
cloop _ (C_Handler {}) = return ()
cloop which (C_Loop at svs vars body) = recordWhich which $ do
  label $ loopLabel which
  -- [ svs, vars ]
  let bindVars = id
        . (bindFromTuple at vars)
        . (bindFromTuple at svs)
  bindVars $ ct body

ch :: LT.Text -> Int -> CHandler -> App ()
ch _ _ (C_Loop {}) = return ()
ch afterLab which (C_Handler at int from prev svs msg timev secsv body) = recordWhich which $ do
  let isCtor = which == 0
  let argSize = 1 + (typeSizeOf $ T_Tuple $ map varType $ msg)
  when (argSize > algoMaxAppTotalArgLen) $
    xxx $ texty $ "Step " <> show which <> "'s argument length is " <> show argSize <> ", but the maximum is " <> show algoMaxAppTotalArgLen
  let bindFromArg ai vs m = do
        argLoad ai
        op "dup"
        op "len"
        cint $ typeSizeOf $ (T_Tuple $ map varType vs)
        asserteq
        bindFromTuple at vs m
  let bindFromSvs m = do
        cSvsLoad $ typeSizeOf $ T_Tuple $ map varType svs
        bindFromTuple at svs m
  comment ("Handler " <> texty which)
  op "dup" -- We assume the method id is on the stack
  cint $ fromIntegral $ which
  op "=="
  code "bz" [ afterLab ]
  op "pop" -- Get rid of the method id since it's us
  -- NOTE: To implement ABIs we'll need to do something like this
  -- clog $ [ DLA_Literal $ DLL_Int at $ fromIntegral which
  --        , ArgMsg
  --        ]
  comment "check step"
  cint $ fromIntegral prev
  gvLoad GV_currentStep
  asserteq
  comment "check time"
  argLoad ArgTime
  cfrombs T_UInt
  op "dup"
  cint 0
  op "=="
  op "swap"
  gvLoad GV_currentTime
  op "=="
  op "||"
  assert
  let bindVars = id
        . (store_let from True (code "txn" [ "Sender" ]))
        . (bindTime timev)
        . (bindSecs secsv)
        . bindFromSvs
        . (bindFromArg ArgMsg msg)
  bindVars $ do
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
  ms <- ((sMaps . eShared) <$> ask)
  return $
    case M.lookup mpv ms of
      Nothing -> impossible "getMapTy"
      Just mi -> dlmi_ty mi

mapDataTy :: DLMapInfos -> DLType
mapDataTy m = T_Tuple $ map (dlmi_tym . snd) $ M.toAscList m

getMapDataTy :: App DLType
getMapDataTy = (sMapDataTy . eShared) <$> ask

type Disp = String -> T.Text -> IO ()

cStateSlice :: SrcLoc -> Integer -> Word8 -> App ()
cStateSlice at size iw = do
  let i = fromIntegral iw
  let k = algoMaxAppBytesValueLen_usable
  csubstring at (k * i) (min size $ k * (i + 1))

compile_algo :: CompilerToolEnv -> Disp -> PLProg -> IO ConnectorInfo
compile_algo env disp pl = do
  let PLProg _at plo dli _ _ cpp = pl
  let CPProg at _ _ai _ (CHandlers hm) = cpp
  let sMaps = dli_maps dli
  resr <- newIORef mempty
  sFailuresR <- newIORef mempty
  sResources <- newResourceGraph
  let sMapDataTy = mapDataTy sMaps
  let sMapDataSize = typeSizeOf sMapDataTy
  let PLOpts {..} = plo
  let sCounter = plo_counter
  let recordSize prefix size = do
        modifyIORef resr $
          M.insert (prefix <> "Size") $
            Aeson.Number $ fromIntegral size
  let recordSizeAndKeys :: T.Text -> Integer -> Integer -> IO [Word8]
      recordSizeAndKeys prefix size limit = do
        let badx = bad_io sFailuresR
        (keys, keysl) <- computeStateSizeAndKeys badx (LT.fromStrict prefix) size limit
        recordSize prefix size
        modifyIORef resr $
          M.insert (prefix <> "Keys") $
            Aeson.Number $ fromIntegral keys
        return $ keysl
  sMapKeysl <- recordSizeAndKeys "mapData" sMapDataSize algoMaxLocalSchemaEntries_usable
  sStateSizeR <- newIORef 0
  let eShared = Shared {..}
  let run :: App () -> IO TEALs
      run m = do
        eLabel <- newCounter 0
        eOutputR <- newIORef mempty
        let eHP = fromIntegral $ fromEnum (maxBound :: GlobalVar)
        let eSP = 255
        let eVars = mempty
        let eLets = mempty
        let eLetSmalls = mempty
        let eWhich = 0
        eNewToks <- newIORef mempty
        eInitToks <- newIORef mempty
        eResources <- newResources
        flip runReaderT (Env {..}) m
        readIORef eOutputR
  let bad' = bad_io sFailuresR
  let addProg lab showCost m = do
        ts <- run m
        let tsl = DL.toList ts
        let tsl' = optimize tsl
        checkCost showCost tsl'
        let lts = tealVersionPragma : (map LT.unwords tsl')
        let lt = LT.unlines lts
        let t = LT.toStrict lt
        modifyIORef resr $ M.insert (T.pack lab) $ Aeson.String t
        disp lab t
  addProg "appApproval" (cte_REACH_DEBUG env) $ do
    checkRekeyTo
    checkLease
    cint 0
    gvStore GV_txnCounter
    code "txn" ["ApplicationID"]
    code "bz" ["alloc"]
    cbs keyState
    op "app_global_get"
    let nats = [0..]
    let shouldDups = reverse $ zipWith (\_ i -> i /= 0) keyState_gvs nats
    forM_ (zip (zip keyState_gvs shouldDups) nats) $ \((gv, shouldDup), i) -> do
      when shouldDup $ op "dup"
      cTupleRef at keyState_ty i
      gvStore gv
    unless (null sMapKeysl) $ do
      -- NOTE We could allow an OptIn if we are not going to halt
      code "txn" ["OnCompletion"]
      code "int" ["OptIn"]
      op "=="
      code "bz" ["normal"]
      code "txn" ["Sender"]
      padding sMapDataSize
      cMapStore at
      code "b" ["checkSize"]
      -- The NON-OptIn case:
      label "normal"
    code "txn" ["NumAppArgs"]
    cint argCount
    asserteq
    argLoad ArgMethod
    cfrombs T_UInt
    label "preamble"
    -- NOTE This could be compiled to a jump table if that were possible or to
    -- a tree to be O(log n) rather than O(n)
    forM_ (M.toAscList hm) $ \(hi, hh) -> do
      afterLab <- freshLabel $ "afterHandler" <> show hi
      ch afterLab hi hh
      label afterLab
    cl $ DLL_Bool False
    assert
    forM_ (M.toAscList hm) $ \(hi, hh) ->
      cloop hi hh
    label "updateState"
    cbs keyState
    forM_ keyState_gvs $ \gv -> do
      gvLoad gv
      ctobs $ gvType gv
    forM_ (tail keyState_gvs) $ const $ op "concat"
    op "app_global_put"
    code "b" [ "checkSize" ]
    label "checkSize"
    gvLoad GV_txnCounter
    op "dup"
    op "dup"
    -- The size is correct
    cint 1
    op "+"
    code "global" [ "GroupSize" ]
    asserteq
    -- We're last
    code "txn" ["GroupIndex"]
    asserteq
    cint algoMinTxnFee
    op "*"
    code "txn" ["Fee"]
    op "<="
    assert
    code "b" [ "done" ]
    defn_done
    label "alloc"
    code "txn" ["OnCompletion"]
    code "int" ["NoOp"]
    asserteq
    forM_ keyState_gvs $ \gv -> do
      ctzero $ gvType gv
      gvStore gv
    code "b" [ "updateState" ]
  -- Clear state is never allowed
  addProg "appClear" False $ do
    cl $ DLL_Bool False
  checkResources bad' =<< readIORef sResources
  stateSize <- readIORef sStateSizeR
  void $ recordSizeAndKeys "state" stateSize algoMaxGlobalSchemaEntries_usable
  sFailures <- readIORef sFailuresR
  modifyIORef resr $
    M.insert "unsupported" $
      aarray $
        S.toList $ S.map (Aeson.String . LT.toStrict) sFailures
  unless (null sFailures) $ do
    emitWarning Nothing $ W_ALGOUnsupported $ S.toList $ S.map LT.unpack sFailures
  modifyIORef resr $
    M.insert "version" $
      Aeson.Number $ fromIntegral $ reachAlgoBackendVersion
  res <- readIORef resr
  return $ Aeson.Object $ HM.fromList $ M.toList res

connect_algo :: CompilerToolEnv -> Connector
connect_algo env = Connector {..}
  where
    conName = conName'
    conCons = conCons'
    conGen moutn = compile_algo env (disp . T.pack)
      where disp which = conWrite moutn (which <> ".teal")
