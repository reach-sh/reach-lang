module Reach.Connector.ALGO (connect_algo) where

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

-- import Debug.Trace

-- General tools that could be elsewhere

longestPathBetween :: forall a . Ord a => M.Map a (M.Map a Int) -> a -> a -> ([(a, Int)], Int)
longestPathBetween g f _d = r f (fixedPoint h)
  where
    r x y = fromMaybe ([], 0) $ M.lookup x y
    h :: M.Map a ([(a, Int)], Int) -> M.Map a ([(a, Int)], Int)
    h m =
      flip M.mapWithKey g $ \n cs ->
        List.maximumBy (compare `on` snd) $
          flip map (M.toAscList cs) $ \(t, c) ->
            let (p, pc) = r t m
                p' = (t, c) : p
                c' = pc + c
                c'' = if n `elem` map fst p' then 0 else c'
            in (p', c'')

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

algoMaxAppProgramCost :: Integer
algoMaxAppProgramCost = 700

minimumBalance_l :: DLLiteral
minimumBalance_l = DLL_Int sb algoMinimumBalance

tealVersionPragma :: LT.Text
tealVersionPragma = "#pragma version 4"

-- Algo specific stuff

_udiv :: Integer -> Integer -> Integer
_udiv x y = z
  where
    (q, d) = quotRem x y
    z = if d == 0 then q else q + 1

typeSizeOf :: DLType -> Integer
typeSizeOf = \case
  T_Null -> 0
  T_Bool -> 1
  T_UInt -> word
  T_Bytes sz -> sz
  T_Digest -> 32
  T_Address -> 32
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
        opt_b1 $ ["substring", s0x, s0xp1] : l
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

checkCost :: [TEAL] -> IO ()
checkCost ts = do
  (cgr :: IORef (M.Map String (M.Map String Int))) <- newIORef $ mempty
  let lTop = "TOP"
  let lBot = "BOT"
  (labr :: IORef String) <- newIORef $ lTop
  (cr :: IORef Int) <- newIORef $ 0
  hasForR <- newIORef $ False
  let l2s = LT.unpack
  let rec c = modifyIORef cr (c +)
  let jump_ t = do
        lab <- readIORef labr
        c <- readIORef cr
        let ff = max c
        let fg = Just . ff . fromMaybe 0
        let f = M.alter fg t
        let g = Just . f . fromMaybe mempty
        modifyIORef cgr $ M.alter g lab
  let switch t = do
        writeIORef labr t
        writeIORef cr 0
  let jump t = rec 1 >> jump_ (l2s t ++ ":")
  forM_ ts $ \case
    ["sha256"] -> rec 35
    ["keccak256"] -> rec 130
    ["sha512_256"] -> rec 45
    ["ed25519verify"] -> rec 1900
    ["divmodw"] -> rec 20
    ["sqrt"] -> rec 4
    ["expw"] -> rec 10
    ["b+"] -> rec 10
    ["b-"] -> rec 10
    ["b/"] -> rec 20
    ["b*"] -> rec 20
    ["b%"] -> rec 20
    ["b|"] -> rec 6
    ["b&"] -> rec 6
    ["b^"] -> rec 6
    ["b~"] -> rec 4
    ["b|"] -> rec 6
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
    [ "//", com ] -> do
      when (com == "<for>") $ do
        writeIORef hasForR True
      return ()
    [lab'] | LT.isSuffixOf ":" lab' -> do
      let lab'' = l2s lab'
      jump_ lab''
      switch lab''
    _ -> rec 1
  cg <- readIORef cgr
  let (p, c) = longestPathBetween cg lTop (l2s lBot)
  hasFor <- readIORef hasForR
  let whenl x e =
        case x of
          True -> [ e ]
          False -> []
  let cs = []
        <> (whenl hasFor $
              "This program contains a loop, which cannot be tracked accurately for cost.")
        <> (whenl (fromIntegral c > algoMaxAppProgramCost) $
              "This program could take " <> show c <> " units of cost, but the limit is " <> show algoMaxAppProgramCost <> ": " <> show p)
  unless (null cs) $
    emitWarning $ W_ALGOConservative cs

data Shared = Shared
  { sFailuresR :: IORef (S.Set LT.Text)
  , sCounter :: Counter
  , sViewSize :: Integer
  , sViewKeysl :: [Word8]
  , sMaps :: DLMapInfos
  , sMapDataTy :: DLType
  , sMapDataSize :: Integer
  , sMapKeysl :: [Word8]
  , sTxnCounts :: IORef (CostGraph Int)
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
  , eTxnCount :: Counter
  }

type App = ReaderT Env IO

recordWhich :: Int -> App a -> App a
recordWhich n = local (\e -> e { eWhich = n }) . dupeTxnCount

type CostGraph a = M.Map a (CostRecord a)
data CostRecord a = CostRecord
  { cr_n :: Int
  , cr_max :: S.Set a
  }
  deriving (Show)
type TxnCountRec = CostRecord Int
updateCostRecord :: Ord a => IORef (CostGraph a) -> a -> (CostRecord a -> CostRecord a) -> IO ()
updateCostRecord cgr lab f = do
  let g = Just . f . fromMaybe (CostRecord 0 mempty)
  modifyIORef cgr $ M.alter g lab

dupeTxnCount :: App a -> App a
dupeTxnCount m = do
  c' <- (liftIO . dupeCounter) =<< asks eTxnCount
  local (\e -> e { eTxnCount = c' }) m
incTxnCount :: App ()
incTxnCount = do
  void $ (liftIO . incCounter) =<< asks eTxnCount
updateTxnCount :: (TxnCountRec -> TxnCountRec) -> App ()
updateTxnCount f = do
  Env {..} <- ask
  let Shared {..} = eShared
  liftIO $ updateCostRecord sTxnCounts eWhich f
addTxnCountEdge :: Int -> App ()
addTxnCountEdge w' = do
  addTxnCount
  updateTxnCount (\t -> t { cr_max = S.insert w' (cr_max t) })
addTxnCount :: App ()
addTxnCount = do
  c <- (liftIO . readCounter) =<< asks eTxnCount
  updateTxnCount (\t -> t { cr_n = max c (cr_n t) })
checkTxnCount :: (LT.Text -> IO ()) -> CostGraph Int -> IO ()
checkTxnCount bad' tcs = do
  -- XXX Do this not dumb
  let maximum' :: [Int] -> Int
      maximum' l = maximum $ 0 : l
  let chase i = cr_n + (maximum' $ map chase $ S.toAscList cr_max)
        where CostRecord {..} = tcs M.! i
  forM_ (M.keys tcs) $ \which -> do
    let amt = chase which
    when (fromIntegral amt > algoMaxTxGroupSize) $ do
      bad' $ "Step " <> texty which <> " could have too many txns: could have " <> texty amt <> " but limit is " <> texty algoMaxTxGroupSize

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
padding = cl . bytesZeroLit

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

freshLabel :: App LT.Text
freshLabel = do
  i <- (liftIO . incCounter) =<< (eLabel <$> ask)
  return $ "l" <> LT.pack (show i)

loopLabel :: Int -> LT.Text
loopLabel w = "loop" <> LT.pack (show w)

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
  T_Token -> cfrombs T_UInt
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Struct {} -> nop

tint :: SrcLoc -> Integer -> LT.Text
tint at i = texty $ checkIntLiteralC at connect_algo i

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

cl :: DLLiteral -> App ()
cl = \case
  DLL_Null -> cl $ DLL_Bytes ""
  DLL_Bool b -> cl $ DLL_Int sb $ if b then 1 else 0
  DLL_Int at i -> code "int" [tint at i]
  DLL_Bytes bs -> code "byte" [base64d bs]

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
  DLA_Constant c -> cl $ conCons connect_algo c
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

cprim :: PrimOp -> [DLArg] -> App ()
cprim = \case
  SELF_ADDRESS -> impossible "self address"
  ADD -> call "+"
  SUB -> call "-"
  MUL -> call "*"
  DIV -> call "/"
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
  BYTES_CONCAT -> \case
    [x, y] -> do
      ca x
      ca y
      op "concat"
    _ -> impossible $ "concat"
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

csubstring :: SrcLoc -> Integer -> Integer -> App ()
csubstring at b c =
  case b < 256 && c < 256 of
    True -> do
      code "substring" [tint at b, tint at c]
    False -> do
      cl $ DLL_Int sb b
      cl $ DLL_Int sb c
      op "substring3"

computeSplice :: SrcLoc -> Integer -> Integer -> Integer -> (App (), App ())
computeSplice at b c e = (before, after)
  where
    before = csubstring at 0 b
    after = csubstring at c e

csplice :: SrcLoc -> Integer -> Integer -> Integer -> App ()
csplice at b c e = do
  -- [ Bytes  = X b Y c Z e , NewBytes = Y' ]
  let len = c - b
  case len == 1 of
    True -> do
      -- [ Bytes, NewByte ]
      cl $ DLL_Int at b
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
                cl $ DLL_Int sb 0
                cl $ DLL_Int sb tsz
                cidx
                op "*"
                op "substring3"
              a = do
                cl $ DLL_Int sb tsz
                op "dup"
                cidx
                op "*"
                op "+"
                cl $ DLL_Int sb (alen * tsz)
                op "substring3"
  csplice3 mcbig cbefore cafter cnew

computeSubstring :: [DLType] -> Integer -> (DLType, Integer, Integer)
computeSubstring ts idx = (t, start, end)
  where
    szs = map typeSizeOf ts
    starts = scanl (+) 0 szs
    ends = zipWith (+) szs starts
    idx' = fromIntegral idx
    tse = zip3 ts starts ends
    (t, start, end) =
      case atMay tse idx' of
        Nothing -> impossible "bad idx"
        Just x -> x

cfor :: Integer -> (App () -> App ()) -> App ()
cfor maxi body = do
  top_lab <- freshLabel
  end_lab <- freshLabel
  salloc_ $ \store_idx load_idx -> do
    cl $ DLL_Int sb 0
    store_idx
    comment $ "<for>"
    label top_lab
    load_idx
    cl $ DLL_Int sb maxi
    op "<"
    code "bz" [end_lab]
    body load_idx
    load_idx
    cl $ DLL_Int sb 1
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
          let end = start + tsz
          csubstring at start end
        _ -> do
          cl $ DLL_Int sb tsz
          ie'
          op "*"
          op "dup"
          cl $ DLL_Int sb tsz
          op "+"
          op "substring3"
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
          Just x -> cl $ DLL_Bytes x
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
    cl $ DLL_Bytes $ B.singleton $ BI.w2c vi
    ca va
    ctobs vt
    let vlen = 1 + typeSizeOf (argTypeOf va)
    op "concat"
    let dlen = typeSizeOf $ T_Data tm
    let zlen = fromIntegral $ dlen - vlen
    padding $ zlen
    op "concat"
    check_concat_len dlen
  DLLA_Struct kvs ->
    cconcatbs $ map (\a -> (argTypeOf a, ca a)) $ map snd kvs

cTupleRef :: SrcLoc -> DLType -> Integer -> App ()
cTupleRef at tt idx = do
  -- [ Tuple ]
  let ts = typeTupleTypes tt
  let (t, start, end) = computeSubstring ts idx
  case (ts, idx) of
    ([ _ ], 0) ->
      return ()
    _ -> do
      csubstring at start end
  -- [ ValueBs ]
  cfrombs t
  -- [ Value ]
  return ()

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
    cl $ DLL_Bytes $ keyVary mi
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
    cl $ DLL_Bytes $ keyVary mi
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
      cl $ DLL_Bytes ""
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
    let (t, start, end) = computeSubstring (map snd fts) fidx
    ca oa
    csubstring at start end
    cfrombs t
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> cdigest $ map go args
    where
      go a = (argTypeOf a, ca a)
  DLE_Transfer ct_at who ct_amt ct_mtok -> do
    let ct_always = False
    let ct_mcrecv = Just $ ca who
    let ct_mcsend = Just cContractAddr
    let ct_mcclose = Nothing
    checkTxn $ CheckTxn {..}
  DLE_TokenInit ct_at tok -> do
    comment $ "Initializing token"
    let ct_always = True
    let ct_mtok = Just tok
    let ct_amt = DLA_Literal $ DLL_Int sb 0
    let ct_mcsend = Just cContractAddr
    let ct_mcrecv = Just cContractAddr
    let ct_mcclose = Nothing
    let cta = CheckTxn {..}
    checkTxn $ cta
      { ct_mtok = Nothing
      , ct_amt = DLA_Literal $ minimumBalance_l
      , ct_mcsend = Nothing }
    checkTxn $ cta
  DLE_CheckPay ct_at fs ct_amt ct_mtok -> do
    show_stack ("CheckPay" :: String) ct_at fs
    let ct_always = False
    let ct_mcsend = Nothing -- Flexible, not necc appl sender
    let ct_mcrecv = Just $ cContractAddr
    let ct_mcclose = Nothing
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
    ca fa
    cMapLoad
    mdt <- getMapDataTy
    cTupleRef sb mdt $ fromIntegral i
  DLE_MapSet at mpv@(DLMVar i) fa mva -> do
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
    let ct_always = True
    let ct_mtok = Nothing
    let ct_mcsend = Nothing
    let ct_mcrecv = Just cContractAddr
    let ct_mcclose = Nothing
    let ct_amt = DLA_Literal $ minimumBalance_l
    checkTxn $ CheckTxn {..}
    checkTxnAlloc
    let vTypeEnum = "acfg"
    checkTxnInit vTypeEnum $ Just cContractAddr
    let i = cl . DLL_Int at
    let za = czaddr
    i 0 >> checkTxn1 "ConfigAsset"
    ca dtn_supply >> checkTxn1 "ConfigAssetTotal"
    i 6 >> checkTxn1 "ConfigAssetDecimals"
    i 0 >> checkTxn1 "ConfigAssetDefaultFrozen"
    ca dtn_sym >> checkTxn1 "ConfigAssetUnitName"
    ca dtn_name >> checkTxn1 "ConfigAssetName"
    ca dtn_url >> checkTxn1 "ConfigAssetURL"
    ca dtn_metadata >> checkTxn1 "ConfigAssetMetadataHash"
    cContractAddr >> checkTxn1 "ConfigAssetManager"
    za >> checkTxn1 "ConfigAssetReserve"
    za >> checkTxn1 "ConfigAssetFreeze"
    za >> checkTxn1 "ConfigAssetClawback"
    op "gaids"
  DLE_TokenBurn {} ->
    -- Burning does nothing on Algorand, because we already own it and we're
    -- the creator, and that's the rule for being able to destroy
    return ()
  DLE_TokenDestroy at aida -> do
    checkTxnAlloc
    let vTypeEnum = "acfg"
    let ct_mcsend = Just cContractAddr
    checkTxnInit vTypeEnum ct_mcsend
    let i0 = cl $ DLL_Int at 0
    let b0 = padding 0
    ca aida >> checkTxn1 "ConfigAsset"
    i0 >> checkTxn1 "ConfigAssetTotal"
    i0 >> checkTxn1 "ConfigAssetDecimals"
    i0 >> checkTxn1 "ConfigAssetDefaultFrozen"
    b0 >> checkTxn1 "ConfigAssetUnitName"
    b0 >> checkTxn1 "ConfigAssetName"
    b0 >> checkTxn1 "ConfigAssetURL"
    czaddr >> checkTxn1 "ConfigAssetMetadataHash"
    czaddr >> checkTxn1 "ConfigAssetManager"
    czaddr >> checkTxn1 "ConfigAssetReserve"
    czaddr >> checkTxn1 "ConfigAssetFreeze"
    czaddr >> checkTxn1 "ConfigAssetClawback"
    op "pop"
    -- XXX We could get the minimum balance back
  DLE_TimeOrder {} -> impossible "timeorder"
  where
    show_stack msg at fs = do
      comment $ texty msg
      comment $ texty $ unsafeRedactAbsStr $ show at
      comment $ texty $ unsafeRedactAbsStr $ show fs

staticZero :: DLArg -> Bool
staticZero = \case
  DLA_Literal (DLL_Int _ 0) -> True
  _ -> False

data CheckTxn = CheckTxn
  { ct_at :: SrcLoc
  , ct_mcsend :: Maybe (App ())
  , ct_mcrecv :: Maybe (App ())
  , ct_mcclose :: Maybe (App ())
  , ct_always :: Bool
  , ct_amt :: DLArg
  , ct_mtok :: Maybe DLArg }

checkTxn1 :: LT.Text -> App ()
checkTxn1 f = do
  code "dig" [ "1" ]
  code "gtxns" [f]
  asserteq

checkTxnAlloc :: App ()
checkTxnAlloc = do
  incTxnCount
  gvLoad GV_txnCounter
  op "dup"
  cl $ DLL_Int sb 1
  op "+"
  gvStore GV_txnCounter

checkTxnInit :: LT.Text -> Maybe (App ()) -> App ()
checkTxnInit vTypeEnum ct_mcsend = do
  -- [ txn ]
  code "int" [vTypeEnum]
  checkTxn1 "TypeEnum"
  cl $ DLL_Int sb 0
  checkTxn1 "Fee"
  -- XXX We could move this into the escrow, since it will always be happening.
  -- The problem, though, is that we use this ALSO for when the user pays us...
  -- and when can we check THOSE?
  czaddr
  checkTxn1 "Lease"
  czaddr
  checkTxn1 "RekeyTo"
  whenJust ct_mcsend $ \csend -> do
    csend
    cfrombs T_Address
    checkTxn1 "Sender"
  -- [ txn ]
  return ()

checkTxn :: CheckTxn -> App ()
checkTxn (CheckTxn {..}) = when (ct_always || not (staticZero ct_amt) ) $ do
  let check1 = checkTxn1
  let (vTypeEnum, fReceiver, fAmount, fCloseTo, extra) =
        case ct_mtok of
          Nothing ->
            ("pay", "Receiver", "Amount", "CloseRemainderTo", return ())
          Just tok ->
            ("axfer", "AssetReceiver", "AssetAmount", "AssetCloseTo", textra)
            where textra = ca tok >> check1 "XferAsset"
  after_lab <- freshLabel
  ca ct_amt
  unless ct_always $ do
    op "dup"
    code "bz" [after_lab]
  checkTxnAlloc
  -- [ amt, id ]
  op "swap"
  -- [ id, amt ]
  check1 fAmount
  extra
  checkTxnInit vTypeEnum ct_mcsend
  whenJust ct_mcclose $ \cclose -> do
    cclose
    cfrombs T_Address
    check1 fCloseTo
  whenJust ct_mcrecv $ \crecv -> do
    crecv
    cfrombs T_Address
    check1 fReceiver
  label after_lab
  op "pop" -- if !always & zero then pop amt ; else pop id

doSwitch :: (a -> App ()) -> SrcLoc -> DLVar -> SwitchCases a -> App ()
doSwitch ck at dv csm = do
  end_lab <- freshLabel
  let cm1 ((_vn, (mov, k)), vi) = do
        next_lab <- freshLabel
        ca $ DLA_Var dv
        cl $ DLL_Int sb 0
        op "getbyte"
        cl $ DLL_Int sb vi
        op "=="
        code "bz" [next_lab]
        case mov of
          Nothing -> ck k
          Just vv -> do
            flip (sallocLet vv) (ck k) $ do
              ca $ DLA_Var dv
              let vt = argTypeOf $ DLA_Var vv
              csubstring at 1 (1 + typeSizeOf vt)
              cfrombs vt
        label next_lab
  mapM_ cm1 $ zip (M.toAscList csm) [0 ..]
  label end_lab

cm :: App () -> DLStmt -> App ()
cm km = \case
  DL_Nop _ -> km
  DL_Let _ DLV_Eff de -> ce de >> km
  DL_Let _ (DLV_Let DVC_Once dv) de -> do
    sm <- exprSmall de
    store_let dv sm (ce de) km
  DL_Let _ (DLV_Let DVC_Many dv) de -> do
    sm <- exprSmall de
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
      cl $ DLL_Bytes ""
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
    false_lab <- freshLabel
    join_lab <- freshLabel
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
    false_lab <- freshLabel
    code "bz" [false_lab]
    nct tt
    label false_lab
    nct ft
  CT_Switch at dv csm ->
    doSwitch nct at dv csm
  CT_Jump _at which svs (DLAssignment msgm) -> do
    cla $ DLLA_Tuple $ map DLA_Var svs
    cla $ DLLA_Tuple $ map snd $ M.toAscList msgm
    addTxnCountEdge which
    code "b" [ loopLabel which ]
  CT_From at which msvs -> do
    isHalt <- do
      case msvs of
        FI_Halt toks -> do
          forM_ toks close_asset
          close_escrow
          czaddr
          gvStore GV_stateHash
          return True
          where
            ct_at = at
            ct_mcrecv = Nothing
            ct_mcsend = Just $ cContractAddr
            ct_always = True
            ct_amt = DLA_Literal $ DLL_Int sb 0
            ct_mcclose = Just $ cDeployer
            close_asset tok = checkTxn $ CheckTxn {..}
              where ct_mtok = Just tok
            close_escrow = checkTxn $ CheckTxn {..}
              where ct_mtok = Nothing
        FI_Continue vis svs -> do
          cViewSave at vis
          cstate (HM_Set which) $ map snd svs
          return False
    code "txn" ["OnCompletion"]
    code "int" [ if isHalt then "DeleteApplication" else "NoOp" ]
    asserteq
    code "b" ["updateState"]
    addTxnCount
  where
    nct = dupeTxnCount . ct

cViewSave :: SrcLoc -> ViewSave -> App ()
cViewSave at (ViewSave vwhich vvs) = do
  let vconcat x y = DLA_Literal (DLL_Int at $ fromIntegral x) : (map snd y)
  let la = DLLA_Tuple $ vconcat vwhich vvs
  let lat = largeArgTypeOf la
  let sz = typeSizeOf lat
  Shared {..} <- eShared <$> ask
  when (sViewSize > 0) $ do
    cla la
    ctobs lat
    padding $ sViewSize - sz
    op "concat"
    -- [ ViewData ]
    forM_ sViewKeysl $ \vi -> do
      -- [ ViewData ]
      cl $ DLL_Bytes $ keyVary vi
      -- [ ViewData, Key ]
      code "dig" [ "1" ]
      -- [ ViewData, Key, ViewData ]
      cStateSlice at sViewSize vi
      -- [ ViewData, Key, ViewData' ]
      op "app_global_put"
      -- [ ViewData ]
      return ()
    -- [ ViewData ]
    op "pop"
    -- [ ]
    return ()

data HashMode
  = HM_Set Int
  | HM_Check Int
  deriving (Eq, Show)

cstate :: HashMode -> [DLArg] -> App ()
cstate hm svs = do
  comment ("compute state in " <> texty hm)
  let go a = (argTypeOf a, ca a)
  let wa w = DLA_Literal $ DLL_Int sb $ fromIntegral w
  let compute w = cdigest $ map go $ wa w : svs
  case hm of
    HM_Set w -> do
      compute w
      gvStore GV_stateHash
    HM_Check p -> do
      compute p
      gvLoad GV_stateHash
      asserteq

-- Reach Constants
reachAlgoBackendVersion :: Int
reachAlgoBackendVersion = 2

-- State:
keyState :: B.ByteString
keyState = ""

keyVary :: Word8 -> B.ByteString
keyVary = B.singleton . BI.w2c

cContractAddr :: App ()
cContractAddr = gvLoad GV_contractAddr

cDeployer :: App ()
cDeployer = code "global" ["CreatorAddress"]

etexty :: Enum a => a -> LT.Text
etexty = texty . fromEnum

data ArgId
  = ArgMethod
  | ArgSvs
  | ArgMsg
  deriving (Eq, Ord, Show, Enum, Bounded)

boundedCount :: forall a . (Enum a, Bounded a) => a -> Integer
boundedCount _ = 1 + (fromIntegral $ fromEnum $ (maxBound :: a))

argCount :: Integer
argCount = boundedCount ArgMethod

data GlobalVar
  = GV_txnCounter
  | GV_stateHash
  | GV_contractAddr
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
  GV_stateHash -> T_Digest
  GV_contractAddr -> T_Address

keyState_ty :: DLType
keyState_ty = T_Tuple [gvType GV_stateHash, gvType GV_contractAddr]

defn_done :: App ()
defn_done = do
  label "done"
  cl $ DLL_Int sb 1
  op "return"

bindTime :: DLVar -> App a -> App a
bindTime dv = store_let dv True (code "global" ["Round"])
bindSecs :: DLVar -> App a -> App a
bindSecs dv = store_let dv True (code "global" ["LatestTimestamp"])

bindFromTuple :: SrcLoc -> [DLVar] -> App a -> App a
bindFromTuple at vs m = do
  let mkArgVar l = DLVar at Nothing (T_Tuple $ map varType l) <$> ((liftIO . incCounter) =<< ((sCounter . eShared) <$> ask))
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
  let argSize = 1 + (typeSizeOf $ T_Tuple $ map varType $ svs <> msg)
  when (argSize > algoMaxAppTotalArgLen) $
    xxx $ texty $ "Step " <> show which <> "'s argument length is " <> show argSize <> ", but the maximum is " <> show algoMaxAppTotalArgLen
  let bindFromArg ai vs m = do
        code "txna" [ "ApplicationArgs", etexty ai ]
        op "dup"
        op "len"
        cl $ DLL_Int sb $ typeSizeOf $ (T_Tuple $ map varType vs)
        asserteq
        bindFromTuple at vs m
  comment ("Handler " <> texty which)
  op "dup" -- We assume the method id is on the stack
  cl $ DLL_Int sb $ fromIntegral $ which
  op "=="
  code "bz" [ afterLab ]
  op "pop" -- Get rid of the method id since it's us
  let bindVars = id
        . (store_let from True (code "txn" [ "Sender" ]))
        . (bindTime timev)
        . (bindSecs secsv)
        . (bindFromArg ArgSvs svs)
        . (bindFromArg ArgMsg msg)
  bindVars $ do
    cstate (HM_Check prev) $ map DLA_Var svs
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

computeViewSize :: Maybe (CPViews, ViewInfos) -> Integer
computeViewSize = h1
  where
    h1 = \case
      Nothing -> 0
      Just (_, vi) -> h2 vi
    h2 = maximum . map h3 . M.elems
    h3 (ViewInfo vs _) = typeSizeOf $ T_Tuple $ T_UInt : h4 vs
    h4 = map varType

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

compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at plo dli _ _ cpp = pl
  let CPProg at csvs vi (CHandlers hm) = cpp
  let sMaps = dli_maps dli
  resr <- newIORef mempty
  sFailuresR <- newIORef mempty
  sTxnCounts <- newIORef mempty
  let sViewSize = computeViewSize vi
  let sMapDataTy = mapDataTy sMaps
  let sMapDataSize = typeSizeOf sMapDataTy
  let PLOpts {..} = plo
  let sCounter = plo_counter
  let divup :: Integer -> Integer -> Integer
      divup x y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y)
  let recordSize prefix size = do
        modifyIORef resr $
          M.insert (prefix <> "Size") $
            Aeson.Number $ fromIntegral size
  let recordSizeAndKeys :: T.Text -> Integer -> Integer -> IO [Word8]
      recordSizeAndKeys prefix size limit = do
        recordSize prefix size
        let keys = size `divup` algoMaxAppBytesValueLen_usable
        when (keys > limit) $ do
          bad_io sFailuresR $ "Too many " <> (LT.fromStrict prefix) <> " keys, " <> texty keys <> ", but limit is " <> texty limit
        modifyIORef resr $
          M.insert (prefix <> "Keys") $
            Aeson.Number $ fromIntegral keys
        return $ take (fromIntegral keys) [0 ..]
  sViewKeysl <- recordSizeAndKeys "view" sViewSize algoMaxGlobalSchemaEntries_usable
  sMapKeysl <- recordSizeAndKeys "mapData" sMapDataSize algoMaxLocalSchemaEntries_usable
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
        eTxnCount <- newCounter 1
        flip runReaderT (Env {..}) m
        readIORef eOutputR
  let bad' = bad_io sFailuresR
  let addProg lab m = do
        ts <- run m
        let tsl = DL.toList ts
        let tsl' = optimize tsl
        checkCost tsl'
        let lts = tealVersionPragma : (map LT.unwords tsl')
        let lt = LT.unlines lts
        let t = LT.toStrict lt
        modifyIORef resr $ M.insert (T.pack lab) $ Aeson.String t
        disp lab t
  addProg "appApproval" $ do
    checkRekeyTo
    checkLease
    cl $ DLL_Int sb 0
    gvStore GV_txnCounter
    code "txn" ["ApplicationID"]
    code "bz" ["alloc"]
    cl $ DLL_Bytes keyState
    op "app_global_get"
    op "dup"
    cTupleRef at keyState_ty 0
    gvStore GV_stateHash
    cTupleRef at keyState_ty 1
    gvStore GV_contractAddr
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
    cl $ DLL_Int sb $ argCount
    asserteq
    code "txna" [ "ApplicationArgs", etexty ArgMethod ]
    cfrombs T_UInt
    op "dup"
    code "bz" [ "ctor" ]
    -- NOTE This could be compiled to a jump table if that were possible or to
    -- a tree to be O(log n) rather than O(n)
    forM_ (M.toAscList hm) $ \(hi, hh) -> do
      afterLab <- freshLabel
      ch afterLab hi hh
      label afterLab
    cl $ DLL_Bool False
    assert
    forM_ (M.toAscList hm) $ \(hi, hh) ->
      cloop hi hh
    label "updateState"
    cl $ DLL_Bytes keyState
    gvLoad GV_stateHash
    gvLoad GV_contractAddr
    op "concat"
    op "app_global_put"
    code "b" [ "checkSize" ]
    label "checkSize"
    gvLoad GV_txnCounter
    op "dup"
    op "dup"
    -- The size is correct
    cl $ DLL_Int sb $ 1
    op "+"
    code "global" [ "GroupSize" ]
    asserteq
    -- We're last
    code "txn" ["GroupIndex"]
    asserteq
    cl $ DLL_Int sb $ algoMinTxnFee
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
    cl $ DLL_Bytes keyState
    padding $ typeSizeOf keyState_ty
    op "app_global_put"
    code "b" [ "checkSize" ]
    label "ctor"
    -- NOTE We are trusting the creator to tell us the correct contractAddr. If
    -- they don't, this whole app is screwed. It would technically be possible
    -- to figure this out by keccak-ing (or something) the source of escrow
    -- with our own appid. We're not going to do that because we know
    -- applications-with-transactions are coming and the escrow account is
    -- almost dead.
    --
    -- Since we can't check it anyways, we're not even going to bother ensuring
    -- that there's a pay with it,
    code "txn" ["Sender"]
    cDeployer
    asserteq
    code "txna" [ "ApplicationArgs", etexty ArgSvs ]
    -- NOTE We could/should check this is address-length
    gvStore GV_contractAddr
    -- NOTE firstMsg => do handler 1
    let bind_csvs =
          case dli_ctimem dli of
            Nothing -> id
            Just (tv, sv) -> bindTime tv . bindSecs sv
    bind_csvs $ ct $ CT_From at 0 $ FI_Continue (ViewSave 0 mempty) $ asnLike csvs
  -- Clear state is never allowed
  addProg "appClear" $ do
    cl $ DLL_Bool False
  -- The escrow account defers to the application
  addProg "escrow" $ do
    code "global" ["GroupSize"]
    cl $ DLL_Int sb 1
    op "-"
    op "dup"
    code "gtxns" ["TypeEnum"]
    code "int" ["appl"]
    asserteq
    code "gtxns" ["ApplicationID"]
    code "int" ["{{ApplicationID}}"]
    asserteq
    code "b" ["done"]
    defn_done
  checkTxnCount bad' =<< readIORef sTxnCounts
  sFailures <- readIORef sFailuresR
  modifyIORef resr $
    M.insert "unsupported" $
      aarray $
        S.toList $ S.map (Aeson.String . LT.toStrict) sFailures
  unless (null sFailures) $ do
    emitWarning $ W_ALGOUnsupported $ S.toList $ S.map LT.unpack sFailures
  modifyIORef resr $
    M.insert "version" $
      Aeson.Number $ fromIntegral $ reachAlgoBackendVersion
  res <- readIORef resr
  return $ Aeson.Object $ HM.fromList $ M.toList res

connect_algo :: Connector
connect_algo = Connector {..}
  where
    conName = "ALGO"
    conCons DLC_UInt_max = DLL_Int sb $ 2 ^ (64 :: Integer) - 1
    conGen moutn = compile_algo (disp . T.pack)
      where disp which = conWrite moutn (which <> ".teal")
