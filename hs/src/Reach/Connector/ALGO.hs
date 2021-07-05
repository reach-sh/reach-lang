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
import Reach.Texty (pretty)
import Reach.UnsafeUtil
import Reach.Util
import Safe (atMay)
import Text.Read

-- import Debug.Trace

-- General tools that could be elsewhere

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

algoMaxTxGroupSize :: TxnIdx
algoMaxTxGroupSize = 16

algoMaxAppBytesValueLen :: Integer
algoMaxAppBytesValueLen = 64

algoMaxAppTxnAccounts :: Word8
algoMaxAppTxnAccounts = 4 -- plus sender

algoMinimumBalance :: Integer
algoMinimumBalance = 100000

accountsL :: [Word8]
accountsL = take (fromIntegral $ algoMaxAppTxnAccounts + 1) [0 ..]

minimumBalance_l :: DLLiteral
minimumBalance_l = DLL_Int sb algoMinimumBalance

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

type TxnIdx = Word8

type TEAL = [LT.Text]

code_ :: LT.Text -> [LT.Text] -> TEAL
code_ fun args = fun : args

label_ :: LT.Text -> TEAL
label_ lab = [lab <> ":"]

comment_ :: LT.Text -> TEAL
comment_ t = ["//", t]

type TEALs = DL.DList TEAL

peep_optimize :: [TEAL] -> [TEAL]
peep_optimize = \case
  [] -> []
  [["return"]] -> []
  ["byte", "base64()"] : ["concat"] : l -> peep_optimize l
  ["byte", x] : ["byte", y] : ["concat"] : l | isBase64 x && isBase64 y ->
    peep_optimize $ [ "byte", (base64d $ base64u x <> base64u y) ] : l
  ["byte", x] : l | isBase64_zeros x -> peep_optimize $
    case B.length $ base64u x of
      32 -> ["global", "ZeroAddress"] : l
      len -> ["int", texty len] : ["bzero"] : l
  ["b", x] : b@[y] : l | y == (x <> ":") -> b : peep_optimize l
  ["btoi"] : ["itob", "// bool"] : ["substring", "7", "8"] : l -> peep_optimize l
  ["btoi"] : ["itob"] : l -> peep_optimize l
  ["itob"] : ["btoi"] : l -> peep_optimize l
  a@["load", x] : ["load", y] : l
    | x == y ->
      -- This misses if there is ANOTHER load of the same thing
      peep_optimize $ a : ["dup"] : l
  a@["store", x] : ["load", y] : l
    | x == y ->
      ["dup"] : peep_optimize (a : l)
  a@["substring", s0, _] : b@["int", x] : c@["getbyte"] : l ->
    case mse of
      Just (s0x, s0xp1) ->
        peep_optimize $ ["substring", s0x, s0xp1] : l
      Nothing ->
        a : (peep_optimize $ b : c : l)
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
        peep_optimize $ ["substring", s2, e2] : l
      Nothing ->
        a : (peep_optimize $ b : l)
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
  --a@["int", x] : b@["itob"] : l ->
  --  case itob x of
  --    Nothing ->
  --      a : (peep_optimize $ b : l)
  --    Just xbs ->
  --      peep_optimize $ ["byte", xbs ] : l
  x : l -> x : peep_optimize l
  where
    parse :: LT.Text -> Maybe Integer
    parse = readMaybe . LT.unpack
    _itob :: LT.Text -> Maybe LT.Text
    _itob x_lt = do
      x <- parse x_lt
      let x_bs = LB.toStrict $ toLazyByteString $ word64BE $ fromIntegral x
      return $ base64d x_bs

render :: TEALs -> T.Text
render ts = tt
  where
    tt = LT.toStrict lt
    lt = LT.unlines lts
    lts = "#pragma version 4" : (map LT.unwords $ peep_optimize $ DL.toList ts)

data Shared = Shared
  { sFailuresR :: IORef (S.Set LT.Text)
  , sCounter :: Counter
  , sViewSize :: Integer
  , sMaps :: DLMapInfos
  , sMapDataTy :: DLType
  , sMapDataSize :: Integer
  , sMapRecordTy :: DLType
  , sMapRecordSize :: Integer
  , sMapArgTy :: DLType
  , sMapArgSize :: Integer
  }

type Lets = M.Map DLVar (App ())
data Env = Env
  { eShared :: Shared
  , eWhich :: String
  , eLabel :: Counter
  , eOutputR :: IORef TEALs
  , eHP :: ScratchSlot
  , eSP :: ScratchSlot
  , eVars :: M.Map DLVar ScratchSlot
  , eLets :: Lets
  , eLetSmalls :: M.Map DLVar Bool
  , eStepInfoR :: IORef (M.Map Int Aeson.Value)
  }

recordWhich :: Int -> App a -> App a
recordWhich w = local (\e -> e { eWhich = (eWhich e) <> " > " <> show w })

type App = ReaderT Env IO

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

cfail :: App ()
cfail = code "b" ["fail"]

op :: LT.Text -> App ()
op = flip code []

nop :: App ()
nop = return ()

padding :: Integer -> App ()
padding = cl . bytesZeroLit

app_global_get :: B.ByteString -> App ()
app_global_get k = do
  cl $ DLL_Bytes $ k
  op "app_global_get"

app_global_put :: B.ByteString -> App ()
app_global_put k = do
  cl $ DLL_Bytes $ k
  op "swap"
  op "app_global_put"

app_local_get :: Word8 -> B.ByteString -> App ()
app_local_get ai k = do
  cl $ DLL_Int sb $ fromIntegral ai
  cl $ DLL_Bytes $ k
  op "app_local_get"

app_local_put :: Word8 -> B.ByteString -> App ()
app_local_put ai k = do
  cl $ DLL_Int sb $ fromIntegral ai
  op "swap"
  cl $ DLL_Bytes $ k
  op "swap"
  op "app_local_put"

checkRekeyTo :: App ()
checkRekeyTo = do
  code "txn" ["RekeyTo"]
  code "global" ["ZeroAddress"]
  asserteq

checkLease :: App ()
checkLease = do
  code "txn" ["Lease"]
  code "global" ["ZeroAddress"]
  asserteq

checkTimeLimits :: SrcLoc -> App ()
checkTimeLimits at = do
  let go f i cmp = do
        code "gtxns" [ f ]
        gvLoad GV_timeLimits
        cTupleRef at (gvType GV_timeLimits) i
        op cmp
        assert
  op "dup"
  go "FirstValid" 0 ">="
  go "LastValid" 1 "<="

bad :: LT.Text -> App ()
bad lab = do
  Env {..} <- ask
  let Shared {..} = eShared
  liftIO $ modifyIORef sFailuresR (S.insert lab)
  output $ comment_ $ "BAD " <> lab

xxx :: LT.Text -> App ()
xxx lab = bad $ "This program uses " <> lab

freshLabel :: App LT.Text
freshLabel = do
  i <- (liftIO . incCounter) =<< (eLabel <$> ask)
  return $ "l" <> LT.pack (show i)

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
      impossible $ eWhich <> " lookup_let " <> show (pretty dv) <> " not in " <> (List.intercalate ", " $ map (show . pretty) $ M.keys eLets)

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

-- XXX check that the number of transactions is never more than
-- algoMaxTxGroupSize

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
    x3 = decodeBase64' $ B.pack $ LT.unpack x2
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

ca :: DLArg -> App ()
ca = \case
  DLA_Var v -> lookup_let v
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

csum_ :: [App ()] -> App ()
csum_ = \case
  [] -> cl $ DLL_Int sb 0
  [m] -> m
  m : ms -> csum_ ms >> m >> op "+"

csum :: [DLArg] -> App ()
csum = csum_ . map ca

cconcatbs :: [(DLType, App ())] -> App ()
cconcatbs l = do
  check_concat_len totlen
  mapM_ (uncurry go) $ zip (no_concat : repeat yes_concat) l
  where
    go may_concat (t, m) = m >> ctobs t >> may_concat
    no_concat = nop
    yes_concat = op "concat"
    totlen = typeSizeOf $ T_Tuple $ map fst l

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
cdigest l = cconcatbs l >> op "keccak256"

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

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_LArg _ a -> cla a
  DLE_Impossible at err -> expect_thrown at err
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
    let ct_mcsend = Just $ ca who
    let ct_mcrecv = Just cContractAddr
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
    checkTxn $ CheckTxn {..}
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
  DLE_MapRef _ mpv fa -> do
    ca fa
    cMapRecAlloc
    cMapRef mpv
  DLE_MapSet _ mpv fa mva -> do
    ca fa
    cMapRecAlloc
    t <- getMapTy mpv
    cla $ mdaToMaybeLA t mva
    cMapSet mpv
  DLE_Remote {} -> xxx "remote objects"
  DLE_TokenNew {} -> xxx "token creation"
  DLE_TokenBurn {} ->
    -- Burning does nothing on Algorand, because we already own it and we're
    -- the creator, and that's the rule for being able to destroy
    -- ....unless we need to do the TEAL3 hack
    xxx "token burn"
  DLE_TokenDestroy {} -> xxx "token destroy"
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
  , ct_mcrecv :: Maybe (App ())
  , ct_mcsend :: Maybe (App ())
  , ct_mcclose :: Maybe (App ())
  , ct_always :: Bool
  , ct_amt :: DLArg
  , ct_mtok :: Maybe DLArg }

checkTxn :: CheckTxn -> App ()
checkTxn (CheckTxn {..}) = when (ct_always || not (staticZero ct_amt) ) $ do
  let check1 f = do
        code "dig" [ "1" ]
        code "gtxns" [f]
        asserteq
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
  gvLoad GV_txnCounter
  op "dup"
  cl $ DLL_Int sb 1
  op "+"
  gvStore GV_txnCounter
  -- [ amt, id ]
  op "swap"
  -- [ id, amt ]
  check1 fAmount
  extra
  code "int" [vTypeEnum]
  check1 "TypeEnum"
  cl $ DLL_Int sb 0
  check1 "Fee"
  op "dup"
  checkTimeLimits ct_at
  code "global" ["ZeroAddress"]
  check1 "Lease"
  code "global" ["ZeroAddress"]
  check1 "RekeyTo"
  whenJust ct_mcclose $ \cclose -> do
    cclose
    cfrombs T_Address
    check1 fCloseTo
  whenJust ct_mcrecv $ \crecv -> do
    crecv
    cfrombs T_Address
    check1 fReceiver
  whenJust ct_mcsend $ \csend -> do
    csend
    cfrombs T_Address
    check1 "Sender"
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

cwhen :: App () -> App ()
cwhen cbody = do
  after_lab <- freshLabel
  code "bz" [after_lab]
  cbody
  label after_lab

ct :: CTail -> App ()
ct = \case
  CT_Com m k -> cm (ct k) m
  CT_If _ a tt ft -> do
    ca a
    false_lab <- freshLabel
    code "bz" [false_lab]
    ct tt
    label false_lab
    ct ft
  CT_Switch at dv csm ->
    doSwitch ct at dv csm
  CT_Jump {} -> xxx "continue"
  CT_From at which msvs -> do
    isHalt <- do
      case msvs of
        FI_Halt toks -> do
          forM_ toks close_asset
          close_escrow
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
    code "b" ["checkSize"]

cViewSave :: SrcLoc -> ViewSave -> App ()
cViewSave at (ViewSave vwhich vvs) = do
  let vconcat x y = DLA_Literal (DLL_Int at $ fromIntegral x) : (map snd y)
  let la = DLLA_Tuple $ vconcat vwhich vvs
  let lat = largeArgTypeOf la
  let sz = typeSizeOf lat
  viewSz <- readViewSize
  when (viewSz > 0) $ do
    comment $ "check view bs"
    cla la
    ctobs lat
    padding $ viewSz - sz
    op "concat"
    xxx "argView"
    asserteq

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
      app_global_put keyState
    HM_Check p -> do
      compute p
      app_global_get keyState
      asserteq

-- Reach Constants
reachAlgoBackendVersion :: Int
reachAlgoBackendVersion = 2

-- Template
template :: LT.Text -> App ()
template x = code "byte" [ "\"{{" <> x <> "}}\"" ]

cApplicationID :: App ()
cApplicationID = template "ApplicationID"

cContractAddr :: App ()
cContractAddr = template "ContractAddr"

cDeployer :: App ()
cDeployer = template "Deployer"

-- State:
keyState :: B.ByteString
keyState = "s"

keyView :: Word8 -> B.ByteString
keyView i = "v" <> B.singleton (BI.w2c i)

keyMap :: Word8 -> B.ByteString
keyMap i = "m" <> B.singleton (BI.w2c i)

data TxnId
  = TxnAppl
  deriving (Eq, Ord, Show, Enum, Bounded)

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
  | GV_timeLimits
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
  GV_timeLimits -> T_Tuple [ T_UInt, T_UInt ]

defn_checkSize :: App ()
defn_checkSize = do
  label "checkSize"
  gvLoad GV_txnCounter
  code "global" [ "GroupSize" ]
  asserteq
  code "b" [ "done" ]

defn_done :: App ()
defn_done = do
  label "done"
  cl $ DLL_Int sb 1
  op "return"

defn_fail :: App ()
defn_fail = do
  label "fail"
  cl $ DLL_Bool False
  assert

init_txnCounter :: App ()
init_txnCounter = do
  cl $ DLL_Int sb $ boundedCount TxnAppl
  gvStore GV_txnCounter

bindRound :: DLVar -> App a -> App a
bindRound dv = store_let dv True (code "global" ["Round"])

readViewSize :: App Integer
readViewSize = sViewSize <$> (eShared <$> ask)

ch :: LT.Text -> Int -> CHandler -> App ()
ch _ _ (C_Loop {}) = return ()
ch afterLab which (C_Handler at int last_timemv from prev svs msg timev body) = recordWhich which $ do
  let mkArgVar l = DLVar at Nothing (T_Tuple $ map varType l) <$> ((liftIO . incCounter) =<< ((sCounter . eShared) <$> ask))
  let argSize = 1 + (typeSizeOf $ T_Tuple $ map varType $ svs <> msg)
  recordStepInfo which $ Aeson.object $
    [ ("size", Aeson.Number $ fromIntegral argSize) ]
  let bindFromArg ai vs m = do
        code "txna" [ "ApplicationArgs", etexty ai ]
        op "dup"
        op "len"
        av <- mkArgVar vs
        cl $ DLL_Int sb $ typeSizeOf $ varType av
        asserteq
        let go = \case
              [] -> op "pop" >> m
              (dv, i) : more -> sallocLet dv cgen $ go more
                where cgen = ce $ DLE_TupleRef at (DLA_Var av) i
        store_let av True (op "dup") $
          go $ zip vs [0..]
  comment ("Handler " <> texty which)
  op "dup" -- We assume the method id is on the stack
  cl $ DLL_Int sb $ fromIntegral $ which
  op "=="
  code "bz" [ afterLab ]
  op "pop" -- Get rid of the method id since it's us
  let bindVars = id
        . (store_let from True (code "txn" [ "Sender" ]))
        . (bindRound timev)
        . (bindFromArg ArgSvs svs)
        . (bindFromArg ArgMsg msg)
  bindVars $ do
    cstate (HM_Check prev) $ map DLA_Var svs
    case last_timemv of
       Nothing -> padding $ typeSizeOf $ gvType GV_timeLimits
       Just last_timev -> do
         let go' = \case
              [] -> cl $ DLL_Int sb 0
              as -> do
                ca $ DLA_Var last_timev
                csum as
                op "+"
         let go x = go' x >> ctobs T_UInt
         let CBetween ifrom ito = int
         go ifrom
         go ito
         op "concat"
    gvStore GV_timeLimits
    cl $ DLL_Int at 0
    checkTimeLimits at
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

-- <MapStuff>
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

mkMapRecordTy :: DLType -> DLType
mkMapRecordTy x =
  --         present?, prev, next, account
  T_Tuple $ [T_Bool, x, x, T_Address]

getMapRecordTy :: App DLType
getMapRecordTy = (sMapRecordTy . eShared) <$> ask

mkMapArgTy :: DLType -> DLType
mkMapArgTy t = T_Array t $ fromIntegral $ length accountsL

getMapArgTy :: App DLType
getMapArgTy = (sMapArgTy . eShared) <$> ask

mapRecSlot_when :: Word8
mapRecSlot_when = 0

mapRecSlot_prev :: Word8
mapRecSlot_prev = mapRecSlot_when + 1

mapRecSlot_next :: Word8
mapRecSlot_next = mapRecSlot_prev + 1

mapRecSlot_acct :: Word8
mapRecSlot_acct = mapRecSlot_next + 1

cMapRecAlloc :: App ()
cMapRecAlloc = do
  -- [ Address ]
  found_lab <- freshLabel
  bad_lab <- freshLabel
  forM_ accountsL $ \ai -> do
    -- [ Address ]
    cMapWhen $ Just ai
    -- [ Address, When ]
    cwhen $ do
      -- [ Address ]
      op "dup"
      -- [ Address, Address ]
      cl $ DLL_Int sb $ fromIntegral ai
      -- [ Address, Address, Offset ]
      op "swap"
      -- [ Address, Offset, Address ]
      cMapAcct $ Just ai
      -- [ Address, Offset, Address, Address ]
      op "=="
      -- [ Address, Offset, Bool ]
      code "bnz" [found_lab]

      -- If it is not found, then the current account count must be greater
      -- than this number, otherwise the accounts were put in out of order.

      -- [ Address, Offset ]
      -- XXX readGV GV_Accounts
      -- [ Address, Offset, AccountCount ]
      op "<="
      -- [ Address, Valid ]
      op "assert"
  -- [ Address ]
  label bad_lab
  -- [ Address ]
  cl $ DLL_Bool False
  -- [ Address, #f ]
  op "assert"
  -- dead
  label found_lab
  -- [ Address, Offset ]
  op "swap"
  -- [ Offset, Address ]
  op "pop"
  -- [ Offset ]

  -- We need to record that we've seen this many accounts, so if this is
  -- bigger, then we need to record it
  op "dup"
  -- [ Offset, Offset ]
  -- XXX readGV GV_Accounts
  -- [ Offset, Offset, AccountCount ]
  op ">"
  -- [ Offset, Bigger ]
  dont_set_lab <- freshLabel
  code "bz" [dont_set_lab]
  -- [ Offset ]
  op "dup"
  -- [ Offset, Offset ]
  -- XXX setGV GV_Accounts
  -- [ Offset ]
  label dont_set_lab

cMapRecLoad :: Maybe Word8 -> App ()
cMapRecLoad mai = do
  -- (maybe [ Offset ] (const []) mai)
  -- XXX readArgCache argMaps
  -- (maybe [ Offset ] (const []) mai) <> [ MapArg ]
  t <- getMapRecordTy
  case mai of
    Just ai -> do
      -- [ MapArg ]
      cArrayRef sb t True (Left $ DLA_Literal $ DLL_Int sb $ fromIntegral ai)
    Nothing -> do
      -- [ Offset, MapArg ]
      op "swap"
      -- [ MapArg, Offset ]
      salloc_ $ \cstore cload -> do
        cstore
        cArrayRef sb t True (Right $ cload)

-- [ Record ]

cMapRecStore :: App ()
cMapRecStore = do
  salloc_ $ \store_val _XXX_load_val -> do
    salloc_ $ \store_idx _XXX_load_idx -> do
      -- [ Index, Value' ]
      store_val
      -- [ Index ]
      store_idx
      -- [ ]
      _XXX_rt <- getMapArgTy
      return ()
      -- XXX cArraySet sb (arrTypeLen rt) (Just $ readArgCache argMaps) (Right $ load_idx) load_val
      -- [ MapArg' ]
      -- XXX setGV $ GV_Arg argMaps

-- [ ]

cMap_slot :: Word8 -> Maybe Word8 -> App ()
cMap_slot slot mai = do
  -- (maybe [ Offset ] (const []) mai)
  cMapRecLoad mai
  -- [ Record ]
  t <- getMapRecordTy
  cTupleRef sb t $ fromIntegral slot

-- [ Field ]

cMapWhen :: Maybe Word8 -> App ()
cMapWhen = cMap_slot mapRecSlot_when

cMapPrev :: Maybe Word8 -> App ()
cMapPrev = cMap_slot mapRecSlot_prev

cMapNext :: Maybe Word8 -> App ()
cMapNext = cMap_slot mapRecSlot_next

cMapAcct :: Maybe Word8 -> App ()
cMapAcct = cMap_slot mapRecSlot_acct

cMapRef :: DLMVar -> App ()
cMapRef (DLMVar i) = do
  -- [ Offset ]
  cMapPrev Nothing
  -- [ Maps ]
  t <- getMapDataTy
  cTupleRef sb t $ fromIntegral i

-- [ MapValue ]

cMapSet :: DLMVar -> App ()
cMapSet (DLMVar i) = do
  -- [ Offset, NewValue ]
  op "swap"
  -- [ NewValue, Offset ]
  op "dup"
  -- [ NewValue, Offset, Offset ]
  cMapPrev Nothing
  -- [ NewValue, Offset, Maps ]
  dt <- getMapDataTy
  code "dig" ["2"]
  -- [ NewValue, Offset, Maps, NewValue ]
  cTupleSet sb dt $ fromIntegral i
  -- [ NewValue, Offset, Maps' ]
  op "swap"
  -- [ NewValue, Maps', Offset ]
  op "dup"
  -- [ NewValue, Maps', Offset, Offset ]
  cMapRecLoad Nothing
  -- [ NewValue, Maps', Offset, Record ]
  code "dig" ["2"]
  -- [ NewValue, Maps', Offset, Record, Maps' ]
  rt <- getMapRecordTy
  cTupleSet sb rt $ fromIntegral mapRecSlot_prev
  -- [ NewValue, Maps', Offset, Record' ]
  cMapRecStore
  -- [ NewValue, Maps' ]
  op "pop"
  -- [ NewValue ]
  op "pop"

-- [ ]

-- </MapStuff>

recordStepInfo :: Int -> Aeson.Value -> App ()
recordStepInfo w v = do
  (liftIO . flip modifyIORef (M.insert w v)) =<< (eStepInfoR <$> ask)

type Disp = String -> T.Text -> IO ()

compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at plo dli _ _ cpp = pl
  let CPProg at vi (CHandlers hm) = cpp
  let sMaps = dli_maps dli
  resr <- newIORef mempty
  sFailuresR <- newIORef mempty
  let sViewSize = computeViewSize vi
  let sMapDataTy = mapDataTy sMaps
  let sMapDataSize = typeSizeOf sMapDataTy
  let sMapRecordTy = mkMapRecordTy sMapDataTy
  let sMapRecordSize = typeSizeOf sMapRecordTy
  let sMapArgTy = mkMapArgTy sMapRecordTy
  let sMapArgSize = typeSizeOf sMapArgTy
  let PLOpts {..} = plo
  let sCounter = plo_counter
  let eShared = Shared {..}
  eStepInfoR <- newIORef mempty
  let run :: String -> App () -> IO TEALs
      run lab m = do
        eLabel <- newCounter 0
        eOutputR <- newIORef mempty
        let eHP = fromIntegral $ fromEnum (maxBound :: GlobalVar)
        let eSP = 255
        let eVars = mempty
        let eLets = mempty
        let eLetSmalls = mempty
        let eWhich = lab
        flip runReaderT (Env {..}) m
        readIORef eOutputR
  let addProg lab m = do
        t <- render <$> run lab m
        modifyIORef resr $ M.insert (T.pack lab) $ Aeson.String t
        disp lab t
  let divup :: Integer -> Integer -> Integer
      divup x y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y)
  let recordSize prefix size = do
        modifyIORef resr $
          M.insert (prefix <> "Size") $
            Aeson.Number $ fromIntegral size
  let recordSizeAndKeys :: T.Text -> Integer -> IO [Word8]
      recordSizeAndKeys prefix size = do
        recordSize prefix size
        let keys = size `divup` algoMaxAppBytesValueLen
        modifyIORef resr $
          M.insert (prefix <> "Keys") $
            Aeson.Number $ fromIntegral keys
        return $ take (fromIntegral keys) [0 ..]
  viewKeysl <- recordSizeAndKeys "view" sViewSize
  mapKeysl <- recordSizeAndKeys "mapData" sMapDataSize
  recordSize "mapRecord" sMapRecordSize
  recordSize "mapArg" sMapArgSize
  let cStateSlice :: Integer -> Word8 -> App ()
      cStateSlice size iw = do
        let i = fromIntegral iw
        let k = algoMaxAppBytesValueLen
        csubstring at (k * i) (min size $ k * (i + 1))
  addProg "appApproval" $ do
    checkRekeyTo
    checkLease
    unless (null mapKeysl) $ do
      -- XXX Allow an OptIn if we are not going to halt
      code "txn" ["OnCompletion"]
      code "int" ["OptIn"]
      op "=="
      code "bz" ["normal"]
      code "global" ["GroupSize"]
      cl $ DLL_Int sb $ 1
      asserteq
      salloc_ $ \store_whole load_whole -> do
        padding sMapDataSize
        store_whole
        forM_ mapKeysl $ \mi -> do
          load_whole
          cStateSlice sMapDataSize mi
          app_local_put 0 (keyMap mi)
      code "b" ["done"]
      -- The NON-OptIn case:
      label "normal"
    init_txnCounter
    code "txn" ["NumAppArgs"]
    cl $ DLL_Int sb $ argCount
    asserteq
    -- NOTE Fee - We don't check that the Fee is correct because we're assuming
    -- that FeePooling is in effect, we check that all the other fees are zero,
    -- and we assume that "extra" fees aren't charged, so there's no reason to
    -- ensure it is low enough.
    code "txna" [ "ApplicationArgs", etexty ArgMethod ]
    forM_ (M.toAscList hm) $ \(hi, hh) -> do
      afterLab <- freshLabel
      ch afterLab hi hh
      label afterLab
    cfail
    -- XXX compile the loops
    defn_checkSize
    defn_done
    defn_fail
  -- The initial version of the Approval program handles the first call and the
  -- update so that the escrow and approval can refer to each other
  addProg "appApproval0" $ do
    checkRekeyTo
    checkLease
    code "txn" ["Sender"]
    cDeployer
    asserteq
    code "txn" ["ApplicationID"]
    code "bz" ["init"]
    init_txnCounter
    checkTxn $ CheckTxn
      { ct_at = at
      , ct_mcrecv = Nothing -- XXX calculate the escrow ctc
      , ct_mcsend = Nothing -- Flexible, not necc appl sender
      , ct_mcclose = Nothing
      , ct_always = False
      , ct_mtok = Nothing
      , ct_amt = DLA_Literal minimumBalance_l }
    code "txn" ["OnCompletion"]
    code "int" ["UpdateApplication"]
    asserteq
    --- NOTE firstMsg => do handler 1
    let (bind_csvs, csvs) =
          case dli_ctimem dli of
            Nothing -> (id, mempty)
            Just v -> (bindRound v, [DLA_Var v])
    bind_csvs $ cstate (HM_Set 0) csvs
    forM_ viewKeysl $ \i -> do
      cl $ DLL_Bytes $ ""
      app_global_put (keyView i)
    code "b" ["checkSize"]
    label "init"
    code "txn" ["OnCompletion"]
    code "int" ["NoOp"]
    asserteq
    code "b" ["checkSize"]
    defn_checkSize
    defn_done
  -- Clear state is only allowed when the program is over.
  -- NOTE: We could allow this when the local value is 100% None
  addProg "appClear" $ do
    checkRekeyTo
    checkLease
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ 1
    asserteq
    app_global_get keyState
    code "global" ["ZeroAddress"]
    asserteq
    code "b" ["done"]
    defn_done
  -- The escrow account defers to the application
  addProg "escrow" $ do
    code "gtxn" [etexty TxnAppl, "TypeEnum"]
    code "int" ["appl"]
    asserteq
    code "gtxn" [etexty TxnAppl, "ApplicationID"]
    cApplicationID
    cfrombs T_UInt
    asserteq
    code "b" ["done"]
    defn_done
  eStepInfo <- readIORef eStepInfoR
  modifyIORef resr $
    M.insert "stepargs" $ aarray $
      map snd $ M.toAscList eStepInfo
  sFailures <- readIORef sFailuresR
  modifyIORef resr $
    M.insert "unsupported" $
      aarray $
        S.toList $ S.map (Aeson.String . LT.toStrict) sFailures
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
