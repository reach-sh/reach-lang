module Reach.Connector.ALGO (connect_algo) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ALGO.hs.dead

import Control.Monad.Reader
import Data.ByteString.Base64 (encodeBase64')
import qualified Data.ByteString.Char8 as B
import qualified Data.DList as DL
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Data.Word
import GHC.Stack (HasCallStack)
import Reach.AST
import Reach.Connector
import Reach.Type
import Reach.Util
import Safe (atMay)

-- General tools that could be elsewhere

sb :: SrcLoc
sb = srcloc_builtin

typeArrayType :: HasCallStack => DLArg -> SLType
typeArrayType a =
  case argTypeOf a of
    T_Array t _ -> t
    _ -> impossible $ "should be array"

typeTupleTypes :: HasCallStack => DLArg -> [SLType]
typeTupleTypes a =
  case argTypeOf a of
    T_Tuple ts -> ts
    _ -> impossible $ "should be tuple"

typeObjectTypes :: HasCallStack => DLArg -> [(SLVar, SLType)]
typeObjectTypes a =
  case argTypeOf a of
    T_Object m -> M.toAscList m
    _ -> impossible $ "should be obj"

-- Algo specific stuff

typeSizeOf :: SLType -> Integer
typeSizeOf = \case
  T_Null -> word
  T_Bool -> word
  T_UInt -> word
  T_Bytes -> word --- XXX This is wrong
  T_Digest -> 32
  T_Address -> 32
  T_Fun {} -> impossible $ "T_Fun"
  T_Array t sz -> sz * typeSizeOf t
  T_Tuple ts -> sum $ map typeSizeOf ts
  T_Object m -> sum $ map typeSizeOf $ M.elems m
  T_Data {} -> word --- XXX This is wrong
  T_Forall {} -> impossible $ "T_Forall"
  T_Var {} -> impossible $ "T_Var"
  T_Type {} -> impossible $ "T_Type"
  where word = 8

encodeBase64 :: B.ByteString -> LT.Text
encodeBase64 bs = LT.pack $ B.unpack $ encodeBase64' bs

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

template :: LT.Text -> LT.Text
template x = "\"{{" <> x <> "}}\""

type ScratchSlot = Word8

type TxnIdx = Word8 --- FIXME actually only 16 IIRC

type TEAL = LT.Text

code_ :: LT.Text -> [LT.Text] -> TEAL
code_ fun args = LT.unwords $ fun : args

label_ :: LT.Text -> TEAL
label_ lab = lab <> ":"

comment_ :: LT.Text -> TEAL
comment_ t = "// " <> t

type TEALs = DL.DList TEAL

optimize :: [LT.Text] -> [LT.Text]
optimize = \case
  [] -> []
  --- FIXME generalize
  "b alone" : "alone:" : l -> "alone:" : l
  "btoi" : "itob" : l -> optimize l
  "itob" : "btoi" : l -> optimize l
  x : l -> x : optimize l

render :: TEALs -> T.Text
render ts = tt
  where
    tt = LT.toStrict lt
    lt = LT.unlines lts
    lts = "#pragma version 2" : (optimize $ DL.toList ts)

data Shared = Shared
  { sHandlers :: M.Map Int CHandler
  , sFailedR :: IORef Bool
  }

type Lets = M.Map DLVar (App ())

data Env = Env
  { eShared :: Shared
  , eWhich :: Int
  , eLabelR :: IORef Int
  , eOutputR :: IORef TEALs
  , eTxnsR :: IORef TxnIdx
  , eHP :: ScratchSlot
  , eSP :: ScratchSlot
  , eVars :: M.Map DLVar ScratchSlot
  , eLets :: Lets
  }

-- I'd rather not embed in IO, but when I used ST in UnrollLoops, it was
-- really annoying to have the world parameter
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

or_fail :: App ()
or_fail = code "bz" ["revert"]

eq_or_fail :: App ()
eq_or_fail = op "==" >> or_fail

op :: LT.Text -> App ()
op = flip code []

nop :: App ()
nop = return ()

app_global_get :: B.ByteString -> App ()
app_global_get k = do
  cl $ DLL_Bytes $ k
  op "app_global_get"

app_global_put :: B.ByteString -> App () -> App ()
app_global_put k mkv = do
  cl $ DLL_Bytes $ k
  mkv
  op "app_global_put"

check_rekeyto :: App ()
check_rekeyto = do
  code "txn" ["RekeyTo"]
  code "global" ["ZeroAddress"]
  eq_or_fail

bad :: LT.Text -> App ()
bad lab = do
  Env {..} <- ask
  let Shared {..} = eShared
  liftIO $ writeIORef sFailedR True
  output $ comment_ $ lab

xxx :: LT.Text -> App ()
xxx lab = bad $ "XXX " <> lab

freshLabel :: App LT.Text
freshLabel = do
  Env {..} <- ask
  i <- liftIO $ readIORef eLabelR
  liftIO $ modifyIORef eLabelR (1 +)
  return $ "l" <> LT.pack (show i)

store_let :: DLVar -> App () -> App a -> App a
store_let dv cgen m = do
  Env {..} <- ask
  local (\e -> e {eLets = M.insert dv cgen eLets}) $
    m

lookup_let :: DLVar -> App ()
lookup_let dv = do
  Env {..} <- ask
  case M.lookup dv eLets of
    Just m -> m
    Nothing -> impossible $ "lookup_let"

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
    bad "too much memory"
  local (\e -> e {eSP = eSP'}) $
    fm eSP

talloc :: App TxnIdx
talloc = do
  Env {..} <- ask
  liftIO $ modifyIORef eTxnsR (1 +)
  txni <- liftIO $ readIORef eTxnsR
  --- FIXME check if > bound
  return txni

how_many_txns :: App TxnIdx
how_many_txns = do
  Env {..} <- ask
  liftIO $ readIORef eTxnsR

ctobs :: SLType -> App ()
ctobs = \case
  T_UInt -> op "itob"
  T_Bool -> op "itob"
  T_Null -> op "itob"
  T_Bytes -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Fun {} -> impossible "fun"
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"
  T_Type {} -> impossible "type"

cfrombs :: SLType -> App ()
cfrombs = \case
  T_UInt -> op "btoi"
  T_Bool -> op "btoi"
  T_Null -> op "btoi"
  T_Bytes -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Fun {} -> impossible "fun"
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"
  T_Type {} -> impossible "type"

tint :: SrcLoc -> Integer -> LT.Text
tint at i = texty $ checkIntLiteralC at connect_algo i

cl :: DLLiteral -> App ()
cl = \case
  DLL_Null -> cl $ DLL_Int sb 0
  DLL_Bool b -> cl $ DLL_Int sb $ if b then 1 else 0
  DLL_Int at i -> code "int" [tint at i]
  DLL_Bytes bs -> code "byte" ["base64(" <> encodeBase64 bs <> ")"]

ca :: DLArg -> App ()
ca = \case
  DLA_Var v -> lookup_let v
  DLA_Constant c -> cl $ conCons connect_algo c
  DLA_Literal c -> cl c
  DLA_Array t as -> cconcatbs $ map (\a -> (t, ca a)) as
  DLA_Tuple as -> cconcatbs $ map (\a -> (argTypeOf a, ca a)) as
  DLA_Obj {} -> xxx "obj"
  DLA_Data {} -> xxx "data"
  DLA_Interact {} -> impossible "consensus interact"

cprim :: PrimOp -> [DLArg] -> App ()
cprim = \case
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
  LSH -> const $ xxx "LSH"
  RSH -> const $ xxx "RSH"
  BAND -> call "&"
  BIOR -> call "|"
  BXOR -> call "^"
  BYTES_EQ -> call "=="
  DIGEST_EQ -> call "=="
  ADDRESS_EQ -> call "=="
  IF_THEN_ELSE -> \case
    [ be, te, fe ] -> do
      labf <- freshLabel
      labj <- freshLabel
      ca be
      code "bz" [ labf ]
      ca te
      code "b" [ labj ]
      label labf
      ca fe
      label labj
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

cconcatbs :: [(SLType, App ())] -> App ()
cconcatbs l = do
  -- XXX We need to predict the length and ensure <4096 bytes
  mapM_ (uncurry go) $ zip (no_concat : repeat yes_concat) l
  where
    go may_concat (t, m) = m >> ctobs t >> may_concat
    no_concat = nop
    yes_concat = op "concat"

cdigest :: [(SLType, App ())] -> App ()
cdigest l = cconcatbs l >> op "keccak256"

csubstring :: SrcLoc -> Integer -> Integer -> App ()
csubstring at b c =
  case b < 256 && c < 256 of
    True -> do
      code "substring" [ tint at b, tint at c ]
    False -> do
      cl $ DLL_Int sb b
      cl $ DLL_Int sb c
      op "substring3"

computeSubstring :: [SLType] -> Integer -> (SLType, Integer, Integer)
computeSubstring ts idx = (t, start, end)
  where szs = map typeSizeOf ts
        starts = scanl (+) 0 szs
        ends = zipWith (+) szs starts
        idx' = fromIntegral idx
        tse = zip3 ts starts ends
        (t, start, end) =
          case atMay tse idx' of
            Nothing -> impossible "bad idx"
            Just x -> x

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_Impossible at msg -> expect_throw at msg
  DLE_PrimOp _ p args -> cprim p args
  DLE_ArrayRef at aa ia -> do
    ca aa
    let t = typeArrayType aa
    let tsz = typeSizeOf t
    case ia of
      DLA_Literal (DLL_Int _ ii) -> do
        let start = ii * tsz
        let end = start + tsz
        csubstring at start end
      _ -> do
        cl $ DLL_Int sb tsz
        ca ia
        op "*"
        op "dup"
        cl $ DLL_Int sb tsz
        op "+"
        op "substring3"
    cfrombs t
  DLE_ArraySet {} -> xxx "array set"
  DLE_ArrayConcat {} -> xxx "array concat"
  DLE_ArrayZip {} -> xxx "array zip"
  DLE_TupleRef at ta idx -> do
    ca ta
    let ts = typeTupleTypes ta
    let (t, start, end) = computeSubstring ts idx
    csubstring at start end
    cfrombs t
  DLE_ObjectRef at oa f -> do
    ca oa
    let fts = typeObjectTypes oa
    let fidx = fromIntegral $ fromMaybe (impossible "bad field") $ findIndex ((== f) . fst) fts
    let (t, start, end) = computeSubstring (map snd fts) fidx
    csubstring at start end
    cfrombs t
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> cdigest $ map go args
    where
      go a = (argTypeOf a, ca a)
  DLE_Transfer _ who amt -> do
    txni <- talloc
    code "gtxn" [texty txni, "TypeEnum"]
    code "int" ["pay"]
    eq_or_fail
    code "gtxn" [texty txni, "Receiver"]
    ca who
    eq_or_fail
    code "gtxn" [texty txni, "Amount"]
    ca amt
    eq_or_fail
    code "gtxn" [texty txni, "Sender"]
    code "byte" [tContractAddr]
    cfrombs T_Address
    eq_or_fail
  DLE_Claim _ _fs t a _mmsg ->
    --- FIXME add fs and mmsg as comments
    case t of
      CT_Assert -> impossible "assert"
      CT_Assume -> check
      CT_Require -> check
      CT_Possible -> impossible "possible"
      CT_Unknowable {} -> impossible "unknowable"
    where
      check = ca a >> or_fail
  DLE_Wait {} -> nop
  DLE_PartSet _ _ a -> ca a

cm :: (a -> App ()) -> PLCommon a -> App ()
cm ck = \case
  PL_Return {} -> xxx "return"
  PL_Let _ PL_Once dv de k ->
    store_let dv (ce de) $ ck k
  PL_Let _ PL_Many dv de k ->
    salloc $ \loc -> do
      let loct = texty loc
      ce de
      code "store" [loct]
      store_let dv (code "load" [loct]) $ ck k
  PL_ArrayMap {} -> xxx "map"
  PL_ArrayReduce {} -> xxx "reduce"
  PL_Eff _ de k -> ce de >> ck k
  PL_Var _ dv k ->
    salloc $ \loc -> do
      store_var dv loc $
        store_let dv (code "load" [texty loc]) $
          ck k
  PL_Set _ dv da k -> do
    loc <- lookup_var dv
    ca da
    code "store" [texty loc]
    ck k
  PL_LocalIf _ _a _tp _fp k -> do
    xxx "local if"
    ck k
  PL_LocalSwitch _ _dv _csm k -> do
    xxx "local switch"
    ck k

cp :: PLTail -> App ()
cp (PLTail m) = cm cp m

ct :: CTail -> App ()
ct = \case
  CT_Com m -> cm ct m
  CT_Seqn _ p t -> cp p >> ct t
  CT_If _ a tt ft -> do
    ca a
    false_lab <- freshLabel
    code "bz" [false_lab]
    ct tt
    label false_lab
    ct ft
  CT_Switch {} -> xxx "switch"
  CT_Jump {} -> xxx "jump"
  CT_From _ msvs -> do
    check_nextSt
    halt_should_be isHalt
    where
      (isHalt, check_nextSt) =
        case msvs of
          --- XXX fix this so it makes sure it is zero bytes
          Nothing -> (True, return ())
          Just svs -> (False, ck)
            where
              ck = do
                cstate HM_Set svs
                code "arg" [texty argNextSt]
                eq_or_fail

data HashMode
  = HM_Set
  | HM_Check Int
  deriving (Eq, Show)

cstate :: HashMode -> [DLVar] -> App ()
cstate hm svs = do
  which <-
    case hm of
      HM_Set -> do
        Env {..} <- ask
        return eWhich
      HM_Check prev ->
        return prev
  let go a = (argTypeOf a, ca a)
  let whicha w = DLA_Literal $ DLL_Int sb $ fromIntegral w
  cdigest $ map go $ whicha which : map DLA_Var svs

halt_should_be :: Bool -> App ()
halt_should_be b = do
  code "arg" [texty argHalts]
  cfrombs T_Bool
  cl $ DLL_Bool b
  eq_or_fail
  code "b" ["done"]

-- Intialization:
--
-- 0. Alice creates the application; gets Id
-- 1. Alice creates the contract account; embeds Id; gets Me
-- 2. Alice creates the handler contracts; embeds Id & Me; gets H_i
-- 3. Alice updates the application; embedding Me & H_i

-- Template
tApplicationID :: LT.Text
tApplicationID = template "ApplicationID"

tContractAddr :: LT.Text
tContractAddr = template "ContractAddr"

tDeployer :: LT.Text
tDeployer = template "Deployer"

-- State:
keyHalts :: B.ByteString
keyHalts = "h"

keyState :: B.ByteString
keyState = "s"

keyLast :: B.ByteString
keyLast = "l"

-- FIXME Dan says to use some cool deriving Enum stuff for Txn and Arg

-- Txns:
-- 0   : Application call
-- 1   : Transfer fee to the handler account
-- 2   : Zero from handler account
-- 3   : Transfer to contract account
-- 4.. : Transfers from contract to user
txnAppl :: Word8
txnAppl = 0

txnToHandler :: Word8
txnToHandler = txnAppl + 1

txnFromHandler :: Word8
txnFromHandler = txnToHandler + 1

txnToContract :: Word8
txnToContract = txnFromHandler + 1

txnFromContract0 :: Word8
txnFromContract0 = txnToContract + 1

-- Args:
-- 0   : Previous state
-- 1   : Next state
-- 2   : Handler halts
-- 3   : Fee amount
-- 4   : Last round
-- 5.. : Handler arguments
argPrevSt :: Word8
argPrevSt = 0

argNextSt :: Word8
argNextSt = argPrevSt + 1

argHalts :: Word8
argHalts = argNextSt + 1

argFeeAmount :: Word8
argFeeAmount = argHalts + 1

argLast :: Word8
argLast = argFeeAmount + 1

argFirstUser :: Word8
argFirstUser = argLast + 1

lookup_sender :: App ()
lookup_sender = code "gtxn" [texty txnToContract, "Sender"]

lookup_last :: App ()
lookup_last = code "arg" [texty argLast] >> cfrombs T_UInt

lookup_fee_amount :: App ()
lookup_fee_amount = code "arg" [texty argFeeAmount] >> cfrombs T_UInt

std_footer :: App ()
std_footer = do
  label "revert"
  cl $ DLL_Int sb 0
  op "return"
  label "done"
  cl $ DLL_Int sb 1
  op "return"

runApp :: Shared -> Int -> Lets -> App () -> IO TEALs
runApp eShared eWhich eLets m = do
  eLabelR <- newIORef 0
  eOutputR <- newIORef mempty
  let eHP = 0
  let eSP = 255
  eTxnsR <- newIORef $ txnFromContract0 - 1
  let eVars = mempty
  flip runReaderT (Env {..}) m
  readIORef eOutputR

ch :: Shared -> Int -> CHandler -> IO (Maybe TEALs)
ch _ _ (C_Loop {}) = return $ Nothing
ch eShared eWhich (C_Handler _ int fs prev svs msg amtv body) = fmap Just $ do
  let mkarg dv@(DLVar _ _ t _) (i :: Int) = (dv, code "arg" [texty i] >> cfrombs t)
  let args = svs <> msg
  let argFirstUser' = fromIntegral argFirstUser
  let eLets0 = M.fromList $ zipWith mkarg args [argFirstUser' ..]
  let argCount = argFirstUser' + length args
  let eLets1 =
        case fs of
          FS_Join dv ->
            M.insert dv lookup_sender eLets0
          FS_Again {} ->
            eLets0
  let lookup_txn_value = do
        code "gtxn" [texty txnToContract, "Amount"]
        lookup_fee_amount
        op "-"
  let eLets =
        M.insert amtv lookup_txn_value eLets1
  runApp eShared eWhich eLets $ do
    comment ("Handler " <> texty eWhich)
    comment "Check txnAppl"
    code "gtxn" [texty txnAppl, "TypeEnum"]
    code "int" ["appl"]
    eq_or_fail
    code "gtxn" [texty txnAppl, "ApplicationID"]
    --- XXX Make this int
    code "byte" [tApplicationID]
    cfrombs T_UInt
    eq_or_fail

    comment "Check txnToHandler"
    code "gtxn" [texty txnToHandler, "TypeEnum"]
    code "int" ["pay"]
    eq_or_fail
    code "gtxn" [texty txnToHandler, "Receiver"]
    code "txn" ["Sender"]
    eq_or_fail
    code "gtxn" [texty txnToHandler, "Amount"]
    code "gtxn" [texty txnFromHandler, "Fee"]
    eq_or_fail

    comment "Check txnToContract"
    code "gtxn" [texty txnToContract, "TypeEnum"]
    code "int" ["pay"]
    eq_or_fail
    code "gtxn" [texty txnToContract, "Receiver"]
    code "byte" [tContractAddr]
    eq_or_fail

    comment "Check txnFromHandler (us)"
    code "txn" ["GroupIndex"]
    cl $ DLL_Int sb $ fromIntegral $ txnFromHandler
    eq_or_fail
    code "txn" ["TypeEnum"]
    code "int" ["pay"]
    eq_or_fail
    code "txn" ["Amount"]
    cl $ DLL_Int sb $ 0
    eq_or_fail
    code "txn" ["Receiver"]
    code "gtxn" [texty txnToHandler, "Sender"]
    eq_or_fail
    code "txn" ["NumArgs"]
    cl $ DLL_Int sb $ fromIntegral $ argCount
    eq_or_fail
    case fs of
      FS_Join {} -> return ()
      FS_Again dv -> do
        lookup_sender
        ca $ DLA_Var dv
        eq_or_fail
    cstate (HM_Check prev) svs
    code "arg" [texty argPrevSt]
    eq_or_fail

    --- XXX close remainder to is deployer if halts, zero otherwise

    comment "Run body"
    ct body

    txns <- how_many_txns
    comment "Check GroupSize"
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ fromIntegral $ 1 + txns
    eq_or_fail

    lookup_fee_amount
    csum_ $ map (\i -> code "gtxn" [texty i, "Fee"]) [txnFromContract0 .. txns]
    eq_or_fail

    comment "Check time limits"
    let check_time f = \case
          [] -> nop
          as -> do
            lookup_last
            csum as
            op "+"
            let go i = do
                  op "dup"
                  code "gtxn" [texty i, f]
                  eq_or_fail
            forM_ [0 .. txns] go
            op "pop"
    (do
       let CBetween from to = int
       check_time "FirstValid" from
       check_time "LastValid" to)

    std_footer

type Disp = String -> T.Text -> IO ()

compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at (PLOpts {..}) _ cpp = pl
  let CPProg _at (CHandlers hm) = cpp
  resr <- newIORef mempty
  let sHandlers = hm
  sFailedR <- newIORef False
  when (plo_deployMode == DM_firstMsg) $ do
    writeIORef sFailedR True
  let shared = Shared {..}
  let addProg lab t = do
        modifyIORef resr (M.insert lab $ CI_Text t)
        disp lab t
  hm_res <- forM (M.toAscList hm) $ \(hi, hh) -> do
    mht <- ch shared hi hh
    case mht of
      Nothing -> return (CI_Null, return ())
      Just ht -> do
        let lab = "m" <> show hi
        return
          ( CI_Text (render ht)
          , do
              code "gtxn" [texty txnFromHandler, "Sender"]
              code "byte" [template $ LT.pack lab]
              op "=="
              op "||"
          )
  let (steps_, hchecks) = unzip hm_res
  let steps = CI_Null : steps_
  modifyIORef resr $ M.insert "steps" $ CI_Array steps
  let howManySteps =
        length $
          filter
            (\case
               CI_Text _ -> True
               _ -> False)
            steps
  let simple m = runApp shared 0 mempty $ m >> std_footer
  app0m <- simple $ do
    comment "Check that we're an App"
    code "txn" ["TypeEnum"]
    code "int" ["appl"]
    eq_or_fail
    check_rekeyto
    code "txn" ["Sender"]
    code "byte" [tDeployer]
    eq_or_fail
    code "txn" ["ApplicationID"]
    code "bz" ["init"]
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ fromIntegral $ 2 + howManySteps
    eq_or_fail
    --- XXX can we constrain the other txns to transfer the correct amount?
    code "txn" ["OnCompletion"]
    code "int" ["UpdateApplication"]
    eq_or_fail
    app_global_put keyState $ do
      cstate HM_Set []
    app_global_put keyLast $ do
      code "global" ["Round"]
    app_global_put keyHalts $ do
      cl $ DLL_Bool $ False
    code "b" ["done"]
    label "init"
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ 1
    eq_or_fail
    code "txn" ["OnCompletion"]
    code "int" ["NoOp"]
    eq_or_fail
    code "b" ["done"]
  appm <- simple $ do
    comment "Check that we're an App"
    code "txn" ["TypeEnum"]
    code "int" ["appl"]
    eq_or_fail
    check_rekeyto
    comment "Check that everyone's here"
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ fromIntegral $ txnFromContract0
    op ">="
    or_fail
    comment "Check txnAppl (us)"
    code "txn" ["GroupIndex"]
    cl $ DLL_Int sb $ fromIntegral $ txnAppl
    eq_or_fail
    comment "Check txnFromHandler"
    cl $ DLL_Bool $ False
    forM_ hchecks id
    or_fail
    app_global_get keyState
    code "gtxna" [texty txnFromHandler, "Args", texty argPrevSt]
    cfrombs T_Bytes
    eq_or_fail
    app_global_get keyLast
    code "gtxna" [texty txnFromHandler, "Args", texty argLast]
    cfrombs T_UInt
    eq_or_fail
    comment "Don't check anyone else, because Handler does"
    comment "Update state"
    app_global_put keyState $ do
      code "gtxna" [texty txnFromHandler, "Args", texty argNextSt]
      cfrombs T_Bytes
    app_global_put keyLast $ do
      code "global" ["Round"]
    --- XXX we don't actually need halts
    app_global_put keyHalts $ do
      code "gtxna" [texty txnFromHandler, "Args", texty argHalts]
      cfrombs T_Bool
    app_global_get keyHalts
    code "bnz" ["halted"]
    code "txn" ["OnCompletion"]
    code "int" ["NoOp"]
    eq_or_fail
    code "b" ["done"]
    label "halted"
    code "txn" ["OnCompletion"]
    code "int" ["DeleteApplication"]
    eq_or_fail
    code "b" ["done"]
  clearm <- simple $ do
    comment "We're alone"
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ 1
    eq_or_fail
    comment "We're halted"
    app_global_get keyHalts
    cl $ DLL_Bool $ True
    eq_or_fail
    code "b" ["done"]
  -- XXX ctc needs to allow deployer to get back minimum balance
  ctcm <- simple $ do
    comment "Check size"
    code "global" ["GroupSize"]
    cl $ DLL_Int sb $ fromIntegral $ txnFromContract0
    op ">="
    or_fail
    comment "Check txnAppl"
    code "gtxn" [texty txnAppl, "TypeEnum"]
    code "int" ["appl"]
    eq_or_fail
    code "gtxn" [texty txnAppl, "ApplicationID"]
    code "byte" [tApplicationID]
    cfrombs T_UInt
    eq_or_fail
    comment "Don't check anything else, because app does"
    comment "Check us"
    code "txn" ["TypeEnum"]
    code "int" ["pay"]
    eq_or_fail
    check_rekeyto
    code "txn" ["CloseRemainderTo"]
    code "global" ["ZeroAddress"]
    eq_or_fail
    code "txn" ["GroupIndex"]
    cl $ DLL_Int sb $ fromIntegral $ txnFromContract0
    op ">="
    or_fail
    code "b" ["done"]
  addProg "appApproval0" $ render app0m
  addProg "appApproval" $ render appm
  addProg "appClear" $ render clearm
  addProg "ctc" $ render ctcm
  res0 <- readIORef resr
  sFailed <- readIORef sFailedR
  let res1 = M.insert "unsupported" (CI_Bool sFailed) res0
  return $ CI_Obj res1

connect_algo :: Connector
connect_algo = Connector {..}
  where
    conName = "ALGO"
    conCons DLC_UInt_max = DLL_Int sb $ 2^(64::Integer) - 1
    conGen moutn pl = do
      let disp which c =
            case moutn of
              Nothing -> return ()
              Just outn ->
                TIO.writeFile (outn $ T.pack $ which <> ".teal") c
      res <- compile_algo disp pl
      return $ res
