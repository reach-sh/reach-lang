module Reach.Connector.ALGO (connect_algo) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Data.ByteString.Base64 (encodeBase64')
import qualified Data.ByteString.Char8 as B
import Control.Monad.Reader
import Data.IORef
import Data.Word
import qualified Data.DList as DL
import Reach.AST
import Reach.Connector
import Reach.Util
import Reach.Type

encodeBase64 :: B.ByteString -> LT.Text
encodeBase64 bs = LT.pack $ B.unpack $ encodeBase64' bs

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

type ScratchSlot = Word8
type TxnIdx = Word8 --- XXX actually only 16 IIRC

type TEAL = LT.Text

code_ :: LT.Text -> [LT.Text] -> TEAL
code_ fun args = LT.unwords $ fun : args
label_ :: LT.Text -> TEAL
label_ lab = lab <> ":"
comment_ :: LT.Text -> TEAL
comment_ t = "// " <> t

type TEALs = DL.DList TEAL

render :: TEALs -> T.Text
render ts = tt
  where tt = LT.toStrict lt
        lt = LT.unlines lts
        lts = "#pragma version 2" : DL.toList ts

data Shared = Shared
  { sHandlers :: M.Map Int CHandler
  , sFailedR :: IORef Bool }
data Env = Env
  { eShared :: Shared
  , eWhich :: Int
  , eLabelR :: IORef Int
  , eOutputR :: IORef TEALs
  , eTxnsR :: IORef TxnIdx
  , eHP :: ScratchSlot
  , eSP :: ScratchSlot
  , eVars :: M.Map DLVar ScratchSlot
  , eLets :: M.Map DLVar (App ()) }

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
op :: LT.Text -> App ()
op = flip code []
nop :: App ()
nop = return ()

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
  local (\e -> e { eLets = M.insert dv cgen eLets }) $
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
  local (\e -> e { eVars = M.insert dv ss eVars }) $
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
  when ( eSP' == eHP ) $ do
    bad "too much memory"
  local (\e -> e { eSP = eSP' }) $
    fm eSP

talloc :: App TxnIdx
talloc = do
  Env {..} <- ask
  liftIO $ modifyIORef eTxnsR (1+)
  txni <- liftIO $ readIORef eTxnsR
  --- XXX check if > bound
  return txni

ctobs :: SLType -> App ()
ctobs = \case
  T_UInt256 -> op "itob"
  T_Bool -> op "itob"
  T_Null -> op "itob"
  T_Bytes -> nop
  T_Address -> nop
  T_Fun {} -> impossible "fun"
  T_Array {} -> xxx "array to bs"
  T_Tuple {} -> xxx "tuple to bs"
  T_Object {} -> xxx "object to bs"
  T_Data {} -> xxx "data to bs"
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"
  T_Type {} -> impossible "type"

cc :: DLConstant -> App ()
cc = \case
  DLC_Null -> cc $ DLC_Int 0
  DLC_Bool b -> cc $ DLC_Int $ if b then 1 else 0
  DLC_Int i -> code "int" [ texty i ]
  DLC_Bytes bs -> code "byte" [ "base64(" <> encodeBase64 bs <> ")" ]

ca :: DLArg -> App ()
ca = \case
  DLA_Var v -> lookup_let v
  DLA_Con c -> cc c
  DLA_Array {} -> xxx "array"
  DLA_Tuple {} -> xxx "tuple"
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
  IF_THEN_ELSE -> const $ xxx "ITE"
  BALANCE -> const $ xxx "BALANCE"
  TXN_VALUE -> const $ code "gtxn" [ texty txnToContract, "Amount" ]
  where
    call o = \args -> do
      forM_ args ca
      op o

csum :: [DLArg] -> App ()
csum = \case
  [] -> cc $ DLC_Int 0
  [v] -> ca v
  v:vs -> csum vs >> ca v >> op "+"

cdigest :: [(SLType, App ())] -> App ()
cdigest l = do
  mapM_ (uncurry go) $ zip (no_concat : repeat yes_concat) l
  op "keccak256"
  op "btoi"
  where go may_concat (t, m) = m >> ctobs t >> may_concat
        no_concat = nop
        yes_concat = op "concat"

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_Impossible at msg -> expect_throw at msg
  DLE_PrimOp _ p args -> cprim p args
  DLE_ArrayRef {} -> xxx "array ref"
  DLE_ArraySet {} -> xxx "array set"
  DLE_ArrayConcat {} -> xxx "array concat"
  DLE_ArrayZip {} -> xxx "array zip"
  DLE_TupleRef {} -> xxx "tuple ref"
  DLE_ObjectRef {} -> xxx "obj ref"
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> cdigest $ map go args
    where go a = (argTypeOf a, ca a)
  DLE_Transfer _ _ who amt -> do
    txni <- talloc
    code "gtxn" [ texty txni, "TypeEnum" ]
    code "int" [ "pay" ]
    op "=="
    or_fail
    code "gtxn" [ texty txni, "Receiver" ]
    ca who
    op "=="
    or_fail
    code "gtxn" [ texty txni, "Amount" ]
    ca amt
    op "=="
    or_fail
    code "gtxn" [ texty txni, "Sender" ]
    lookup_me
    op "=="
    or_fail
  DLE_Claim _ _ t a ->
    case t of
      CT_Assert -> impossible "assert"
      CT_Assume -> check
      CT_Require -> check
      CT_Possible -> impossible "possible"
      CT_Unknowable {} -> impossible "unknowable"
      where check = ca a >> or_fail
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
      code "store" [ loct ]
      store_let dv (code "load" [ loct ]) $ ck k
  PL_ArrayMap {} -> xxx "map"
  PL_ArrayReduce {} -> xxx "reduce"
  PL_Eff _ de k -> ce de >> ck k
  PL_Var _ dv k ->
    salloc $ \loc -> do
      store_var dv loc $
        store_let dv (code "load" [ texty loc ]) $
          ck k
  PL_Set _ dv da k -> do
    loc <- lookup_var dv
    ca da
    code "store" [ texty loc ]
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
  CT_Wait _ svs -> do
    cstate HM_Set svs
    code "arg" [ texty argNextSt ]
    op "=="
    or_fail
    halt_should_be False
  CT_Jump {} -> xxx "jump"
  CT_Halt _ -> halt_should_be True

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
  let whicha w = DLA_Con $ DLC_Int $ fromIntegral w
  cdigest $ map go $ whicha which : map DLA_Var svs

halt_should_be :: Bool -> App ()
halt_should_be b = do
  code "arg" [ texty argHalts ]
  cc $ DLC_Bool b
  op "=="
  or_fail
  code "b" ["done"]

-- Txns:
-- 0   : Application call
-- 1   : Zero to handler account
-- 2   : Transfer to contract account
-- 3.. : Transfers from contract to user
txnAppl :: Word8
txnAppl = 0
txnToHandler :: Word8
txnToHandler = txnAppl + 1
txnToContract :: Word8
txnToContract = txnToHandler + 1
txnFirstUser :: Word8
txnFirstUser = txnToContract + 1

-- Args:
-- 0   : Previous state
-- 1   : Next state
-- 2   : Contract account address
-- 3   : Handler halts
-- 4   : Last round
-- 5.. : Handler arguments
argPrevSt :: Word8
argPrevSt = 0
argNextSt :: Word8
argNextSt = argPrevSt + 1
argMe :: Word8
argMe = argNextSt + 1
argHalts :: Word8
argHalts = argMe + 1
argLast :: Word8
argLast = argHalts + 1
argFirstUser :: Word8
argFirstUser = argLast + 1

lookup_me :: App ()
lookup_me = code "arg" [ texty argMe ]
lookup_sender :: App ()
lookup_sender = code "gtxn" [ texty txnToContract, "Sender" ]
lookup_last :: App ()
lookup_last = code "arg" [ texty argLast ]

std_footer :: App ()
std_footer = do
  label "revert"
  cc $ DLC_Int 0
  op "return"
  label "done"
  cc $ DLC_Int 1
  op "return"

ch :: Shared -> Int -> CHandler -> IO (Maybe TEALs)
ch _ _ (C_Loop {}) = return $ Nothing
ch eShared eWhich (C_Handler _ int fs prev svs msg body) = do
  eLabelR <- newIORef 0
  eOutputR <- newIORef mempty
  let eHP = 0
  let eSP = 255
  eTxnsR <- newIORef $ txnFirstUser - 1
  let eVars = mempty
  let mkarg dv (i::Int) = (dv, code "arg" [ texty i ])
  let args = svs <> msg
  let eLets0 = M.fromList $ zipWith mkarg args [ (fromIntegral argFirstUser) .. ]
  let eLets =
        case fs of
          FS_Join dv ->
            M.insert dv lookup_sender eLets0
          FS_Again {} ->
            eLets0
  flip runReaderT (Env {..}) $ do
    --- XXX it does not appear to be possible to check that the number of arguments is correct... so what happens if we go beyond, you just crash? or you get 0?
    case fs of
      FS_Join {} -> return ()
      FS_Again dv -> do
        lookup_sender
        ca $ DLA_Var dv
        op "=="
        or_fail
    cstate (HM_Check prev) svs
    code "arg" [ texty argPrevSt ]
    op "=="
    or_fail
    ct body
    std_footer
  post_ts <- readIORef eOutputR
  writeIORef eOutputR mempty
  txns <- readIORef eTxnsR
  flip runReaderT (Env {..}) $ do
    code "global" [ "GroupSize" ]
    cc $ DLC_Int $ fromIntegral $ 1 + txns
    op "=="
    or_fail
    let check_time f = \case
          [] -> nop
          as -> do
            lookup_last
            csum as
            op "+"
            let go i = do
                  op "dup"
                  code "gtxn" [ texty i, f ]
                  op "=="
                  or_fail
            forM_ [0..txns] go
            op "pop"
    (do
      let CBetween from to = int
      check_time "FirstValid" from
      check_time "LastValid" to)
  pre_ts <- readIORef eOutputR
  return $ Just $ pre_ts <> post_ts

type Disp = String -> T.Text -> IO ()
compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at (PLOpts {..}) _ cpp = pl
  let CPProg _at (CHandlers hm) = cpp
  resr <- newIORef mempty
  let sHandlers = hm
  sFailedR <- newIORef False
  let shared = Shared {..}
  let addProg lab t = do
        modifyIORef resr (M.insert lab t)
        disp lab t
  forM_ (M.toList hm) $ \ (hi, hh) -> do
    mht <- ch shared hi hh
    case mht of
      Nothing -> return ()
      Just ht -> addProg ("m" <> show hi) $ render ht
  let simple m = do
        -- FIXME this is a hack. make fun parameterized over the interesting stuff and merge with ch
        let eShared = shared
        let eWhich = 0
        eLabelR <- newIORef 0
        eOutputR <- newIORef mempty
        eTxnsR <- newIORef 0
        let eHP = 0
        let eSP = 0
        let eVars = mempty
        let eLets = mempty
        flip runReaderT (Env {..}) $ m >> std_footer
        outs <- readIORef eOutputR
        return outs
  appm <- simple $ do
    xxx "if not initialized, do it and quit"
    xxx "not halts"
    xxx "check group size"
    xxx "check txnAppl?"
    xxx "check txnToHandler"
    xxx "check txnToContract?"
    xxx "prev st is correct"
    xxx "me is correct"
    xxx "last is correct"
    xxx "update next state"
    xxx "update last"
    xxx "update halts"
    code "b" ["done"]
  clearm <- simple $ do
    xxx "check halts"
  ctcm <- simple $ do
    xxx "check group size"
    xxx "check txnAppl"
  addProg "appApproval" $ render appm
  addProg "appClear" $ render clearm
  addProg "ctc" $ render ctcm
  res0 <- readIORef resr
  sFailed <- readIORef sFailedR
  let res1 = M.insert "unsupported" (T.pack $ show sFailed) res0
  return res1

connect_algo :: Connector
connect_algo moutn pl = do
  let disp which c =
        case moutn of
          Nothing -> return ()
          Just outn ->
            TIO.writeFile (outn $ T.pack $ which <> ".teal") c
  res <- compile_algo disp pl
  return $ M.fromList [ ("ALGO", res) ]

