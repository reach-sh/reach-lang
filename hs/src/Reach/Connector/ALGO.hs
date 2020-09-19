module Reach.Connector.ALGO (connect_algo) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Control.Monad.Reader
import Data.IORef
import Data.Word
import qualified Data.DList as DL
import Reach.AST
import Reach.Connector
import Reach.Util

type ScratchSlot = Word8

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

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
        lts = DL.toList ts

data Shared = Shared
  { sHandlers :: M.Map Int CHandler
  , sFailedR :: IORef Bool }
data Env = Env
  { eShared :: Shared
  , eWhich :: Int
  , eLabelR :: IORef Int
  , eOutputR :: IORef TEALs
  , eHP :: Word8
  , eSP :: Word8
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
    Nothing -> impossible $ "lookup_let " <> show dv <> " in " <> show (M.keys eLets)

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
    Nothing -> impossible "lookup_var"

salloc :: (ScratchSlot -> App a) -> App a
salloc fm = do
  Env {..} <- ask
  let eSP' = eSP - 1
  when ( eSP' == eHP ) $ do
    bad "too much memory"
  local (\e -> e { eSP = eSP' }) $
    fm eSP

ca :: DLArg -> App ()
ca = \case
  DLA_Var v -> lookup_let v
  DLA_Con {} -> xxx "con"
  DLA_Array {} -> xxx "array"
  DLA_Tuple {} -> xxx "tuple"
  DLA_Obj {} -> xxx "obj"
  DLA_Data {} -> xxx "data"
  DLA_Interact {} -> impossible "consensus interact"

cprim :: PrimOp -> App ()
cprim = \case
  p -> xxx $ texty p

ce :: DLExpr -> App ()
ce = \case
  DLE_Arg _ a -> ca a
  DLE_Impossible at msg -> expect_throw at msg
  DLE_PrimOp _ p args -> do
    forM_ args ca
    cprim p
  DLE_ArrayRef {} -> xxx "array ref"
  DLE_ArraySet {} -> xxx "array set"
  DLE_ArrayConcat {} -> xxx "array concat"
  DLE_ArrayZip {} -> xxx "array zip"
  DLE_TupleRef {} -> xxx "tuple ref"
  DLE_ObjectRef {} -> xxx "obj ref"
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest {} -> xxx "digest"
  DLE_Transfer {} -> xxx "transfer"
  DLE_Claim _ _ t a ->
    case t of
      CT_Assert -> impossible "assert"
      CT_Assume -> check
      CT_Require -> check
      CT_Possible -> impossible "possible"
      CT_Unknowable {} -> impossible "unknowable"
      where
        check = do
          ca a
          code "bz" ["revert"]
  DLE_Wait {} -> return ()
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
  CT_Wait {} -> xxx "wait"
  CT_Jump {} -> xxx "jump"
  CT_Halt _ -> do
    code "b" ["halt"]

ch :: Shared -> Int -> CHandler -> IO (Maybe TEALs)
ch _ _ (C_Loop {}) = return $ Nothing
ch eShared eWhich (C_Handler _at _XXX_int _XXX_fs _XXX_last _XXX_svs _XXX_msg body) = do
  eLabelR <- newIORef 0
  eOutputR <- newIORef mempty
  let eHP = 0
  let eSP = 255
  let eVars = mempty
  let eLets = mempty
  flip runReaderT (Env {..}) (ct body)
  ts <- readIORef eOutputR
  return $ Just ts

type Disp = String -> T.Text -> IO ()
compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at (PLOpts {..}) _ cpp = pl
  let CPProg _at (CHandlers hm) = cpp
  resr <- newIORef mempty
  let sHandlers = hm
  sFailedR <- newIORef False
  let shared = Shared {..}
  forM_ (M.toList hm) $ \ (hi, hh) -> do
    mht <- ch shared hi hh
    case mht of
      Nothing -> return ()
      Just ht -> do
        let lab = "m" <> show hi
        let htt = render ht
        modifyIORef resr (M.insert lab htt)
        disp lab htt
  res <- readIORef resr
  sFailed <- readIORef sFailedR
  let res' = M.insert "unsupported" (T.pack $ show sFailed) res
  return res'

connect_algo :: Connector
connect_algo moutn pl = do
  let disp which c =
        case moutn of
          Nothing -> return ()
          Just outn ->
            TIO.writeFile (outn $ T.pack $ which <> ".teal") c
  res <- compile_algo disp pl
  return $ M.fromList [ ("ALGO", res) ]

