module Reach.Connector.ALGO (connect_algo) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Control.Monad.Reader
import Data.IORef
import qualified Data.DList as DL
import Reach.AST
import Reach.Connector

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
  { sHandlers :: M.Map Int CHandler }
data Env = Env
  { eShared :: Shared
  , eWhich :: Int
  , eLabelR :: IORef Int
  , eOutputR :: IORef TEALs }

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

freshLabel :: App LT.Text
freshLabel = do
  Env {..} <- ask
  i <- liftIO $ readIORef eLabelR
  liftIO $ modifyIORef eLabelR (1 +)
  return $ "l" <> LT.pack (show i)

ca :: DLArg -> App ()
ca _da = comment "XXX arg"

cm :: (a -> App ()) -> PLCommon a -> App ()
cm ck = \case
  PL_Return {} -> comment "XXX return"
  PL_Let _ _lc _dv _de k -> do
    comment "XXX let"
    ck k
  PL_ArrayMap {} -> comment "XXX array map"
  PL_ArrayReduce {} -> comment "XXX array reduce"
  PL_Eff _ _de k -> do
    comment "XXX eff"
    ck k
  PL_Var _ _dv k -> do
    comment "XXX var"
    ck k
  PL_Set _ _dv _da k -> do
    comment "XXX set"
    ck k
  PL_LocalIf _ _a _tp _fp k -> do
    comment "XXX local if"
    ck k
  PL_LocalSwitch _ _dv _csm k -> do
    comment "XXX local switch"
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
  CT_Switch {} -> comment "XXX switch"
  CT_Wait {} -> comment "XXX wait"
  CT_Jump {} -> comment "XXX jump"
  CT_Halt _ -> do
    code "b" ["halt"]

ch :: Shared -> Int -> CHandler -> IO (Maybe TEALs)
ch _ _ (C_Loop {}) = return $ Nothing
ch eShared eWhich (C_Handler _at _XXX_int _XXX_fs _XXX_last _XXX_svs _XXX_msg body) = do
  eLabelR <- newIORef 0
  eOutputR <- newIORef mempty
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
  let shared = Shared {..}
  forM_ (M.toList hm) $ \ (hi, hh) -> do
    mht <- ch shared hi hh
    case mht of
      Nothing -> return ()
      Just ht -> do
        let lab = "h" <> show hi
        let htt = render ht
        modifyIORef resr (M.insert lab htt)
        disp lab htt
  res <- readIORef resr
  return res

connect_algo :: Connector
connect_algo moutn pl = do
  let disp which c =
        case moutn of
          Nothing -> return ()
          Just outn ->
            TIO.writeFile (outn $ T.pack $ which <> ".teal") c
  res <- compile_algo disp pl
  return $ M.fromList [ ("ALGO", res) ]

