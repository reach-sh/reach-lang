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

code :: LT.Text -> [LT.Text] -> TEAL
code fun args = LT.unwords $ fun : args

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
  , eOutput :: IORef TEALs }
type App = ReaderT Env IO

output :: TEAL -> App ()
output t = do
  Env {..} <- ask
  liftIO $ modifyIORef eOutput (flip DL.snoc t)

ct :: CTail -> App ()
ct _ = output $ code "XXX" []

ch :: Shared -> Int -> CHandler -> IO (Maybe TEALs)
ch _ _ (C_Loop {}) = return $ Nothing
ch eShared eWhich (C_Handler _at _XXX_int _XXX_fs _XXX_last _XXX_svs _XXX_msg body) = do
  eOutput <- newIORef mempty
  flip runReaderT (Env {..}) (ct body)
  ts <- readIORef eOutput
  return $ Just ts

type Disp = String -> T.Text -> IO ()
compile_algo :: Disp -> PLProg -> IO ConnectorInfo
compile_algo disp pl = do
  let PLProg _at (PLOpts {..}) _ cp = pl
  let CPProg _at (CHandlers hm) = cp
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

