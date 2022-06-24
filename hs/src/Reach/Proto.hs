{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Reach.Proto
  ( FDescOut'(..)
  , FDescOut(..)
  , Req(..)
  , Res(..)
  , Proto
  , cmd''
  , cmd
  , say''
  , say
  , listen
  , appStubServer
  , runStubServer
  , stubPort
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.Proxy
import Data.Text
import Data.Time.Clock
import GHC.Generics
import Servant
import Servant.Client
import Servant.Types.SourceT
import System.Exit
import System.IO
import System.Process (CreateProcess(..), StdStream(..), createProcess, waitForProcess, shell)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Media as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Client.Streaming as S

-- https://docs.servant.dev/en/latest/cookbook/basic-streaming/Streaming.html
-- https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints
-- https://docs.servant.dev/en/stable/tutorial/Client.html#querying-streaming-apis
-- https://hackage.haskell.org/package/servant-0.19/docs/Servant-Types-SourceT.html#t:StepT
--
-- TODO much of the following ought to be renamed
-- TODO "AppT" over `say`, `listen`, and `cmd` request helpers
-- TODO prolong client timeouts

stubPort :: Int
stubPort = 8123

data EventStream

data FDescOut'
  = Stdout'
  | Stderr'
  | Stdboth'
  deriving Eq

instance FromHttpApiData FDescOut' where
  parseUrlPiece = \case
    "1" -> Right Stdout'
    "2" -> Right Stderr'
    "3" -> Right Stdboth'
    a -> Left $ "Invalid fd: " <> a

instance ToHttpApiData FDescOut' where
  toUrlPiece = \case
    Stdout'  -> "1"
    Stderr'  -> "2"
    Stdboth' -> "3"

data FDescOut
  = Stdout Integer BL.ByteString
  | Stderr Integer BL.ByteString
  | Keepalive
  | ExitCode' Integer Int

type Project = M.Map FilePath Text -- path:hash

data Req = Req
  { req_args  :: [Text]
  , req_files :: Maybe Project
  , req_env   :: M.Map Text Text
  } deriving (Eq, Generic, FromJSON, ToJSON)

data Res = Res -- TODO
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type Pid = Text

instance Accept EventStream where
  contentType _ = "text" H.// "event-stream"

-- TODO handle truncation of long `data` lines: CORE-1995
toSSE' :: Show a => BL.ByteString -> a -> BL.ByteString -> BL.ByteString
toSSE' e n m
  = "id: " <> BL.fromString (show n) <> "\n"
 <> "event: " <> e <> "\n"
 <> "data: " <> m <> "\n\n"

instance MimeRender EventStream FDescOut where
  mimeRender _ = \case
    Stdout n m -> toSSE' "stdout" n m
    Stderr n m -> toSSE' "stderr" n m
    Keepalive  -> ": keepalive\n\n"
    ExitCode' n m -> toSSE' "exit" n . BL.fromString $ show m

pFDescOut :: Parsec String () FDescOut
pFDescOut = pka <|> try pout <|> pexit where
  TokenParser {..} = makeTokenParser emptyDef
  pka = (string ": keepalive\n\n" *> pure Keepalive) <* eof
  pnum = (try $ char '0' *> pure 0 <* newline <* try newline) <|> natural
  pid = string "id: " *> pnum
  pdata x = string "data: " *> x <* eof
  pout = do
    i <- pid
    e <- string "event: "
      *> ((try $ string "stdout" *> pure Stdout') <|> (string "stderr" *> pure Stderr'))
      <* newline
    m <- BL.fromString <$> pdata (manyTill anyChar (try $ string "\n\n"))
    case e of
      Stdout' -> pure $ Stdout i m
      Stderr' -> pure $ Stderr i m
      _ -> fail "Event was neither `stdout` nor `stderr`"
  pexit = ExitCode'
    <$> (pid <* string "event: exit" <* newline)
    <*> (fromIntegral <$> pdata pnum)

instance MimeUnrender EventStream FDescOut where
  mimeUnrender _ = either (Left . show) Right . runParser pFDescOut () "" . BL.toString

type V0_Sync = "v0" :>
    ( "exec" :> "reach" :> Capture "cmd" String :> ReqBody '[JSON] Req :> Post '[JSON] Res
 :<|> "proc" :> Capture "pid" Pid :> "fd" :> "0" :> ReqBody '[PlainText] Text :> PutNoContent
    )

type V0_Stream = "v0"
  :> "proc" :> Capture "pid" Pid
  :> "fd"   :> Capture "fd"  FDescOut'
  :> StreamGet NoFraming EventStream (SourceIO FDescOut)

type Proto = V0_Sync :<|> V0_Stream

withKeepalives :: ((FDescOut -> IO ()) -> IO ()) -> StepT IO FDescOut
withKeepalives f = Effect $ do
  q <- newTQueueIO
  let tk' = atomically (writeTQueue q Keepalive) *> threadDelay 2500000 *> tk'
  let tf' = f (atomically . writeTQueue q)
  tk <- forkIO tk'
  tf <- forkIO tf'
  let go = atomically (readTQueue q) >>= \case
        ExitCode' n m -> do
          killThread tf
          killThread tk
          pure $ Yield (ExitCode' n m) Stop
        v -> pure . Yield v $ Effect go
  go

-- "stub" because in real life these processes will live in remote cloud
-- containers and their output-tracking will be more reliable + sophisticated.
-- This design is useful for demonstration but not robust enough for our
-- purposes in production.
stubUnixProc :: String -> (FDescOut -> IO ()) -> IO ()
stubUnixProc c f = do
  (_, Just o, Just e, ph) <- createProcess (shell c) { std_out = CreatePipe, std_err = CreatePipe }
  i' <- newIORef 0 -- Represents message ID
  let onEOF = \(_ :: IOError) -> modifyIORef i' (+ (-1))
  let i = do
        n <- readIORef i'
        writeIORef i' $ n + 1
        pure n
  let both = do
        n <- i
        l <- (BL.fromString <$> hGetLine e) `race` (BL.fromString <$> hGetLine o)
        f $ either (Stderr n) (Stdout n) l
        both
  -- std(out|err) reach EOF independently of one another; fully exhaust both before terminating
  let mkS t s = f =<< t <$> i <*> (BL.fromString <$> hGetLine s)
  let fo = mkS Stdout o *> fo
  let fe = mkS Stderr e *> fe
  both `catch` onEOF
  fo `catch` onEOF
  fe `catch` onEOF
  waitForProcess ph >>= \case
    ExitSuccess -> i >>= f . flip ExitCode' 0
    ExitFailure x -> i >>= f . flip ExitCode' x

appStubServer :: Bool -> Maybe String -> Application
appStubServer l t = serve (Proxy @Proto) ((r :<|> pin) :<|> pout) where
  r c Req {..} = do
    when l . liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": reach " <> c <> " " <> unpack (intercalate " " req_args)
    pure Res

  pin p s = do
    when l . liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": PUT /v0/proc/" <> unpack p <> "/fd/0 " <> unpack s
    pure NoContent

  pout pid fd = do
    when l . liftIO . putStrLn $ "Received listen request for PID# " <> unpack pid
    pure $ case fd of
      Stdout'  -> source [Stdout 0 "first", Keepalive, Stdout 1 "second"]
      Stderr'  -> source []
      Stdboth' -> fromStepT . withKeepalives . stubUnixProc
        $ maybe "ls -alh && sleep 5 && uname -a && sleep 3 && date" id t

runStubServer :: IO ()
runStubServer = Warp.run stubPort $ appStubServer True Nothing

cmd'' :: String -> Req -> ClientM Res
say'' :: Pid -> Text -> ClientM NoContent
cmd'' :<|> say'' = client $ Proxy @V0_Sync

todoErrorHandler :: ClientError -> IO a
todoErrorHandler = fail . show

cmd :: ClientEnv -> String -> Req -> IO ()
cmd e c r = runClientM (cmd'' c r) e >>= either todoErrorHandler (print . encode)

say :: ClientEnv -> Pid -> Text -> IO ()
say e p t = runClientM (say'' p t) e >>= either todoErrorHandler (const $ pure ())

listen :: ClientEnv -> (String -> IO ()) -> (FDescOut -> IO ()) -> Pid -> FDescOut' -> IO ()
listen e x f p o = S.withClientM (S.client (Proxy @V0_Stream) p o) e
  $ either todoErrorHandler (foreach x f)
