{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Reach.Proto
  ( FDescOut'(..)
  , FDescOut(..)
  , Req(..)
  , Proto
  , cmd
  , say
  , listen
  , runStubServer
  , stubPort
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Time.Clock
import Data.Typeable
import GHC.Generics
import Servant
import Servant.Client
import Servant.Types.SourceT
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
-- TODO easy thread-safety
-- TODO prolong client timeouts

stubPort :: Int
stubPort = 8123

data EventStream deriving Typeable

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

type Project = M.Map FilePath Text -- path:hash

data Req = Req
  { req_args  :: [Text]
  , req_files :: Maybe Project
  , req_env   :: M.Map Text Text
  } deriving (Eq, Generic, FromJSON, ToJSON)

data Res = Res -- TODO
  deriving (Eq, Generic, FromJSON, ToJSON)

type Pid = Text

instance Accept EventStream where
  contentType _ = "text" H.// "event-stream"

class ToSSE a where
  toSSE :: a -> BL.ByteString

toSSE' :: Show a => BL.ByteString -> a -> BL.ByteString -> BL.ByteString
toSSE' e n m
  = "id: " <> BL.fromString (show n) <> "\n"
 <> "event: " <> e <> "\n"
 <> "data: " <> m <> "\n\n"

instance ToSSE FDescOut where
  toSSE = \case
    Stdout n m -> toSSE' "stdout" n m
    Stderr n m -> toSSE' "stderr" n m
    Keepalive  -> ": keepalive\n\n"

instance ToSSE a => MimeRender EventStream a where
  mimeRender _ = toSSE

pFDescOut :: Parsec String () FDescOut
pFDescOut = pka <|> pout where
  TokenParser {..} = makeTokenParser emptyDef
  pka = (string ": keepalive\n\n" *> pure Keepalive) <* eof
  pout = do
    i <- string "id: " *> ((try $ char '0' *> pure 0 <* newline) <|> natural)
    e <- string "event: "
      *> ((try $ string "stdout" *> pure Stdout') <|> (string "stderr" *> pure Stderr'))
      <* newline
    m <- BL.fromString <$> (string "data: " *> manyTill anyChar (try $ string "\n\n") <* eof)
    case e of
      Stdout' -> pure $ Stdout i m
      Stderr' -> pure $ Stderr i m
      _ -> fail "Event was neither `stdout` nor `stderr`"

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

runStubServer :: IO ()
runStubServer = Warp.run stubPort $ serve (Proxy @Proto) ((r :<|> pin) :<|> pout) where
  r c Req {..} = do
    liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": reach " <> c <> " " <> unpack (intercalate " " req_args)
    pure Res

  pin p s = do
    liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": PUT /v0/proc/" <> unpack p <> "/fd/0 " <> unpack s
    pure NoContent

  pout pid fd = do
    liftIO . putStrLn $ "Received listen request for PID# " <> unpack pid
    pure $ case fd of
      Stdout'  -> source [Stdout 0 "first", Keepalive, Stdout 1 "second"]
      Stderr'  -> source []
      Stdboth' -> fromStepT . Effect $ stdb 0

  stdb n
    | n == 500 = pure Stop
    | n /= 0 && n `rem` 7 == 0 = do
      pure . Yield (Stderr n "Multiple of 7 detected") . Effect $ stdb (n + 1)
    | n `rem` 15 == 0 = do
      pure . Yield Keepalive . Effect $ stdb (n + 1) -- Obviously we won't skip IDs in real life
    | n `rem` 100 == 0 = do
      threadDelay 10000
      pure . Yield (Stdout n "reach.sh") . Effect $ stdb (n + 1)
    | otherwise = do
      threadDelay 1000000
      utc <- getCurrentTime
      pure . Yield (Stdout n . BL.fromString $ show utc) . Effect $ stdb (n + 1)

cmd' :: String -> Req -> ClientM Res
say' :: Pid -> Text -> ClientM NoContent
cmd' :<|> say' = client $ Proxy @V0_Sync

todoErrorHandler :: ClientError -> IO a
todoErrorHandler = fail . show

cmd :: ClientEnv -> String -> Req -> IO ()
cmd e c r = runClientM (cmd' c r) e >>= either todoErrorHandler (print . encode)

say :: ClientEnv -> Pid -> Text -> IO ()
say e p t = runClientM (say' p t) e >>= either todoErrorHandler (const $ pure ())

listen :: ClientEnv -> (String -> IO ()) -> (FDescOut -> IO ()) -> Pid -> FDescOut' -> IO ()
listen e x f p o = S.withClientM (S.client (Proxy @V0_Stream) p o) e
  $ either todoErrorHandler (foreach x f)
