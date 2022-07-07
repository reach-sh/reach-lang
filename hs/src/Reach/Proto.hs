{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Reach.Proto
  ( FDescOut'(..)
  , FDescOut(..)
  , Req(..)
  , ReqProjPut(..)
  , Res(..)
  , ResInterpret(..)
  , StubConfig(..)
  , XProject(..)
  , Proto
  , appStubServer
  , cmd
  , cmd''
  , fromXProject
  , listen
  , listen''
  , projPut
  , projPut''
  , runStubServer
  , say
  , say''
  , stubDispatchReach
  , stubPort
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.List.NonEmpty
import Data.Proxy
import Data.Text hiding (concat)
import Data.Time.Clock
import GHC.Generics
import Options.Applicative hiding ((<|>))
import Options.Applicative.Help.Pretty (text)
import Reach.CommandLine
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
import qualified Data.Text.IO as T
import qualified Network.HTTP.Media as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as O
import qualified Servant.Client.Streaming as S

-- https://docs.servant.dev/en/latest/cookbook/basic-streaming/Streaming.html
-- https://docs.servant.dev/en/stable/tutorial/Server.html#streaming-endpoints
-- https://docs.servant.dev/en/stable/tutorial/Client.html#querying-streaming-apis
-- https://hackage.haskell.org/package/servant-0.19/docs/Servant-Types-SourceT.html#t:StepT
--
-- TODO much of the following ought to be renamed
-- TODO "AppT" over `say`, `listen`, and `cmd` request helpers
-- TODO prolong client timeouts

type Pid = Text
type ProjectHashes = M.Map FilePath Text
type ProjectContent = M.Map FilePath Text

data SCEnv c = SCEnv
  { sce_req :: Req
  , _sce_cnf :: c -- Implementors can supply their own configuration types
  }

type SubcommandStepT c = StepT (ReaderT (SCEnv c) IO) Res
type Subcommand c = Mod CommandFields (SubcommandStepT c)
type SubcommandDispatch = Req -> String -> SourceT IO Res

stubPort :: Int
stubPort = 8123

ecode :: ExitCode -> Int
ecode = \case
  ExitSuccess -> 0
  ExitFailure n -> n

data XProject = XProject
  { xproj_orgOrUser :: Text
  , xproj_name :: Text
  } deriving (Eq, Show)

fromXProject :: XProject -> Text
fromXProject XProject {..} = "@" <> xproj_orgOrUser <> "/" <> xproj_name

toXProject :: Text -> Either String XProject
toXProject = either (Left . show) Right . runParser pXProject () ""

instance ToJSON XProject where
  toJSON = String . fromXProject

instance ToHttpApiData XProject where
  toUrlPiece = fromXProject

pXProject :: Parsec Text () XProject
pXProject = XProject -- e.g. "@reach-sh/rps"
  <$> (pack <$> (string "@" *> ok (string "/")))
  <*> (pack <$> ok eof)
 where
  ok x = do
    a <- alphaNum
    b <- manyTill (alphaNum <|> char '-') (try . lookAhead $ alphaNum <* x)
    c <- alphaNum <* x
    pure $ [a] <> b <> [c]

instance FromJSON XProject where
  parseJSON = withText "XProject" $ either fail pure . toXProject

instance FromHttpApiData XProject where
  parseUrlPiece = either (Left . pack) Right . toXProject

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

data Req = Req
  { req_args  :: [String]
  , req_files :: Maybe ProjectHashes
  , req_env   :: M.Map Text Text
  } deriving (Eq, Generic, FromJSON, ToJSON)

data ReqProjPut = ReqProjPut
  { req_projput :: ProjectContent
  } deriving (Eq, Generic, FromJSON, ToJSON)

data ResInterpret
  = ExitStdout Int Text
  | ExitStderr Int Text
  | AttachStreamJustListen Pid Integer
  | AttachStreamListenSpeak Pid Integer
  | PutProjectUpdates (NonEmpty FilePath)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Res
  = Redirect String
  | Interpret ResInterpret
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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

-- TODO octet stream
type V0_Proj = "proj"
  :> Header' '[Required, Strict] "X-Reach-Project" XProject
  :> ReqBody '[JSON] ReqProjPut
  :> PutNoContent -- TODO: replace me with HTTP 202

type V0_ProcIn = "proc"
  :> Capture "pid" Pid
  :> "fd" :> "0"
  :> ReqBody '[PlainText] Text
  :> PutNoContent -- TODO: replace me with HTTP 202

type V0_Sync = "v0" :> (V0_Proj :<|> V0_ProcIn)

type V0_ProcOut = "proc"
  :> Capture "pid" Pid
  :> "fd" :> Capture "fd"  FDescOut'
  :> StreamGet NoFraming EventStream (SourceIO FDescOut)

type V0_Exec = "exec" :> "reach"
  :> Capture "cmd" String
  :> Header "X-Reach-Project" XProject
  :> ReqBody '[JSON] Req
  :> StreamPost NewlineFraming JSON (SourceIO Res)

type V0_Stream = "v0" :> (V0_ProcOut :<|> V0_Exec)

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
  n <- waitForProcess ph
  i >>= f . flip ExitCode' (ecode n)

mkDispatchOptparse :: SCEnv c -> ParserInfo (SubcommandStepT c) -> SubcommandDispatch
mkDispatchOptparse e i Req {..} c = fromStepT $ case execParserPure p i (c : req_args) of
  CompletionInvoked _ -> Yield
    (Interpret $ ExitStderr 1 "Reach doesn't support shell completions.")
    Stop
  Failure x ->
    Yield (Interpret . ExitStderr (ecode n) $ pack x') Stop
    where (x', n) = renderFailure x "reach"
  O.Success f -> (flip runReaderT e) `hoist` f
 where
  p = prefs $ showHelpOnError <> showHelpOnEmpty <> helpShowGlobals

cmdJust :: ResInterpret -> SubcommandStepT c
cmdJust f = Yield (Interpret f) Stop

cmdIO :: ReaderT (SCEnv c) IO Res -> SubcommandStepT c
cmdIO f = Effect $ f >>= pure . flip Yield Stop

-- TODO
fsHashPresent :: FilePath -> Text -> ReaderT (SCEnv c) IO Bool
fsHashPresent _ _ = pure False

withSyncedProject :: SubcommandStepT c -> SubcommandStepT c
withSyncedProject f = Effect $ do
  asks (req_files . sce_req) >>= \case
    Nothing -> pure $ Yield (Interpret $ ExitStderr 1 "Missing mandatory project hashes") Stop
    Just fs -> do -- TODO
      _ns <- fmap M.fromList . flip filterM (M.toList fs) $ \(k, a) ->
        not <$> fsHashPresent k a
      pure f

-- These won't really live in the protocol definition --------------------------
stubHelp :: Subcommand c
stubHelp = command "help" $ info f d where
  d = progDesc "Show usage"
  f = pure . cmdJust $ ExitStdout 0 "TODO: build non-stub version"

stubInit :: Subcommand c
stubInit = command "init" (info f $ d <> foot) where
  d = progDesc "Set up source files for a simple app in the current directory"
  f = go <$> strArgument (metavar "TEMPLATE" <> value "_default" <> showDefault)
  foot = footerDoc . Just $ text "Available templates:\n"
    <> text "_default"
    <> text "\n\nAborts if index.rsh or index.mjs already exist"
  go t = cmdJust . ExitStdout 0 $ "TODO: build non-stub version\nSelected: " <> t

stubCompile :: Subcommand c
stubCompile = command "compile" $ info f d where
  d = progDesc "Compile an app"
  f = go <$> compiler
  go _ = withSyncedProject . cmdIO . pure . Interpret $ AttachStreamJustListen "54321" 0
--------------------------------------------------------------------------------

stubDispatchReach :: SCEnv c -> SubcommandDispatch
stubDispatchReach e = mkDispatchOptparse e p where
  p = info (hsubparser s <**> helper) fullDesc
  s = stubHelp <> stubInit <> stubCompile

data StubConfig = StubConfig

appStubServer :: StubConfig -> (SCEnv StubConfig -> SubcommandDispatch) -> Bool -> Maybe String -> Application
appStubServer e d l t = serve (Proxy @Proto) ((pput :<|> pin) :<|> (pout :<|> r)) where
  -- Update project files
  pput x _fs = do
    when l . liftIO . T.putStrLn $ "Syncing " <> fromXProject x <> "..."
    -- TODO _fs
    pure NoContent

  -- stdin
  pin p s = do
    when l . liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": PUT /v0/proc/" <> unpack p <> "/fd/0 " <> unpack s
    pure NoContent

  -- stdout/stderr
  pout pid fd = do
    when l . liftIO . putStrLn $ "Received listen request for PID# " <> unpack pid
    pure $ case fd of
      Stdout'  -> source [Stdout 0 "first", Keepalive, Stdout 1 "second"]
      Stderr'  -> source []
      Stdboth' -> fromStepT . withKeepalives . stubUnixProc
        $ maybe "cd ../examples/ttt && ../../reach compile" id t

  -- reach $cmd [ $args ]
  r c _ a@Req {..} = do
    when l . liftIO $ do
      utc <- getCurrentTime
      putStrLn $ show utc <> ": reach " <> c <> " " <> unpack (intercalate " " $ pack <$> req_args)
    pure $ d (SCEnv a e) a c

runStubServer :: IO ()
runStubServer = Warp.run stubPort $ appStubServer StubConfig stubDispatchReach True Nothing

projPut'' :: XProject -> ReqProjPut -> ClientM NoContent
say'' :: Pid -> Text -> ClientM NoContent
projPut'' :<|> say'' = client $ Proxy @V0_Sync

listen'' :: Text -> FDescOut' -> S.ClientM (SourceT IO FDescOut)
cmd'' :: String -> Maybe XProject -> Req -> S.ClientM (SourceT IO Res)
listen'' :<|> cmd'' = S.client $ Proxy @V0_Stream

todoErrorHandler :: ClientError -> IO a
todoErrorHandler = fail . show

-- TODO refactor us ------------------------------------------------------------
projPut :: ClientEnv -> XProject -> ReqProjPut -> IO ()
projPut e p r = runClientM (projPut'' p r) e >>= either todoErrorHandler (const $ pure ())

say :: ClientEnv -> Pid -> Text -> IO ()
say e p t = runClientM (say'' p t) e >>= either todoErrorHandler (const $ pure ())

listen :: ClientEnv -> (String -> IO ()) -> (FDescOut -> IO ()) -> Pid -> FDescOut' -> IO ()
listen e x f p o = S.withClientM (listen'' p o) e $ either todoErrorHandler (foreach x f)

cmd :: ClientEnv -> (String -> IO ()) -> String -> Maybe XProject -> Req -> (Res -> IO ()) -> IO ()
cmd e x c mp r f = S.withClientM (cmd'' c mp r) e $ either todoErrorHandler (foreach x f)
