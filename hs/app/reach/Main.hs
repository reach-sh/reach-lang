{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, encode, object, parseJSON, withObject, withText, (.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import Data.Bits
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.ByteString.UTF8 (toString)
import Data.Char
import Data.Either
import Data.Functor
import Data.IORef
import qualified Data.List.Extra as L
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Text (Text, intercalate, pack, stripEnd, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Data.Time
import Data.Time.Format.ISO8601
import Data.Tuple.Extra (first)
import Data.UnixTime
import qualified Data.Yaml as Y
import GHC.Float
import GHC.Generics
import qualified NeatInterpolation as N
import Network.HTTP.Simple
import Options.Applicative
import Options.Applicative.Help.Pretty (text, (<$$>))
import Reach.CommandLine
import Reach.Report
import Reach.Util
import Reach.Version
import Reach.EverestUtil
import Safe
import System.Directory.Extra
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Posix.Files
import Text.Parsec (ParsecT, eof, parse, runParserT, try)
import Text.Parsec.Char
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator (count)
import Text.ParserCombinators.Parsec.Token
import Text.Pretty.Simple

onlyEverest :: String -> String
onlyEverest x = x <> me
  where
    me = if hasReachEverest then "" else " (" <> onlyWithReachEverest <> ")"

f_onlyEverest :: String -> Parser App
f_onlyEverest sub = pure $ do
  putStrLnPacked $ "reach " <> sub <> " is " <> onlyWithReachEverest

uriIssues :: Text
uriIssues = "https://github.com/reach-sh/reach-lang/issues"

uriReachScript :: IsString a => a
uriReachScript = "https://docs.reach.sh/reach"

esc :: FilePath -> FilePath
esc x = "'" <> e <> "'" where
  e = L.foldl' (\a c -> a <> (if c == '\'' then "'\\''" else [c])) "" x

esc' :: FilePath -> Text
esc' = pack . esc

data Effect
  = Script Text
  | InProcess

data Connector
  = ALGO
  | ETH
  deriving (Eq, Show, Enum, Bounded)

data Mode
  = Devnet
  | Live
  | Browser
  deriving (Eq, Enum, Bounded)

data ConnectorMode = ConnectorMode Connector Mode
  deriving (Eq)

instance Show Mode where
  show = \case
    Devnet -> "devnet"
    Live -> "live"
    Browser -> "browser"

instance Show ConnectorMode where
  show (ConnectorMode c m) = show c <> "-" <> show m

data RVEnvNumeric = RVEnvNumeric
  { rvEnvNumericMajor :: Integer
  , rvEnvNumericMinor :: Maybe Integer
  , rvEnvNumericPatch :: Maybe Integer
  }
  deriving (Eq, Ord, Show)

data RVWithMaj'
  = RVDefault
  | RVStable
  | RVNumeric
      { rvEnvNumeric :: RVEnvNumeric
      , rvMinor :: Integer
      , rvPatch :: Integer
      }
  deriving (Eq, Ord, Show)

data ReachVersionOf
  = RVWithMaj RVWithMaj'
  | RVHash Text
  | RVDate Day
  deriving (Eq, Ord, Show)

data ReachVersion = ReachVersion
  { rvEnvRaw :: Maybe Text
  , rv :: ReachVersionOf
  }
  deriving (Show)

data ImageHost = DockerHub
  deriving (Show)

mkReachVersionOf :: (String -> a) -> Maybe String -> Either a ReachVersionOf
mkReachVersionOf iv = \case
  Nothing -> Right $ RVWithMaj RVDefault
  Just "" -> Right $ RVWithMaj RVDefault
  Just "stable" -> Right $ RVWithMaj RVStable
  Just m -> either (const . Left $ iv m) Right $ parse (hash <|> numeric <|> datestamp m) "" m
  where
    TokenParser {..} = makeTokenParser emptyDef
    xx = eof $> Nothing
    ti = toInteger

    hash = RVHash . pack <$> try (count 8 (oneOf $ ['0' .. '9'] <> ['a' .. 'f']) <* eof)

    datestamp m = maybe (fail m) (pure . RVDate) $ parseTimeM False defaultTimeLocale "%Y-%m-%d" m

    numeric = try $ do
      rvEnvNumeric@RVEnvNumeric {..} <-
        RVEnvNumeric
          <$> (optional (string "v") *> decimal)
          <*> ((Just <$> (dot *> decimal)) <|> xx)
          <*> ((Just <$> (dot *> decimal <* eof)) <|> xx)
      -- Beware: minor/patch defaults are meaningless when updating
      let rvMinor = fromMaybe (if rvEnvNumericMajor /= ti major then 0 else ti minor) rvEnvNumericMinor
      let rvPatch = fromMaybe (if rvMinor /= ti minor then 0 else ti patch) rvEnvNumericPatch
      pure $ RVWithMaj RVNumeric {..}

mkReachVersionOf' :: Text -> Either () ReachVersionOf
mkReachVersionOf' = mkReachVersionOf (const ()) . Just . unpack

mkReachVersion :: IO ReachVersion
mkReachVersion = do
  mt <- lookupEnv "REACH_VERSION"
  let rvEnvRaw = pack <$> mt
  rv <- either die pure $ mkReachVersionOf ("Invalid `REACH_VERSION`: " <>) mt
  pure $ ReachVersion {..}

majMinPat :: RVWithMaj' -> Text
majMinPat = \case
  RVDefault -> pack versionStr
  RVStable -> pack versionStr
  RVNumeric {rvEnvNumeric = RVEnvNumeric {..}, ..} ->
    T.intercalate "." $ map packs [rvEnvNumericMajor, rvMinor, rvPatch]

majMin :: RVWithMaj' -> Text
majMin = \case
  RVDefault -> pack compatibleVersionStr
  RVStable -> pack compatibleVersionStr
  RVNumeric {rvEnvNumeric = RVEnvNumeric {..}, ..} ->
    T.intercalate "." $ map packs [rvEnvNumericMajor, rvMinor]

_maj :: RVWithMaj' -> Text
_maj = \case
  RVDefault -> packs major
  RVStable -> packs major
  RVNumeric {rvEnvNumeric = RVEnvNumeric {..}} -> packs rvEnvNumericMajor

versionBy :: (RVWithMaj' -> Text) -> ReachVersion -> Text
versionBy f ReachVersion {..} = case rv of
  RVWithMaj v -> f v
  RVHash v -> v
  RVDate v -> packs v

data Shell
  = ShellUnknown
  | Bash
  | Zsh

instance Show Shell where
  show = \case
    ShellUnknown -> "unknown"
    Bash -> "bash"
    Zsh -> "zsh"

mkShell :: IO (Shell, Text)
mkShell =
  lookupEnv "SHELL" >>= \case
    Nothing -> pure (ShellUnknown, "")
    Just "" -> pure (ShellUnknown, "")
    Just s -> do
      let p a b =
            try $
              optional (string "/" *> many (try $ many alphaNum *> string "/"))
                *> string a
                *> eof
                $> b
      either (const $ pure (ShellUnknown, pack s)) (pure . (,pack s)) $
        parse
          (p "bash" Bash <|> p "zsh" Zsh)
          ""
          s

data Var = Var
  { reachEx :: Text
  , connectorMode :: Maybe ConnectorMode
  , debug :: Bool
  , ide :: Bool
  , rpcKey :: Text
  , rpcPort :: Text
  , rpcServer'' :: Text
  , rpcTLSCrt :: Text
  , rpcTLSKey :: Text
  , rpcTLSPassphrase :: Text
  , rpcTLSRejectUnverified :: Bool
  , version'' :: ReachVersion
  , ci :: Bool
  , shell :: Shell
  , shellRaw :: Text
  , imageHost :: ImageHost
  }

data Env = Env
  { e_dirEmbed :: FilePath
  , e_dirPwdContainer :: FilePath
  , e_dirPwdHost :: FilePath
  , e_dirTmpContainer :: FilePath
  , e_dirTmpHost :: FilePath
  , e_dirConfigContainer :: FilePath
  , e_dirConfigHost :: FilePath
  , e_emitRaw :: Bool
  , e_disableReporting :: Bool
  , e_effect :: IORef Effect
  , e_var :: Var
  }

type App = ReaderT Env IO ()

type AppT a = ReaderT Env IO a

type Subcommand = Mod CommandFields App

data Cli = Cli
  { c_env :: Env
  , c_cmd :: App
  }

defRPCKey :: Text
defRPCKey = "opensesame"

defRPCTLSPassphrase :: Text
defRPCTLSPassphrase = "rpc-demo"

warnDefRPCKey :: App
warnDefRPCKey = do
  Var {..} <- asks e_var
  when (rpcKey == defRPCKey) . liftIO . T.putStrLn $
    "Warning! Using development RPC key: REACH_RPC_KEY=" <> defRPCKey <> "."

warnDeprecatedFlagUseExistingDevnet :: Bool -> App
warnDeprecatedFlagUseExistingDevnet u =
  when u . liftIO . putStrLn $
    "`--use-existing-devnet` is deprecated and no longer necessary - please remove."

warnDeprecatedFlagIsolate :: Bool -> App
warnDeprecatedFlagIsolate i =
  when i . liftIO . putStrLn $
    "`--isolate` is deprecated and no longer has any effect - please remove."

dieConnectorModeBrowser :: App
dieConnectorModeBrowser =
  connectorMode <$> asks e_var >>= \case
    Just (ConnectorMode _ Browser) ->
      liftIO . die $
        "`REACH_CONNECTOR_MODE` cannot select the `browser` target; `browser`"
          <> " is only available via the Reach standard library."
    _ -> pure ()

dieConnectorModeNotSpecified :: AppT ConnectorMode
dieConnectorModeNotSpecified =
  connectorMode <$> asks e_var >>= \case
    Just cm -> pure cm
    Nothing ->
      liftIO . die . unpack . intercalate "\n" $
        "Missing `REACH_CONNECTOR_MODE` environment variable - must be one of:" :
        L.sort s
          <> [ "Reach recommends adding this variable to your shell's profile settings by running `reach config`. See:"
             , " - https://docs.reach.sh/tool/#ref-usage-config"
             ]
  where
    s =
      [" * " <> packs c | c <- [minBound .. maxBound :: Connector]]
        <> [ " * " <> packs c <> "-" <> packs m
           | c <- [minBound .. maxBound :: Connector]
           , m <- [minBound .. maxBound :: Mode]
           ]

diePathContainsParentDir :: FilePath -> IO ()
diePathContainsParentDir x =
  when (any (== "..") $ splitDirectories x) . die $
    x <> " cannot contain parent directories (\"..\")."

warnScaffoldDefRPCTLSPair :: Project -> App
warnScaffoldDefRPCTLSPair (Project {..}) = do
  Env {..} <- ask
  let warnDev = putStrLn "Warning! The current TLS certificate is only suitable for development purposes."
  let embd r = e_dirEmbed </> "rpc" </> r
  let dock r = projDirContainer </> "tls" </> r
  let host r = projDirHost </> "tls" </> r
  let orw = ownerReadMode .|. ownerWriteMode
  let key = unpack $ rpcTLSKey e_var
  let crt = unpack $ rpcTLSCrt e_var
  liftIO $
    (,) <$> doesFileExist (dock key) <*> doesFileExist (dock crt) >>= \case
      (False, False) -> do
        createDirectoryIfMissing False $ dock ""
        readFile (embd "tls-default.key") >>= writeFile (dock key)
        readFile (embd "tls-default.crt") >>= writeFile (dock crt)
        setFileMode (dock key) orw
        setFileMode (dock crt) $ orw .|. groupReadMode .|. otherReadMode
        warnDev
      (False, True) -> die $ host key <> " doesn't exist!"
      (True, False) -> die $ host crt <> " doesn't exist!"
      _ -> do
        keyC <- readFile (dock key)
        defC <- readFile (embd "tls-default.key")
        when (keyC == defC) warnDev

mkVar :: IO Var
mkVar = do
  let packed = pure . pack
  let q e n = lookupEnv e >>= maybe n packed
  let m e n f = lookupEnv e >>= maybe n (\case "" -> n; j -> f j)
  rpcPort <- q "REACH_RPC_PORT" (pure "3000")
  rpcServer'' <- q "REACH_RPC_SERVER" (pure "127.0.0.1")
  rpcKey <- q "REACH_RPC_KEY" (pure defRPCKey)
  rpcTLSPassphrase <- q "REACH_RPC_TLS_PASSPHRASE" (pure defRPCTLSPassphrase)
  rpcTLSKey <- q "REACH_RPC_TLS_KEY" (pure "reach-server.key")
  rpcTLSCrt <- q "REACH_RPC_TLS_CRT" (pure "reach-server.crt")
  version'' <- mkReachVersion
  debug <- truthyEnv <$> lookupEnv "REACH_DEBUG"
  ide <- truthyEnv <$> lookupEnv "REACH_IDE"
  rpcTLSRejectUnverified <-
    lookupEnv "REACH_RPC_TLS_REJECT_UNVERIFIED"
      >>= maybe (pure True) (pure . (/= "0"))
  reachEx <-
    lookupEnv "REACH_EX"
      >>= maybe (die "Unset `REACH_EX` environment variable") packed
  connectorMode <- do
    let e = "REACH_CONNECTOR_MODE"
    m e (pure Nothing) (pure . Just) >>= \case
      Nothing -> pure Nothing
      Just rcm ->
        runParserT ((ConnectorMode <$> pConnector <*> pMode) <* eof) () "" rcm
          >>= either (const . die $ "Invalid `" <> e <> "`: " <> rcm) (pure . Just)
  ci <- truthyEnv <$> lookupEnv "CI"
  (shell, shellRaw) <- mkShell
  let imageHost = DockerHub
  pure $ Var {..}

mkScript :: Text -> App -> App
mkScript connectorMode' wrapped = do
  Var {..} <- asks e_var
  let debug' = if debug then "REACH_DEBUG=1\nset -x\n" else ""
  let ide' = if ide then "REACH_IDE=1\n" else ""
  let rpcTLSRejectUnverified' = case rpcTLSRejectUnverified of
        True -> ""
        False -> "REACH_RPC_TLS_REJECT_UNVERIFIED=0\n"
  let defOrBlank e d r = if d == r then [N.text| $e=$d |] <> "\n" else ""

  -- Recursive invocation of script should base version on what user specified
  -- (or didn't) via REACH_VERSION rather than what we've parsed/inferred;
  -- subcommands that actually use the version should instead rely on what
  -- we've computed and stuffed in `e_var`
  let v = maybe "" id $ rvEnvRaw version''

  asks e_effect
    >>= liftIO
      . flip
        writeIORef
        (Script $
           [N.text|
          #!/bin/sh
          set -e
       |]
             <> "\n\n"
             <> debug'
             <> ide'
             <> rpcTLSRejectUnverified'
             -- Don't leak production `REACH_RPC_KEY` or `REACH_RPC_TLS_PASSPHRASE`
             <> defOrBlank "REACH_RPC_KEY" defRPCKey rpcKey
             <> defOrBlank "REACH_RPC_TLS_PASSPHRASE" defRPCTLSPassphrase rpcTLSPassphrase
             <> [N.text|
          REACH_CONNECTOR_MODE=$connectorMode'
          REACH_RPC_PORT=$rpcPort
          REACH_RPC_SERVER=$rpcServer''
          REACH_RPC_TLS_CRT=$rpcTLSCrt
          REACH_RPC_TLS_KEY=$rpcTLSKey
          REACH_VERSION=$v

          export REACH_CONNECTOR_MODE
          export REACH_DEBUG
          export REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY
          export REACH_NO_WARN
          export REACH_IDE
          export REACH_RPC_KEY
          export REACH_RPC_PORT
          export REACH_RPC_SERVER
          export REACH_RPC_TLS_CRT
          export REACH_RPC_TLS_KEY
          export REACH_RPC_TLS_PASSPHRASE
          export REACH_RPC_TLS_PASSPHRASE
          export REACH_RPC_TLS_REJECT_UNVERIFIED
          export REACH_VERSION
      |]
             <> "\n\n")
  wrapped

scriptWithConnectorMode :: App -> App
scriptWithConnectorMode wrapped = do
  rcm <- dieConnectorModeNotSpecified
  mkScript (packs rcm) wrapped

scriptWithConnectorModeOptional :: App -> App
scriptWithConnectorModeOptional wrapped = do
  rcm <- maybe "" packs <$> asks (connectorMode . e_var)
  mkScript rcm wrapped

script :: App -> App
script = mkScript ""

write :: Text -> App
write t = asks e_effect >>= liftIO . flip modifyIORef w
  where
    w = \case
      Script t' -> Script $ t' <> t <> "\n\n"
      InProcess -> impossible "Cannot `write` to an in-process `Effect`"

writeFrom :: FilePath -> App
writeFrom p = asks e_dirEmbed >>= liftIO . T.readFile . (</> p) >>= write

putStrLnPacked :: String -> App
putStrLnPacked = liftIO . T.putStrLn . pack

realpath :: App
realpath = writeFrom "sh/realpath.sh"

swap :: Text -> Text -> Text -> Text
swap a b src = T.replace ("${" <> a <> "}") b src

packs :: Show a => a -> Text
packs = pack . show

type Digest = Text

type TagRaw = Text

type Image = Text

newtype Image' = Image' Image
  deriving (Show, Eq)

instance ToJSON Image' where toJSON (Image' i) = toJSON i

instance FromJSON Image' where
  parseJSON = withText "Image'" $ \case
    i | i `elem` (("reachsh/" <>) <$> imagesAll) -> pure $ Image' i
    i -> error $ "unrecognized Image \"" <> unpack i <> "\""

imagesCommon :: [Image]
imagesCommon =
  [ "reach"
  , "reach-cli"
  , "react-runner"
  , "rpc-server"
  , "runner"
  ]

imagesFor :: Connector -> [Image]
imagesFor = \case
  ALGO -> [devnetFor ALGO]
  ETH -> [devnetFor ETH]

imagesForAllConnectors :: [Image]
imagesForAllConnectors = L.foldl' (<>) [] $ imagesFor <$> [minBound .. maxBound]

imagesAll :: [Image]
imagesAll = imagesCommon <> imagesForAllConnectors

serviceConnector :: Env -> ConnectorMode -> [Text] -> Text -> Text -> IO (Text, [Text])
serviceConnector Env {..} (ConnectorMode c m) ports appService' v = do
  let ports' = case ports of
        [] -> "[]"
        ps -> intercalate "\n    " $ map ("- " <>) ps
  let n = show m <> "-" <> (toLower <$> show c)
  let d = packs c
  fmt <- T.readFile $ e_dirEmbed </> "docker" </> "service-" <> n <> ".yml"
  cns <- do
    let f a = \case A.String x -> [x] <> a; _ -> a
    cs <- (\(vs :: KM.KeyMap A.Value) -> [x | A.Object x <- KM.elems vs])
      <$> Y.decodeThrow (T.encodeUtf8 fmt)
    pure $ L.foldl' (\a -> maybe a (f a) . KM.lookup "container_name") [] cs

  let labels = [N.text| - "sh.reach.devnet-for=$d" |]
  let y = swap "REACH_VERSION" v
        . swap "PORTS" ports'
        . swap "NETWORK" "reach-devnet"
        . swap "APP_SERVICE" (if appService' == "" then "" else "-" <> appService')
        . swap "LABELS" labels
        $ fmt
  pure (y, cns)

connectorEnv :: Env -> ConnectorMode -> IO Text
connectorEnv Env {..} (ConnectorMode c m) = do
  let c' = toLower <$> show c
  T.readFile $ e_dirEmbed </> "docker" </> "service-" <> show m <> "-" <> c' <> "-env.yml"

devnetFor :: Connector -> Text
devnetFor = \case
  ALGO -> "devnet-algo"
  ETH -> "devnet-eth"

pConnector :: ParsecT String () IO Connector
pConnector =
  f ALGO "ALGO"
    <|> f ETH "ETH"
  where
    f a b = const a <$> string b

pMode :: ParsecT String () IO Mode
pMode =
  f Devnet "devnet"
    <|> f Live "live"
    <|> f Browser "browser"
    <|> string "" *> pure Devnet
  where
    f a b = const a <$> try (char '-' *> string b)

data Project = Project
  { projName :: Text
  , projDirContainer :: FilePath
  , projDirHost :: FilePath
  , projDirRel :: FilePath
  }

data Scaffold = Scaffold
  { containerDockerIgnore :: FilePath
  , containerGitIgnore :: FilePath
  , containerDockerfile :: FilePath
  , containerPackageJson :: FilePath
  , hostDockerIgnore :: FilePath
  , hostGitIgnore :: FilePath
  , hostDockerfile :: FilePath
  , hostPackageJson :: FilePath
  }

data WP
  = Console
  | React
  | RPC
  deriving (Bounded, Enum)

instance Show WP where
  show = \case
    Console -> "console"
    React -> "react"
    RPC -> "rpc"

data Compose
  = WithProject WP Project
  | StandaloneDevnet

data DockerMeta = DockerMeta
  { appProj :: Text
  , appService :: Text
  , appImage :: Text
  , appImageTag :: Text
  , compose :: Compose
  }

mkScaffold :: Project -> Scaffold
mkScaffold Project {..} =
  Scaffold
    { containerDockerIgnore = c ".dockerignore"
    , containerGitIgnore = c ".gitignore"
    , containerDockerfile = c "Dockerfile"
    , containerPackageJson = c "package.json"
    , hostDockerIgnore = h ".dockerignore"
    , hostGitIgnore = h ".gitignore"
    , hostDockerfile = h "Dockerfile"
    , hostPackageJson = h "package.json"
    }
  where
    c = (projDirContainer </>)
    h = (projDirHost </>)

mkDockerMetaProj :: Env -> Project -> WP -> DockerMeta
mkDockerMetaProj (Env {..}) (p@Project {..}) wp = DockerMeta {..}
  where
    f c = if isAlphaNum c && isAscii c then c else '-'
    g = L.foldl' (\x y -> maybe [y] (\z -> if z == '-' && y == '-' then x else x <> [y]) $ lastMay x) []
    s = T.dropWhile (== '-')
    e = T.dropWhileEnd (== '-')
    appProj = case wp of
      React -> ""
      _ -> case T.toLower . s . e . pack . g . map f . takeBaseName . dropTrailingPathSeparator $ projDirHost of
        "" -> "x"
        a -> a
    appService = case wp of
      React -> "react-runner"
      _ -> "reach-app-" <> appProj
    appImage = case wp of
      RPC -> "reachsh/rpc-server"
      _ -> "reachsh/" <> appService
    appImageTag = appImage <> ":" <> versionBy majMinPat (version'' e_var)
    compose = WithProject wp p

mkDockerMetaStandaloneDevnet :: DockerMeta
mkDockerMetaStandaloneDevnet = DockerMeta {..}
  where
    appProj = "reach-devnet"
    appService = ""
    appImage = ""
    appImageTag = ""
    compose = StandaloneDevnet

projectFrom :: FilePath -> AppT Project
projectFrom a = do
  Env {..} <- ask
  liftIO $ do
    when (isAbsolute a) . die $ "Please replace " <> a <> " with a relative path."
    case a of
      "" -> pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."
      "." -> pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."
      _ -> ifM
        (andM [pure . (< 2) . length $ splitDirectories a, not <$> doesDirectoryExist a])
        (pure $ Project (pack a) e_dirPwdContainer e_dirPwdHost ".")
        $ do
          let dph = e_dirPwdHost </> a
          let dpc = e_dirPwdContainer </> a
          diePathContainsParentDir dph
          diePathContainsParentDir dpc
          pure $ Project "index" dpc dph a

projectPwdIndex :: AppT Project
projectPwdIndex = do
  Env {..} <- ask
  pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."

scaff :: Bool -> FilePath -> Text -> IO ()
scaff quiet n f = do
  when (not quiet) . putStrLn $ "Writing " <> takeFileName n <> "..."
  T.writeFile n f

scaffIfAbsent :: Bool -> FilePath -> Text -> IO ()
scaffIfAbsent quiet n f = whenM (not <$> doesFileExist n) $ scaff quiet n f

readScaff :: FilePath -> AppT Text
readScaff p = do
  Env {..} <- ask
  liftIO . T.readFile $ e_dirEmbed </> "scaffold" </> p

withCompose :: DockerMeta -> App -> App
withCompose DockerMeta {..} wrapped = do
  env@Env {..} <- ask
  cm@(ConnectorMode c m) <- dieConnectorModeNotSpecified
  let Var {..} = e_var
  let connPorts = case (compose, c, m) of
        (_, _, Live) -> []
        (WithProject Console _, ALGO, Devnet) -> ["9392"]
        (_, ALGO, _) -> ["4180:4180", "8980:8980", "9392:9392"]
        (_, ETH, _) -> ["8545:8545"]
  let reachConnectorMode = packs cm
  let debug' = if debug then "1" else ""
  let projDirHost' = case compose of
        StandaloneDevnet -> ""
        WithProject _ Project {..} -> pack projDirHost
  let projDirHost'' = T.replace "\"" "\\\"" projDirHost'
  let devnetALGO =
        [N.text|
        - ALGO_SERVER=http://reach-devnet-algo
        - ALGO_PORT=4180
        - ALGO_INDEXER_SERVER=http://reach-devnet-algo
        - ALGO_INDEXER_PORT=8980
        - ALGO_NODE_WRITE_ONLY=no
      |]
  let deps'' d =
        [N.text|
    depends_on:
      - $d
  |]
  let deps' = maybe "" (deps'' . devnetFor)
  let (deps, extraEnv) = first deps' $ case (c, m) of
        (_, Live) -> (Nothing, "")
        (ALGO, Devnet) -> (Just c, devnetALGO)
        (ALGO, Browser) -> (Just c, devnetALGO)
        (ETH, Devnet) -> (Just c, "- ETH_NODE_URI=http://reach-devnet-eth:8545")
        (ETH, Browser) -> (Just c, "- ETH_NODE_URI=http://reach-devnet-eth:8545")
  connEnv <- case compose of
    StandaloneDevnet -> liftIO $ connectorEnv env cm
    WithProject Console _ -> liftIO $ connectorEnv env cm
    WithProject React _ ->
      pure
        [N.text|
      volumes:
        - $projDirHost':/app/src
      ports:
        - "3000:3000"
      stdin_open: true
      tty: true
      environment:
        - REACH_DEBUG
        - REACH_NO_WARN
        - REACH_CONNECTOR_MODE
        - REACH_ISOLATED_NETWORK
        - REACT_APP_REACH_DEBUG=$debug'
        - REACT_APP_REACH_CONNECTOR_MODE=$reachConnectorMode
        - REACT_APP_REACH_ISOLATED_NETWORK=$${REACH_ISOLATED_NETWORK}
        $extraEnv
      $deps
    |]
    WithProject RPC _ ->
      pure
        [N.text|
      volumes:
        - $projDirHost'/build:/app/build
        - $projDirHost'/tls:/app/tls
      ports:
        - "$rpcPort:$rpcPort"
      stdin_open: true
      tty: true
      environment:
        - REACH_DEBUG
        - REACH_NO_WARN
        - REACH_CONNECTOR_MODE=$reachConnectorMode
        - REACH_ISOLATED_NETWORK
        - REACH_RPC_PORT
        - REACH_RPC_KEY
        - REACH_RPC_TLS_KEY
        - REACH_RPC_TLS_CRT
        - REACH_RPC_TLS_PASSPHRASE
        $extraEnv
      $deps
    |]
  let stdConnSvs = liftIO . serviceConnector env cm connPorts appService $ versionBy majMinPat version''
  (connSvs, devnetCs) <- case (m, compose) of
    (Live, _) -> pure ("", [])
    (_, StandaloneDevnet) -> stdConnSvs
    (_, WithProject Console _) -> stdConnSvs
    (_, WithProject React _) -> stdConnSvs
    (_, WithProject RPC _) -> stdConnSvs
  let build = case compose of
        WithProject Console _ ->
          [N.text|
           build:
             context: "$projDirHost''"
        |]
        _ -> ""
  let e_dirTmpHost' = pack e_dirTmpHost
  let a = pack $ (\case WithProject a' _ -> show a'; _ -> "") compose
  let appService' = case compose of
        StandaloneDevnet -> ""
        _ ->
          [N.text|
               $appService:
                 image: $appImageTag
                 networks:
                   - reach-devnet
                 extra_hosts:
                   - "host.docker.internal:host-gateway"
                 labels:
                   - "sh.reach.dir-tmp=$e_dirTmpHost'"
                   - "sh.reach.dir-project=$projDirHost''"
                   - "sh.reach.app-type=$a"
                 $build
                 $connEnv
             |]
  let f =
        [N.text|
     version: '3.5'

     networks:
       reach-devnet:
         name: reach-devnet

     services:
       $connSvs

       $appService'
    |]
  liftIO $ scaff True (e_dirTmpContainer </> "docker-compose.yml") (notw f)
  scriptWithConnectorMode $ do
    -- https://docs.docker.com/engine/reference/commandline/ps/#filtering
    forM_ devnetCs $ \dc -> write [N.text|
      docker ps -aqf 'name=^$dc$$' \
        -f 'status=removing' \
        -f 'status=paused'   \
        -f 'status=exited'   \
        -f 'status=dead' | while IFS= read -r d; do docker rm -fv "$$d" >/dev/null 2>&1; done
    |]
    wrapped
  where
    notw = intercalate "\n" . fmap stripEnd . T.lines

argAppOrDir :: Parser FilePath
argAppOrDir =
  strArgument $
    metavar "APP or DIR"
      <> help
        "May be either a module name without its extension (e.g. \"index\") or a relative sub-directory path"
      <> value ""

manyArgs :: String -> Parser [Text]
manyArgs n =
  many . strArgument $
    metavar "ARGS"
      <> help ("Zero or more arguments to be passed into " <> n)

switchUseExistingDevnet :: Parser Bool
switchUseExistingDevnet =
  switch $
    long "use-existing-devnet"
      <> help "This switch has been deprecated and is no longer necessary"
      <> internal

switchIsolate :: Parser Bool
switchIsolate =
  switch $
    long "isolate"
      <> help "This switch has been deprecated and no longer has any effect"
      <> internal

switchQuiet :: Parser Bool
switchQuiet =
  switch $
    long "quiet"
      <> help "Withhold progress messages"

switchInteractiveAUs :: Parser Bool
switchInteractiveAUs = switch $ long "interactive" <> help
  "Report available updates and prompt to synchronize"

switchNonInteractiveAUs :: Parser Bool
switchNonInteractiveAUs = switch $ long "non-interactive" <> help
  "Report available updates and immediately exit"

switchJSONAUs :: Parser Bool
switchJSONAUs = switch $ long "json" <> help
  "Report available updates in JSON format (implies --non-interactive)"

recursiveDisableReporting :: Bool -> Text
recursiveDisableReporting d = if d then " --disable-reporting" else ""

mkEnv :: IORef Effect -> Maybe Var -> IO (Parser Env)
mkEnv eff mv = do
  var <- maybe mkVar pure mv
  pure $
    Env
      <$> strOption (long "dir-embed" <> internal <> value "/app/embed")
      <*> strOption (long "dir-project-container" <> internal <> value "/app/src")
      <*> strOption (long "dir-project-host" <> internal)
      <*> strOption (long "dir-tmp-container" <> internal)
      <*> strOption (long "dir-tmp-host" <> internal)
      <*> strOption (long "dir-config-container" <> internal <> value "/app/config")
      <*> strOption (long "dir-config-host" <> internal)
      <*> switch (long "emit-raw" <> internal)
      <*> switch (long "disable-reporting" <> internal)
      <*> pure eff
      <*> pure var

envFileContainer :: AppT FilePath
envFileContainer = (</> "env") <$> asks e_dirConfigContainer

envFileHost :: AppT FilePath
envFileHost = (</> "env") <$> asks e_dirConfigHost

dirInitTemplates :: AppT FilePath
dirInitTemplates = dirInitTemplates' <$> ask

dirInitTemplates' :: Env -> FilePath
dirInitTemplates' Env {..} = e_dirEmbed </> "init"

forwardedCli :: Text -> AppT Text
forwardedCli n = do
  Env {..} <- ask
  env <- liftIO $ mkEnv e_effect (Just e_var)
  (_, _, _, f) <-
    liftIO . execParser . flip info forwardOptions $
      (,,,)
        <$> env
        <*> subparser (command (unpack n) (info (pure ()) mempty))
        <*> switchUseExistingDevnet
        <*> manyArgs "a recursive invocation of `reachEx`"
  pure . intercalate " " $ filter (/= "--disable-reporting") f

scaffold' :: Bool -> Bool -> Project -> App
scaffold' i quiet proj@Project {..} = do
  warnDeprecatedFlagIsolate i
  e@Env {..} <- ask
  let Scaffold {..} = mkScaffold proj
  let DockerMeta {..} = mkDockerMetaProj e proj Console
  let scaffIfAbsent' n f = liftIO $ scaffIfAbsent quiet n f
  let tmpl p =
        swap "APP" projName
          . swap "MJS" (projName <> ".mjs")
          . swap "PROJ" appProj
          . swap "REACH_VERSION" (versionBy majMinPat $ version'' e_var)
          <$> readScaff p
  -- TODO: s/lint/preapp. It's disabled because sometimes our
  -- generated code trips the linter
  tmpl "package.json" >>= scaffIfAbsent' containerPackageJson
  tmpl "Dockerfile" >>= scaffIfAbsent' containerDockerfile
  tmpl ".gitignore" >>= scaffIfAbsent' containerGitIgnore
  tmpl ".dockerignore" >>= scaffIfAbsent' containerDockerIgnore
  when (not quiet) . liftIO $ putStrLn "Done."

scaffold :: Subcommand
scaffold = command "scaffold" $ info f d
  where
    d = progDesc "Set up Docker scaffolding for a simple app in the current directory"
    f = go <$> switchIsolate <*> switchQuiet
    go i q = projectPwdIndex >>= scaffold' i q

unscaffold :: Subcommand
unscaffold = command "unscaffold" $ info f fullDesc
  where
    f = go <$> switchIsolate <*> switchQuiet <*> argAppOrDir
    go i quiet appOrDir = do
      warnDeprecatedFlagIsolate i
      Scaffold {..} <- mkScaffold <$> projectFrom appOrDir
      liftIO $ do
        forM_ [containerDockerfile, containerPackageJson] $ \n ->
          whenM (doesFileExist n) $ do
            when (not quiet) . putStrLn $ "Deleting " <> takeFileName n <> "..."
            removeFile n
        when (not quiet) $ putStrLn "Done."

clean :: Subcommand
clean = command "clean" . info f $ fullDesc <> desc <> fdoc
  where
    desc = progDesc "Delete 'build/$MODULE.$IDENT.mjs'"
    fdoc =
      footerDoc . Just $
        text "MODULE is \"index\" by default"
          <$$> text "IDENT  is \"main\"  by default"
          <$$> text ""
          <$$> text "If:"
          <$$> text " * MODULE is a directory then `cd $MODULE && rm -f \"build/index.$IDENT.mjs\";"
          <$$> text " * MODULE is <something-else> then `rm -f \"build/$MODULE.$IDENT.mjs\""
    go m' i = do
      let m = esc' m'
      script $ write [N.text|
        MODULE=$m

        if [ ! $m = "index" ] && [ -d $m ]; then
          cd $m || exit 1
          MODULE="index"
        fi

        rm -f "build/$$MODULE.$i.mjs"
      |]
    f =
      go
        <$> strArgument (metavar "MODULE" <> value "index" <> showDefault)
        <*> strArgument (metavar "IDENT" <> value "main" <> showDefault)

compile :: Subcommand
compile = command "compile" $ info f d
  where
    d = progDesc "Compile an app"
    f = go <$> compiler
    go CompilerToolArgs {cta_co = CompilerOpts {..}} = do
      liftIO $ do
        diePathContainsParentDir co_source
        maybe (pure ()) diePathContainsParentDir co_mdirDotReach
        maybe (pure ()) diePathContainsParentDir co_moutputDir
      Env {e_var = Var {..}, ..} <- ask
      rawArgs <- fmap (\a -> if a == co_source then esc a else a) <$> liftIO getArgs
      let rawArgs' = dropWhile (/= "compile") rawArgs
      let o' (o, a) e = case o of
            True -> (False, a)
            False -> case e == "-o" || e == "--output" of
              True -> (True, a)
              False -> (False, a <> [e])
      let argsl = intercalate " "
            . map pack
            . filter (not . ("--output=" `L.isPrefixOf`))
            . snd
            . L.foldl' o' (False, [])
            . filter (/= "--disable-reporting")
            $ case rawArgs' of
              "compile" : x -> x
              _ -> impossible $ "compile args do not start with 'compile': " <> show rawArgs
      args <- do
        md <- liftIO $ case co_moutputDir of
          Nothing -> pure Nothing
          Just od -> do
            when (isAbsolute od) . die
              $ "-o|--output must be a relative subdirectory of " <> e_dirPwdHost <> "."
            pure $ Just od
        pure $ argsl
          <> maybe "" (\o -> " -o " <> esc' o) md
          <> recursiveDisableReporting e_disableReporting

      let v = versionBy majMinPat version''
      let cn = flip T.map v $ \c -> if isAlphaNum c && isAscii c then c else '-'
      let ci' = if ci then "true" else ""
      let ports = if co_sim then "-p 3001:3001" else ""
      scriptWithConnectorModeOptional $ do
        realpath
        write
          [N.text|
        REACH="$$(realpath "$reachEx")"
        HS="$$(dirname "$$REACH")/hs"

        export REACH

        if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
          ~/.local/bin/reachc $args

        elif [ "$${REACH_DOCKER}" = "0" ] \
          && [ -d "$${HS}/.stack-work"  ] \
          && which stack >/dev/null 2>&1; then

          STACK_YAML="$${REACH_STACK_YAML:-"$${HS}/stack.yaml"}"
          REACHC_HASH="$$("$${HS}/../scripts/git-hash.sh")"
          export STACK_YAML REACHC_HASH

          (cd "$$HS" && make hs-build)
          stack exec -- reachc $args

        else
          cid="$(docker ps -f "ancestor=reachsh/reach:$v" --format '{{.ID}} {{.Labels}}' \
            | grep -e "sh.reach.dir-project=$$(pwd)\$$" \
                   -e "sh.reach.dir-project=$$(pwd)," \
            | awk '{print $$1}' \
            | head -n1)"

          if [ -z "$$cid" ]; then
            cid="$(docker run -d --rm \
              --volume "$$PWD:/app" \
              -l "sh.reach.dir-project=$$PWD" \
              -u "$(id -ru):$(id -rg)" \
              --name "reachc-${cn}-$$$$" \
              --entrypoint tail \
              $ports \
              reachsh/reach:$v -f /dev/null)"
          fi

          docker exec \
            -u "$(id -ru):$(id -rg)" \
            -e REACH_CONNECTOR_MODE \
            -e REACH_IDE \
            -e REACH_DEBUG \
            -e REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY \
            -e REACH_NO_WARN \
            -e "REACHC_ID=$whoami'" \
            -e "CI=$ci'" \
            "$$cid" reachc $args
        fi
      |]

init' :: [String] -> Subcommand
init' its = command "init" . info f $ d <> foot
  where
    d = progDesc "Set up source files for a simple app in the current directory"
    f = go <$> strArgument (metavar "TEMPLATE" <> value "_default" <> showDefault)
    foot =
      footerDoc . Just $
        text "Available templates:\n"
          <> text (L.intercalate "\n" its)
          <> text "\n\nAborts if index.rsh or index.mjs already exist"
    go template = do
      Env {..} <- ask
      Project {..} <- projectPwdIndex
      ts <- dirInitTemplates
      let tmpl n = ts </> n
      let app = "index" -- Used to be configurable via CLI; now we try to nudge default of "index"
      liftIO $ do
        tmpl' <-
          ifM
            (doesDirectoryExist $ tmpl template)
            (pure $ tmpl template)
            (pure $ tmpl "_default")
        fmtInitRsh <- T.readFile $ tmpl' </> "index.rsh"
        fmtInitMjs <- T.readFile $ tmpl' </> "index.mjs"
        let rsh = projDirContainer </> unpack app <> ".rsh"
        let mjs = projDirContainer </> unpack app <> ".mjs"
        let abortIf x = whenM (doesFileExist x) . die $ x <> " already exists."
        abortIf rsh
        abortIf mjs
        T.putStrLn $ "Writing " <> app <> ".rsh..."
        T.writeFile rsh $ swap "REACH_VERSION_SHORT" (versionBy majMin $ version'' e_var) fmtInitRsh
        T.putStrLn $ "Writing " <> app <> ".mjs..."
        T.writeFile mjs $ swap "APP" app fmtInitMjs
        putStrLn "Done."

-- Tell `docker-compose` to skip connector containers if they're already running
devnetDeps :: Bool -> AppT Text
devnetDeps nolog = do
  ConnectorMode c' _ <- dieConnectorModeNotSpecified
  let c = packs c'
  l <- if nolog then pure "export REACH_DISABLE_REPORTING=1" else log'' "devnet_create"
  pure [N.text|
    NO_DEPS=''
    if [ "$(docker ps -qf label=sh.reach.devnet-for=$c)x" = 'x' ]; then
      :
      $l
    else
      NO_DEPS=' --no-deps'
    fi
  |]

run' :: Subcommand
run' = command "run" . info f $ d <> noIntersperse
  where
    d = progDesc "Run a simple app"
    f =
      go <$> switchIsolate
        <*> argAppOrDir
        <*> manyArgs "APP"
    go i appOrDir args = do
      (appOrDir', args') <- liftIO $ do
        "run" : as <- dropWhile (/= "run") <$> getArgs
        pure $ case L.split (== "--") as of
          [] : [cs] -> ("", map pack cs)
          bs : [cs] -> (headDef "" (dropWhile (== "--isolate") bs), map pack cs)
          _ -> (appOrDir, args)

      warnDeprecatedFlagIsolate i
      dieConnectorModeBrowser
      e@Env {..} <- ask
      proj@Project {..} <- projectFrom appOrDir'
      dd <- devnetDeps e_disableReporting
      let Var {..} = e_var
      let Scaffold {..} = mkScaffold proj
      toClean <-
        filterM
          (fmap not . liftIO . doesFileExist . fst)
          [ (containerPackageJson, hostPackageJson)
          , (containerDockerfile, hostDockerfile)
          , (containerGitIgnore, hostGitIgnore)
          , (containerDockerIgnore, hostDockerIgnore)
          ]
      cleanup <- intercalate "\n" <$> forM toClean (pure . (\a -> "rm " <> esc' a) . snd)
      let rsh = projDirContainer </> unpack projName <> ".rsh"
      let mjs = projDirContainer </> unpack projName <> ".mjs"
      -- XXX the program may not have the export 'main'
      let bjs = projDirContainer </> "build" </> unpack projName <> ".main.mjs"
      let abortIfAbsent p =
            liftIO . whenM (not <$> doesFileExist p)
              . die
              $ takeFileName p <> " doesn't exist."
      abortIfAbsent rsh
      abortIfAbsent mjs
      scaffold' False True proj

      let target = pack $ projDirRel </> unpack projName <> ".rsh"
      let dr = recursiveDisableReporting e_disableReporting
      let recompile' =
            [N.text|
      set +e
      $reachEx$dr compile $target
      RES="$?"
      set -e

      if [ ! "$$RES" -eq 0 ]; then
        $cleanup
        exit "$$RES"
      fi
    |]

      recompile <- liftIO $
        ifM
          (not <$> doesFileExist bjs)
          (pure $ Just recompile')
          $ do
            b <- modificationTime <$> getFileStatus bjs
            r <- modificationTime <$> getFileStatus rsh
            pure $ if r > b then Just recompile' else Nothing

      let dm@DockerMeta {..} = mkDockerMetaProj e proj Console
      let dockerfile' = esc' hostDockerfile
      let projDirHost' = esc' projDirHost
      let args'' = intercalate " " . map (<> "'") . map ("'" <>) $ projName : args'
      withCompose dm $ do
        write dd
        maybe (pure ()) write recompile
        unless e_disableReporting $ log'' "run" >>= write
        write
          [N.text|
        cd $projDirHost'
        CNAME="$appService-$$$$"

        set +e
        docker build -f $dockerfile' --tag=$appImageTag --pull=false . \
          && docker-compose -f "$$TMP/docker-compose.yml" run \
            -e REACHC_ID="$whoami'" \
            --name "$$CNAME"$$NO_DEPS --rm $appService $args''
        RES="$?"
        set -e

        $cleanup
        exit "$$RES"
      |]

down' :: App
down' = script $ do
  let apps = T.intercalate " " $ packs <$> [minBound .. maxBound :: WP]
  write
    [N.text|
    name () { docker inspect --format="{{ index .Name }}" "$$1";     }
    pds  () { printf 'Stopping %s%s... ' "$$1" "$(name "$$1")";      }
    pdr  () { printf 'Removing %s%s... ' "$$1" "$(name "$$1")";      }
    dds  () { docker stop   "$$1" >/dev/null 2>&1; printf 'Done.\n'; }
    ddk  () { docker kill   "$$1" >/dev/null 2>&1; printf 'Done.\n'; }
    ddr  () { docker rm -fv "$$1" >/dev/null    && printf 'Done.\n'; }

    for a in $apps; do
      docker ps  -qf "label=sh.reach.app-type=$$a" | while IFS= read -r d; do pds "$$d"; dds "$$d"; done
      docker ps -aqf "label=sh.reach.app-type=$$a" | while IFS= read -r d; do pdr "$$d"; ddr "$$d"; done
    done

    for a in reach reach-cli; do
      docker ps  -qf "ancestor=reachsh/$$a" | while IFS= read -r d; do pds "$$d"; ddk "$$d"; done
      docker ps -aqf "ancestor=reachsh/$$a" | while IFS= read -r d; do pdr "$$d"; ddr "$$d"; done
    done

    # Remove `dir-project` stragglers (e.g. `reachc` w/out ancestor, which can
    # happen if `reachc` is rebuilt while containers are still running)
    docker ps -aqf 'label=sh.reach.dir-project' | while IFS= read -r d; do
      pdr "$$d"; ddr "$$d"
    done
  |]
  forM_ (packs <$> [ALGO, ETH]) $ \c ->
    write
      [N.text|
    # Stop devnet containers w/ status == running
    docker ps -qf label=sh.reach.devnet-for=$c | while IFS= read -r d; do
      pds "$$d"; dds "$$d"
    done

    # Remove devnet stragglers (containers w/ status != running)
    docker ps -aqf label=sh.reach.devnet-for=$c | while IFS= read -r d; do
      pdr "$$d"; ddr "$$d"
    done
  |]
  write
    [N.text|
    if [ ! "$(docker network ls -qf 'name=reach-devnet' | wc -l)" -eq 0 ]; then
      printf 'Removing network "reach-devnet"... '
      docker network rm reach-devnet >/dev/null && printf 'Done.\n'
    fi
  |]

down :: Subcommand
down = command "down" $ info (pure down') d
  where
    d = progDesc "Halt all Dockerized Reach services and devnets"

mkDeprecatedDown :: String -> Subcommand
mkDeprecatedDown n = command n $ info (pure f) d
  where
    m = "`reach " <> n <> "` has been deprecated. Please use `reach down` instead."
    d = progDesc m
    f = do
      liftIO . putStrLn $ m
      down'

reactDown :: Subcommand
reactDown = mkDeprecatedDown "react-down"

rpcServerDown :: Subcommand
rpcServerDown = mkDeprecatedDown "rpc-server-down"

react :: Subcommand
react = command "react" $ info f d
  where
    d = progDesc "Run a simple React app"
    f = go <$> switchUseExistingDevnet <*> compiler
    go ued _ = do
      warnDeprecatedFlagUseExistingDevnet ued
      ConnectorMode c _ <- dieConnectorModeNotSpecified
      v@Var {..} <- asks e_var
      local (\e -> e {e_var = v {connectorMode = Just (ConnectorMode c Browser)}}) $ do
        Env {..} <- ask
        let dr = recursiveDisableReporting e_disableReporting
        dm@DockerMeta {..} <- mkDockerMetaProj <$> ask <*> projectPwdIndex <*> pure React
        dd <- devnetDeps e_disableReporting
        cargs <- forwardedCli "react"
        withCompose dm $ do
          unless e_disableReporting $ log'' "react" >>= write
          write
            [N.text|
          $dd
          $reachEx$dr compile $cargs
          docker-compose -f "$$TMP/docker-compose.yml" run \
            -e REACHC_ID="$whoami'" \
            --name $appService$$NO_DEPS --service-ports --rm $appService
        |]

rpcServer' :: Text -> Bool -> AppT Text
rpcServer' appService nolog = do
  Var {..} <- asks e_var
  dd <- devnetDeps nolog
  let dr = recursiveDisableReporting nolog
  pure
    [N.text|
    $dd
    $reachEx$dr compile
    docker-compose -f "$$TMP/docker-compose.yml" run \
      -e REACHC_ID="$whoami'" \
      --name $appService$$NO_DEPS --service-ports --rm $appService
  |]

rpcServer :: Subcommand
rpcServer = command "rpc-server" $ info f d
  where
    d = progDesc "Run a simple Reach RPC server"
    f = go <$> switchUseExistingDevnet
    go ued = do
      env@Env {..} <- ask
      prj <- projectPwdIndex
      let dm@DockerMeta {..} = mkDockerMetaProj env prj RPC
      dieConnectorModeBrowser
      warnDefRPCKey
      warnScaffoldDefRPCTLSPair prj
      warnDeprecatedFlagUseExistingDevnet ued
      withCompose dm $ do
        unless e_disableReporting $ log'' "rpc_server" >>= write
        rpcServer' appService e_disableReporting >>= write

rpcServerAwait' :: Int -> AppT Text
rpcServerAwait' t = do
  let t' = packs t
  Var {..} <- asks e_var
  pure
    [N.text|
    # Be patient while rpc-server comes online...
    i=0
    s=0
    while [ "$$i" -lt $t' ]; do
      sleep 1
      i=$$((i+1))
      s=$$(curl \
        --http1.1 \
        -sk \
        -o /dev/null \
        -w '%{http_code}' \
        -H "X-API-Key: $$REACH_RPC_KEY" \
        -X POST \
        "https://$rpcServer'':$rpcPort/health" || :)

      [ "$$s" -eq 200 ] && break
    done
  |]

rpcServerAwait :: Subcommand
rpcServerAwait = command "rpc-server-await" $ info f d
  where
    d = progDesc "Await RPC server availability"
    f = go <$> option auto (long "timeout-seconds" <> value 30)
    go t = do
      rsa <- rpcServerAwait' t
      script $
        write
          [N.text|
      $rsa

      if [ ! "$$s" -eq 200 ]; then
        echo "RPC server returned HTTP $$s after $$i seconds."
        exit 1
      fi
    |]

rpcRun :: Subcommand
rpcRun = command "rpc-run" $ info f $ fullDesc <> desc <> fdoc <> noIntersperse
  where
    desc = progDesc "Run an RPC server + frontend with development configuration"
    fdoc =
      footerDoc . Just $
        text "Example:"
          <$$> text " $ reach rpc-run python3 -u ./index.py"
    f =
      go <$> strArgument (metavar "EXECUTABLE")
        <*> manyArgs "EXECUTABLE"
    go exe args = do
      env@Env {..} <- ask
      prj <- projectPwdIndex
      rsa <- rpcServerAwait' 30
      let dm@DockerMeta {..} = mkDockerMetaProj env prj RPC
      runServer <- rpcServer' appService e_disableReporting
      let args' = intercalate " " args
      dieConnectorModeBrowser
      warnDefRPCKey
      warnScaffoldDefRPCTLSPair prj
      -- TODO detect if process is already listening on $REACH_RPC_PORT
      -- `lsof -i` cannot necessarily be used without `sudo`
      withCompose dm $ do
        unless e_disableReporting $ log'' "rpc_run" >>= write
        write
          [N.text|
        [ "x$$REACH_RPC_TLS_REJECT_UNVERIFIED" = "x" ] && REACH_RPC_TLS_REJECT_UNVERIFIED=0
        export REACH_RPC_TLS_REJECT_UNVERIFIED

        $runServer &
        spid="$!" # We'll SIGTERM `reach rpc-server` and all its child processes below

        $rsa

        killbg () {
          echo
          pkill -TERM -P "$$spid"
        }

        [ ! "$$s" -eq 200 ] \
          && killbg \
          && echo "RPC server returned HTTP $$s after $$i seconds." \
          && exit 1

        sh -c "$exe $args'"; killbg
      |]

devnet :: Subcommand
devnet = command "devnet" $ info f d
  where
    d = progDesc "Run only the devnet"
    f = go <$> switch (long "await-background" <> help "Run in background and await availability")
    go abg = do
      Env {..} <- ask
      ConnectorMode c m <- dieConnectorModeNotSpecified
      dieConnectorModeBrowser
      dd <- devnetDeps e_disableReporting
      let c' = packs c
      let s = devnetFor c
      let n = "reach-" <> s
      let a = if abg then " >/dev/null 2>&1 &" else ""
      let max_wait_s = "120"
      unless (m == Devnet) . liftIO $
        die "`reach devnet` may only be used when `REACH_CONNECTOR_MODE` ends with \"-devnet\"."
      withCompose mkDockerMetaStandaloneDevnet $ do
        write
          [N.text|
        $dd
        docker-compose -f "$$TMP/docker-compose.yml" run \
          -e REACHC_ID="$whoami'" --name $n$$NO_DEPS --service-ports --rm $s$a
      |]
        when abg $
          write
            [N.text|
        printf 'Bringing up devnet...'
        i=0
        while [ $$i -lt $max_wait_s ]; do
          if [ "$(docker ps -qf "label=sh.reach.devnet-for=$c'" | wc -l)" -gt 0 ]; then break; fi
          printf '.'
          sleep 1
          i=$$(( i + 1 ))
        done
        if [ $$i -eq $max_wait_s ]; then printf '\nSomething may have gone wrong.\n'; exit 1; fi
        printf ' Done.\n'
      |]

upgrade :: Subcommand
upgrade = command "upgrade" $ info f d
  where
    d = progDesc "Upgrade Reach"
    f = pure . liftIO $ putStrLn
      "`reach upgrade` has been deprecated. Please use `reach info` and `reach update` instead."

info' :: Subcommand
info' = command "info" $ info f d
  where
    d = progDesc "List available updates"
    f = g <$> switchInteractiveAUs
    g i = versionCompare' (not i) False False True

update :: Subcommand
update = command "update" $ info (pure f) d
  where
    d = progDesc "Perform available updates"
    f = versionCompare' True False True True

dockerReset :: Subcommand
dockerReset = command "docker-reset" $ info f d
  where
    d = progDesc "Kill and remove all Docker containers"
    f =
      go
        <$> switch
          (short 'y'
             <> long "even-non-reach"
             <> help "Acknowledge non-interactively that ALL containers will be halted")
    go = \case
      True -> script $ write reset
      False ->
        script $
          write
            [N.text|
      echo "Are you sure? This will halt non-Reach containers as well."
      printf 'Type "y" to continue... '
      read -r c

      case "$$c" in
        y|Y)
          $reset
          ;;
      esac
    |]
      where
        reset =
          [N.text|
      echo 'Killing all Docker containers...'
      # shellcheck disable=SC2046
      docker kill $$(docker ps -q) >/dev/null 2>&1 || :
      echo 'Removing all Docker containers...'
      # shellcheck disable=SC2046
      docker rm $$(docker ps -qa) >/dev/null 2>&1 || :
      echo 'Done.'
    |]

version' :: Subcommand
version' = command "version" $ info (pure f) d
  where
    d = progDesc "Display version"
    f = putStrLnPacked $ "reach " <> versionStr

numericVersion :: Subcommand
numericVersion = command "numeric-version" $ info (pure f) fullDesc
  where
    f = putStrLnPacked versionStr

help' :: Subcommand
help' = command "help" $ info f d
  where
    d = progDesc "Show usage"
    f = pure $ do
      Var {..} <- asks e_var
      script $ write [N.text| $reachEx --help |]

hashes :: Subcommand
hashes = command "hashes" $ info f d
  where
    d = progDesc "Display git hashes used to build each Docker image"
    h t i = write [N.text|
      if [ ! "$(docker image ls -q "reachsh/$i:$t")" = '' ]; then
        echo "$i:" "$(docker image inspect -f '{{json .Config.Env}}' reachsh/${i}:$t \
          | sed -E 's/^.*REACH_GIT_HASH=([^"]+).*$/\1/')"
      fi
    |]
    f = pure $ do
      v <- versionBy majMinPat . version'' <$> asks e_var
      let is = filter (/= "reach-cli") imagesAll
      script $ do
        h "latest" "reach-cli"
        forM_ is $ h v

zulu :: AppT String
zulu = formatShow iso8601Format <$> liftIO getCurrentTime

config :: Subcommand
config = command "config" $ info f d
  where
    d = progDesc "Configure default Reach settings"
    f = go <$> switch (short 'v' <> long "verbose" <> help "Print additional config info to `stdout`")
    go v' = do
      Var {..} <- asks e_var
      let v = if v' then " -v" else ""
      scriptWithConnectorModeOptional $ write [N.text| $reachEx config2$v $whoami' |]

config2 :: Subcommand
config2 = command "config2" $ info f mempty
  where
    f = go <$> switch (short 'v') <*> strArgument (metavar "REACHC_ID")
    nets = zip [0 ..] $ Nothing : (Just <$> [ALGO, ETH])
    maxNet = length . show . maybe 0 id . maximumMay . fmap fst $ nets
    lpad (i :: Int) = replicate (maxNet - (length $ show i)) ' ' <> show i
    go v' rcid = do
      Var {..} <- asks e_var
      efc <- envFileContainer
      efh <- envFileHost
      dcc <- asks e_dirConfigContainer
      dch <- asks e_dirConfigHost
      now <- pack <$> zulu

      _envExists <-
        (liftIO $ doesFileExist efc) >>= \case
          True -> liftIO $ do
            putStrLn $ "Reach detected an existing configuration file at " <> efh <> "."
            getY' "Would you like to back it up before creating a new one?" >>= \case
              False -> putStrLn "Skipped backup - use ctrl+c to abort overwriting!"
              True -> do
                let b = (</> "_backup")
                let n = "env-" <> unpack now
                createDirectoryIfMissing True (b dcc)
                copyFile efc (b dcc </> n)
                putStrLn $ "Backed up " <> efh <> " to " <> b dch </> n <> "."
            pure True
          False -> liftIO $ do
            putStrLn $ "Reach didn't detect a configuration file at " <> efh <> "."
            getY "Would you like to create one?"
            pure False

      dnet <- liftIO $ do
        (_, net) <- promptNetSet connectorMode >> netSet connectorMode
        pure $ maybe "" packs net

      let e =
            [N.text|
      # Automatically generated with `reach config` at $now
      export REACHC_ID=$rcid
      export REACH_CONNECTOR_MODE=$dnet
    |] <> "\n"

      liftIO $ do
        createDirectoryIfMissing True dcc
        putStrLn ""

        when v' $ do
          putStrLn $ "Writing " <> efh <> "..."
          T.putStrLn e
          putStrLn ""

        T.writeFile efc e
        putStrLn $ "Configuration has been saved in " <> efh <> ".\n"

      let efh' = pack efh
      let shell' = packs shell
      let sourceMe = write [N.text|
        if [ -f "$$P" ]; then
          echo "You appear to be using the \`$shell'\` shell, with environment configuration stored in $$P."
        fi

        if ! (grep -E '^if \[ -f $efh' \]; then . $efh'; fi$$' "$$P" >/dev/null); then
          echo
          printf "Enter 'y' to update %s if you'd like to automatically source $efh' at login: " "$$P"
          read -r r
          echo
          case $$r in
            [Yy]) printf '\nif [ -f $efh' ]; then . $efh'; fi\n' >> "$$P" ;;
               *) : ;;
          esac
        fi

        echo "Paste the following command into your terminal to activate your new configuration:"
        echo
        echo " $ . $$P"
        echo
      |]

      scriptWithConnectorModeOptional $ do
        case shell of
          ShellUnknown -> pure ()
          Bash -> do
            write [N.text|
              if [ -f ~/.bash_profile ]; then
                P=~/.bash_profile
              elif [ -f ~/.bash_login ]; then
                P=~/.bash_login
              else
                P=~/.profile
              fi
            |]
            sourceMe
          Zsh -> do
            write [N.text|
              P=${ZDOTDIR:-$${HOME}}/.zshenv
              touch "$$P"
            |]
            sourceMe
      where
        nope i = putStrLn $ show i <> " is not a valid selection."
        snet c = \case
          Nothing -> False
          Just cm -> cm == ConnectorMode c Devnet

        mkGetY n y m p = do
          putStr $ p <> m
          hFlush stdout >> getLine >>= \case
            z | L.upper z /= "Y" -> n
            _ -> y
        getY = mkGetY (exitWith ExitSuccess) (pure ()) " (Type 'y' if so): "
        getY' = mkGetY (pure False) (pure True) " (Type 'y' if so): "

        promptNetSet cm = do
          putStrLn "\nWould you like to set a default connector?"
          forM_ nets $ \case
            (_, Nothing) -> putStrLn $ "  " <> lpad 0 <> ": No preference - I want them all!"
            (i, Just n) ->
              putStrLn $
                "  " <> lpad i <> ": " <> show n
                  <> if snet n cm then " (Currently selected with `REACH_CONNECTOR_MODE`)" else ""
          putStr " Select from the numbers above: "
          hFlush stdout

        netSet cm =
          getLine >>= \n ->
            maybe (nope n >> promptNetSet cm >> netSet cm) (confirmNodef cm) $
              L.find ((==) n . show . fst) nets

        confirmNodef cm = \case
          n@(0, _) -> do
            T.putStrLn $
              intercalate
                "\n"
                [ "\nDeclining to set a default connector means you'll need to explicitly supply"
                , "`REACH_CONNECTOR_MODE` at the command-line or in your scripts. See:"
                , "\nhttps://docs.reach.sh/tool/#ref-usage-envvar-connector-mode"
                , "\nIf this isn't what you want you may re-run `reach config` at any time to select one."
                ]
            mkGetY (promptNetSet cm >> netSet cm) (pure n) " (Type 'y'): " "Continue anyway?"
          n -> pure n

data ImageHostAPIDockerHubResultImage = ImageHostAPIDockerHubResultImage
  { dhri_architecture :: String
  , dhri_digest :: Maybe Text
  }
  deriving (Show, Generic, Eq)

data ImageHostAPIDockerHubResult = ImageHostAPIDockerHubResult
  { dhr_name :: Text -- Tags e.g. "latest" or "0.1.7"
  , dhr_images :: [ImageHostAPIDockerHubResultImage]
  }
  deriving (Show, Generic)

data ImageHostAPIDockerHub = ImageHostAPIDockerHub
  { dh_next :: Maybe String
  , dh_results :: [ImageHostAPIDockerHubResult]
  }
  deriving (Show, Generic)

data DockerILS = DockerILS -- `docker image ls`
  { dils_Digest :: Text
  , dils_Repository :: Text
  , dils_Tag :: Text
  }
  deriving (Show, Generic, Eq)

parseJSON' :: (Generic a, A.GFromJSON A.Zero (Rep a)) => Int -> A.Value -> A.Parser a
parseJSON' i = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop i}

toEncoding' :: (Generic a, A.GToJSON' A.Encoding A.Zero (Rep a)) => Int -> a -> A.Encoding
toEncoding' i = A.genericToEncoding A.defaultOptions {A.fieldLabelModifier = drop i}

instance FromJSON ImageHostAPIDockerHubResultImage where parseJSON = parseJSON' 5

instance FromJSON ImageHostAPIDockerHubResult where parseJSON = parseJSON' 4

instance FromJSON ImageHostAPIDockerHub where parseJSON = parseJSON' 3

instance FromJSON DockerILS where parseJSON = parseJSON' 5

data TagFor
  = TFReach ReachVersionOf
  | TFReachCLI
  deriving (Eq, Ord, Show)

tagFor :: TagFor -> Text
tagFor = \case
  TFReachCLI -> "latest"
  TFReach (RVDate t) -> packs t
  TFReach (RVHash t) -> t
  TFReach (RVWithMaj RVDefault) -> "stable"
  TFReach (RVWithMaj RVStable) -> "stable"
  TFReach (RVWithMaj RVNumeric {rvEnvNumeric = RVEnvNumeric {..}}) ->
    packs rvEnvNumericMajor <> d rvEnvNumericMinor <> d rvEnvNumericPatch
  where
    d = maybe "" (("." <>) . packs)

instance ToJSON TagFor where toJSON = String . tagFor

instance FromJSON TagFor where
  parseJSON = withText "TagFor" $ \case
    "latest" -> pure TFReachCLI -- NB this is incidentally true today, but may not always be
    "" -> fail "Invalid `TagFor` \"\""
    t -> pure . either (error $ "Invalid `TagFor` \"" <> unpack t <> "\"") TFReach $ mkReachVersionOf' t

type DockerAssoc = M.Map Image (M.Map Digest [TagFor])

data ImageHostAPIFail
  = IHAFRetriesExhausted Text Int
  | IHAFUnexpectedResponse Text

data DockerAssocQueryL
  = DAQLMatch Image Digest [TagFor]
  | DAQLMissingImg Image TagRaw
  | DAQLMissingTag Image TagRaw
  deriving (Show)

data DockerAssocQueryR
  = DAQRMatch Image Digest [TagFor]
  | DAQRMissingImg Image TagRaw
  | DAQRMissingTag Image TagRaw
  deriving (Show)

data DockerAssocQuery
  = DAQSync Image Digest [TagFor]
  | DAQNewTags Image Digest [TagFor]
  | DAQNewDigestAvailable Image Digest [TagFor]
  | DAQNewConnectorAvailable Image Digest [TagFor]
  | DAQUnknownImg Image TagRaw
  | DAQUnknownTag Image TagRaw
  deriving (Show)

data VersionCompareIDTs = VersionCompareIDTs
  { vc_image :: Image'
  , vc_digest :: Digest
  , vc_tags :: [TagFor]
  } deriving (Show, Eq, Generic)

data VersionCompare = VersionCompare
  { vc_synced :: [VersionCompareIDTs]
  , vc_newDigest :: [VersionCompareIDTs]
  , vc_newTag :: [VersionCompareIDTs]
  , vc_newConnector :: [VersionCompareIDTs]
  } deriving (Show, Eq, Generic)

instance ToJSON VersionCompareIDTs where toEncoding = toEncoding' 3
instance ToJSON VersionCompare where toEncoding = toEncoding' 3

instance FromJSON VersionCompareIDTs where parseJSON = parseJSON' 3
instance FromJSON VersionCompare where parseJSON = parseJSON' 3

arch' :: String
arch' = case arch of
  "aarch64" -> "arm64"
  "x86_64" -> "amd64"
  a -> a

remoteDockerAssocFor :: FilePath -> FilePath -> Image -> Maybe String -> ImageHost -> IO (Either ImageHostAPIFail DockerAssoc)
remoteDockerAssocFor tmpC tmpH img mtag h = go
  where
    go = case h of
      -- https://docs.docker.com/docker-hub/api/latest/#tag/rate-limiting
      -- https://docs.docker.com/docker-hub/download-rate-limit/#other-limits
      DockerHub ->
        fetch 0 8 "hub.docker.com" dh_next dh_results uDockerHub >>= assoc aDH

    mkTag t = case img of
      "reach-cli" -> Just $ if t == tagFor TFReachCLI then [TFReachCLI] else []
      _ -> either (const Nothing) (\a -> Just [TFReach a]) $ mkReachVersionOf' t

    (img', img'') = (x, pack x) where x = "reachsh/" <> unpack img

    aDH a ImageHostAPIDockerHubResult {..} = maybe a id $ do
      d <- L.find ((== arch') . dhri_architecture) dhr_images >>= dhri_digest
      t <- mkTag dhr_name
      guard $ d /= "" && t /= []
      Just $ M.insertWith (<>) d t a

    uDockerHub = parseRequest_
      $ "https://hub.docker.com/v2/repositories/" <> img' <> "/tags?page_size=100&ordering=last_updated"
     <> maybe "" ("&name=" <>) mtag

    assoc f = either (pure . Left) $ pure . Right . M.singleton img'' . L.foldl' f mempty

    grh r l = readMay . toString
      =<< getResponseHeader ("X-" <> l) r `atMay` 0 <|> getResponseHeader l r `atMay` 0

    -- Exponential back-off with `c` rate-limit events + max `t` tries
    --  *OR*  max `t` tries using API's `X-Retry-After` header if available
    --  *AND* voluntarily easing off when rate-limit is nearly exhausted
    -- Note: DockerHub API's headers are sometimes inconsistent with their own docs, dropping `X-` prefix
    fetch c t fqdn nextPage results u = do
      r <- httpJSONEither u
      threadDelay . (* 100000) . fromMaybe 0 $ do
        l <- grh r "RateLimit-Remaining"
        pure $ if (l :: Int) < length imagesAll then 1 else 0
      if getResponseStatusCode r == 429
        then do
          if c > t
            then pure . Left . IHAFRetriesExhausted (pack img') $ float2Int t
            else do
              n <- getUnixTime
              ms <- pure . (* 1000000) . fromMaybe (2 ** c) $ do
                a <- grh r "Retry-After"
                readMay . show . udtSeconds $ UnixTime a 0 `diffUnixTime` n
              threadDelay $ float2Int ms
              fetch (c + 1) t fqdn nextPage results u
        else case getResponseBody r of
          Left e -> do
            let x = fqdn <> "-api-fail-" <> unpack img <> ".txt"
            T.writeFile (tmpC </> x) . toStrict $ pShowNoColor e
            pure . Left . IHAFUnexpectedResponse . pack $
              show h <> " API served unexpected response when querying `"
                <> img'
                <> "` tags. Output stashed in: "
                <> tmpH </> x
                <> "."
          Right r' ->
            -- Automatically de-paginate
            maybe (pure $ Right []) (fetch c t fqdn nextPage results . parseRequest_) (nextPage r')
              >>= either (pure . Left) (pure . Right . (results r' <>))

remoteUpdates :: Bool -> [Image] -> AppT (Text, DockerAssoc)
remoteUpdates psb imgs = do
  Env {..} <- ask

  let mtag = \case
        "reach-cli" -> Just . unpack $ tagFor TFReachCLI
        _ -> Nothing

  let d = threadDelay 2500000 *> putStr "." *> hFlush stdout *> d
  pdots <- liftIO . forkIO . when psb $ putStr "Please stand-by..." *> hFlush stdout *> d

  (sh, ts) <- liftIO . concurrently (httpLBS uriReachScript)
    $ forConcurrently imgs $ \i ->
      remoteDockerAssocFor e_dirTmpContainer e_dirTmpHost i (mtag i) $ imageHost e_var

  liftIO $ do
    killThread pdots
    when psb $ putStrLn ""

  let rn = "reach.new"
  liftIO $ case getResponseStatusCode sh of
    200 -> BSL.writeFile (e_dirTmpContainer </> rn) $ getResponseBody sh
    _ -> do
      let x = "reach-script-new-fail.txt"
      T.writeFile (e_dirTmpContainer </> x) . toStrict $ pShowNoColor sh
      putStrLn $ "Received unexpected response while fetching " <> uriReachScript <> "."
      putStrLn $ "Please open an issue at "
        <> unpack uriIssues
        <> " including the contents of "
        <> e_dirTmpHost </> x
        <> "."
      exitWith $ ExitFailure 1

  unless (all isRight ts) . liftIO $ do
    let us = [t | IHAFUnexpectedResponse t <- lefts ts]
    let rs = [(i, packs n) | (IHAFRetriesExhausted i n) <- lefts ts]
    when (length rs > 0) $ do
      putStrLn "Exhausted rate-limited retries for:"
      let li = L.foldl' max 0 [T.length i | (i, _) <- rs]
      let ln = L.foldl' max 0 [T.length n | (_, n) <- rs]
      let p l x = T.replicate (l - T.length x) " "
      mapM_
        T.putStrLn
        [ " * " <> i <> p li i
          <> " after "
          <> p ln n
          <> n
          <> " attempts."
        | (i, n) <- rs
        ]
      putStrLn "\nPlease wait awhile and try again, or, if the issue persists, reply to the following thread:"
      putStrLn " https://github.com/reach-sh/reach-lang/discussions/1030#discussioncomment-2940142\n"
    when (length us > 0) $ do
      mapM_ T.putStrLn us
      T.putStrLn $
        "Please open an issue at "
          <> uriIssues
          <> " including the contents of the files listed above."
    exitWith $ ExitFailure 1
  pure (pack $ e_dirTmpHost </> rn, L.foldl' M.union mempty $ rights ts)

imgTxt :: Image' -> Text
imgTxt (Image' x) = x

versionCompare' :: Bool -> Bool -> Bool -> Bool -> App
versionCompare' i' j' u' p' = scriptWithConnectorModeOptional $ do
  now <- zulu
  Env {e_var = Var {..}, ..} <- ask
  let x l s = if l then " --" <> s else ""
  let i = x i' "non-interactive"
  let j = x j' "json"
  let u = x u' "update"
  let p = x (p' && not j') "print-stand-by"
  let confE = "_docker" </> "ils-" <> now <> ".json"
  let confC = pack $ e_dirConfigContainer </> confE
  let confH = pack $ e_dirConfigHost </> confE
  write [N.text|
    f () {
      $q
    }

    mkdir -p "$$(dirname $confH)"
    echo '['  > $confH
    f | paste -s -d ' ' - | sed 's/} {/},\
    {/g' >> $confH
    echo ']' >> $confH

    $reachEx version-compare2$i$j$u$p --rm-ils --ils="$confC"
  |]
 where
  t = mappend ("docker image ls --digests --format "
    <> "'{ \"Digest\": \"{{.Digest}}\", \"Repository\": \"{{.Repository}}\", \"Tag\": \"{{.Tag}}\" }' ")

  q = T.intercalate " && \\\n" $ t . ("reachsh/" <>) <$> imagesAll

versionCompare :: Subcommand
versionCompare = command "version-compare" $ info f mempty
  where
    f = versionCompare'
      <$> switchNonInteractiveAUs
      <*> switchJSONAUs
      <*> switch (long "update")
      <*> pure False

versionCompare2 :: Subcommand
versionCompare2 = command "version-compare2" $ info f mempty
  where
    f = g
      <$> switchNonInteractiveAUs
      <*> switchJSONAUs
      <*> strOption (long "ils")
      <*> switch (long "rm-ils")
      <*> strOption (long "stub-remote" <> value "")
      <*> strOption (long "stub-script" <> value "")
      <*> switch (long "update")
      <*> switch (long "print-stand-by")
    g ni j l rl mr ms up psb = do
      Env {e_var = Var {..}, ..} <- ask
      assocL <- liftIO (A.eitherDecodeFileStrict' l) >>= \case
        Left e -> liftIO $ do
          u <- liftIO $ BSL.readFile l
          let x = "docker-digests-parse-fail.txt"
          T.writeFile (e_dirTmpContainer </> x) . toStrict $
            "Unparsed input: " <> decodeUtf8 u <> "\n" <> pShowNoColor e
          putStrLn "Failed to parse local Docker image digests."
          T.putStrLn $
            "Please open an issue at " <> uriIssues
              <> " including the contents of "
              <> pack (e_dirTmpHost </> x)
              <> "."
          exitWith $ ExitFailure 1
        Right ils' -> pure $ L.foldl' x mempty ils''
          where
            x a (r, d, t) =
              M.insertWith
                (<>)
                r
                (maybe (M.singleton d t) (M.insertWith (<>) d t) $ a !? r)
                a

            t' = either (const []) ((: []) . TFReach) . mkReachVersionOf'

            ils'' = flip mapMaybe ils' $ \DockerILS {..} -> do
              guard $ dils_Digest /= "<none>"
              let ir = dils_Repository `elem` (("reachsh/" <>) <$> imagesAll)
              let dt = case dils_Repository of
                    "reachsh/reach-cli" | dils_Tag == tagFor TFReachCLI -> const [TFReachCLI]
                    _ | ir -> t'
                    i -> error $ "Unrecognized image \"" <> unpack i <> "\""
              dr <- case dils_Repository of
                _ | ir -> Just dils_Repository
                _ -> Nothing
              Just (dr, dils_Digest, dt dils_Tag)

      when rl . liftIO $ removeFile l

      (latestScript, assocR) <- case (ms, mr) of
        _ | ms /= "" && mr /= "" -> (pack ms, ) . fromMaybe mempty <$> liftIO (A.decodeFileStrict' mr)
        _ -> remoteUpdates psb imagesAll

      -- Treat remote tags as unique and authoritative, but local tags might be
      -- repeated due to Docker manifest strangeness
      let isR i d = isJust $ assocR !? i >>= (!? d)

      let tfReach i t (d, ts) = isR i d && (TFReach <$> mkReachVersionOf' t) `elem` (Right <$> ts)
      let tfReachCLI i (d, ts) = isR i d && TFReachCLI `elem` ts

      let mkQ mi mt m a i t = case a !? i of
            Nothing -> mi i t
            Just i' -> case i of
              "reachsh/reach-cli" -> maybe' $ tfReachCLI i
              _ -> maybe' $ tfReach i t
              where
                maybe' x =
                  maybe (mt i t) (\(d, ts) -> m i d ts)
                    . L.find x
                    $ M.toList i'

      let ql = mkQ DAQLMissingImg DAQLMissingTag DAQLMatch assocL
      let qr = mkQ DAQRMissingImg DAQRMissingTag DAQRMatch assocR

      let query t' i' = case (ql i' t', qr i' t') of
            (_, DAQRMissingImg i t) -> DAQUnknownImg i t
            (_, DAQRMissingTag i t) -> DAQUnknownTag i t
            (DAQLMatch _ ld lts, DAQRMatch i rd rts)
              | dm && lts == rts -> DAQSync i rd rts
              | dm && length rts' < 1 -> DAQSync i rd rts
              | dm && length rts' > 0 -> DAQNewTags i rd rts'
              where
                dm = ld == rd
                rts' = filter (`notElem` lts) rts
            (DAQLMissingTag _ _, DAQRMatch i rd rts) -> mDorTs i rd rts
            (DAQLMissingImg li _, DAQRMatch ri rd rts)
              | li == ri && ri `elem` (pre <$> imagesForAllConnectors)
              -> mkNca (DAQNewDigestAvailable ri rd rts) ri rd rts
            (_, DAQRMatch i d rts) -> DAQNewDigestAvailable i d rts
            where
              pre = ("reachsh/" <>)
              -- When `REACH_CONNECTOR_MODE` is set (except for `$conn-live`)
              -- synchronizing `devnet-$conn` should be mandatory
              mkNca x ri rd rts = case connectorMode of
                Just (ConnectorMode c m) | m /= Live && ri `elem` (pre <$> imagesFor c) -> x
                _ -> DAQNewConnectorAvailable ri rd rts
              mDorTs i rd rts = maybe
                (DAQNewDigestAvailable i rd rts)
                (\lts -> DAQNewTags i rd $ filter (`notElem` lts) rts)
                $ assocL !? i >>= (!? rd)

      -- Avoid "double update" problem when current CLI image doesn't yet know a
      -- new numeric branch has been released, e.g. 0.1.7 -> 0.1.8
      let t = case rv version'' of
            RVWithMaj RVStable -> "stable"
            RVWithMaj RVDefault -> z
            RVWithMaj RVNumeric {rvEnvNumeric = RVEnvNumeric {..}}
              | rvEnvNumericPatch /= Nothing -> v
              | otherwise -> q (Just rvEnvNumericMajor) rvEnvNumericMinor
            _ -> v
            where
              v = versionBy majMinPat version''
              z = q Nothing Nothing
              -- `q` for highest numeric branch satisfying major + minor predicates:
              q mj mn = maybe v majMinPat . lastMay $ L.sort ns
                where
                  ns =
                    [ n
                    | TFReach (RVWithMaj n@RVNumeric {rvEnvNumeric = RVEnvNumeric {..}}) <-
                        L.nubOrd $ M.foldl' (\a b -> a <> L.concat (M.elems b)) [] assocR
                    , maybe True (== rvEnvNumericMajor) mj
                    , maybe True ((== rvEnvNumericMinor) . Just) mn
                    ]

      let images = query t . ("reachsh/" <>) <$> imagesAll

      let uImgs = [Image' a | DAQUnknownImg a _ <- images]
      let uTags = [(Image' x, y) | DAQUnknownTag x y <- images]

      let vc@VersionCompare{..} = VersionCompare
            [VersionCompareIDTs (Image' x) y z | DAQSync x y z <- images]
            [VersionCompareIDTs (Image' x) y z | DAQNewDigestAvailable x y z <- images]
            [VersionCompareIDTs (Image' x) y z | DAQNewTags x y z <- images]
            [VersionCompareIDTs (Image' x) y z | DAQNewConnectorAvailable x y z <- images]

      case (length uImgs > 0, length uTags > 0, j, up) of
        (True, _, _, _) -> liftIO $ do
          forM_ uImgs $ \a -> T.putStrLn $ "Unknown image: " <> imgTxt a <> "."
          T.putStrLn $ "Please open an issue at " <> uriIssues <> " including the failures listed above."
          exitWith $ ExitFailure 1

        (_, True, _, _) -> liftIO $ do
          forM_ uTags $ \(x, y) -> T.putStrLn $ "Unknown tag: " <> y <> " for image: " <> imgTxt x <> "."
          exitWith $ ExitFailure 1

        -- non-interactive JSON mode
        (_, _, True, _) -> scriptWithConnectorModeOptional $ do
          now <- zulu
          let confE = "_docker" </> "vc-" <> now <> ".json"
          let confF = e_dirConfigContainer </> confE
          let confC = pack confF
          let confH = pack $ e_dirConfigHost </> confE
          let ec = if length (vc_synced <> vc_newConnector) >= length images then "0" else "60"

          liftIO $ do
            createDirectoryIfMissing True $ takeDirectory confF
            BSLC8.writeFile confF $ encode vc

          write [N.text|
            if ! command -v diff >/dev/null; then
              echo '{ "noDiff": true, "script": false, "dockerH": "$confH", "dockerC": "$confC" }'
              exit $ec
            elif [ -f $latestScript ] && ! diff $reachEx $latestScript >/dev/null; then
              echo '{ "noDiff": false, "script": true, "dockerH": "$confH", "dockerC": "$confC"}'
              exit 60
            else
              echo '{ "noDiff": false, "script": false, "dockerH": "$confH", "dockerC": "$confC"}'
              exit $ec
            fi
          |]

        -- `reach update`
        (_, _, _, True) -> scriptWithConnectorModeOptional $ do
          update' False vc
          us <- updateScript
          write [N.text|
            if ! command -v diff >/dev/null; then
              # This is less polite but perhaps less trouble-prone
              $us
            elif [ -f $latestScript ] && ! diff $reachEx $latestScript >/dev/null; then
              $us
            fi
          |]

        -- plaintext modes
        _ -> scriptWithConnectorModeOptional $ do
          let prompt' p n y = (liftIO $ putStr ("\n" <> p <> " (Enter 'y' for yes): ")
                >> hFlush stdout >> getLine) >>= \case
                  z | L.upper z == "Y" -> y
                  _ -> n

          let s = T.take 8 . T.drop 7
          let m = maybe 0 id . maximumMay $ (\(VersionCompareIDTs x _ _) -> T.length $ imgTxt x)
                <$> vc_newDigest <> vc_newTag <> vc_newConnector <> vc_synced

          let p x = imgTxt x <> T.replicate (m - T.length (imgTxt x)) " "
          let n a = when (any ((> 0) . length) a) $ putStrLn ""

          let say = mapM_ $ \(VersionCompareIDTs x@(Image' _) y z) ->
                T.putStrLn $
                  " * " <> p x <> "  " <> s y <> ":  "
                    <> T.intercalate ", " (tagFor <$> L.sort z)

          let ancas (VersionCompareIDTs x' y zs) = "\n" <> a <> "\n" <> T.intercalate "\n" b where
                x = imgTxt x'
                a = [N.text| docker pull $x@$y |]
                b = zs <&> \z' -> let z = tagFor z' in [N.text| docker tag $x@$y $x:$z |]

          let addNewCAs = T.intercalate "\n" $ ancas <$> vc_newConnector

          let dch = pack e_dirConfigHost
          now <- pack <$> zulu

          let andTheScript' ec = case ni of
                True -> [N.text| exit 60 |]
                False -> [N.text|
                  printf "Type 'y' to update it; anything else aborts: "; read -r x
                  case "$$x" in
                    y|Y) mkdir -p "$dch/_backup"
                         cp $reachEx "$dch/_backup/reach-$now"
                         echo
                         echo "Backed up $reachEx to $dch/_backup/reach-$now."
                         cp $latestScript $reachEx \
                           && chmod +x $reachEx \
                           && echo "Replaced $reachEx with latest version." \
                           && rm -r "$$TMP" \
                           && exit $ec
                         ;;
                      *) echo
                         echo "Problems may arise when the script is out of sync with Reach's Docker images."
                         echo "Update your script by rerunning or following the instructions at https://docs.reach.sh/tool/#ref-install."
                         exit 60
                         ;;
                  esac
                |]

          let andTheScript ec = write [N.text|
            if ! command -v diff >/dev/null; then
              echo
              echo "A newer version of the \`reach\` script may be available."
              echo "Please install \`diff\` and retry or manually compare with $uriReachScript."
            elif [ -f $latestScript ] && ! diff $reachEx $latestScript >/dev/null; then
              echo
              echo "There's a newer version of the \`reach\` script available."
              $ats
            fi

            exit $ec
          |] where ats = andTheScript' ec

          let prompt ec p' y = prompt' p' (andTheScript ec) y

          let utd = "Reach's Docker images are up-to-date"
          let wyl = "Would you like to add the new connectors listed above?"
          case length (vc_synced <> vc_newConnector) >= length images of
            True -> case length vc_newConnector == 0 of
              True -> do
                liftIO $ do
                  putStrLn $ utd <> "."
                  say vc_synced
                andTheScript "0"

              False -> do
                liftIO $ do
                  putStrLn $ utd <> " but the following (optional) connectors are also available:"
                  say vc_newConnector
                  putStrLn "\nThe following images are fully synchronized:"
                  say vc_synced
                case ni of
                  True -> andTheScript "0"
                  False -> prompt "0" wyl $ do
                    write addNewCAs
                    andTheScript "0"

            False -> do
              liftIO $ do
                when (length vc_newDigest > 0) $ do
                  putStrLn "New images are available for:"
                  say vc_newDigest
                  n [vc_newTag, vc_newConnector, vc_synced]

                when (length vc_newTag > 0) $ do
                  putStrLn "New tags are available for:"
                  say vc_newTag
                  n [vc_newConnector, vc_synced]

                when (length vc_newConnector > 0) $ do
                  putStrLn "New (optional) connectors are available:"
                  say vc_newConnector
                  n [vc_synced]

                when (length vc_synced > 0) $ do
                  putStrLn "The following images are fully synchronized:"
                  say vc_synced

              case ni of
                True -> andTheScript "60"
                False -> do
                  prompt "60" "Would you like to perform an update?" $ do
                    when (length vc_newConnector > 0)
                      . prompt' wyl (pure ())
                        . write $ "echo\n" <> addNewCAs

                    when (length vc_newDigest > 0) $ write "echo"
                    forM_ vc_newDigest $ \(VersionCompareIDTs x' y zs) -> do
                      let x = imgTxt x'
                      write [N.text| docker pull $x@$y |]
                      forM_ zs $ \z' -> do
                        let z = tagFor z'
                        write [N.text| docker tag $x@$y $x:$z |]

                    forM_ vc_newTag $ \(VersionCompareIDTs x' y zs) -> do
                      let x = imgTxt x'
                      forM_ zs $ \z' -> do
                        let z = tagFor z'
                        write [N.text| docker tag $x@$y $x:$z |]

                    andTheScript "0"

updateScript :: AppT Text
updateScript = do
  Env {e_var = Var{..}, ..} <- ask
  now <- pack <$> zulu
  let dch = pack e_dirConfigHost
  pure [N.text|
    mkdir -p "$dch/_backup"
    cp $reachEx "$dch/_backup/reach-$now"
    echo
    echo "Backed up $reachEx to $dch/_backup/reach-$now."
    curl -sS -o $reachEx https://docs.reach.sh/reach \
      && chmod +x $reachEx \
      && echo "Replaced $reachEx with latest version." \
      && rm -r "$$TMP" \
      && exit 0
  |]

update' :: Bool -> VersionCompare -> App
update' s VersionCompare {..} = do
  forM_ vc_newDigest $ \(VersionCompareIDTs x' y zs) -> do
    let x = imgTxt x'
    write [N.text| docker pull $x@$y |]
    forM_ zs $ \z' -> do
      let z = tagFor z'
      write [N.text| docker tag $x@$y $x:$z |]

  forM_ vc_newTag $ \(VersionCompareIDTs x' y zs) -> do
    let x = imgTxt x'
    forM_ zs $ \z' -> do
      let z = tagFor z'
      write [N.text| docker tag $x@$y $x:$z |]

  when s $ updateScript >>= write

updateIDE :: Subcommand
updateIDE = command "update-ide" $ info f mempty where
  f = g
    <$> switch (long "script")
    <*> switch (long "rm-json") -- Delete input JSON once it's served its purpose
    <*> strOption (long "json")
  g s r j = scriptWithConnectorModeOptional $ do
    v <- liftIO $ A.eitherDecodeFileStrict' j
      >>= either (\e -> putStrLn e >> exitWith (ExitFailure 1)) pure
    when r . liftIO $ removeFile j
    update' s v

whoami' :: Text
whoami' = [N.text| ${REACHC_ID:-$(docker info --format '{{.ID}}' 2>/dev/null)} |]

whoami :: Subcommand
whoami = command "whoami" $ info f fullDesc
  where
    f = pure . script $ write [N.text| echo "$whoami'" |]

newtype GitHubGistResponse = GitHubGistResponse Text
instance FromJSON GitHubGistResponse where
  parseJSON = withObject "GitHubGistResponse" $ \o -> GitHubGistResponse <$> o .: "html_url"

support :: Subcommand
support = command "support" $ info h d
  where
    d = progDesc "Create a GitHub gist of index.rsh and index.mjs (default), or specify your own files to upload!"
    h = go <$> supportFromCommandLineHs
    go SupportToolArgs {sta_so = SupportOpts {}} = do
      rawArgs <- liftIO getArgs
      let rawArgs' = dropWhile (/= "support") rawArgs
      let useArgs xs = upload =<< mapM z xs 
      case tailMay rawArgs' of
        Nothing -> impossible $ "support args do not start with 'support': " <> show rawArgs
        Just [] -> liftIO $ useArgs [ "index.rsh", "index.mjs" ]
        Just xs -> liftIO $ useArgs xs
    f i c = i .= object
      [ "content" .= if T.null (T.strip $ pack c) then "// (Empty source file)" else c
      , "language" .= ("JavaScript" :: String)
      , "type" .= ("application/javascript" :: String)
      ]
    z i = doesFileExist i >>= \case
      False -> do
          putStrLn $ "Couldn't find the following file: " <> i
          putStrLn "\nNothing uploaded"
          exitWith $ ExitFailure 1
      -- "Contents files can't be in subdirectories or include '/' in the name"
      True -> (\a -> [f (K.fromText $ T.replace "/" "\\" $ pack i) a]) <$> readFile i
    clientId = "c4bfe74cc8be5bbaf00e" :: String
    is l = maybe False (== pack l) . headMay
    by a x = maybe (putStrLn ("Missing field `" <> x <> "`.") >> exitWith (ExitFailure 1)) pure
      $ (headMay $ filter (is x) a) >>= (`atMay` 1)
    req u x = fmap (map (T.splitOn "=") . T.splitOn "&" . pack . BSLC8.unpack . getResponseBody)
        $ setRequestBodyJSON (object x)
      <$> parseRequest ("POST " <> u)
      >>= httpLBS
    upload arrayOfPairs = liftIO $ do
      a <- req "https://github.com/login/device/code"
        [ "client_id" .= clientId
        , "scope" .= ("gist" :: String)
        ]
      deviceCode <- a `by` "device_code"
      userCode <- a `by` "user_code"
      T.putStrLn [N.text|
        Please enter $userCode at https://github.com/login/device.

        Type 'y' after successful authorization to upload your files:
      |]
      hFlush stdout >> getChar >>= \c -> unless (toUpper c == 'Y') $ do
        putStrLn "\nNo files were uploaded. Run `reach support` again to retry."
        exitWith $ ExitFailure 1
      t <- req "https://github.com/login/oauth/access_token"
        [ "client_id" .= clientId
        , "device_code" .= deviceCode
        , "grant_type" .= ("urn:ietf:params:oauth:grant-type:device_code" :: String)
        ]
      -- @TODO: Save accessToken; git-credential-store
      -- Warning: Permission errors when doing this^
      when (null $ filter (is "access_token") t) $ do
        case headMay $ filter (is "error") t of
          Nothing -> putStrLn "Upload unsuccessful; couldn't find an authorization code or error!"
          Just _ -> t `by` "error" >>= T.putStrLn . (pack "\nError while acquiring access token:\n" <>)
        exitWith $ ExitFailure 1
      gat <- unpack <$> t `by` "access_token"
      -- @TODO: Also add output of reach hashes!
      parseRequest "POST https://api.github.com/gists"
        >>= httpBS
          . setRequestHeader "User-Agent" [ BSI.packChars "reach" ]
          . setRequestHeader "Authorization" [ BSI.packChars ("token " <> gat) ]
          . setRequestHeader "Accept" [ BSI.packChars "application/vnd.github.v3+json" ]
          . setRequestBodyJSON (object [ "files" .= object (concat arrayOfPairs) ])
        >>= Y.decodeThrow . getResponseBody
        >>= \(GitHubGistResponse r) -> T.putStrLn $ "\n" <> [N.text|
              Your gist is viewable at:
              $r
            |]

log' :: Subcommand
log' = command "log" $ info f fullDesc
  where
    f =
      g <$> strOption (long "user-id")
        <*> strOption (long "initiator")
    g w i = liftIO $ startReport (Just w) i >>= \r -> r $ Right ()

log'' :: Text -> AppT Text
log'' i = do
  Var {..} <- asks e_var
  case ci of
    True -> pure [N.text| # Skip logging $i on CI |]
    False ->
      pure
        [N.text|
      log_$i () { $reachEx log --user-id=$whoami' --initiator=$i >/dev/null 2>&1; }
      log_$i &
    |]

failNonAbsPaths :: Env -> IO ()
failNonAbsPaths Env {..} =
  mapM_
    (\p -> unless (isAbsolute p) . die $ p <> " is not an absolute path.")
    [ e_dirEmbed
    , e_dirPwdContainer
    , e_dirPwdHost
    , e_dirTmpContainer
    , e_dirTmpHost
    ]

initTemplates :: Env -> IO [String]
initTemplates = fmap f . listDirectory . dirInitTemplates'
  where
    f = (:) "  - default" . fmap ("  - " <>) . L.sort . filter (/= "_default")

main :: IO ()
main = do
  eff <- newIORef InProcess
  env <- mkEnv eff Nothing
  arg <- getArgs
  its <- case execParserPure defaultPrefs (flip info forwardOptions $ (,) <$> env <*> manyArgs "") arg of
    Success (e, _) -> initTemplates e
    _ -> pure []
  let header' = "reach " <> versionHashStr <> " - Reach command-line tool"
  let cs =
        clean
          <> compile
          <> config
          <> devnet
          <> dockerReset
          <> down
          <> hashes
          <> help'
          <> info'
          <> init' its
          <> react
          <> rpcRun
          <> rpcServer
          <> run'
          <> scaffold
          <> support
          <> update
          <> version'
  let hs =
        internal
          <> commandGroup "hidden subcommands"
          <> numericVersion
          <> reactDown
          <> rpcServerAwait
          <> rpcServerDown
          <> unscaffold
          <> whoami
          <> log'
          <> upgrade
          <> versionCompare
          <> versionCompare2
          <> updateIDE
          <> config2
  let cli =
        Cli
          <$> env
          <*> (hsubparser cs <|> hsubparser hs <**> helper)
  customExecParser (prefs showHelpOnError) (info cli (header header' <> fullDesc))
    >>= \Cli {..} -> do
      cc <- lookupEnv "CIRCLECI"
      rd <- lookupEnv "REACH_DOCKER"
      let cenv_no = c_env { e_disableReporting = True }
      let cenv =
            case (cc, rd) of
              (Just "true", _) -> cenv_no
              (_, Just "0") -> cenv_no
              _ -> c_env
      failNonAbsPaths cenv
      runReaderT c_cmd cenv
      readIORef eff >>= \case
        InProcess -> pure ()
        Script t -> case e_emitRaw cenv of
          True -> T.putStrLn t
          False -> do
            T.writeFile (e_dirTmpContainer cenv </> "out.sh") t
            exitWith $ ExitFailure 42
