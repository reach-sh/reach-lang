{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bits
import Data.Char
import Data.Either
import Data.IORef
import Data.Text (Text, intercalate, pack, unpack, stripEnd)
import Data.Time
import Data.Time.Format.ISO8601
import Data.Tuple.Extra (first)
import Options.Applicative
import Options.Applicative.Help.Pretty ((<$$>), text)
import Safe
import System.Directory.Extra
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import Text.Parsec (ParsecT, parse, runParserT, eof, try)
import Text.Parsec.Char
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator (count)
import Text.ParserCombinators.Parsec.Token

import Reach.CommandLine
import Reach.Report
import Reach.Util
import Reach.Version

import qualified Data.List.Extra as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NeatInterpolation as N

data Effect
  = Script Text
  | InProcess

data Connector
  = ALGO
  | CFX
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
  { rvEnvNumericMajor :: Maybe Integer
  , rvEnvNumericMinor :: Maybe Integer
  , rvEnvNumericPatch :: Maybe Integer
  }

data RVWithMaj'
  = RVDefault
  | RVStable
  | RVNumeric
    { rvEnvNumeric :: RVEnvNumeric
    , rvMajor :: Integer
    , rvMinor :: Integer
    , rvPatch :: Integer
    }

data ReachVersionOf
  = RVWithMaj RVWithMaj'
  | RVHash Text
  | RVDate Day

data ReachVersion = ReachVersion
  { rvEnvRaw :: Maybe Text
  , rv :: ReachVersionOf
  }

mkReachVersion :: IO ReachVersion
mkReachVersion = do
  mt <- lookupEnv "REACH_VERSION"
  let rvEnvRaw = pack <$> mt
  rv <- case mt of
    Nothing -> pure $ RVWithMaj RVDefault
    Just "" -> pure $ RVWithMaj RVDefault
    Just "stable" -> pure $ RVWithMaj RVStable
    Just m -> either (iv m) pure $ parse (numeric <|> hash <|> datestamp m) "" m
  pure $ ReachVersion {..}

 where
  TokenParser {..} = makeTokenParser emptyDef
  xx = eof *> pure Nothing
  iv = const . die . ("Invalid `REACH_VERSION`: " <>)
  ti = toInteger

  hash = RVHash . pack <$> (try $ count 8 (oneOf $ ['0'..'9'] <> ['a'..'f']) <* eof)

  datestamp m = maybe (fail m) (pure . RVDate) $ parseTimeM False defaultTimeLocale "%Y-%m-%d" m

  numeric = try $ do
    rvEnvNumeric@RVEnvNumeric {..} <- RVEnvNumeric
      <$> (Just <$> (optional (string "v") *> decimal))
      <*> ((Just <$> (dot *> decimal)) <|> xx)
      <*> ((Just <$> (dot *> decimal <* eof)) <|> xx)
    let rvMajor = maybe (ti major) id rvEnvNumericMajor
    let rvMinor = maybe (if rvMajor /= ti major then 0 else ti minor) id rvEnvNumericMinor
    let rvPatch = maybe (if rvMinor /= ti minor then 0 else ti patch) id rvEnvNumericPatch
    pure $ RVWithMaj RVNumeric {..}

majMinPat :: RVWithMaj' -> Text
majMinPat = \case
  RVDefault -> pack versionStr
  RVStable -> pack versionStr
  RVNumeric {..} -> T.intercalate "." $ map packs [ rvMajor, rvMinor, rvPatch ]

majMin :: RVWithMaj' -> Text
majMin = \case
  RVDefault -> pack compatibleVersionStr
  RVStable -> pack compatibleVersionStr
  RVNumeric {..} -> T.intercalate "." $ map packs [ rvMajor, rvMinor ]

maj :: RVWithMaj' -> Text
maj = \case
  RVDefault -> packs major
  RVStable -> packs major
  RVNumeric {..} -> packs rvMajor

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
mkShell = lookupEnv "SHELL" >>= \case
  Nothing -> pure (ShellUnknown, "")
  Just "" -> pure (ShellUnknown, "")
  Just s -> do
    let p a b = try $ optional (string "/" *> many (try $ many alphaNum *> string "/"))
             *> string a *> eof *> pure b
    either (const $ pure (ShellUnknown, pack s)) (pure . (, pack s)) $ parse
       (p "bash" Bash <|> p "zsh" Zsh) "" s

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
  when (rpcKey == defRPCKey) . liftIO . T.putStrLn
    $ "Warning! Using development RPC key: REACH_RPC_KEY=" <> defRPCKey <> "."

warnDeprecatedFlagUseExistingDevnet :: Bool -> App
warnDeprecatedFlagUseExistingDevnet u = when u . liftIO . putStrLn
  $ "`--use-existing-devnet` is deprecated and no longer necessary - please remove."

warnDeprecatedFlagIsolate :: Bool -> App
warnDeprecatedFlagIsolate i = when i . liftIO . putStrLn
  $ "`--isolate` is deprecated and no longer has any effect - please remove."

dieConnectorModeBrowser :: App
dieConnectorModeBrowser = connectorMode <$> asks e_var >>= \case
  Just (ConnectorMode _ Browser) -> liftIO . die
    $ "`REACH_CONNECTOR_MODE` cannot select the `browser` target; `browser`"
   <> " is only available via the Reach standard library."
  _ -> pure ()

dieConnectorModeNotSpecified :: AppT ConnectorMode
dieConnectorModeNotSpecified = connectorMode <$> asks e_var >>= \case
  Just cm -> pure cm
  Nothing -> liftIO . die . unpack . intercalate "\n"
    $ "Missing `REACH_CONNECTOR_MODE` environment variable - must be one of:"
    : L.sort s
   <> [ "Reach recommends adding this variable to your shell's profile settings by running `reach config`. See:"
      , " - https://docs.reach.sh/tool/usage/#ref-usage-config"
      ]
 where
  s = [ " * " <> packs c | c <- [ minBound .. maxBound :: Connector ]]
   <> [ " * " <> packs c <> "-" <> packs m
      | c <- [ minBound .. maxBound :: Connector ]
      , m <- [ minBound .. maxBound :: Mode ]
      ]

diePathContainsParentDir :: FilePath -> IO ()
diePathContainsParentDir x = when (any (== "..") $ splitDirectories x) . die
  $ x <> " cannot contain parent directories (\"..\")."

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
  liftIO $ (,) <$> doesFileExist (dock key) <*> doesFileExist (dock crt) >>= \case
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
  rpcTLSRejectUnverified <- lookupEnv "REACH_RPC_TLS_REJECT_UNVERIFIED"
    >>= maybe (pure True) (pure . (/= "0"))
  reachEx <- lookupEnv "REACH_EX"
    >>= maybe (die "Unset `REACH_EX` environment variable") packed
  connectorMode <- do
    let e = "REACH_CONNECTOR_MODE"
    m e (pure Nothing) (pure . Just) >>= \case
      Nothing -> pure Nothing
      Just rcm -> runParserT ((ConnectorMode <$> pConnector <*> pMode) <* eof) () "" rcm
        >>= either (const . die $ "Invalid `" <> e <> "`: " <> rcm) (pure . Just)
  ci <- truthyEnv <$> lookupEnv "CI"
  (shell, shellRaw) <- mkShell
  pure $ Var {..}

mkScript :: Text -> App -> App
mkScript connectorMode' wrapped = do
  Var {..} <- asks e_var
  let debug' = if debug then "REACH_DEBUG=1\n" else ""
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

  asks e_effect >>= liftIO . flip writeIORef (Script
     $ [N.text|
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
  rcm <- asks (connectorMode . e_var) >>= pure . maybe "" packs
  mkScript rcm wrapped

script :: App -> App
script = mkScript ""

write :: Text -> App
write t = asks e_effect >>= liftIO . flip modifyIORef w where
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

-- Left: 3rd party; Right: Reach
type Image = Either Text Text

imagesCommon :: [Image]
imagesCommon =
  [ Right "reach"
  , Right "reach-cli"
  , Right "react-runner"
  , Right "rpc-server"
  , Right "runner"
  ]

imagesFor :: Connector -> [Image]
imagesFor = \case
  ALGO -> [ Right $ devnetFor ALGO, Left "postgres:11-alpine" ]
  CFX -> [ Right $ devnetFor CFX ]
  ETH -> [ Right $ devnetFor ETH ]

imagesForAllConnectors :: [Image]
imagesForAllConnectors = L.foldl' (<>) [] $ imagesFor <$> [ minBound .. maxBound ]

serviceConnector :: Env -> ConnectorMode -> [Text] -> Text -> Text -> IO Text
serviceConnector Env {..} (ConnectorMode c m) ports appService' v = do
  let ports' = case ports of
        [] -> "[]"
        ps -> intercalate "\n    " $ map ("- " <>) ps
  let n = show m <> "-" <> (toLower <$> show c)
  let d = packs c
  fmt <- T.readFile $ e_dirEmbed </> "docker" </> "service-" <> n <> ".yml"
  let labels = [N.text| - "sh.reach.devnet-for=$d" |]
  pure
    . swap "REACH_VERSION" v
    . swap "PORTS" ports'
    . swap "NETWORK" "reach-devnet"
    . swap "APP_SERVICE" (if appService' == "" then "" else "-" <> appService')
    . swap "LABELS" labels
    $ fmt

connectorEnv :: Env -> ConnectorMode -> IO Text
connectorEnv Env {..} (ConnectorMode c m) = do
  let c' = toLower <$> show c
  T.readFile $ e_dirEmbed </> "docker" </> "service-" <> show m <> "-" <> c' <> "-env.yml"

devnetFor :: Connector -> Text
devnetFor = \case
  ALGO -> "devnet-algo"
  CFX -> "devnet-cfx"
  ETH -> "devnet-eth"

pConnector :: ParsecT String () IO Connector
pConnector =
      f ALGO "ALGO"
  <|> f CFX "CFX"
  <|> f ETH "ETH"
 where f a b = const a <$> string b

pMode :: ParsecT String () IO Mode
pMode =
      f Devnet "devnet"
  <|> f Live "live"
  <|> f Browser "browser"
  <|> string "" *> pure Devnet
 where f a b = const a <$> try (char '-' *> string b)

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
mkScaffold Project {..} = Scaffold
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

appProj' :: FilePath -> Text
appProj' = T.toLower . pack . takeBaseName . dropTrailingPathSeparator

mkDockerMetaProj :: Env -> Project -> WP -> DockerMeta
mkDockerMetaProj (Env {..}) (p@Project {..}) wp = DockerMeta {..} where
  appProj = case wp of
              React -> ""
              _ -> appProj' projDirHost
  appService = case wp of
                React -> "react-runner"
                _ -> "reach-app-" <> appProj
  appImage = case wp of
                RPC -> "reachsh/rpc-server"
                _ -> "reachsh/" <> appService
  appImageTag = appImage <> ":" <> versionBy majMinPat (version'' e_var)
  compose = WithProject wp p

mkDockerMetaStandaloneDevnet :: DockerMeta
mkDockerMetaStandaloneDevnet = DockerMeta {..} where
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
      ""  -> pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."
      "." -> pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."
      _ -> ifM (andM [ pure . (< 2) . length $ splitDirectories a, not <$> doesDirectoryExist a ])
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
        (WithProject Console _, ALGO, Devnet) -> [ "9392" ]
        (_, ALGO, _) -> [ "4180:4180", "8980:8980", "9392:9392" ]
        (_, CFX, _) -> [ "12537:12537" ]
        (_, ETH, _) -> [ "8545:8545" ]
  let reachConnectorMode = packs cm
  let debug' = if debug then "1" else ""
  let projDirHost' = case compose of
        StandaloneDevnet -> ""
        WithProject _ Project {..} -> pack projDirHost
  let devnetALGO = [N.text|
        - ALGO_SERVER=http://reach-devnet-algo
        - ALGO_PORT=4180
        - ALGO_INDEXER_SERVER=http://reach-devnet-algo
        - ALGO_INDEXER_PORT=8980
      |]
  let devnetCFX = [N.text|
        - CFX_DEBUG
        - CFX_NODE_URI=http://reach-devnet-cfx:12537
        - CFX_NETWORK_ID=999
      |]
  let deps'' d = [N.text|
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
        (CFX, Devnet) -> (Just c, devnetCFX)
        (CFX, Browser) -> (Just c, devnetCFX)
  connEnv <- case compose of
    StandaloneDevnet -> liftIO $ connectorEnv env cm
    WithProject Console _ -> liftIO $ connectorEnv env cm
    WithProject React _ -> pure [N.text|
      volumes:
        - $projDirHost':/app/src
      ports:
        - "3000:3000"
      stdin_open: true
      tty: true
      environment:
        - REACH_DEBUG
        - REACH_CONNECTOR_MODE
        - REACH_ISOLATED_NETWORK
        - REACT_APP_REACH_DEBUG=$debug'
        - REACT_APP_REACH_CONNECTOR_MODE=$reachConnectorMode
        - REACT_APP_REACH_ISOLATED_NETWORK=$${REACH_ISOLATED_NETWORK}
        $extraEnv
      $deps
    |]
    WithProject RPC _ -> pure [N.text|
      volumes:
        - $projDirHost'/build:/app/build
        - $projDirHost'/tls:/app/tls
      ports:
        - "$rpcPort:$rpcPort"
      stdin_open: true
      tty: true
      environment:
        - REACH_DEBUG
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
  connSvs <- case (m, compose) of
    (Live, _) -> pure ""
    (_, StandaloneDevnet) -> stdConnSvs
    (_, WithProject Console _) -> stdConnSvs
    (_, WithProject React _) -> stdConnSvs
    (_, WithProject RPC _) -> stdConnSvs
  let build = case compose of
        WithProject Console _ -> [N.text|
           build:
             context: $projDirHost'
        |]
        _ -> ""
  let e_dirTmpHost' = pack e_dirTmpHost
  let appService' = case compose of
        StandaloneDevnet -> ""
        _ -> [N.text|
               $appService:
                 image: $appImageTag
                 networks:
                   - reach-devnet
                 extra_hosts:
                   - "host.docker.internal:host-gateway"
                 labels:
                   - "sh.reach.dir-tmp=$e_dirTmpHost'"
                   - "sh.reach.dir-project=$projDirHost'"
                 $build
                 $connEnv
             |]
  let f = [N.text|
     version: '3.5'

     networks:
       reach-devnet:
         name: reach-devnet

     services:
       $connSvs

       $appService'
    |]
  liftIO $ scaff True (e_dirTmpContainer </> "docker-compose.yml") (notw f)
  wrapped
 where
  notw = intercalate "\n" . fmap stripEnd . T.lines

argAppOrDir :: Parser FilePath
argAppOrDir = strArgument
  $ metavar "APP or DIR"
 <> help "May be either a module name without its extension (e.g. \"index\") \
         \or a relative sub-directory path"
 <> value ""

manyArgs :: String -> Parser [Text]
manyArgs n = many . strArgument
  $ metavar "ARGS"
 <> help ("Zero or more arguments to be passed into " <> n)

switchUseExistingDevnet :: Parser Bool
switchUseExistingDevnet = switch
  $ long "use-existing-devnet"
 <> help "This switch has been deprecated and is no longer necessary"
 <> internal

switchIsolate :: Parser Bool
switchIsolate = switch
  $ long "isolate"
 <> help "This switch has been deprecated and no longer has any effect"
 <> internal

switchQuiet :: Parser Bool
switchQuiet = switch
  $ long "quiet"
 <> help "Withhold progress messages"

recursiveDisableReporting :: Bool -> Text
recursiveDisableReporting d = if d then " --disable-reporting" else ""

mkEnv :: IORef Effect -> Maybe Var -> IO (Parser Env)
mkEnv eff mv = do
  var <- maybe mkVar pure mv
  pure $ Env
    <$> strOption (long "dir-embed" <> internal <> value "/app/embed")
    <*> strOption (long "dir-project-container" <> internal <> value "/app/src")
    <*> strOption (long "dir-project-host" <> internal)
    <*> strOption (long "dir-tmp-container" <> internal <> value "/app/tmp")
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
  (_, _, _, f) <- liftIO . execParser . flip info forwardOptions $ (,,,)
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
scaffold = command "scaffold" $ info f d where
  d = progDesc "Set up Docker scaffolding for a simple app in the current directory"
  f = go <$> switchIsolate <*> switchQuiet
  go i q = projectPwdIndex >>= scaffold' i q

unscaffold :: Subcommand
unscaffold = command "unscaffold" $ info f fullDesc where
  f = go <$> switchIsolate <*> switchQuiet <*> argAppOrDir
  go i quiet appOrDir = do
    warnDeprecatedFlagIsolate i
    Scaffold {..} <- mkScaffold <$> projectFrom appOrDir
    liftIO $ do
      forM_ [ containerDockerfile, containerPackageJson ] $ \n ->
        whenM (doesFileExist n) $ do
          when (not quiet) . putStrLn $ "Deleting " <> takeFileName n <> "..."
          removeFile n
      when (not quiet) $ putStrLn "Done."

clean :: Subcommand
clean = command "clean" . info f $ fullDesc <> desc <> fdoc where
  desc = progDesc "Delete 'build/$MODULE.$IDENT.mjs'"
  fdoc = footerDoc . Just
     $  text "MODULE is \"index\" by default"
   <$$> text "IDENT  is \"main\"  by default"
   <$$> text ""
   <$$> text "If:"
   <$$> text " * MODULE is a directory then `cd $MODULE && rm -f \"build/index.$IDENT.mjs\";"
   <$$> text " * MODULE is <something-else> then `rm -f \"build/$MODULE.$IDENT.mjs\""
  go m i = script $ write [N.text|
      MODULE="$m"

      if [ ! "$m" = "index" ] && [ -d "$m" ]; then
        cd "$m" || exit 1
        MODULE="index"
      fi

      rm -f "build/$$MODULE.$i.mjs"
    |]
  f = go
    <$> strArgument (metavar "MODULE" <> value "index" <> showDefault)
    <*> strArgument (metavar "IDENT"  <> value "main"  <> showDefault)

compile :: Subcommand
compile = command "compile" $ info f d where
  d = progDesc "Compile an app"
  f = go <$> compiler
  go (CompilerToolArgs {..}) = do
    Env{e_var = Var {..}, ..} <- ask
    rawArgs <- liftIO $ getArgs
    let rawArgs' = dropWhile (/= "compile") rawArgs
    let argsl = intercalate " " . map pack . filter (/= "--disable-reporting") $ case rawArgs' of
                 "compile" : x -> x
                 _ -> impossible $ "compile args do not start with 'compile': " <> show rawArgs
    let args = argsl <> recursiveDisableReporting e_disableReporting
    let CompilerOpts {..} = cta_co
    let v = versionBy majMinPat version''
    let ci' = if ci then "true" else ""
    let ports = if (elem "--sim" rawArgs || co_sim) then "-p 3001:3001" else ""
    liftIO $ do
      diePathContainsParentDir co_source
      maybe (pure ()) diePathContainsParentDir co_mdirDotReach
      maybe (pure ()) diePathContainsParentDir co_moutputDir
    let reachc_release = [N.text| stack build && stack exec -- reachc $args |]
    let reachc_dev = [N.text| stack build --fast && stack exec -- reachc $args |]
    let reachc_prof = [N.text|
        stack build --profile --fast \
          && stack exec --profile -- reachc $args +RTS -p
      |]
    scriptWithConnectorModeOptional $ do
      realpath
      write [N.text|
        REACH="$$(realpath "$reachEx")"
        HS="$$(dirname "$$REACH")/hs"
        ID=$$($whoami')

        export REACH

        if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
          ~/.local/bin/reachc $argsl --disable-reporting

        elif [ "$${REACH_DOCKER}" = "0" ] \
          && [ -d "$${HS}/.stack-work"  ] \
          && which stack >/dev/null 2>&1; then

          export STACK_YAML="$${REACH_STACK_YAML:-"$${HS}/stack.yaml"}"
          export REACHC_ID=$${ID}

          # https://github.com/koalaman/shellcheck/wiki/SC2155
          REACHC_HASH="$$("$${HS}/../scripts/git-hash.sh")"
          export REACHC_HASH

          (cd "$$HS" && make expand)

          if [ "$${REACHC_RELEASE}" = "Y" ]; then
            $reachc_release
          elif [ "$${REACHC_PROFILE}" = "Y" ]; then
            $reachc_prof
          else
            $reachc_dev
          fi
        else
          docker run \
            --rm \
            --volume "$$PWD:/app" \
            -u "$(id -ru):$(id -rg)" \
            -e REACH_CONNECTOR_MODE \
            -e REACH_IDE \
            -e "REACHC_ID=$${ID}" \
            -e "CI=$ci'" \
            $ports \
            reachsh/reach:$v \
            $args
        fi
      |]

init' :: [String] -> Subcommand
init' its = command "init" . info f $ d <> foot where
  d = progDesc "Set up source files for a simple app in the current directory"
  f = go <$> strArgument (metavar "TEMPLATE" <> value "_default" <> showDefault)
  foot = footerDoc . Just
    $ text "Available templates:\n"
   <> text (L.intercalate "\n" its)
   <> text "\n\nAborts if index.rsh or index.mjs already exist"
  go template = do
    Env {..} <- ask
    Project {..} <- projectPwdIndex
    ts <- dirInitTemplates
    let tmpl n = ts </> n
    let app = "index" -- Used to be configurable via CLI; now we try to nudge default of "index"
    liftIO $ do
      tmpl' <- ifM (doesDirectoryExist $ tmpl template)
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
  l <- if nolog then pure "" else log'' "devnet_create"
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
run' = command "run" . info f $ d <> noIntersperse where
  d = progDesc "Run a simple app"
  f = go <$> switchIsolate
         <*> argAppOrDir
         <*> manyArgs "APP"
  go i appOrDir args = do
    (appOrDir', args') <- liftIO $ do
      "run" : as <- dropWhile (/= "run") <$> getArgs
      pure $ case L.split (== "--") as of
        []:[cs] -> ("", map pack cs)
        bs:[cs] -> (headDef "" (dropWhile (== "--isolate") bs), map pack cs)
        _ -> (appOrDir, args)

    warnDeprecatedFlagIsolate i
    dieConnectorModeBrowser
    e@Env {..} <- ask
    proj@Project {..} <- projectFrom appOrDir'
    dd <- devnetDeps e_disableReporting
    let Var {..} = e_var
    let Scaffold {..} = mkScaffold proj
    toClean <- filterM (fmap not . liftIO . doesFileExist . fst)
      [ (containerPackageJson, hostPackageJson)
      , (containerDockerfile, hostDockerfile)
      , (containerGitIgnore, hostGitIgnore)
      , (containerDockerIgnore, hostDockerIgnore)
      ]
    cleanup <- intercalate "\n" <$> forM toClean (pure . pack . ("rm " <>) . snd)
    let rsh = projDirContainer </> unpack projName <> ".rsh"
    let mjs = projDirContainer </> unpack projName <> ".mjs"
    let bjs = projDirContainer </> "build" </> unpack projName <> ".main.mjs"
    let abortIfAbsent p = liftIO . whenM (not <$> doesFileExist p)
          . die $ takeFileName p <> " doesn't exist."
    abortIfAbsent rsh
    abortIfAbsent mjs
    scaffold' False True proj

    let target = pack $ projDirRel </> unpack projName <> ".rsh"
    let dr = recursiveDisableReporting e_disableReporting
    let recompile' = [N.text|
      set +e
      $reachEx$dr compile $target
      RES="$?"
      set -e

      if [ ! "$$RES" -eq 0 ]; then
        $cleanup
        exit "$$RES"
      fi
    |]

    recompile <- liftIO $ ifM (not <$> doesFileExist bjs)
      (pure $ Just recompile')
      $ do
        b <- modificationTime <$> getFileStatus bjs
        r <- modificationTime <$> getFileStatus rsh
        pure $ if r > b then Just recompile' else Nothing

    let dm@DockerMeta {..} = mkDockerMetaProj e proj Console
    let dockerfile' = pack hostDockerfile
    let projDirHost' = pack projDirHost
    let args'' = intercalate " " . map (<> "'") . map ("'" <>) $ projName : args'
    withCompose dm . scriptWithConnectorMode $ do
      write dd
      maybe (pure ()) write recompile
      unless e_disableReporting $ log'' "run" >>= write
      write [N.text|
        cd $projDirHost'
        CNAME="$appService-$$$$"

        set +e
        docker build -f $dockerfile' --tag=$appImageTag . \
          && docker-compose -f "$$TMP/docker-compose.yml" run \
            --name "$$CNAME"$$NO_DEPS --rm $appService $args''
        RES="$?"
        set -e

        $cleanup
        exit "$$RES"
      |]

down' :: App
down' = script $ do
  write [N.text|
    name () { docker inspect --format="{{ index .Name }}" "$$1"; }

    # Stop app containers w/ status == running
    docker ps -qf label=sh.reach.dir-tmp | while IFS= read -r d; do
      printf 'Stopping %s%s... ' "$$d" "$(name "$$d")"
      docker stop "$$d" >/dev/null && printf 'Done.\n'
    done

    # Remove app stragglers (containers w/ status != running)
    docker ps -aqf label=sh.reach.dir-tmp | while IFS= read -r d; do
      printf 'Removing %s%s... ' "$$d" "$(name "$$d")"
      docker rm -fv "$$d" >/dev/null && printf 'Done.\n'
    done
  |]
  forM_ (packs <$> [ ALGO, CFX, ETH ]) $ \c -> write [N.text|
    # Stop devnet containers w/ status == running
    docker ps -qf label=sh.reach.devnet-for=$c | while IFS= read -r d; do
      printf 'Stopping %s%s... ' "$$d" "$(name "$$d")"
      docker stop "$$d" >/dev/null && printf 'Done.\n'
    done

    # Remove devnet stragglers (containers w/ status != running)
    docker ps -aqf label=sh.reach.devnet-for=$c | while IFS= read -r d; do
      printf 'Removing %s%s... ' "$$d" "$(name "$$d")"
      docker rm -fv "$$d" >/dev/null && printf 'Done.\n'
    done
  |]
  write [N.text|
    if [ ! "$(docker network ls -qf 'name=reach-devnet' | wc -l)" -eq 0 ]; then
      printf 'Removing network "reach-devnet"... '
      docker network rm reach-devnet >/dev/null && printf 'Done.\n'
    fi
  |]

down :: Subcommand
down = command "down" $ info (pure down') d where
  d = progDesc "Halt all Dockerized Reach services and devnets"

mkDeprecatedDown :: String -> Subcommand
mkDeprecatedDown n = command n $ info (pure f) d where
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
react = command "react" $ info f d where
  d = progDesc "Run a simple React app"
  f = go <$> switchUseExistingDevnet <*> compiler
  go ued _ = do
    warnDeprecatedFlagUseExistingDevnet ued
    ConnectorMode c _ <- dieConnectorModeNotSpecified
    v@Var {..} <- asks e_var
    local (\e -> e { e_var = v { connectorMode = Just (ConnectorMode c Browser) }}) $ do
      Env {..} <- ask
      let dr = recursiveDisableReporting e_disableReporting
      dm@DockerMeta {..} <- mkDockerMetaProj <$> ask <*> projectPwdIndex <*> pure React
      dd <- devnetDeps e_disableReporting
      cargs <- forwardedCli "react"
      withCompose dm . scriptWithConnectorMode $ do
        unless e_disableReporting $ log'' "react" >>= write
        write [N.text|
          $dd
          $reachEx$dr compile $cargs
          docker-compose -f "$$TMP/docker-compose.yml" run \
            --name $appService$$NO_DEPS --service-ports --rm $appService
        |]

rpcServer' :: Text -> Bool -> AppT Text
rpcServer' appService nolog = do
  Var {..} <- asks e_var
  dd <- devnetDeps nolog
  let dr = recursiveDisableReporting nolog
  pure [N.text|
    $dd
    $reachEx$dr compile
    docker-compose -f "$$TMP/docker-compose.yml" run \
      --name $appService$$NO_DEPS --service-ports --rm $appService
  |]

rpcServer :: Subcommand
rpcServer = command "rpc-server" $ info f d where
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
    withCompose dm . scriptWithConnectorMode $ do
      unless e_disableReporting $ log'' "rpc_server" >>= write
      rpcServer' appService e_disableReporting >>= write

rpcServerAwait' :: Int -> AppT Text
rpcServerAwait' t = do
  let t' = packs t
  Var {..} <- asks e_var
  pure [N.text|
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
rpcServerAwait = command "rpc-server-await" $ info f d where
  d = progDesc "Await RPC server availability"
  f = go <$> option auto (long "timeout-seconds" <> value 30)
  go t = do
    rsa <- rpcServerAwait' t
    script $ write [N.text|
      $rsa

      if [ ! "$$s" -eq 200 ]; then
        echo "RPC server returned HTTP $$s after $$i seconds."
        exit 1
      fi
    |]

rpcRun :: Subcommand
rpcRun = command "rpc-run" $ info f $ fullDesc <> desc <> fdoc <> noIntersperse where
  desc = progDesc "Run an RPC server + frontend with development configuration"
  fdoc = footerDoc . Just
     $  text "Example:"
   <$$> text " $ reach rpc-run python3 -u ./index.py"
  f = go <$> strArgument (metavar "EXECUTABLE")
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
    withCompose dm . scriptWithConnectorMode $ do
      unless e_disableReporting $ log'' "rpc_run" >>= write
      write [N.text|
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
devnet = command "devnet" $ info f d where
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
    let max_wait_s = "120";
    unless (m == Devnet) . liftIO
      $ die "`reach devnet` may only be used when `REACH_CONNECTOR_MODE` ends with \"-devnet\"."
    withCompose mkDockerMetaStandaloneDevnet . scriptWithConnectorMode $ do
      write [N.text|
        $dd
        docker-compose -f "$$TMP/docker-compose.yml" run --name $n$$NO_DEPS --service-ports --rm $s$a
      |]
      when abg $ write [N.text|
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
upgrade = command "upgrade" $ info f d where
  d = progDesc "Upgrade Reach"
  f = pure . liftIO . exitWith $ ExitFailure 50

update :: Subcommand
update = command "update" $ info (pure f) d where
  d = progDesc "Update Reach Docker images"
  f = do
    ReachVersion {..} <- asks (version'' . e_var)
    let ts = case rv of
              RVWithMaj v -> [ "latest", maj v, majMin v, majMinPat v ]
              RVHash v -> [ v ]
              RVDate v -> [ packs v ]
    let ps = either (\i -> [ "docker pull " <> i ])
                   $ \i -> [ "docker pull reachsh/" <> i <> ":" <> t | t <- ts ]
    let w = write . intercalate "\n" . ps
    scriptWithConnectorModeOptional $ do
      case rv of -- Always pull `reach-cli:latest` regardless of `REACH_VERSION`
        RVWithMaj _ -> pure ()
        _ -> write [N.text| docker pull reachsh/reach-cli:latest |]
      mapM_ w imagesCommon
      connectorMode <$> asks e_var >>= \case
        Nothing -> mapM_ w imagesForAllConnectors
        Just (ConnectorMode c _) -> do
          let is = imagesFor c
          let is' = filter ((==) Nothing . flip L.elemIndex is) imagesForAllConnectors
          mapM_ w is
          forM_ is' $ \i' -> do
            let i = either id ("reachsh/" <>) i'
            let ps' = intercalate "\n" $ ps i'
            write [N.text|
              if [ ! "$(docker image ls -q "$i")" = '' ]; then
                $ps'
              fi
            |]

dockerReset :: Subcommand
dockerReset = command "docker-reset" $ info f d where
  d = progDesc "Kill and remove all Docker containers"
  f = go <$> switch (short 'y'
            <> long "even-non-reach"
            <> help "Acknowledge non-interactively that ALL containers will be halted")
  go = \case
    True -> script $ write reset
    False -> script $ write [N.text|
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
    reset = [N.text|
      echo 'Killing all Docker containers...'
      # shellcheck disable=SC2046
      docker kill $$(docker ps -q) >/dev/null 2>&1 || :
      echo 'Removing all Docker containers...'
      # shellcheck disable=SC2046
      docker rm $$(docker ps -qa) >/dev/null 2>&1 || :
      echo 'Done.'
    |]

version' :: Subcommand
version' = command "version" $ info (pure f) d where
  d = progDesc "Display version"
  f = putStrLnPacked $ "reach " <> versionStr

numericVersion :: Subcommand
numericVersion = command "numeric-version" $ info (pure f) fullDesc where
  f = putStrLnPacked versionStr

help' :: Subcommand
help' = command "help" $ info f d where
  d = progDesc "Show usage"
  f = pure $ do
    Var {..} <- asks e_var
    script $ write [N.text| $reachEx --help |]

hashes :: Subcommand
hashes = command "hashes" $ info f d where
  d = progDesc "Display git hashes used to build each Docker image"
  f = pure $ do
    v <- versionBy majMinPat . version'' <$> asks e_var
    let is = rights $ imagesCommon <> imagesForAllConnectors
    script . forM_ is $ \i -> write [N.text|
      if [ ! "$(docker image ls -q "reachsh/$i:$v")" = '' ]; then
        echo "$i:" "$(docker image inspect -f '{{json .Config.Env}}' reachsh/${i}:$v | sed -E 's/^.*REACH_GIT_HASH=([^"]+).*$/\1/')"
      fi
    |]

config :: Subcommand
config = command "config" $ info f d where
  d = progDesc "Configure default Reach settings"
  f = go <$> switch (short 'v' <> long "verbose" <> help "Print additional config info to `stdout`")
  nets = zip [0..] $ Nothing : (Just <$> [ ALGO, CFX, ETH ])
  maxNet = length . show . maximum . fmap fst $ nets
  lpad (i :: Int) = replicate (maxNet - (length $ show i)) ' ' <> show i
  go v' = do
    Var {..} <- asks e_var
    efc <- envFileContainer
    efh <- envFileHost
    dcc <- asks e_dirConfigContainer
    dch <- asks e_dirConfigHost
    now <- pack . formatShow iso8601Format <$> liftIO getCurrentTime

    envExists <- (liftIO $ doesFileExist efc) >>= \case
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

    let e = [N.text|
      # Automatically generated with `reach config` at $now
      export REACH_CONNECTOR_MODE=$dnet
    |]

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
    let sourceMe = do
          let shell' = packs shell
          let s = case envExists of
                True -> [N.text|
                  echo "Run the following command to activate your new configuration:"
                  echo
                  echo " $ . $$P"
                  echo
                |]
                False -> [N.text|
                  echo "Run the following command to activate your new configuration and make it permanent:"
                  echo
                  printf ' $ printf '\''\\nif [ -f $efh' ]; then . $efh'; fi\\n'\'' >> %s && . %s\n\n' "$$P" "$$P"
                |]
          write [N.text|
            if [ -f "$$P" ]; then
              echo "You appear to be using the \`$shell'\` shell, with environment configuration stored in $$P."
              $s
            else
              echo "Reach didn't detect a suitable $$P"
              cat << 'EOF'
            and cannot recommend steps to make this configuration permanent.
            Please consult the `$shell'` documentation for tips on how to set
            up your environment correctly or file an issue at:

            https://github.com/reach-sh/reach-lang/issues
            EOF
            fi
          |]

    case shell of
      ShellUnknown -> liftIO $ T.putStrLn [N.text|
        Reach didn't recognize shell "$shellRaw" and cannot recommend steps
        to make this configuration permanent. Please consult your shell's
        documentation in order to complete configuration manually.

        If you believe this is a mistake or would like to request support for
        a new shell, please file an issue at:

        https://github.com/reach-sh/reach-lang/issues
      |]

      Bash -> script $ do
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

      Zsh -> script $ do
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
        (i, Just n) -> putStrLn $ "  " <> lpad i <> ": " <> show n
          <> if snet n cm then " (Currently selected with `REACH_CONNECTOR_MODE`)" else ""
      putStr " Select from the numbers above: "
      hFlush stdout

    netSet cm = getLine >>= \n -> maybe (nope n >> promptNetSet cm >> netSet cm) (confirmNodef cm)
      $ L.find ((==) n . show . fst) nets

    confirmNodef cm = \case
      n@(0, _) -> do
        T.putStrLn $ intercalate "\n"
          [ "\nDeclining to set a default connector means you'll need to explicitly supply"
          , "`REACH_CONNECTOR_MODE` at the command-line or in your scripts. See:"
          , "\nhttps://docs.reach.sh/ref-usage.html#%28env._.R.E.A.C.H_.C.O.N.N.E.C.T.O.R_.M.O.D.E%29"
          , "\nIf this isn't what you want you may re-run `reach config` at any time to select one."
          ]
        mkGetY (promptNetSet cm >> netSet cm) (pure n) " (Type 'y'): " "Continue anyway?"
      n -> pure n

whoami' :: Text
whoami' = "docker info --format '{{.ID}}' 2>/dev/null"

whoami :: Subcommand
whoami = command "whoami" $ info f fullDesc where
  f = pure . script $ write whoami'

log' :: Subcommand
log' = command "log" $ info f fullDesc where
  f = g <$> strOption (long "user-id")
        <*> strOption (long "initiator")
  g w i = liftIO $ startReport (Just w) i >>= \r -> r $ Right ()

log'' :: Text -> AppT Text
log'' i = do
  Var {..} <- asks e_var
  case ci of
    True -> pure [N.text| # Skip logging $i on CI |]
    False -> pure [N.text|
      log_$i () { $reachEx log --user-id=$$($whoami') --initiator=$i >/dev/null 2>&1; }
      log_$i &
    |]

failNonAbsPaths :: Env -> IO ()
failNonAbsPaths Env {..} =
  mapM_ (\p -> unless (isAbsolute p) . die $ p <> " is not an absolute path.")
    [ e_dirEmbed
    , e_dirPwdContainer
    , e_dirPwdHost
    , e_dirTmpContainer
    , e_dirTmpHost
    ]

initTemplates :: Env -> IO [String]
initTemplates = fmap f . listDirectory . dirInitTemplates' where
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
  let cs = config
        <> compile
        <> clean
        <> init' its
        <> run'
        <> down
        <> scaffold
        <> react
        <> rpcServer
        <> rpcRun
        <> devnet
        <> upgrade
        <> update
        <> dockerReset
        <> version'
        <> hashes
        <> help'
  let hs = internal
        <> commandGroup "hidden subcommands"
        <> numericVersion
        <> reactDown
        <> rpcServerAwait
        <> rpcServerDown
        <> unscaffold
        <> whoami
        <> log'
  let cli = Cli
        <$> env
        <*> (hsubparser cs <|> hsubparser hs <**> helper)
  customExecParser (prefs showHelpOnError) (info cli (header header' <> fullDesc))
    >>= \Cli {..} -> do
      failNonAbsPaths c_env
      runReaderT c_cmd c_env
    >>  readIORef eff
    >>= \case
      InProcess -> pure ()
      Script t -> case e_emitRaw c_env of
        True -> T.putStrLn t
        False -> do
          T.writeFile (e_dirTmpContainer c_env </> "out.sh") t
          exitWith $ ExitFailure 42
