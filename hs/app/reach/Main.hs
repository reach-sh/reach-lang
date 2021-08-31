{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bits
import Data.Char
import Data.IORef
import Data.Text hiding (any, filter, length, map, toLower)
import Options.Applicative
import Options.Applicative.Help.Pretty ((<$$>), text)
import Safe
import System.Directory.Extra
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Files
import Text.Parsec (ParsecT, runParserT, char, string, eof, try)

import Reach.CommandLine
import Reach.Util
import Reach.Version

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NeatInterpolation as N


data Effect
  = Script Text
  | InProcess


data Connector
  = Algo
  | Cfx
  | Eth
  deriving Eq

data Mode
  = Devnet
  | Live
  | Browser
  deriving Eq

data ConnectorMode = ConnectorMode Connector Mode
  deriving Eq


instance Show Connector where
  show = \case
    Algo -> "ALGO"
    Cfx -> "CFX"
    Eth -> "ETH"

instance Show Mode where
  show = \case
    Devnet -> "devnet"
    Live -> "live"
    Browser -> "browser"

instance Show ConnectorMode where
  show (ConnectorMode c m) = show c <> "-" <> show m


data Var = Var
  { reachEx :: Text
  , connectorMode :: ConnectorMode
  , debug :: Bool
  , rpcKey :: Text
  , rpcPort :: Text
  , rpcServer'' :: Text
  , rpcTlsCrt :: Text
  , rpcTlsKey :: Text
  , rpcTlsPassphrase :: Text
  , rpcTlsRejectUnverified :: Bool
  , version'' :: Text
  , versionShort :: Text
  }


data Env = Env
  { e_dirEmbed :: FilePath
  , e_dirPwdContainer :: FilePath
  , e_dirPwdHost :: FilePath
  , e_dirTmpContainer :: FilePath
  , e_dirTmpHost :: FilePath
  , e_emitRaw :: Bool
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


--------------------------------------------------------------------------------
defRpcKey :: Text
defRpcKey = "opensesame"


defRpcTlsPassphrase :: Text
defRpcTlsPassphrase = "rpc-demo"


warnDefRpcKey :: App
warnDefRpcKey = do
  Var {..} <- asks e_var
  when (rpcKey == defRpcKey) . liftIO . T.putStrLn
    $ "Warning! Using development RPC key: REACH_RPC_KEY=" <> defRpcKey <> "."


warnDeprecatedFlagUseExistingDevnet :: Bool -> App
warnDeprecatedFlagUseExistingDevnet u = when u . liftIO . putStrLn
  $ "`--use-existing-devnet` is deprecated and no longer necessary - please remove."


warnDeprecatedFlagIsolate :: Bool -> App
warnDeprecatedFlagIsolate i = when i . liftIO . putStrLn
  $ "`--isolate` is deprecated and no longer has any effect - please remove."


dieConnectorModeBrowser :: App
dieConnectorModeBrowser = connectorMode <$> asks e_var >>= \case
  ConnectorMode _ Browser -> liftIO . die
    $ "`REACH_CONNECTOR_MODE` cannot select the `browser` target; `browser`"
   <> " is only available via the Reach standard library."
  _ -> pure ()


diePathContainsParentDir :: FilePath -> IO ()
diePathContainsParentDir x = when (any (== "..") $ splitDirectories x) . die
  $ x <> " cannot contain parent directories (\"..\")."


warnScaffoldDefRpcTlsPair :: Project -> App
warnScaffoldDefRpcTlsPair Project {..} = do
  Env {..} <- ask

  let warnDev = putStrLn "Warning! The current TLS certificate is only suitable for development purposes."

  let embd r = e_dirEmbed </> "sh" </> "_common" </> "rpc" </> r
  let dock r = projDirContainer </> "tls" </> r
  let host r = projDirHost </> "tls" </> r

  let orw = ownerReadMode .|. ownerWriteMode
  let key = unpack $ rpcTlsKey e_var
  let crt = unpack $ rpcTlsCrt e_var

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
  rpcKey <- q "REACH_RPC_KEY" (pure defRpcKey)
  rpcTlsPassphrase <- q "REACH_RPC_TLS_PASSPHRASE" (pure defRpcTlsPassphrase)
  rpcTlsKey <- q "REACH_RPC_TLS_KEY" (pure "reach-server.key")
  rpcTlsCrt <- q "REACH_RPC_TLS_CRT" (pure "reach-server.crt")
  version'' <- q "REACH_VERSION" (packed versionStr)

  debug <- lookupEnv "REACH_DEBUG" >>= maybe (pure False) (const $ pure True)

  rpcTlsRejectUnverified <- lookupEnv "REACH_RPC_TLS_REJECT_UNVERIFIED"
    >>= maybe (pure True) (pure . (/= "0"))

  reachEx <- lookupEnv "REACH_EX"
    >>= maybe (die "Unset `REACH_EX` environment variable") packed

  connectorMode <- do
    rcm <- m "REACH_CONNECTOR_MODE" (pure "ETH-devnet") (pure . id)
    runParserT ((ConnectorMode <$> pConnector <*> pMode) <* eof) () "" rcm
      >>= either (const . die $ "Invalid `REACH_CONNECTOR_MODE`: " <> rcm) pure

  let versionShort = case version'' of
        "stable" -> pack compatibleVersionStr
        v -> a <> "." <> b where
          f ('v':s) = s
          f s = s
          v' = splitOn "." . pack . f $ unpack v
          a = maybe "0" id $ atMay v' 0
          b = maybe "0" id $ atMay v' 1

  pure Var {..}


--------------------------------------------------------------------------------
script :: App -> App
script wrapped = do
  Var {..} <- asks e_var
  let debug' = if debug then "REACH_DEBUG=1\n" else ""

  let rpcTlsRejectUnverified' = case rpcTlsRejectUnverified of
        True -> ""
        False -> "REACH_RPC_TLS_REJECT_UNVERIFIED=0\n"

  let defOrBlank e d r = if d == r then [N.text| $e=$d |] <> "\n" else ""

  let connectorMode' = packs connectorMode

  asks e_effect >>= liftIO . flip writeIORef (Script
     $ [N.text|
          #!/bin/sh
          set -e
       |]
    <> "\n\n"
    <> debug'
    <> rpcTlsRejectUnverified'

    -- Don't leak production `REACH_RPC_KEY` or `REACH_RPC_TLS_PASSPHRASE`
    <> defOrBlank "REACH_RPC_KEY" defRpcKey rpcKey
    <> defOrBlank "REACH_RPC_TLS_PASSPHRASE" defRpcTlsPassphrase rpcTlsPassphrase

    <> [N.text|
          REACH_CONNECTOR_MODE=$connectorMode'
          REACH_RPC_PORT=$rpcPort
          REACH_RPC_SERVER=$rpcServer''
          REACH_RPC_TLS_CRT=$rpcTlsCrt
          REACH_RPC_TLS_KEY=$rpcTlsKey
          REACH_VERSION=$version''

          export REACH_CONNECTOR_MODE
          export REACH_DEBUG
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
realpath = writeFrom "sh/_common/realpath.sh"


swap :: Text -> Text -> Text -> Text
swap a b src = T.replace ("${" <> a <> "}") b src


packs :: Show a => a -> Text
packs = pack . show


reachImages :: [Text]
reachImages =
  [ "reach"
  , "reach-cli"
  , "react-runner"
  , "rpc-server"
  , "runner"
  , "devnet-algo"
  , "devnet-cfx"
  , "devnet-eth"
  ]


--------------------------------------------------------------------------------
serviceConnector :: Env -> ConnectorMode -> [Text] -> Text -> Text -> IO Text
serviceConnector Env {..} (ConnectorMode c m) ports appService' version'' = do
  let ports' = case ports of
        [] -> "[]"
        ps -> intercalate "\n    " $ map ("- " <>) ps

  let n = show m <> "-" <> (toLower <$> show c)
  let d = packs c

  fmt <- T.readFile $ e_dirEmbed
    </> "sh" </> "_common" </> "_docker-compose" </> "service-" <> n <> ".yml"

  let labels = [N.text| - "sh.reach.devnet-for=$d" |]

  pure
    . swap "REACH_VERSION" version''
    . swap "PORTS" ports'
    . swap "NETWORK" "reach-devnet"
    . swap "APP_SERVICE" (if appService' == "" then "" else "-" <> appService')
    . swap "LABELS" labels
    $ fmt


connectorEnv :: Env -> ConnectorMode -> IO Text
connectorEnv Env {..} (ConnectorMode c m) = do
  let c' = toLower <$> show c
  T.readFile $ e_dirEmbed </> "sh" </> "_common" </> "_docker-compose"
    </> "service-" <> show m <> "-" <> c' <> "-env.yml"


--------------------------------------------------------------------------------
devnetFor :: Connector -> Text
devnetFor = \case
  Algo -> "devnet-algo"
  Cfx -> "devnet-cfx"
  Eth -> "devnet-eth"


pConnector :: ParsecT String () IO Connector
pConnector =
      f Algo "ALGO"
  <|> f Cfx "CFX"
  <|> f Eth "ETH"
 where f a b = const a <$> string b


pMode :: ParsecT String () IO Mode
pMode =
      f Devnet "devnet"
  <|> f Live "live"
  <|> f Browser "browser"
  <|> string "" *> pure Devnet
 where f a b = const a <$> try (char '-' *> string b)


--------------------------------------------------------------------------------
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
  | Rpc

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


mkDockerMetaConsole :: Project -> DockerMeta
mkDockerMetaConsole p@Project {..} = DockerMeta {..} where
  appProj = appProj' projDirHost
  appService = "reach-app-" <> appProj
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"
  compose = WithProject Console p


mkDockerMetaReact :: Project -> DockerMeta
mkDockerMetaReact p = DockerMeta {..} where
  appProj = ""
  appService = "react-runner"
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"
  compose = WithProject React p


mkDockerMetaRpc :: Env -> Project -> DockerMeta
mkDockerMetaRpc Env {..} p@Project {..} = DockerMeta {..} where
  appProj = appProj' projDirHost
  appService = "reach-app-" <> appProj
  appImage = "reachsh/rpc-server"
  appImageTag = appImage <> ":" <> versionShort e_var
  compose = WithProject Rpc p


mkDockerMetaStandaloneDevnet :: DockerMeta
mkDockerMetaStandaloneDevnet = DockerMeta {..} where
  appProj = "reach-devnet"
  appService = ""
  appImage = ""
  appImageTag = ""
  compose = StandaloneDevnet


--------------------------------------------------------------------------------
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
  liftIO . T.readFile $ e_dirEmbed </> "sh" </> "subcommand" </> "scaffold" </> p


withCompose :: DockerMeta -> App -> App
withCompose DockerMeta {..} wrapped = do
  env@Env {..} <- ask
  let Var {..} = e_var
  let cm@(ConnectorMode c m) = connectorMode

  let connPorts = case (compose, c, m) of
        (_, _, Live) -> []
        (WithProject Console _, Algo, Devnet) -> [ "9392" ]
        (_, Algo, _) -> [ "4180:4180", "8980:8980", "9392:9392" ]
        (_, Cfx, _) -> [ "12537:12537" ]
        (_, Eth, _) -> [ "8545:8545" ]

  let reachConnectorMode = packs cm
  let debug' = if debug then "1" else ""

  let projDirHost' = case compose of
        StandaloneDevnet -> ""
        WithProject _ Project {..} -> pack projDirHost

  let devnetAlgo = [N.text|
        - ALGO_SERVER=http://reach-devnet-algo
        - ALGO_PORT=4180
        - ALGO_INDEXER_SERVER=http://reach-devnet-algo
        - ALGO_INDEXER_PORT=8980
      |]

  let devnetCfx = [N.text|
        - CFX_DEBUG
        - CFX_NODE_URI=http://reach-devnet-cfx:12537
        - CFX_NETWORK_ID=999
      |]

  let (deps, extraEnv) = case (c, m) of
        (_, Live) -> ("", "")
        (Algo, Devnet) -> (devnetFor c, devnetAlgo)
        (Algo, Browser) -> (devnetFor c, devnetAlgo)
        (Eth, Devnet) -> (devnetFor c, "- ETH_NODE_URI=http://reach-devnet-eth:8545")
        (Eth, Browser) -> (devnetFor c, "- ETH_NODE_URI=http://reach-devnet-eth:8545")
        (Cfx, Devnet) -> (devnetFor c, devnetCfx)
        (Cfx, Browser) -> (devnetFor c, devnetCfx)

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
      depends_on:
        - $deps
      environment:
        - REACH_DEBUG
        - REACH_CONNECTOR_MODE
        - REACH_ISOLATED_NETWORK
        - REACT_APP_REACH_DEBUG=$debug'
        - REACT_APP_REACH_CONNECTOR_MODE=$reachConnectorMode
        - REACT_APP_REACH_ISOLATED_NETWORK=$${REACH_ISOLATED_NETWORK}
        $extraEnv
    |]

    WithProject Rpc _ -> pure [N.text|
      volumes:
        - $projDirHost'/build:/app/build
        - $projDirHost'/tls:/app/tls
      ports:
        - "$rpcPort:$rpcPort"
      stdin_open: true
      tty: true
      depends_on:
        - $deps
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
    |]

  let mkConnSvs = liftIO . serviceConnector env cm connPorts appService

  connSvs <- case (m, compose) of
    (Live, _) -> pure ""
    (_, StandaloneDevnet) -> mkConnSvs "latest"
    (_, WithProject Console _) -> mkConnSvs version''
    (_, WithProject React _) -> mkConnSvs version''
    (_, WithProject Rpc _) -> mkConnSvs "latest"

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


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
scaffold' :: Bool -> Bool -> Project -> App
scaffold' i quiet proj@Project {..} = do
  warnDeprecatedFlagIsolate i
  Env {..} <- ask

  let Scaffold {..} = mkScaffold proj
  let DockerMeta {..} = mkDockerMetaConsole proj
  let scaffIfAbsent' n f = liftIO $ scaffIfAbsent quiet n f

  let tmpl p =
          swap "APP" projName
        . swap "MJS" (projName <> ".mjs")
        . swap "PROJ" appProj
        . swap "REACH_VERSION" (version'' e_var)
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


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
compile :: Subcommand
compile = command "compile" $ info f d where
  d = progDesc "Compile an app"
  f = go <$> compiler

  go CompilerToolArgs {..} = do
    Var {..} <- asks e_var

    liftIO $ do
      diePathContainsParentDir cta_source
      maybe (pure ()) diePathContainsParentDir cta_dirDotReach
      maybe (pure ()) diePathContainsParentDir cta_outputDir

    script $ do
      realpath

      write [N.text|
        REACH="$$(realpath "$reachEx")"
        HS="$$(dirname "$$REACH")/hs"
        ID=$$($whoami')

        export REACH

        if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
          ~/.local/bin/reachc --disable-reporting $args

        elif [ "$${REACH_DOCKER}" = "0" ] \
          && [ -d "$${HS}/.stack-work"  ] \
          && which stack >/dev/null 2>&1; then

          export STACK_YAML="$${REACH_STACK_YAML:-"$${HS}/stack.yaml"}"
          export REACHC_ID=$${ID}

          # https://github.com/koalaman/shellcheck/wiki/SC2155
          REACHC_HASH="$$("$${HS}/../scripts/git-hash.sh")"
          export REACHC_HASH

          (cd "$$HS" && make stack)

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
            -e "REACHC_ID=$${ID}" \
            reachsh/reach:$version'' \
            $args
        fi
      |]

     where
      if' b t = if b then pack t else ""

      opt x a = case x of
        Nothing -> ""
        Just t -> pack $ a <> "=" <> t

      args = intercalate " " $
        [ if' cta_disableReporting "--disable-reporting"
        , if' cta_errorFormatJson "--error-format-json"
        , if' cta_intermediateFiles "--intermediate-files"
        , if' cta_installPkgs "--install-pkgs"
        , opt cta_dirDotReach "--dir-dot-reach"
        , opt cta_outputDir "--output"
        , pack cta_source
        ] <> (pack <$> cta_tops)

      drp' = if' (not cta_disableReporting) "--disable-reporting"
      ifs' = if' (not cta_intermediateFiles) "--intermediate-files"

      reachc_release = [N.text| stack build && stack exec -- reachc $args |]

      reachc_dev = [N.text| stack build --fast && stack exec -- reachc $drp' $ifs' $args |]

      reachc_prof = [N.text|
        stack build --profile --fast \
          && stack exec --profile -- reachc $drp' $ifs' $args +RTS -p
      |]



--------------------------------------------------------------------------------
init' :: Subcommand
init' = command "init" . info f $ d <> foot where
  d = progDesc "Set up source files for a simple app in the current directory"
  f = go <$> strArgument (metavar "TEMPLATE" <> value "_default" <> showDefault)

  -- TODO list available templates?
  foot = footerDoc . Just $ text "Aborts if index.rsh or index.mjs already exist"

  go template = do
    Env {..} <- ask
    Project {..} <- projectPwdIndex
    let tmpl n = e_dirEmbed </> "template" </> "init" </> n
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
      T.writeFile rsh $ swap "REACH_VERSION_SHORT" (versionShort e_var) fmtInitRsh

      T.putStrLn $ "Writing " <> app <> ".mjs..."
      T.writeFile mjs $ swap "APP" app fmtInitMjs
      putStrLn "Done."


--------------------------------------------------------------------------------
-- Tell `docker-compose` to skip connector containers if they're already running
devnetDeps :: AppT Text
devnetDeps = do
  ConnectorMode c' _ <- asks $ connectorMode . e_var
  let c = packs c'
  pure [N.text| $([ "$(docker ps -qf label=sh.reach.devnet-for=$c)x" = 'x' ] || echo '--no-deps') |]


run' :: Subcommand
run' = command "run" . info f $ d <> noIntersperse where
  d = progDesc "Run a simple app"
  f = go <$> switchIsolate
         <*> argAppOrDir
         <*> manyArgs "APP"

  go i appOrDir args = do
    warnDeprecatedFlagIsolate i
    dieConnectorModeBrowser

    Env {..} <- ask
    proj@Project {..} <- projectFrom appOrDir
    dd <- devnetDeps

    let Var {..} = e_var
    let Scaffold {..} = mkScaffold proj

    toClean <- filterM (fmap not . liftIO . doesFileExist . fst)
      [ (containerPackageJson, hostPackageJson)
      , (containerDockerfile, hostDockerfile)
      , (containerGitIgnore, hostGitIgnore)
      , (containerDockerIgnore, hostDockerIgnore)
      ]

    cleanup <- intercalate "\n" <$> forM toClean (pure . pack . ("rm " <>) . snd)

    scaffold' False True proj

    let rsh = projDirContainer </> unpack projName <> ".rsh"
    let mjs = projDirContainer </> unpack projName <> ".mjs"
    let bjs = projDirContainer </> "build" </> unpack projName <> ".main.mjs"

    let abortIfAbsent p = liftIO . whenM (not <$> doesFileExist p)
          . die $ takeFileName p <> " doesn't exist."

    abortIfAbsent rsh
    abortIfAbsent mjs

    let recompile' = reachEx <> " compile " <> pack (projDirRel </> unpack projName <> ".rsh\n")

    recompile <- liftIO $ ifM (not <$> doesFileExist bjs)
      (pure $ Just recompile')
      $ do
        b <- modificationTime <$> getFileStatus bjs
        r <- modificationTime <$> getFileStatus rsh
        pure $ if r > b then Just recompile' else Nothing

    let dm@DockerMeta {..} = mkDockerMetaConsole proj
    let dockerfile' = pack hostDockerfile
    let projDirHost' = pack projDirHost
    let args' = intercalate " " . map (<> "'") . map ("'" <>) $ projName : args

    withCompose dm . script $ do
      maybe (pure ()) write recompile
      write [N.text|
        cd $projDirHost'
        CNAME="$appService-$$$$"

        set +e
        docker build -f $dockerfile' --tag=$appImageTag . \
          && docker-compose -f "$$TMP/docker-compose.yml" run \
            --name "$$CNAME" $dd --rm $appService $args'
        RES="$?"
        set -e

        $cleanup
        exit "$$RES"
      |]


--------------------------------------------------------------------------------
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

  forM_ (packs <$> [ Algo, Cfx, Eth ]) $ \c -> write [N.text|
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


--------------------------------------------------------------------------------
react :: Subcommand
react = command "react" $ info f d where
  d = progDesc "Run a simple React app"
  f = go <$> switchUseExistingDevnet <*> compiler

  -- Leverage `optparse` for help/completions/validation/etc but disregard its
  -- product and instead thread raw command line back through during `compile`
  -- sub-shell
  go ued _ = do
    warnDeprecatedFlagUseExistingDevnet ued

    v@Var { connectorMode = ConnectorMode c _, ..} <- asks e_var

    local (\e -> e { e_var = v { connectorMode = ConnectorMode c Browser }}) $ do
      dm@DockerMeta {..} <- mkDockerMetaReact <$> projectPwdIndex
      dd <- devnetDeps

      -- TODO generalize this pattern for other subcommands?
      cargs <- liftIO $ do
        let x "react" = False
            x "--use-existing-devnet" = False
            x a | "--dir-project-host" `isPrefixOf` a = False
            x a | "--dir-tmp-host" `isPrefixOf` a = False
                | otherwise = True
        intercalate " " . filter x . fmap pack <$> getArgs

      withCompose dm . script $ write [N.text|
        $reachEx compile $cargs
        docker-compose -f "$$TMP/docker-compose.yml" run \
          --name $appService $dd --service-ports --rm $appService
      |]


--------------------------------------------------------------------------------
rpcServer' :: Text -> AppT Text
rpcServer' appService = do
  Var {..} <- asks e_var
  dd <- devnetDeps
  pure [N.text|
    $reachEx compile
    docker-compose -f "$$TMP/docker-compose.yml" run --name $appService $dd --service-ports --rm $appService
  |]


rpcServer :: Subcommand
rpcServer = command "rpc-server" $ info f d where
  d = progDesc "Run a simple Reach RPC server"
  f = go <$> switchUseExistingDevnet

  go ued = do
    env <- ask
    prj <- projectPwdIndex
    let dm@DockerMeta {..} = mkDockerMetaRpc env prj

    dieConnectorModeBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair prj
    warnDeprecatedFlagUseExistingDevnet ued

    withCompose dm . script $ rpcServer' appService >>= write


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
    env <- ask
    prj <- projectPwdIndex
    rsa <- rpcServerAwait' 30

    let dm@DockerMeta {..} = mkDockerMetaRpc env prj
    runServer <- rpcServer' appService

    let args' = intercalate " " args

    dieConnectorModeBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair prj

    -- TODO detect if process is already listening on $REACH_RPC_PORT
    -- `lsof -i` cannot necessarily be used without `sudo`
    withCompose dm . script $ write [N.text|
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


--------------------------------------------------------------------------------
devnet :: Subcommand
devnet = command "devnet" $ info f d where
  d = progDesc "Run only the devnet"
  f = go <$> switch (long "await-background" <> help "Run in background and await availability")

  go abg = do
    Env {..} <- ask
    dd <- devnetDeps
    let Var {..} = e_var
    let ConnectorMode c m = connectorMode
    let c' = packs c
    let s = devnetFor c
    let n = "reach-" <> s
    let a = if abg then " >/dev/null 2>&1 &" else ""

    dieConnectorModeBrowser

    unless (m == Devnet) . liftIO
      $ die "`reach devnet` may only be used when `REACH_CONNECTOR_MODE` ends with \"-devnet\"."

    withCompose mkDockerMetaStandaloneDevnet . script $ do
      write [N.text|
        docker-compose -f "$$TMP/docker-compose.yml" run --name $n $dd --service-ports --rm $s$a
      |]
      when abg $ write [N.text|
        printf 'Bringing up devnet...'
        while true; do
          if [ "$(docker ps -qf "label=sh.reach.devnet-for=$c'" | wc -l)" -gt 0 ]; then break; fi
          printf '.'
          sleep 1
        done
        printf ' Done.\n'
      |]


--------------------------------------------------------------------------------
upgrade :: Subcommand
upgrade = command "upgrade" $ info f d where
  d = progDesc "Upgrade Reach"
  f = pure . liftIO . exitWith $ ExitFailure 50


update :: Subcommand
update = command "update" $ info (pure f) d where
  d = progDesc "Update Reach Docker images"
  f = script . forM_ reachImages $ \i -> write
    $ "docker pull reachsh/" <> i <> ":" <> pack compatibleVersionStr


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
version' :: Subcommand
version' = command "version" $ info (pure f) d where
  d = progDesc "Display version"
  f = putStrLnPacked versionHeader


numericVersion :: Subcommand
numericVersion = command "numeric-version" $ info (pure f) fullDesc where
  f = putStrLnPacked compatibleVersionStr


--------------------------------------------------------------------------------
help' :: Subcommand
help' = command "help" $ info f d where
  d = progDesc "Show usage"
  f = pure $ do
    Var {..} <- asks e_var
    script $ write [N.text| $reachEx --help |]


--------------------------------------------------------------------------------
hashes :: Subcommand
hashes = command "hashes" $ info f d where
  d = progDesc "Display git hashes used to build each Docker image"
  f = pure $ do
    Var {..} <- asks e_var
    script . forM_ reachImages $ \i -> write [N.text|
      echo "$i:" "$(docker run --entrypoint /bin/sh "reachsh/$i:$version''" -c 'echo $$REACH_GIT_HASH')"
    |]


--------------------------------------------------------------------------------
whoami' :: Text
whoami' = "docker info --format '{{.ID}}' 2>/dev/null"


whoami :: Subcommand
whoami = command "whoami" $ info f fullDesc where
  f = pure . script $ write whoami'


--------------------------------------------------------------------------------
-- TODO better header
header' :: String
header' = "https://reach.sh"


failNonAbsPaths :: Env -> IO ()
failNonAbsPaths Env {..} =
  mapM_ (\p -> unless (isAbsolute p) . die $ p <> " is not an absolute path.")
    [ e_dirEmbed
    , e_dirPwdContainer
    , e_dirPwdHost
    , e_dirTmpContainer
    , e_dirTmpHost
    ]


main :: IO ()
main = do
  eff <- newIORef InProcess
  var <- mkVar

  let env = Env
        <$> strOption (long "dir-embed" <> value "/app/embed" <> internal)
        <*> strOption (long "dir-project-container" <> value "/app/src" <> internal)
        <*> strOption (long "dir-project-host" <> internal)
        <*> strOption (long "dir-tmp-container" <> value "/app/tmp" <> internal)
        <*> strOption (long "dir-tmp-host" <> internal)
        <*> switch (long "emit-raw" <> internal)
        <*> pure eff
        <*> pure var

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

 where
  cs = compile
    <> clean
    <> init'
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

  hs = internal
    <> commandGroup "hidden subcommands"
    <> numericVersion
    <> reactDown
    <> rpcServerAwait
    <> rpcServerDown
    <> unscaffold
    <> whoami
