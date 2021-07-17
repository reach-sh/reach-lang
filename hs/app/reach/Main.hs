{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bits
import Data.Char
import Data.IORef
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

-- TODO update `ref-usage` docs once stabilized


data Effect
  = Script T.Text
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
  { reachEx :: T.Text
  , connectorMode :: ConnectorMode
  , debug :: Bool
  , rpcKey :: T.Text
  , rpcPort :: T.Text
  , rpcServer'' :: T.Text
  , rpcTlsCrt :: T.Text
  , rpcTlsKey :: T.Text
  , rpcTlsPassphrase :: T.Text
  , rpcTlsRejectUnverified :: Bool
  , version'' :: T.Text
  , versionShort :: T.Text
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
defRpcKey :: T.Text
defRpcKey = "opensesame"


warnDefRpcKey :: App
warnDefRpcKey = do
  Var {..} <- asks e_var
  when (rpcKey == defRpcKey) . liftIO . T.putStrLn
    $ "Warning! Using development RPC key: REACH_RPC_KEY=" <> defRpcKey <> "."


dieConnectorModeNonBrowser :: App
dieConnectorModeNonBrowser = connectorMode <$> asks e_var >>= \case
  ConnectorMode _ Browser -> liftIO . die
    $ "`REACH_CONNECTOR_MODE` cannot select the `browser` target; `browser`"
   <> " is only available via the Reach standard library."
  _ -> pure ()


warnScaffoldDefRpcTlsPair :: Project -> App
warnScaffoldDefRpcTlsPair Project {..} = do
  Env {..} <- ask

  let warnDev = putStrLn "Warning! The current TLS certificate is only suitable for development purposes."

  let embd r = e_dirEmbed </> "sh" </> "_common" </> "rpc" </> r
  let dock r = projDirContainer </> "tls" </> r
  let host r = projDirHost </> "tls" </> r

  let orw = ownerReadMode .|. ownerWriteMode
  let key = T.unpack $ rpcTlsKey e_var
  let crt = T.unpack $ rpcTlsCrt e_var

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
  let packed = pure . T.pack
  let q e n = lookupEnv e >>= maybe n packed

  rpcPort <- q "REACH_RPC_PORT" (pure "3000")
  rpcServer'' <- q "REACH_RPC_SERVER" (pure "127.0.0.1")
  rpcKey <- q "REACH_RPC_KEY" (pure defRpcKey)
  rpcTlsPassphrase <- q "REACH_RPC_TLS_PASSPHRASE" (pure "rpc-demo")
  rpcTlsKey <- q "REACH_RPC_TLS_KEY" (pure "reach-server.key")
  rpcTlsCrt <- q "REACH_RPC_TLS_CRT" (pure "reach-server.crt")
  version'' <- q "REACH_VERSION" (packed versionStr)

  debug <- lookupEnv "REACH_DEBUG" >>= maybe (pure False) (const $ pure True)

  rpcTlsRejectUnverified <- lookupEnv "REACH_RPC_TLS_REJECT_UNVERIFIED"
    >>= maybe (pure True) (pure . (/= "0"))

  reachEx <- lookupEnv "REACH_EX"
    >>= maybe (die "Unset `REACH_EX` environment variable") packed

  connectorMode <- do
    rcm <- maybe "ETH-devnet" id <$> lookupEnv "REACH_CONNECTOR_MODE"
    runParserT ((ConnectorMode <$> pConnector <*> pMode) <* eof) () "" rcm
      >>= either (const . die $ "Invalid `REACH_CONNECTOR_MODE`: " <> rcm) pure

  let versionShort = case version'' of
        "stable" -> T.pack compatibleVersionStr
        v -> a <> "." <> b where
          f ('v':s) = s
          f s = s
          v' = T.splitOn "." . T.pack . f $ T.unpack v
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

  let connectorMode' = T.pack $ show connectorMode

  asks e_effect >>= liftIO . flip writeIORef (Script
     $ [N.text|
          #!/bin/sh
          set -e
       |]
    <> "\n\n"
    <> debug'
    <> rpcTlsRejectUnverified'
    <> [N.text|
          REACH_CONNECTOR_MODE=$connectorMode'
          REACH_RPC_KEY=$rpcKey
          REACH_RPC_PORT=$rpcPort
          REACH_RPC_SERVER=$rpcServer''
          REACH_RPC_TLS_CRT=$rpcTlsCrt
          REACH_RPC_TLS_KEY=$rpcTlsKey
          REACH_RPC_TLS_PASSPHRASE=$rpcTlsPassphrase
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


write :: T.Text -> App
write t = asks e_effect >>= liftIO . flip modifyIORef w where
  w = \case
    Script t' -> Script $ t' <> t <> "\n\n"
    InProcess -> impossible "Cannot `write` to an in-process `Effect`"


writeFrom :: FilePath -> App
writeFrom p = asks e_dirEmbed >>= liftIO . T.readFile . (</> p) >>= write


realpath :: App
realpath = writeFrom "sh/_common/realpath.sh"


swap :: T.Text -> T.Text -> T.Text -> T.Text
swap a b src = T.replace ("${" <> a <> "}") b src


swap' :: T.Text -> String -> T.Text -> T.Text
swap' a b src = swap a (T.pack b) src


reachImages :: [T.Text]
reachImages =
  [ "reach"
  , "reach-cli"
  , "react-runner"
  , "rpc-server"
  , "runner"
  , "devnet-algo"
  , "devnet-cfx"
  , "ethereum-devnet"
  ]


--------------------------------------------------------------------------------
serviceConnector :: Env -> ConnectorMode -> Compose -> [T.Text] -> T.Text -> T.Text -> IO T.Text
serviceConnector Env {..} (ConnectorMode c m) compose ports appService' version'' = do
  let ports' = case ports of
        [] -> "[]"
        ps -> T.intercalate "\n    " $ map ("- " <>) ps

  let n = show m <> "-" <> (toLower <$> show c)

  let (sad, dph) = case compose of
        StandaloneDevnet -> ("True", "")
        Console (Project {..}) -> ("False", T.pack projDirHost)
        React (Project {..}) -> ("False", T.pack projDirHost)
        Rpc (Project {..}) -> ("False", T.pack projDirHost)

  fmt <- T.readFile $ e_dirEmbed
    </> "sh" </> "_common" </> "_docker-compose" </> "service-" <> n <> ".yml"

  pure
    . swap "REACH_VERSION" version''
    . swap "PORTS" ports'
    . swap "NETWORK" "reach-devnet"
    . swap "APP_SERVICE" (if appService' == "" then "" else "-" <> appService')
    . swap "STANDALONE_DEVNET" sad
    . swap "DIR_TMP_HOST" (T.pack e_dirTmpHost)
    . swap "DIR_PROJECT_HOST" dph
    $ fmt


connectorEnv :: Env -> ConnectorMode -> IO T.Text
connectorEnv Env {..} (ConnectorMode c m) = do
  let c' = toLower <$> show c
  T.readFile $ e_dirEmbed </> "sh" </> "_common" </> "_docker-compose"
    </> "service-" <> show m <> "-" <> c' <> "-env.yml"


--------------------------------------------------------------------------------
devnetFor :: Connector -> T.Text
devnetFor = \case
  Algo -> "devnet-algo"
  Cfx -> "devnet-cfx"
  Eth -> "ethereum-devnet"


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
  { projName :: T.Text
  , projDirContainer :: FilePath
  , projDirHost :: FilePath
  , projDirRel :: FilePath
  }

data Scaffold = Scaffold
  { containerDockerfile :: FilePath
  , containerPackageJson :: FilePath
  , containerMakefile :: FilePath
  , hostDockerfile :: FilePath
  , hostPackageJson :: FilePath
  , hostMakefile :: FilePath
  }

-- TODO make this more amenable to ergonomic pattern-matching
data Compose
  = Console Project
  | React Project
  | Rpc Project
  | StandaloneDevnet

data DockerMeta = DockerMeta
  { appProj :: T.Text
  , appService :: T.Text
  , appImage :: T.Text
  , appImageTag :: T.Text
  , compose :: Compose
  }


mkScaffold :: Project -> Bool -> Scaffold
mkScaffold Project {..} isolate = Scaffold
  { containerDockerfile = f $ projDirContainer </> "Dockerfile"
  , containerPackageJson = f $ projDirContainer </> "package.json"
  , containerMakefile = f $ projDirContainer </> "Makefile"
  , hostDockerfile = f $ projDirHost </> "Dockerfile"
  , hostPackageJson = f $ projDirHost </> "package.json"
  , hostMakefile = f $ projDirHost </> "Makefile"
  } where f a = if isolate then a <> "." <> T.unpack projName else a


appProj' :: FilePath -> T.Text
appProj' = T.toLower . T.pack . takeBaseName . dropTrailingPathSeparator


mkDockerMetaConsole :: Project -> Bool -> DockerMeta
mkDockerMetaConsole p@Project {..} isolate = DockerMeta {..} where
  appProj = appProj' projDirHost <> if isolate then ("-" <> projName) else ""
  appService = "reach-app-" <> appProj
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"
  compose = Console p


mkDockerMetaReact :: Project -> DockerMeta
mkDockerMetaReact p = DockerMeta {..} where
  appProj = ""
  appService = "react-runner"
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"
  compose = React p


mkDockerMetaRpc :: Env -> Project -> DockerMeta
mkDockerMetaRpc Env {..} p@Project {..} = DockerMeta {..} where
  appProj = appProj' projDirHost
  appService = "reach-app-" <> appProj
  appImage = "reachsh/rpc-server"
  appImageTag = appImage <> ":" <> versionShort e_var
  compose = Rpc p


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
        (pure $ Project (T.pack a) e_dirPwdContainer e_dirPwdHost ".")
        $ do
          let dph = e_dirPwdHost </> a
          let dpc = e_dirPwdContainer </> a
          let f x = when (any (== "..") $ splitDirectories x) . die
                      $ x <> " cannot contain parent directories (\"..\")."
          f dph
          f dpc
          pure $ Project "index" dpc dph a


projectPwdIndex :: AppT Project
projectPwdIndex = do
  Env {..} <- ask
  pure $ Project "index" e_dirPwdContainer e_dirPwdHost "."


scaff :: Bool -> FilePath -> T.Text -> IO ()
scaff quiet n f = do
  when (not quiet) . putStrLn $ "Writing " <> takeFileName n <> "..."
  T.writeFile n f


scaffIfAbsent :: Bool -> FilePath -> T.Text -> IO ()
scaffIfAbsent quiet n f = whenM (not <$> doesFileExist n) $ scaff quiet n f


readScaff :: FilePath -> AppT T.Text
readScaff p = do
  Env {..} <- ask
  liftIO . T.readFile $ e_dirEmbed </> "sh" </> "subcommand" </> "scaffold" </> p


withCompose :: DockerMeta -> App -> App
withCompose DockerMeta {..} wrapped = do
  env@Env {..} <- ask
  let Var {..} = e_var
  let cm@(ConnectorMode c m) = connectorMode

  -- TODO push ports into templates instead (?)
  let connPorts = case (compose, c, m) of
        (_, _, Live) -> []
        (Console _, Algo, Devnet) -> [ "9392" ]
        (_, Algo, _) -> [ "4180:4180", "8980:8980", "9392:9392" ]
        (_, Cfx, _) -> [ "12537:12537" ]
        (_, Eth, _) -> [ "8545:8545" ]

  let reachConnectorMode = T.pack $ show cm
  let reachIsolatedNetwork = "1" -- TODO
  let debug' = if debug then "1" else ""

  let projDirHost' = case compose of
        StandaloneDevnet -> ""
        Console (Project {..}) -> T.pack projDirHost
        React (Project {..}) -> T.pack projDirHost
        Rpc (Project {..}) -> T.pack projDirHost

  connEnv <- case compose of
    StandaloneDevnet -> liftIO $ connectorEnv env cm

    Console _ -> liftIO $ connectorEnv env cm

    React _ -> pure [N.text|
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
        - REACT_APP_REACH_ISOLATED_NETWORK=$reachIsolatedNetwork
    |]

    Rpc _ -> do
      let devnetAlgo = [N.text|
            - ALGO_SERVER=http://devnet-algo
            - ALGO_PORT=4180
            - ALGO_INDEXER_SERVER=http://devnet-algo
            - ALGO_INDEXER_PORT=8980
          |]

      -- TODO `USE_EXISTING_DEVNET`-handling, CFX, consolidate + push config
      -- out of here
      let (d, e) = case (c, m) of
            (Algo, Devnet) -> (devnetFor c, devnetAlgo)
            (Eth, Devnet) -> (devnetFor c, "- ETH_NODE_URI=http://ethereum-devnet:8545")
            _ -> ("", "")

      pure [N.text|
        volumes:
          - $projDirHost'/build:/app/build
          - $projDirHost'/tls:/app/tls
        ports:
          - "$rpcPort:$rpcPort"
        stdin_open: true
        tty: true
        depends_on:
          - $d
        environment:
          - REACH_DEBUG
          - REACH_CONNECTOR_MODE=$reachConnectorMode
          - REACH_ISOLATED_NETWORK
          - REACH_RPC_PORT
          - REACH_RPC_KEY
          - REACH_RPC_TLS_KEY
          - REACH_RPC_TLS_CRT
          - REACH_RPC_TLS_PASSPHRASE
          $e
      |]

  connSvs <- case (m, compose) of
    (Live, _) -> pure ""
    (_, cmp@StandaloneDevnet) -> liftIO $ serviceConnector env cm cmp connPorts appService "latest"

    -- TODO fix remaining combos (e.g. `rpc-server` latest vs. REACH_VERSION)
    (_, cmp) -> liftIO $ serviceConnector env cm cmp connPorts appService version''

  let topLevelNets = case m of
        Live -> "{}"
        _ -> [N.text|
               reach-devnet:
                 name: reach-devnet
             |]

  let svcNets = case m of
        Live -> "[]"
        _ -> "- reach-devnet"

  let e_dirTmpHost' = T.pack e_dirTmpHost
  let appService' = case compose of
        StandaloneDevnet -> ""
        _ -> [N.text|
               $appService:
                 image: $appImageTag
                 networks:
                   $svcNets
                 labels:
                   - "sh.reach.standalone-devnet=False"
                   - "sh.reach.dir-tmp=$e_dirTmpHost'"
                   - "sh.reach.dir-project=$projDirHost'"
                 build:
                   context: $projDirHost'
                 $connEnv
             |]

  let f = [N.text|
     version: '3.5'

     networks:
       $topLevelNets

     services:
       $connSvs

       $appService'
    |]

  liftIO $ scaff True (e_dirTmpContainer </> "docker-compose.yml") (notw f)
  wrapped

 where
  notw = T.intercalate "\n" . fmap T.stripEnd . T.lines


--------------------------------------------------------------------------------
argAppOrDir :: Parser FilePath
argAppOrDir = strArgument
  $ metavar "APP or DIR"
 <> help "May be either a module name without its extension (e.g. \"index\") \
         \or a relative sub-directory path"
 <> value ""


switchUseExistingDevnet :: Parser Bool
switchUseExistingDevnet = switch $ long "use-existing-devnet"


--------------------------------------------------------------------------------
scaffold' :: Bool -> Bool -> Project -> App
scaffold' isolate quiet proj@Project {..} = do
  Env {..} <- ask

  let Scaffold {..} = mkScaffold proj isolate
  let DockerMeta {..} = mkDockerMetaConsole proj isolate
  let scaffIfAbsent' n f = liftIO $ scaffIfAbsent quiet n f

  let cpline = case isolate of
        False -> ""
        True -> "RUN cp /app/" <> T.pack (takeFileName hostPackageJson) <> " /app/package.json"

  -- TODO prune
  let tmpl p =
          swap "APP" projName
        . swap "MJS" (projName <> ".mjs")
        . swap "RSH" (projName <> ".rsh")
        . swap' "MAKEFILE" (takeFileName hostMakefile)
        . swap' "DOCKERFILE" (takeFileName hostDockerfile)
        . swap' "PACKAGE_JSON" (takeFileName hostPackageJson)
        . swap "CPLINE" cpline
        . swap "PROJ" appProj -- TODO
        . swap "SERVICE" appService
        . swap "IMAGE" appImage
        . swap "IMAGE_TAG" appImageTag
        . swap "REACH_VERSION" (version'' e_var)
       <$> readScaff p

  -- TODO: s/lint/preapp. It's disabled because sometimes our
  -- generated code trips the linter
  tmpl "package.json" >>= scaffIfAbsent' containerPackageJson
  tmpl "Dockerfile" >>= scaffIfAbsent' containerDockerfile
  tmpl "Makefile" >>= scaffIfAbsent' containerMakefile

  tmpl ".gitignore" >>= scaffIfAbsent' (projDirContainer </> ".gitignore")
  tmpl ".dockerignore" >>= scaffIfAbsent' (projDirContainer </> ".dockerignore")

  when (not quiet) . liftIO $ putStrLn "Done."


unscaffold' :: Bool -> Bool -> FilePath -> App
unscaffold' isolate quiet appOrDir = do
  Scaffold {..} <- flip mkScaffold isolate <$> projectFrom appOrDir
  liftIO $ do
    forM_ [ containerDockerfile, containerPackageJson, containerMakefile ] $ \n -> do
      -- TODO each by whether file exists
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
  f = go <$> compiler -- TODO appOrDir

  go CompilerToolArgs {..} = do
    Var {..} <- asks e_var
    script $ do
      realpath

      write [N.text|
        REACH="$$(realpath "$reachEx")"
        HS="$$(dirname "$$REACH")/hs"
        ID=$$($whoami')

        export REACH

        if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
          ~/.local/bin/reachc --disable-reporting $args

        elif [ -z "$${REACH_DOCKER}" ] \
          && [ -d "$${HS}/.stack-work" ] \
          && (which stack > /dev/null 2>&1); then

          export STACK_YAML="$${HS}/stack.yaml"
          export REACHC_ID=$${ID}
          export REACHC_HASH="$$("$${HS}/../scripts/git-hash.sh")"

          (cd "$$HS" && make stack)

          if [ "x$${REACHC_RELEASE}" = "xY" ]; then
            $reachc_release
          elif [ "x$${REACHC_PROFILE}" = "xY" ]; then
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
      if' b t = if b then T.pack t else ""

      opt x a = case x of
        Nothing -> ""
        Just t -> T.pack $ a <> "=" <> t

      args = T.intercalate " " $
        [ if' cta_disableReporting "--disable-reporting"
        , if' cta_errorFormatJson "--error-format-json"
        , if' cta_intermediateFiles "--intermediate-files"
        , if' cta_installPkgs "--install-pkgs"
        , opt cta_dirDotReach "--dir-dot-reach"
        , opt cta_outputDir "--output"
        , T.pack cta_source -- TODO fix directory mismatches between host/container
        ] <> (T.pack <$> cta_tops)

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
  d = progDesc "Set up source files for a simple app"
  f = go <$> strArgument (metavar "TEMPLATE" <> value "_default" <> showDefault)
         <*> argument str (metavar "APP" <> value "index" <> showDefault)

  -- TODO list available templates?
  foot = footerDoc . Just
     $  text "APP is \"index\" by default"
   <$$> text ""
   <$$> text "Aborts if $APP.rsh or $APP.mjs already exist"

  go template app = do
    Env {..} <- ask
    Project {..} <- projectPwdIndex
    let tmpl n = e_dirEmbed </> "template" </> "init" </> n

    liftIO $ do
      tmpl' <- ifM (doesDirectoryExist $ tmpl template)
        (pure $ tmpl template)
        (pure $ tmpl "_default")

      fmtInitRsh <- T.readFile $ tmpl' </> "index.rsh"
      fmtInitMjs <- T.readFile $ tmpl' </> "index.mjs"

      let rsh = projDirContainer </> T.unpack app <> ".rsh"
      let mjs = projDirContainer </> T.unpack app <> ".mjs"
      let abortIf x = whenM (doesFileExist x) . die $ x <> " already exists."

      abortIf rsh
      abortIf mjs

      T.putStrLn $ "Writing " <> app <> ".rsh..."
      T.writeFile rsh $ swap "REACH_VERSION_SHORT" (versionShort e_var) fmtInitRsh

      T.putStrLn $ "Writing " <> app <> ".mjs..."
      T.writeFile mjs $ swap "APP" app fmtInitMjs
      putStrLn "Done."


--------------------------------------------------------------------------------
run' :: Subcommand
run' = command "run" . info f $ d <> noIntersperse where
  d = progDesc "Run a simple app"
  f = go <$> switch (long "isolate")
         <*> argAppOrDir
         <*> many (strArgument (metavar "ARG"))

  go isolate appOrDir args = do
    dieConnectorModeNonBrowser

    Env {..} <- ask
    proj@Project {..} <- projectFrom appOrDir

    let Var {..} = e_var
    let Scaffold {..} = mkScaffold proj isolate

    toClean <- filterM (fmap not . liftIO . doesFileExist . fst)
      [ (containerMakefile, hostMakefile)
      , (containerPackageJson, hostPackageJson)
      , (containerDockerfile, hostDockerfile)
      ]

    cleanup <- T.intercalate "\n" <$> forM toClean (pure . T.pack . ("rm " <>) . snd)

    scaffold' isolate True proj

    let rsh = projDirContainer </> T.unpack projName <> ".rsh"
    let mjs = projDirContainer </> T.unpack projName <> ".mjs"
    let bjs = projDirContainer </> "build" </> T.unpack projName <> ".main.mjs"

    let abortIfAbsent p = liftIO . whenM (not <$> doesFileExist p)
          . die $ takeFileName p <> " doesn't exist."

    abortIfAbsent rsh
    abortIfAbsent mjs

    let recompile' = reachEx <> " compile " <> T.pack (projDirRel </> T.unpack projName <> ".rsh\n")

    recompile <- liftIO $ ifM (not <$> doesFileExist bjs)
      (pure $ Just recompile')
      $ do
        b <- modificationTime <$> getFileStatus bjs
        r <- modificationTime <$> getFileStatus rsh
        pure $ if r > b then Just recompile' else Nothing

    let dm@DockerMeta {..} = mkDockerMetaConsole proj isolate
    let dockerfile' = T.pack hostDockerfile
    let projDirHost' = T.pack projDirHost
    let args' = T.intercalate " " . map (<> "'") . map ("'" <>) $ projName : args

    withCompose dm . script $ do
      maybe (pure ()) write recompile
      write [N.text|
        cd $projDirHost'

        set +e
        docker build -f $dockerfile' --tag=$appImageTag . \
          && docker-compose -f "$$TMP/docker-compose.yml" run --rm $appService $args'
        RES="$?"
        set -e

        $cleanup
        exit "$$RES"
      |]


--------------------------------------------------------------------------------
-- TODO fix crazy names like `reach2021-07-13t23-40-21z-5smc_algorand-devnet_run_1`
downProject :: Project -> App
downProject Project {..} = script $ do
  let pdh = T.pack projDirHost
  realpath
  write [N.text|
    dend () { docker-compose -f "$$1/docker-compose.yml" down --volumes --remove-orphans; }
    insp () { docker inspect --format="{{ index .Config.Labels \"$$1\" }}" "$$2"; }
    real () {
      i="$(insp "$$1" "$$2")"
      ([ "x$$i" = "x" ] && echo "") || realpath "$$i"
    }

    ds="$(docker container ls -q)"

    if [ "$$ds" = "" ]; then
      dend "$$TMP"
    else
      echo "$$ds" | while IFS= read -r d; do
        if [ "$(real "sh.reach.dir-project" "$$d")" = "$(realpath "$pdh")" ]; then
          dend "$(real "sh.reach.dir-tmp" "$$d")"
          break
        fi

        if [ "$(insp "sh.reach.standalone-devnet" "$$d")" = "True" ]; then
          (docker exec "$$d" killall -INT geth 2>/dev/null && sleep 3 && dend "$$TMP") \
            || dend "$(real "sh.reach.dir-tmp" "$$d")"
          break
        fi
      done
    fi
  |]


down :: Subcommand
down = command "down" $ info f d where
  d = progDesc "Halt any Dockerized devnets for this app"
  f = go <$> argAppOrDir
  go a = do
    p <- projectFrom a
    withCompose (mkDockerMetaConsole p False) $ downProject p


reactDown :: Subcommand
reactDown = command "react-down" $ info (pure f) fullDesc where
  f = undefined


--------------------------------------------------------------------------------
scaffold :: Subcommand
scaffold = command "scaffold" $ info f d where
  d = progDesc "Set up Docker scaffolding for a simple app"
  f = go
    <$> switch (long "isolate")
    <*> switch (long "quiet")
    <*> argAppOrDir
  go i q a = projectFrom a >>= scaffold' i q


--------------------------------------------------------------------------------
-- TODO `USE_EXISTING_DEVNET` (?)
react :: Subcommand
react = command "react" $ info f d where
  d = progDesc "Run a simple React app"
  f = go <$> compiler

  -- Leverage `optparse` for help/completions/validation/etc but disregard its
  -- product and instead thread raw command line back through during `compile`
  -- sub-shell
  go _ = do
    Var {..} <- asks e_var
    dm@DockerMeta {..} <- mkDockerMetaReact <$> projectPwdIndex

    -- TODO generalize this pattern for other subcommands?
    cargs <- liftIO $ do
      let x "react" = False
          x a | "--dir-project-host" `T.isPrefixOf` a = False
          x a | "--dir-tmp-host" `T.isPrefixOf` a = False
              | otherwise = True
      T.intercalate " " . filter x . fmap T.pack <$> getArgs

    withCompose dm . script $ write [N.text|
      $reachEx compile $cargs
      docker-compose -f "$$TMP/docker-compose.yml" run --service-ports --rm $appService
    |]


--------------------------------------------------------------------------------
rpcServer' :: T.Text -> AppT T.Text
rpcServer' appService = do
  Var {..} <- asks e_var
  pure [N.text|
    $reachEx compile
    docker-compose -f "$$TMP/docker-compose.yml" run --service-ports --rm $appService
  |]


rpcServer :: Subcommand
rpcServer = command "rpc-server" $ info f d where
  d = progDesc "Run a simple Reach RPC server"
  f = go <$> switchUseExistingDevnet

  go _ued = do
    env <- ask
    prj <- projectPwdIndex
    let dm@DockerMeta {..} = mkDockerMetaRpc env prj

    dieConnectorModeNonBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair prj

    withCompose dm . script $ do
      rpcServer' appService >>= write
      write "docker-compose -f \"$TMP/docker-compose.yml\" down --volumes --remove-orphans"


rpcRun :: Subcommand
rpcRun = command "rpc-run" $ info f $ fullDesc <> desc <> fdoc <> noIntersperse where
  desc = progDesc "Run an RPC server + frontend with development configuration"
  fdoc = footerDoc . Just
     $  text "Example:"
   <$$> text " $ reach rpc-run python3 -u ./index.py"

  f = go <$> strArgument (metavar "EXECUTABLE")
         <*> many (strArgument (metavar "ARG"))

  go exe args = do
    env@Env {..} <- ask
    prj <- projectPwdIndex

    let dm@DockerMeta {..} = mkDockerMetaRpc env prj
    runServer <- rpcServer' appService

    let Var {..} = e_var
    let args' = T.intercalate " " args

    dieConnectorModeNonBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair prj

    withCompose dm . script $ write [N.text|
      if ! (which curl >/dev/null 2>&1); then
        echo "\`reach rpc-run\` relies on an installation of \`curl\` - please install it."
        exit 1
      fi

      [ "x$$REACH_RPC_TLS_REJECT_UNVERIFIED" = "x" ] && REACH_RPC_TLS_REJECT_UNVERIFIED=0
      export REACH_RPC_TLS_REJECT_UNVERIFIED

      $runServer &
      spid="$!" # We'll SIGTERM `reach rpc-server` and all its child processes below

      # Be patient while rpc-server comes online...
      i=0
      s=0
      while [ "$$i" -lt 30 ]; do
        sleep 1
        i=$$((i+1))
        s=$$(curl \
          --http1.1 \
          -sk \
          -o /dev/null \
          -w '%{http_code}' \
          -H "X-API-Key: $rpcKey" \
          -X POST \
          "https://$rpcServer'':$rpcPort/health" || :)

        [ "$$s" -eq 200 ] && break
      done

      killbg () {
        echo
        pkill -TERM -P "$$spid"
        docker-compose -f "$$TMP/docker-compose.yml" down --volumes --remove-orphans
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
  f = pure $ do
    Env {..} <- ask
    let Var {..} = e_var
    let ConnectorMode c m = connectorMode
    let s = devnetFor c

    dieConnectorModeNonBrowser

    unless (m == Devnet) . liftIO
      $ die "`reach devnet` may only be used when `REACH_CONNECTOR_MODE` ends with \"-devnet\"."

    withCompose mkDockerMetaStandaloneDevnet . script $ write [N.text|
      docker-compose -f "$$TMP/docker-compose.yml" run --service-ports --rm $s
    |]


--------------------------------------------------------------------------------
upgrade :: Subcommand
upgrade = command "upgrade" $ info f d where
  d = progDesc "Upgrade Reach"
  f = undefined


--------------------------------------------------------------------------------
update :: Subcommand
update = command "update" $ info (pure f) d where
  d = progDesc "Update Reach Docker images"
  f = script . forM_ reachImages $ \i -> write
    $ "docker pull reachsh/" <> i <> ":" <> T.pack compatibleVersionStr


--------------------------------------------------------------------------------
dockerReset :: Subcommand
dockerReset = command "docker-reset" $ info f d where
  d = progDesc "Docker kill and rm all images"
  f = pure . script $ write [N.text|
    echo 'Docker kill all the things...'
    # shellcheck disable=SC2046
    docker kill $$(docker ps -q) >/dev/null 2>&1 || :
    echo 'Docker rm all the things...'
    # shellcheck disable=SC2046
    docker rm $$(docker ps -qa) >/dev/null 2>&1 || :
    echo 'Done.'
  |]


--------------------------------------------------------------------------------
version' :: Subcommand
version' = command "version" $ info f d where
  d = progDesc "Display version"
  f = pure . liftIO . T.putStrLn $ T.pack versionHeader


--------------------------------------------------------------------------------
help' :: Subcommand
help' = command "help" $ info f d where
  d = progDesc "Show usage"
  f = undefined


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
whoami' :: T.Text
whoami' = "docker info --format '{{.ID}}' 2>/dev/null"


whoami :: Subcommand
whoami = command "whoami" $ info f fullDesc where
  f = pure . script $ write whoami'


--------------------------------------------------------------------------------
numericVersion :: Subcommand
numericVersion = command "numeric-version" $ info f fullDesc where
  f = pure . liftIO . T.putStrLn $ T.pack compatibleVersionStr


--------------------------------------------------------------------------------
unscaffold :: Subcommand
unscaffold = command "unscaffold" $ info f fullDesc where
  f = unscaffold'
    <$> switch (long "isolate")
    <*> switch (long "quiet")
    <*> argAppOrDir


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
    <> unscaffold
    <> whoami
