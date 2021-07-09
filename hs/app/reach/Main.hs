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
  , e_dirProjectContainer :: FilePath
  , e_dirProjectHost :: FilePath
  , e_dirTmp :: FilePath
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


warnScaffoldDefRpcTlsPair :: App
warnScaffoldDefRpcTlsPair = do
  Env {..} <- ask

  let warnDev = putStrLn
        "Warning! The current TLS certificate is only suitable for development purposes."

  let embd r = e_dirEmbed </> "sh" </> "_common" </> "rpc" </> r
  let dock r = e_dirProjectContainer </> "tls" </> r
  let host r = e_dirProjectHost </> "tls" </> r
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
  , "ethereum-devnet"
  , "algorand-devnet"
  , "devnet-cfx"
  , "runner"
  , "react-runner"
  , "rpc-server"
  ]


--------------------------------------------------------------------------------
serviceConnector :: Env -> ConnectorMode -> [T.Text] -> T.Text -> T.Text -> IO T.Text
serviceConnector Env {..} (ConnectorMode c m) ports appService' version'' = do
  let ports' = case ports of
        [] -> "[]"
        ps -> T.intercalate "\n    " $ map ("- " <>) ps

  let n = show m <> "-" <> (toLower <$> show c)

  fmt <- T.readFile $ e_dirEmbed
    </> "sh" </> "_common" </> "_docker-compose" </> "service-" <> n <> ".yml"

  pure
    . swap "REACH_VERSION" version''
    . swap "PORTS" ports'
    . swap "NETWORK" appService'
    . swap "APP_SERVICE" appService'
    $ fmt


connectorEnv :: Env -> ConnectorMode -> IO T.Text
connectorEnv Env {..} (ConnectorMode c m) = do
  let c' = toLower <$> show c
  T.readFile $ e_dirEmbed </> "sh" </> "_common" </> "_docker-compose"
    </> "service-" <> show m <> "-" <> c' <> "-env.yml"


--------------------------------------------------------------------------------
devnetFor :: Connector -> T.Text
devnetFor = \case
  Algo -> "algorand-devnet"
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
data Scaffold = Scaffold
  { containerDockerfile :: FilePath
  , containerPackageJson :: FilePath
  , containerMakefile :: FilePath
  , hostDockerfile :: FilePath
  , hostPackageJson :: FilePath
  , hostMakefile :: FilePath
  }


mkScaffold :: Env -> Bool -> T.Text -> Scaffold
mkScaffold Env {..} isolate app = Scaffold
  { containerDockerfile = f $ e_dirProjectContainer </> "Dockerfile"
  , containerPackageJson = f $ e_dirProjectContainer </> "package.json"
  , containerMakefile = f $ e_dirProjectContainer </> "Makefile"
  , hostDockerfile = f $ e_dirProjectHost </> "Dockerfile"
  , hostPackageJson = f $ e_dirProjectHost </> "package.json"
  , hostMakefile = f $ e_dirProjectHost </> "Makefile"
  } where f a = if isolate then a <> "." <> T.unpack app else a


--------------------------------------------------------------------------------
data DockerMeta = DockerMeta
  { appProj :: T.Text
  , appService :: T.Text
  , appImage :: T.Text
  , appImageTag :: T.Text
  }


appProj' :: FilePath -> T.Text
appProj' = T.toLower . T.pack . takeBaseName . dropTrailingPathSeparator


mkDockerMetaConsole :: Env -> T.Text -> Bool -> DockerMeta
mkDockerMetaConsole Env {..} app isolate = DockerMeta {..} where
  appProj = appProj' e_dirProjectHost <> if isolate then ("-" <> app) else ""

  appService = "reach-app-" <> appProj
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"


mkDockerMetaReact :: DockerMeta
mkDockerMetaReact = DockerMeta {..} where
  appProj = ""
  appService = "react-runner"
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"


mkDockerMetaRpc :: Env -> DockerMeta
mkDockerMetaRpc Env {..} = DockerMeta {..} where
  appProj = appProj' e_dirProjectHost
  appService = "reach-app-" <> appProj
  appImage = "reachsh/rpc-server"
  appImageTag = appImage <> ":" <> versionShort e_var


mkDockerMetaStandaloneDevnet :: Env -> DockerMeta
mkDockerMetaStandaloneDevnet Env {..} = DockerMeta {..} where
  appProj = appProj' e_dirProjectHost
  appService = "reach-app-" <> appProj
  appImage = ""
  appImageTag = ""


--------------------------------------------------------------------------------
data Compose
  = Console
  | React
  | Rpc
  | StandaloneDevnet


withCompose :: Compose -> DockerMeta -> ConnectorMode -> App -> App
withCompose t DockerMeta {..} cm@(ConnectorMode c m) wrapped = do
  env@Env {..} <- ask
  let Var {..} = e_var

  let reachConnectorMode = T.pack $ show cm
  let reachIsolatedNetwork = "1" -- TODO
  let dirProjectHost = T.pack e_dirProjectHost
  let debug' = if debug then "1" else ""

  connEnv <- case t of
    Console -> liftIO $ connectorEnv env cm

    StandaloneDevnet -> liftIO $ connectorEnv env cm

    React -> pure [N.text|
      volumes:
        - $dirProjectHost:/app/src
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

    Rpc -> do
      let devnetAlgo = [N.text|
            - ALGO_SERVER=http://algorand-devnet
            - ALGO_PORT=4180
            - ALGO_INDEXER_SERVER=http://algorand-devnet
            - ALGO_INDEXER_PORT=8980
          |]

      let devnetEth = [N.text|
            - ETH_NODE_URI=http://ethereum-devnet:8545
          |]

      -- TODO `USE_EXISTING_DEVNET`-handling, CFX, consolidate + push config
      -- out of here
      let (d, e) = case (c, m) of
            (Algo, Devnet) -> (devnetFor c, devnetAlgo)
            (Eth, Devnet) -> (devnetFor c, devnetEth)
            _ -> ("", "")

      pure [N.text|
        volumes:
          - $dirProjectHost/build:/app/build
          - $dirProjectHost/tls:/app/tls
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

  connSvs <- case (m, t) of
    (Live, _) -> pure ""
    (_, StandaloneDevnet) -> liftIO $ serviceConnector env cm connPorts appService "latest"

    -- TODO fix remaining combos (e.g. `rpc-server` latest vs. REACH_VERSION)
    (_, _) -> liftIO $ serviceConnector env cm connPorts appService version''

  let appService' = case t of
        StandaloneDevnet -> ""
        _ -> [N.text|
               $appService:
                 image: $appImageTag
                 networks:
                   - $appService
                 build:
                   context: $dirProjectHost
                 $connEnv
             |]

  let f = [N.text|
     version: '3.5'

     networks:
       $appService:
         name: $appService

     services:
       $connSvs

       $appService'
    |]

  liftIO $ scaff True (e_dirTmp </> "docker-compose.yml") (notw f)
  wrapped

 where
  notw = T.intercalate "\n" . fmap T.stripEnd . T.lines
  -- TODO push ports into templates instead (?)
  connPorts = case (t, c, m) of
    (_, _, Live) -> []
    (Console, Algo, Devnet) -> [ "9392" ]
    (_, Algo, _) -> [ "4180:4180", "8980:8980", "9392:9392" ]
    (_, Cfx, _) -> [ "12537:12537" ]
    (_, Eth, _) -> [ "8545:8545" ]


--------------------------------------------------------------------------------
scaff :: Bool -> FilePath -> T.Text -> IO ()
scaff quiet n f = do
  when (not quiet) . putStrLn $ "Writing " <> takeFileName n <> "..."
  T.writeFile n f


scaffold' :: Bool -> Bool -> T.Text -> App
scaffold' isolate quiet app = do
  env@Env {..} <- ask

  liftIO $ do
    let Scaffold {..} = mkScaffold env isolate app
    let DockerMeta {..} = mkDockerMetaConsole env app isolate
    let scaffIfAbsent n f = whenM (not <$> doesFileExist n) $ scaff quiet n f

    let cpline = case isolate of
          False -> ""
          True -> "RUN cp /app/" <> T.pack (takeFileName hostPackageJson) <> " /app/package.json"

    -- TODO prune
    let tmpl n =
            swap "APP" app
          . swap "MJS" (app <> ".mjs")
          . swap "RSH" (app <> ".rsh")
          . swap' "MAKEFILE" (takeFileName hostMakefile)
          . swap' "DOCKERFILE" (takeFileName hostDockerfile)
          . swap' "PACKAGE_JSON" (takeFileName hostPackageJson)
          . swap "CPLINE" cpline
          . swap "PROJ" appProj
          . swap "SERVICE" appService
          . swap "IMAGE" appImage
          . swap "IMAGE_TAG" appImageTag
          . swap "REACH_VERSION" (version'' e_var)
         <$> (T.readFile $ e_dirEmbed </> "sh" </> "subcommand" </> "scaffold" </> n)

    -- TODO: s/lint/preapp. It's disabled because sometimes our
    -- generated code trips the linter
    tmpl "package.json" >>= scaffIfAbsent containerPackageJson
    tmpl "Dockerfile" >>= scaffIfAbsent containerDockerfile
    tmpl "Makefile" >>= scaffIfAbsent containerMakefile

    tmpl ".gitignore" >>= scaffIfAbsent (e_dirProjectContainer </> ".gitignore")
    tmpl ".dockerignore" >>= scaffIfAbsent (e_dirProjectContainer </> ".dockerignore")

    when (not quiet) $ putStrLn "Done."


unscaffold' :: Bool -> Bool -> T.Text -> App
unscaffold' isolate quiet app = do
  env <- ask
  liftIO $ do
    let Scaffold {..} = mkScaffold env isolate app
    forM_ [ containerDockerfile, containerPackageJson, containerMakefile ] $ \n -> do
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
    script $ do
      realpath

      write [N.text|
        REACH="$$(realpath "$reachEx")"
        HS="$$(dirname "$$REACH")/hs"
        ID=$$($whoami')

        export REACH

        if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
          # TODO test
          ~/.local/bin/reachc --disable-reporting "$@@"

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
    let tmpl n = e_dirEmbed </> "template" </> "init" </> n

    liftIO $ do
      tmpl' <- ifM (doesDirectoryExist $ tmpl template)
        (pure $ tmpl template)
        (pure $ tmpl "_default")

      fmtInitRsh <- T.readFile $ tmpl' </> "index.rsh"
      fmtInitMjs <- T.readFile $ tmpl' </> "index.mjs"

      let rsh = e_dirProjectContainer </> T.unpack app <> ".rsh"
      let mjs = e_dirProjectContainer </> T.unpack app <> ".mjs"
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
         <*> strArgument (metavar "APP" <> value "")
         <*> many (strArgument (metavar "ARG"))

  go isolate app args = do
    dieConnectorModeNonBrowser

    env@Env {..} <- ask
    let Var {..} = e_var

    (app', dirContainer, dirHost, dirRel) <- liftIO $ case app of
      "" -> pure ("index", e_dirProjectContainer, T.pack e_dirProjectHost, ".")
      _ -> ifM (not <$> doesDirectoryExist app)
        (pure (T.pack app, e_dirProjectContainer, T.pack e_dirProjectHost, "."))
        $ case isAbsolute app of
          True -> die $ "Please replace " <> app <> " with a relative path."
          False -> pure ("index", e_dirProjectContainer </> app, T.pack $ e_dirProjectHost </> app, app)

    let env' = env { e_dirProjectContainer = dirContainer, e_dirProjectHost = T.unpack dirHost }
    let Scaffold {..} = mkScaffold env' isolate app'

    toClean <- filterM (fmap not . liftIO . doesFileExist . fst)
      [ (containerMakefile, hostMakefile)
      , (containerPackageJson, hostPackageJson)
      , (containerDockerfile, hostDockerfile)
      ]

    cleanup <- T.intercalate "\n" <$> forM toClean (pure . T.pack . ("rm " <>) . snd)

    local (const env') $ scaffold' isolate True app'

    let rsh = dirContainer </> T.unpack app' <> ".rsh"
    let mjs = dirContainer </> T.unpack app' <> ".mjs"
    let bjs = dirContainer </> "build" </> T.unpack app' <> ".main.mjs"

    let abortIfAbsent p = liftIO . whenM (not <$> doesFileExist p)
          . die $ takeFileName p <> " doesn't exist."

    abortIfAbsent rsh
    abortIfAbsent mjs

    let recompile' = reachEx <> " compile " <> T.pack (dirRel </> T.unpack app' <> ".rsh\n")

    recompile <- liftIO $ ifM (not <$> doesFileExist bjs)
      (pure $ Just recompile')
      $ do
        b <- modificationTime <$> getFileStatus bjs
        r <- modificationTime <$> getFileStatus rsh
        pure $ if r > b then Just recompile' else Nothing

    let dm@DockerMeta {..} = mkDockerMetaConsole env app' isolate
    let dockerfile' = T.pack hostDockerfile
    let args' = T.intercalate " " . map (<> "'") . map ("'" <>) $ app' : args

    withCompose Console dm connectorMode . script $ do
      maybe (pure ()) write recompile
      write [N.text|
        cd $dirHost

        set +e
        docker build -f $dockerfile' --tag=$appImageTag . \
          && docker-compose -f "$$TMP/docker-compose.yml" run --rm $appService $args'
        RES="$?"
        set -e

        $cleanup
        docker-compose -f "$$TMP/docker-compose.yml" down --remove-orphans

        exit "$$RES"
      |]


--------------------------------------------------------------------------------
down :: Subcommand
down = command "down" $ info f d where
  d = progDesc "Halt any Dockerized devnets for this app"
  f = undefined


--------------------------------------------------------------------------------
scaffold :: Subcommand
scaffold = command "scaffold" $ info f d where
  d = progDesc "Set up Docker scaffolding for a simple app"
  f = scaffold'
    <$> switch (long "isolate")
    <*> switch (long "quiet")
    <*> strArgument (metavar "APP" <> value "index" <> showDefault)


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
    let dm@DockerMeta {..} = mkDockerMetaReact
    let cm = ConnectorMode Eth Browser -- TODO

    -- TODO generalize this pattern for other subcommands?
    cargs <- liftIO $ do
      let x "react" = False
          x a | "--dir-project-host" `T.isPrefixOf` a = False
              | otherwise = True
      T.intercalate " " . filter x . fmap T.pack <$> getArgs

    withCompose React dm cm . script $ write [N.text|
      $reachEx compile $cargs
      docker-compose -f "$$TMP/docker-compose.yml" \
        run --service-ports --rm $appService
    |]


--------------------------------------------------------------------------------
switchUseExistingDevnet :: Parser Bool
switchUseExistingDevnet = switch $ long "use-existing-devnet"


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
    env@Env {..} <- ask
    let Var {..} = e_var
    let dm@DockerMeta {..} = mkDockerMetaRpc env

    dieConnectorModeNonBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair

    rpcServer' appService >>= withCompose Rpc dm connectorMode . script . write


--------------------------------------------------------------------------------
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
    let Var {..} = e_var

    let args' = T.intercalate " " args

    let dm@DockerMeta {..} = mkDockerMetaRpc env
    runServer <- rpcServer' appService

    dieConnectorModeNonBrowser
    warnDefRpcKey
    warnScaffoldDefRpcTlsPair

    withCompose Rpc dm connectorMode . script $ write [N.text|
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
        docker-compose -f "$$TMP/docker-compose.yml" down --remove-orphans
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
    env@Env {..} <- ask
    let Var {..} = e_var
    let cm@(ConnectorMode c m) = connectorMode
    let s = devnetFor c

    dieConnectorModeNonBrowser

    unless (m == Devnet) . liftIO
      $ die "`reach devnet` may only be used when `REACH_CONNECTOR_MODE` ends with \"-devnet\"."

    withCompose StandaloneDevnet (mkDockerMetaStandaloneDevnet env) cm . script $ write [N.text|
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
reactDown :: Subcommand
reactDown = command "react-down" $ info f fullDesc where
  f = undefined


--------------------------------------------------------------------------------
rpcServerDown :: Subcommand
rpcServerDown = command "rpc-server-down" $ info f fullDesc where
  f = undefined


--------------------------------------------------------------------------------
unscaffold :: Subcommand
unscaffold = command "unscaffold" $ info f fullDesc where
  f = unscaffold'
    <$> switch (long "isolate")
    <*> switch (long "quiet")
    <*> strArgument (metavar "APP" <> value "index" <> showDefault)


--------------------------------------------------------------------------------
-- TODO better header
header' :: String
header' = "https://reach.sh"


main :: IO ()
main = do
  eff <- newIORef InProcess
  var <- mkVar

  let env = Env
        <$> strOption (long "dir-embed" <> value "/app/embed" <> internal)
        <*> strOption (long "dir-project-container" <> value "/app/src" <> internal)
        <*> strOption (long "dir-project-host" <> internal)
        <*> strOption (long "dir-tmp" <> value "/app/tmp" <> internal)
        <*> switch (long "emit-raw" <> internal)
        <*> pure eff
        <*> pure var

  let cli = Cli
        <$> env
        <*> (hsubparser cs <|> hsubparser hs <**> helper)

  customExecParser (prefs showHelpOnError) (info cli (header header' <> fullDesc))
    >>= \Cli {..} -> runReaderT c_cmd c_env
    >>  readIORef eff
    >>= \case
      InProcess -> pure ()
      Script t -> case e_emitRaw c_env of
        True -> T.putStrLn t
        False -> do
          T.writeFile (e_dirTmp c_env </> "out.sh") t
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
    <> rpcServerDown
    <> unscaffold
    <> whoami
