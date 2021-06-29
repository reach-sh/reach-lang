{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import Control.Monad.Extra
import Control.Monad.Reader
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

import Reach.Util
import Reach.Version
import Reach.CommandLine

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NeatInterpolation as N

-- TODO update `ref-usage` docs once stabilized


data Effect
  = Script T.Text
  | InProcess


data Env = Env
  { e_dirEmbed :: FilePath
  , e_dirProject :: FilePath
  , e_dirTmp :: FilePath
  , e_emitRaw :: Bool
  , e_effect :: IORef Effect
  }

type App = ReaderT Env IO ()
type Subcommand = Mod CommandFields App


data Cli = Cli
  { c_env :: Env
  , c_cmd :: App
  }


script :: App -> App
script a = do
  asks e_effect >>= liftIO . flip writeIORef (Script "#!/bin/sh\nset -e\n\n")
  a


write :: T.Text -> App
write t = asks e_effect >>= liftIO . flip modifyIORef w where
  w = \case
    Script t' -> Script $ t' <> t <> "\n\n"
    InProcess -> impossible "Cannot `write` to an in-process `Effect`"


writeFrom :: FilePath -> App
writeFrom p = asks e_dirEmbed >>= liftIO . T.readFile . (</> p) >>= write


_runSubScript :: App -> Env -> IO T.Text
_runSubScript a e = readIORef (e_effect e) >>= \case
  Script _ -> do
    r <- newIORef $ Script ""
    Script r' <- runReaderT a (e { e_effect = r }) >> readIORef r
    pure r'
  _ -> impossible "`runSubScript` may only be applied to a `Script`"


--------------------------------------------------------------------------------
serviceConnector :: FilePath -> Env -> T.Text -> [T.Text] -> IO T.Text
serviceConnector name Env {..} rv ports = do
  let ports' = case ports of
        [] -> "[]"
        ps -> T.intercalate "\n" $ map ("- " <>) ps

  fmt <- T.readFile $ e_dirEmbed
    </> "sh" </> "_common" </> "_docker-compose" </> "service-" <> name <> ".yml"

  pure
    . swap "REACH_VERSION" rv
    . swap "PORTS" ports'
    $ fmt


serviceDevnetAlgo :: Env -> T.Text -> [T.Text] -> IO T.Text
serviceDevnetAlgo = serviceConnector "devnet-algo"

serviceDevnetCfx :: Env -> T.Text -> [T.Text] -> IO T.Text
serviceDevnetCfx = serviceConnector "devnet-cfx"


serviceDevnetEth :: Env -> T.Text -> [T.Text] -> IO T.Text
serviceDevnetEth = serviceConnector "devnet-eth"


-- TODO dynamic `appService'` definition
mkComposeYml :: T.Text -> T.Text -> [T.Text] -> T.Text
mkComposeYml appService' appImageTag' svs =
  f [N.text|
     version: '3.4'
     services:
       $svs'

       $appService':
         image: $appImageTag'
         depends_on:
           - ethereum-devnet
         environment:
           - REACH_DEBUG
           - REACH_CONNECTOR_MODE=ETH-test-dockerized-geth
           - ETH_NODE_URI=http://ethereum-devnet:8545
    |]
 where
  svs' = T.intercalate "\n" svs
  f = T.intercalate "\n" . fmap T.stripEnd . T.lines


--------------------------------------------------------------------------------
data Connector
  = Algo
  | Cfx
  | Eth

data Mode
  = Devnet
  | Live
  | Browser

data ConnectorMode = ConnectorMode Connector Mode


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


connectorModeNonBrowser :: IO ConnectorMode
connectorModeNonBrowser = do
  rcm <- maybe "ETH-devnet" id <$> lookupEnv "REACH_CONNECTOR_MODE"
  runParserT ((ConnectorMode <$> pConnector <*> pMode) <* eof) () "" rcm
    >>= either (const . die $ "Invalid `REACH_CONNECTOR_MODE`: " <> rcm) pure
    >>= \case
      ConnectorMode _ Browser -> die
        $ "`REACH_CONNECTOR_MODE` cannot select the `browser` target; `browser`"
       <> " is only available via the Reach standard library."
      cm -> pure cm


--------------------------------------------------------------------------------
data Scaffold = Scaffold
  { dockerfile :: T.Text
  , packageJson :: T.Text
  , composeYml :: T.Text
  , makefile :: T.Text
  }


mkScaffold :: Bool -> T.Text -> Scaffold
mkScaffold isolate app = Scaffold
  { dockerfile = f "Dockerfile"
  , packageJson = f "package.json"
  , composeYml = f "docker-compose.yml"
  , makefile = f "Makefile"
  }
 where
  f a = if isolate then a <> "." <> app else a


--------------------------------------------------------------------------------
data DockerMeta = DockerMeta
  { appProj :: T.Text
  , appService :: T.Text
  , appImage :: T.Text
  , appImageTag :: T.Text
  }


-- TODO `e_dirProject` doesn't make sense when containerized
mkDockerMeta :: T.Text -> FilePath -> Bool -> DockerMeta
mkDockerMeta app dirProject isolate = DockerMeta {..} where
  appProj = T.toLower . T.pack $ takeBaseName dirProject
    <> if isolate then ("-" <> T.unpack app) else ""

  appService = "reach-app-" <> appProj
  appImage = "reachsh/" <> appService
  appImageTag = appImage <> ":latest"


--------------------------------------------------------------------------------
swap :: T.Text -> T.Text -> T.Text -> T.Text
swap a b src = T.replace ("${" <> a <> "}") b src


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
reachVersionInProcess :: IO T.Text
reachVersionInProcess = lookupEnv "REACH_VERSION" >>= \case
  Nothing -> pure $ T.pack versionStr
  Just v -> pure $ T.pack v


reachVersionShortInProcess :: IO T.Text
reachVersionShortInProcess = reachVersionInProcess >>= \case
  "stable" -> pure $ T.pack compatibleVersionStr
  v -> pure $ a <> "." <> b where
    f ('v':s) = s
    f s = s
    v' = T.splitOn "." . T.pack . f $ T.unpack v
    a = maybe "0" id $ atMay v' 0
    b = maybe "0" id $ atMay v' 1


reachEx :: IO T.Text
reachEx = lookupEnv "REACH_EX" >>= \case
  Just r -> pure $ T.pack r
  Nothing -> die "Unset `REACH_EX` environment variable"


--------------------------------------------------------------------------------
reachVersionScript :: App
reachVersionScript =
  let v = T.pack versionStr
  in write [N.text|
    if [ "x$$REACH_VERSION" = "x" ]; then
      REACH_VERSION="$v"
    fi
  |]


_reachVersionShortScript :: App
_reachVersionShortScript =
  let compat = T.pack compatibleVersionStr
  in write [N.text|
    if [ "$$REACH_VERSION" = "stable" ]; then
      REACH_VERSION_SHORT="$compat"
    else
      REACH_VERSION_SHORT=$(echo "$$REACH_VERSION" | sed 's/^v//' | awk -F. '{print $$1"."$$2}')
    fi
  |]


--------------------------------------------------------------------------------
realpath :: App
realpath = writeFrom "sh/_common/realpath.sh"


_ensureConnectorMode :: App
_ensureConnectorMode = writeFrom "sh/_common/ensureConnectorMode.sh"


_declareFatalInfiniteReachRunLoop :: App
_declareFatalInfiniteReachRunLoop = writeFrom "sh/_common/declareFatalInfiniteReachRunLoop.sh"


--------------------------------------------------------------------------------
scaffold' :: Bool -> Bool -> T.Text -> App
scaffold' isolate quiet app = do
  env@(Env {..}) <- ask

  liftIO $ do
    rv <- reachVersionInProcess
    let Scaffold {..} = mkScaffold isolate app

    let scaff n f = do
          when (not quiet) . T.putStrLn $ "Writing " <> n <> "..."
          T.writeFile (e_dirProject </> T.unpack n) f

    let scaffIfAbsent n f =
          whenM (not <$> doesFileExist (e_dirProject </> T.unpack n)) $ scaff n f

    let cpline = case isolate of
          False -> ""
          True -> "RUN cp /app/" <> packageJson <> " /app/package.json"

    let DockerMeta {..} = mkDockerMeta app e_dirProject isolate

    let tmpl n =
            swap "APP" app
          . swap "MJS" (app <> ".mjs")
          . swap "RSH" (app <> ".rsh")
          . swap "MAKEFILE" makefile
          . swap "DOCKERFILE" dockerfile
          . swap "PACKAGE_JSON" packageJson
          . swap "DOCKER_COMPOSE_YML" composeYml
          . swap "CPLINE" cpline
          . swap "PROJ" appProj
          . swap "SERVICE" appService
          . swap "IMAGE" appImage
          . swap "IMAGE_TAG" appImageTag
          . swap "REACH_VERSION" rv
         <$> (T.readFile $ e_dirEmbed </> "sh" </> "subcommand" </> "scaffold" </> n)

    sAlgo <- serviceDevnetAlgo env rv [ "9392" ]
    sCfx <- serviceDevnetCfx env rv []
    sEth <- serviceDevnetEth env rv []

    -- TODO: s/lint/preapp. It's disabled because sometimes our
    -- generated code trips the linter
    tmpl "package.json" >>= scaff packageJson
    tmpl "Dockerfile" >>= scaff dockerfile
    tmpl "Makefile" >>= scaff makefile
    scaff composeYml $ mkComposeYml appService appImageTag [ sAlgo, sCfx, sEth ]

    tmpl ".gitignore" >>= scaffIfAbsent ".gitignore"
    tmpl ".dockerignore" >>= scaffIfAbsent ".dockerignore"

    when (not quiet) $ putStrLn "Done."


unscaffold' :: Bool -> Bool -> T.Text -> App
unscaffold' isolate quiet app = do
  Env {..} <- ask
  liftIO $ do
    let Scaffold {..} = mkScaffold isolate app
    forM_ [ dockerfile, packageJson, composeYml, makefile ] $ \n -> do
      when (not quiet) . T.putStrLn $ "Deleting " <> n <> "..."
      removeFile $ e_dirProject </> T.unpack n
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

  go CompilerToolArgs {..} = script $ do
    reach <- liftIO reachEx
    rv <- liftIO reachVersionInProcess
    realpath

    write [N.text|
      export REACH="$$(realpath "$reach")"
      HS="$$(dirname "$$REACH")/hs"
      ID=$$($whoami')

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
          -e "REACHC_ID=$${ID}" \
          reachsh/reach:$rv \
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
      rvs <- reachVersionShortInProcess

      let rsh = e_dirProject </> T.unpack app <> ".rsh"
      let mjs = e_dirProject </> T.unpack app <> ".mjs"

      let abortIf x = whenM (doesFileExist x) . die
            $ x <> " already exists."

      abortIf rsh
      abortIf mjs

      T.putStrLn $ "Writing " <> app <> ".rsh..."
      T.writeFile rsh $ swap "REACH_VERSION_SHORT" rvs fmtInitRsh

      T.putStrLn $ "Writing " <> app <> ".mjs..."
      T.writeFile mjs $ swap "APP" app fmtInitMjs
      putStrLn "Done."


--------------------------------------------------------------------------------
-- TODO do we need to preserve $APP-as-directory behavior? Test more thoroughly
run' :: Subcommand
run' = command "run" $ info f d where
  d = progDesc "Run a simple app"
  f = go
    <$> switch (long "isolate")
    <*> strArgument (metavar "APP" <> value "")

  go isolate app = do
    Env {..} <- ask
    mode <- T.pack . show <$> liftIO connectorModeNonBrowser
    reach <- liftIO reachEx

    (app', dir) <- liftIO $ case app of
      "" -> pure ("index", e_dirProject)
      _ -> ifM (not <$> doesDirectoryExist app)
        (pure (T.pack app, e_dirProject))
        (pure ("index", if isAbsolute app then app else e_dirProject </> app))

    let Scaffold {..} = mkScaffold isolate app'

    noneExist <- andM $ fmap not . liftIO . doesFileExist . (dir </>) . T.unpack
      <$> [ dockerfile, packageJson, composeYml ]

    when noneExist $ scaffold' isolate True app'

    let isolate' = if isolate then "--isolate" else ""
    let cleanup = case noneExist of
          True -> [N.text| $reach unscaffold $isolate' --quiet $app' |]
          False -> ":"

    let rsh = T.unpack app' <> ".rsh"
    let mjs = T.unpack app' <> ".mjs"
    let bjs = "build" </> T.unpack app' <> ".main.mjs"

    let abortIfAbsent p = liftIO . whenM (not <$> doesFileExist p)
          . die $ p <> " doesn't exist."

    abortIfAbsent rsh
    abortIfAbsent mjs

    let recompile' = reach <> " compile " <> T.pack rsh <> "\n"

    recompile <- liftIO $ ifM (not <$> doesFileExist bjs)
      (pure $ Just recompile')
      $ do
        b <- modificationTime <$> getFileStatus bjs
        r <- modificationTime <$> getFileStatus rsh
        pure $ if r > b then Just recompile' else Nothing

    let DockerMeta {..} = mkDockerMeta app' dir isolate

    -- TODO args to `docker-compose run` (?)
    script $ do
      maybe (pure ()) write recompile
      write [N.text|
        export REACH_CONNECTOR_MODE=$mode
        docker build -f $dockerfile --tag=$appImageTag .
        docker-compose -f $composeYml run --rm $appService
        docker-compose -f $composeYml down --remove-orphans
        $cleanup
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
react :: Subcommand
react = command "react" $ info f d where
  d = progDesc "Run a simple React app"
  f = undefined


--------------------------------------------------------------------------------
rpcServer :: Subcommand
rpcServer = command "rpc-server" $ info f d where
  d = progDesc "Run a simple Reach RPC server"
  f = undefined


--------------------------------------------------------------------------------
rpcRun :: Subcommand
rpcRun = command "rpc-run" $ info f d where
  d = progDesc "Run an RPC server + frontend with development configuration"
  f = undefined


--------------------------------------------------------------------------------
devnet :: Subcommand
devnet = command "devnet" $ info f d where
  d = progDesc "Run only the devnet"
  f = undefined


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
  f = pure . script $ do
    reachVersionScript
    forM_ reachImages $ \i -> write [N.text|
      echo "$i:" "$(docker run --entrypoint /bin/sh "reachsh/$i:$$REACH_VERSION" -c 'echo $$REACH_GIT_HASH')"
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

  let env = Env
        <$> strOption (long "dir-embed" <> value "/app/embed" <> hidden)
        <*> strOption (long "dir-project" <> value "/app/src" <> hidden)
        <*> strOption (long "dir-tmp" <> value "/app/tmp" <> hidden)
        <*> switch (long "emit-raw" <> hidden)
        <*> pure eff

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
