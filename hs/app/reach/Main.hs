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
import System.Posix.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NeatInterpolation as N

import qualified Reach.Util as U
import qualified Reach.Version as V

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
    InProcess -> U.impossible "Cannot `write` to an in-process `Effect`"


writeFrom :: FilePath -> App
writeFrom p = asks e_dirEmbed >>= liftIO . T.readFile . (</> p) >>= write


runSubScript :: App -> Env -> IO T.Text
runSubScript a e = readIORef (e_effect e) >>= \case
  Script _ -> do
    r <- newIORef $ Script ""
    Script r' <- runReaderT a (e { e_effect = r }) >> readIORef r
    pure r'
  _ -> U.impossible "`runSubScript` may only be applied to a `Script`"


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
reachVersionScript :: App
reachVersionScript =
  let v = T.pack V.versionStr
  in write [N.text|
    if [ "x$$REACH_VERSION" = "x" ]; then
      REACH_VERSION="$v"
    fi
  |]


_reachVersionShortScript :: App
_reachVersionShortScript =
  let compat = T.pack V.compatibleVersionStr
  in write [N.text|
    if [ "$$REACH_VERSION" = "stable" ]; then
      REACH_VERSION_SHORT="$compat"
    else
      REACH_VERSION_SHORT=$(echo "$$REACH_VERSION" | sed 's/^v//' | awk -F. '{print $$1"."$$2}')
    fi
  |]


reachVersionInProcess :: IO T.Text
reachVersionInProcess = lookupEnv "REACH_VERSION" >>= \case
  Nothing -> pure $ T.pack V.versionStr
  Just v -> pure $ T.pack v


reachVersionShortInProcess :: IO T.Text
reachVersionShortInProcess = reachVersionInProcess >>= \case
  "stable" -> pure $ T.pack V.compatibleVersionStr
  v -> pure $ a <> "." <> b where
    f ('v':s) = s
    f s = s
    v' = T.splitOn "." . T.pack . f $ T.unpack v
    a = maybe "0" id $ atMay v' 0
    b = maybe "0" id $ atMay v' 1


realpath :: App
realpath = writeFrom "sh/_common/realpath.sh"


ensureConnectorMode :: App
ensureConnectorMode = writeFrom "sh/_common/ensureConnectorMode.sh"


declareFatalInfiniteReachRunLoop :: App
declareFatalInfiniteReachRunLoop = writeFrom "sh/_common/declareFatalInfiniteReachRunLoop.sh"


--------------------------------------------------------------------------------
data Scaffold = Scaffold
  { dockerfile :: T.Text
  , packageJson :: T.Text
  , composeYml :: T.Text
  , makefile :: T.Text
  , isolate' :: T.Text
  , verbose :: T.Text
  }


mkScaffold :: Bool -> Bool -> T.Text -> Scaffold
mkScaffold isolate quiet app = Scaffold
  { dockerfile = f "Dockerfile"
  , packageJson = f "package.json"
  , composeYml = f "docker-compose.yml"
  , makefile = f "Makefile"
  , isolate' = if isolate then "true" else "false"
  , verbose = if quiet then "false" else "true"
  }
 where
  f a = if isolate then a <> "." <> app else a


scaffold' :: Bool -> Bool -> T.Text -> App
scaffold' isolate quiet app = do
  Env {..} <- ask
  let Scaffold {..} = mkScaffold isolate quiet app

  let cpline = case isolate of
        False -> ""
        True -> "RUN cp /app/" <> packageJson <> " /app/package.json"

  let tmpl n =
          swap "APP" app
        . swap "MJS" (app <> ".mjs")
        . swap "RSH" (app <> ".rsh")
        . swap "MAKEFILE" makefile
        . swap "DOCKERFILE" dockerfile
        . swap "PACKAGE_JSON" packageJson
        . swap "DOCKER_COMPOSE_YML" composeYml
        . swap "CPLINE" cpline
       <$> (liftIO . T.readFile $ e_dirEmbed </> "sh" </> "subcommand" </> "scaffold" </> n)

  let scaff n f = write [N.text|
        $verbose && echo "Writing $n"
        cat >"$n" <<EOF
        $f
        EOF
      |]

  let scaffIfAbsent n f = do
        c <- ask >>= liftIO . runSubScript (scaff n f)
        write [N.text|
          if [ ! -f $n ]; then
          $c
          fi
        |]

  fmtDockerfile <- tmpl "Dockerfile"
  fmtPackageJson <- tmpl "package.json"
  fmtComposeYml <- tmpl "docker-compose.yml"
  fmtMakefile <- tmpl "Makefile"
  fmtGitignore <- tmpl ".gitignore"
  fmtDockerignore <- tmpl ".dockerignore"

  reachVersionScript
  ensureConnectorMode

  write [N.text|
    PROJ="$$(basename "$$(pwd)" | tr '[:upper:]' '[:lower:]')"
    "$isolate'" && PROJ="$$PROJ-$app"

    SERVICE="reach-app-$${PROJ}"
    IMAGE="reachsh/$${SERVICE}"
    IMAGE_TAG="$${IMAGE}:latest"
  |]

  -- TODO: s/lint/preapp. It's disabled because sometimes our
  -- generated code trips the linter
  -- TODO: ^ The same goes for js/runner_package.template.json
  scaff packageJson fmtPackageJson

  scaff dockerfile fmtDockerfile
  scaff composeYml fmtComposeYml
  scaff makefile fmtMakefile

  scaffIfAbsent ".gitignore" fmtGitignore
  scaffIfAbsent ".dockerignore" fmtDockerignore


unscaffold' :: Bool -> Bool -> T.Text -> App
unscaffold' isolate quiet app = do
  let Scaffold {..} = mkScaffold isolate quiet app
  write [N.text|
    for file in $dockerfile $packageJson $composeYml $makefile; do
      if $verbose; then echo deleting $$file; fi
      rm -f $$file
    done
  |]


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
-- TODO flesh out `optparse` usage help + defaults
-- TODO extend interface to expose subset of hooks offered by `reachc` CLI
--  * -o|--output DIR
--  * --install-pkgs
--  * --dir-dot-reach
--  * [SOURCE]
--  * [EXPORTS...]
compile :: Subcommand
compile = command "compile" $ info f d where
  d = progDesc "Compile an app"
  f = pure . script $ do
    realpath
    write [N.text|
      REACH="$$(realpath "$$0")"
      export REACH
      HS="$$(dirname "$$REACH")/hs"

      reachc_release () {
        stack build && stack exec -- reachc "$$@"
      }

      reachc_prof () {
        stack build --profile --fast && \
          stack exec --profile -- \
                reachc --disable-reporting --intermediate-files "$$@" +RTS -p
      }

      reachc_dev () {
        stack build --fast && \
          stack exec -- reachc --disable-reporting --intermediate-files "$$@"
      }

      ID=$$($whoami')
      if [ "$$CIRCLECI" = "true" ] && [ -x ~/.local/bin/reachc ]; then
        # TODO test
        ~/.local/bin/reachc --disable-reporting "$@@"

      elif [ -z "$${REACH_DOCKER}" ] \
        && [ -d "$${HS}/.stack-work" ] \
        && (which stack > /dev/null 2>&1); then

        export STACK_YAML="$${HS}/stack.yaml"
        export REACHC_ID=$${ID}
        REACHC_HASH="$$("$${HS}/../scripts/git-hash.sh")"
        export REACHC_HASH

        (cd "$$HS" && make stack)

        # TODO replace dollar-@s below
        if [ "x$${REACHC_RELEASE}" = "xY" ]; then
          reachc_release "$$@"
        elif [ "x$${REACHC_PROFILE}" = "xY" ]; then
          reachc_prof "$$@"
        else
          reachc_dev "$$@"
        fi

      else
        # TODO "docker: invalid reference format"
        docker run \
          --rm \
          --volume "$$PWD:/app" \
          -e "REACHC_ID=$${ID}" \
          reachsh/reach:$${REACH_VERSION} \
          "$$@"
      fi
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

      let abortIf x = whenM (doesFileExist x) $ do
            putStrLn $ x <> " already exists."
            exitImmediately $ ExitFailure 1

      abortIf rsh
      abortIf mjs

      T.putStrLn $ "Writing " <> app <> ".rsh..."
      T.writeFile rsh $ swap "REACH_VERSION_SHORT" rvs fmtInitRsh

      T.putStrLn $ "Writing " <> app <> ".mjs..."
      T.writeFile mjs $ swap "APP" app fmtInitMjs
      putStrLn "Done."


--------------------------------------------------------------------------------
run' :: Subcommand
run' = command "run" $ info f d where
  d = progDesc "Run a simple app"
  f = go <$> strArgument (metavar "APP" <> value "")

  -- reach run args
  -- check state of scaffolded files
  -- * if none exist: scaffold in --isolate --quiet mode, set flag UNSCAFFOLD
  -- * if all exist: just use the existing things
  -- * if some exist: error
  -- make build run
  -- unscaffold if UNSCAFFOLD
  --
  -- XXX Can we add eslint on the JS?
  go :: T.Text -> App
  go app = script $ do
    let app' = if app == "" then "index" else app
    doScaffold <- ask >>= liftIO . runSubScript (scaffold' True True app')

    declareFatalInfiniteReachRunLoop

    write [N.text|
      ANY_CUSTOMIZATION=false
      if ! [ "x$$REACH_CONNECTOR_MODE" = "x" ]; then
        ANY_CUSTOMIZATION=true
      fi
    |]

    ensureConnectorMode
    write [N.text| export RUN_FROM_REACH=${RUN_FROM_REACH:-false} |]

    case app of
      "" -> write [N.text|
        BARE_REACH_RUN=true
        APP="index"
      |]

      app'' -> write [N.text|
        BARE_REACH_RUN=false
        ANY_CUSTOMIZATION=true
        ARG=$app''

        [ "x$$ARG" = "x--" ] && ARG="index"
        [ -d "$$ARG"       ] && ARG="$$ARG/index"

        cd "$(dirname "$$ARG")" || exit 1
        APP="$(basename "$$ARG")"
      |]

    write [N.text|
      RSH="$${APP}.rsh"
      MJS="$${APP}.mjs"

      ! [ -f "$$RSH" ] && (echo "$$RSH doesn't exit"; exit 1)
      ! [ -f "$$MJS" ] && (echo "$$MJS doesn't exit"; exit 1)

      MAKEFILE=Makefile
      DOCKERFILE=Dockerfile
      PACKAGE_JSON=package.json
      DOCKER_COMPOSE_YML=docker-compose.yml

      NONE_EXIST=true
      # Note: Makefile excluded from this check
      [ -f "$$DOCKERFILE" ] || [ -f "$$PACKAGE_JSON" ] || [ -f "$$DOCKER_COMPOSE_YML" ] \
        && NONE_EXIST=false

      do_scaffold () {
      $doScaffold
      }

      if $$NONE_EXIST; then
        do_scaffold

        cleanup () {
          # TODO equivalent doUnscaffold
          # do_unscaffold --isolate --quiet "$$APP"
          :
        }

        # Note: do_scaffold has mutated these vars like so:
        # MAKEFILE="$$MAKEFILE.$app'"
        # DOCKERFILE="$$Dockerfile.$app'"
        # PACKAGE_JSON="$$PACKAGE_JSON.$app'"
        # DOCKER_COMPOSE_YML="$$DOCKER_COMPOSE_YML.$app'"
      else
        cleanup () {
          :
        }

        ALL_EXIST=false
        : && [ -f "$$MAKEFILE" ] \
          && [ -f "$$DOCKERFILE" ] \
          && [ -f "$$PACKAGE_JSON" ] \
          && [ -f "$$DOCKER_COMPOSE_YML" ] \
          && ALL_EXIST=true

        # We trust our scaffolded makefiles,
        # so we only need to check for infinite recurrsion on:
        # * reach run ($$BARE_REACH_RUN), since this is the only potential avenue for inf recursion
        # * a proj with customized scaffolding. ($$ALL_EXIST)
        # * running from inside another reach run ($$RUN_FROM_REACH)
        if $$BARE_REACH_RUN && $$ALL_EXIST && $$RUN_FROM_REACH; then
          fatal_infinite_reach_run_loop
        fi

        if ! $$ALL_EXIST; then
          # TODO: more description on err
          echo "I'm confused, some scaffolded files exist, but not all"
          exit 1
        fi
      fi

      ## TODO abstract and finalize the following ##
      reach_make () {
        RUN_FROM_REACH=true make "$$@" REACH="$${REACH}"
        MAKE_EXIT=$$?
        if [ $$MAKE_EXIT -ne 0 ]; then
          cleanup
          exit $$MAKE_EXIT
        fi
      }

      reach_make_f () {
        reach_make -f "$$MAKEFILE" "$$@"
      }

      if $$BARE_REACH_RUN; then
        # Always build from "scaffolded" file
        reach_make_f build
        # Run from Makefile if present and not "run from reach"
        if [ -f Makefile ] && ! $$RUN_FROM_REACH && ! $$ANY_CUSTOMIZATION; then
          reach_make run "$$@" # TODO thread args correctly
        else
          reach_make_f run "$$@" # TODO thread args correctly
        fi
      else
        # It is assumed that if this is being called from within reach run,
        # then the build step has already been handled.
        # TODO: better use of makefiles so that we just call make build anyway,
        # and it is a noop if nothing needs to be done.
        if ! $$RUN_FROM_REACH; then
          reach_make_f build
        fi

        # This is nuts and possibly a little bit wrong.
        # Easier methods exist but they are outside of POSIX standard.
        escape_args () {
          for arg in "$$APP" "$$@"; do
            escaped_arg=""
            for word in $$arg; do
              escaped_arg="$$escaped_arg$$(printf "%s\ " "$$word")"
            done
            echo "$${escaped_arg%??}"
          done
        }
        ARGS=$$(escape_args "$$@")
        # Yes it apparently has to be exactly "$$(echo $$ARGS)" because reasons.
        # shellcheck disable=SC2116,SC2086
        reach_make_f run-target ARGS="$$(echo $$ARGS)"
      fi

      cleanup
    |]


--------------------------------------------------------------------------------
down :: Subcommand
down = command "down" $ info f d where
  d = progDesc "Halt any Dockerized devnets for this app"
  f = undefined


--------------------------------------------------------------------------------
scaffold :: Subcommand
scaffold = command "scaffold" $ info (script <$> f) d where
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
    $ "docker pull reachsh/" <> i <> ":" <> T.pack V.compatibleVersionStr


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
    echo '...done'
  |]


--------------------------------------------------------------------------------
version :: Subcommand
version = command "version" $ info f d where
  d = progDesc "Display version"
  f = pure . liftIO . T.putStrLn $ T.pack V.versionHeader


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
  f = pure . liftIO . T.putStrLn $ T.pack V.compatibleVersionStr


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
unscaffold = command "unscaffold" $ info (script <$> f) fullDesc where
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
          exitImmediately $ ExitFailure 42

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
    <> version
    <> hashes
    <> help'

  hs = internal
    <> commandGroup "hidden subcommands"
    <> numericVersion
    <> reactDown
    <> rpcServerDown
    <> unscaffold
    <> whoami
