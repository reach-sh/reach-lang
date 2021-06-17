{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.IORef
import Options.Applicative
import Options.Applicative.Help.Pretty ((<$$>), text)
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.Posix.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NeatInterpolation as N

import qualified Reach.Version as V


data Env = Env
  { e_embedDir :: FilePath
  , e_emitRaw :: Bool
  , e_emit :: IORef T.Text
  }

type App = ReaderT Env IO ()
type Subcommand = Mod CommandFields App


data Cli = Cli
  { c_env :: Env
  , c_cmd :: App
  }


write :: T.Text -> App
write t = asks e_emit >>= liftIO . flip modifyIORef (<> t <> "\n\n")


writeFrom :: FilePath -> App
writeFrom p = asks e_embedDir >>= liftIO . T.readFile . (</> p) >>= write


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


reachVersion :: App
reachVersion =
  let v = T.pack V.versionStr
  in write [N.text|
    if [ "x$$REACH_VERSION" = "x" ]; then
      REACH_VERSION="$v"
    fi
  |]


reachVersionShort :: App
reachVersionShort =
  let compat = T.pack V.compatibleVersionStr
  in write [N.text|
    if [ "$$REACH_VERSION" = "stable" ] ; then
      REACH_VERSION_SHORT="$compat"
    else
      REACH_VERSION_SHORT=$(echo "$$REACH_VERSION" | sed 's/^v//' | awk -F. '{print $$1"."$$2}')
    fi
  |]


realpath :: App
realpath = writeFrom "sh/_common/realpath.sh"


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

  go m i = write [N.text|
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
  f = pure $ do
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
        ~/.local/bin/reachc --disable-reporting --intermediate-files "$@@"

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

  go :: FilePath -> T.Text -> App
  go template app = do
    Env {..} <- ask
    let tmpl n = e_embedDir </> "template" </> "init" </> n

    path <- ifM (liftIO . doesDirectoryExist $ tmpl template)
      (pure $ tmpl template)
      (pure $ tmpl "_default")

    fmtInitRsh <- liftIO . T.readFile $ path </> "index.rsh"
    fmtInitMjs <- liftIO . T.readFile $ path </> "index.mjs"

    let rsh = app <> ".rsh"
    let mjs = app <> ".mjs"

    let fmtInitMjs' = swap "APP" app fmtInitMjs

    reachVersion
    reachVersionShort

    write [N.text|
      if [ -f "$rsh" ] ; then
        echo "$rsh already exists"
        exit 1
      fi

      if [ -f "$mjs" ] ; then
        echo "$mjs already exists"
        exit 1
      fi

      echo Writing "$rsh"
      cat >"${rsh}" <<EOF
      $fmtInitRsh
      EOF

      echo Writing "$mjs"
      cat >"${mjs}" <<EOF
      $fmtInitMjs'
      EOF
    |]


--------------------------------------------------------------------------------
run' :: Subcommand
run' = command "run" $ info f d where
  d = progDesc "Run a simple app"
  f = undefined


--------------------------------------------------------------------------------
down :: Subcommand
down = command "down" $ info f d where
  d = progDesc "Halt any Dockerized devnets for this app"
  f = undefined


--------------------------------------------------------------------------------
scaffold :: Subcommand
scaffold = command "scaffold" $ info f d where
  d = progDesc "Set up Docker scaffolding for a simple app"
  f = undefined


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
  f = forM_ reachImages $ \i -> write
    $ "docker pull reachsh/" <> i <> ":" <> T.pack V.compatibleVersionStr


--------------------------------------------------------------------------------
dockerReset :: Subcommand
dockerReset = command "docker-reset" $ info f d where
  d = progDesc "Docker kill and rm all images"
  f = pure $ write [N.text|
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
  f = let v = T.pack V.versionHeader
       in pure $ write [N.text| echo $v |]


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
    reachVersion
    forM_ reachImages $ \i -> write [N.text|
      echo "$i:" "$(docker run --entrypoint /bin/sh "reachsh/$i:$$REACH_VERSION" -c 'echo $$REACH_GIT_HASH')"
    |]


--------------------------------------------------------------------------------
whoami' :: T.Text
whoami' = "docker info --format '{{.ID}}' 2>/dev/null"


whoami :: Subcommand
whoami = command "whoami" $ info f fullDesc where
  f = pure $ write whoami'


--------------------------------------------------------------------------------
numericVersion :: Subcommand
numericVersion = command "numeric-version" $ info f fullDesc where
  f = let v = T.pack V.compatibleVersionStr
       in pure $ write [N.text| echo $v |]


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
  f = undefined


--------------------------------------------------------------------------------
env :: IORef T.Text -> Parser Env
env emit = Env
  <$> strOption (long "embed-dir" <> value "/app/embed" <> hidden)
  <*> switch (long "emit-raw" <> hidden)
  <*> pure emit


--------------------------------------------------------------------------------
-- TODO better header
header' :: String
header' = "https://reach.sh"


main :: IO ()
main = do
  emit <- newIORef "#!/bin/sh\nset -e\n\n"

  let cli = Cli
        <$> env emit
        <*> (hsubparser cs <|> hsubparser hs <**> helper)

  customExecParser (prefs showHelpOnError) (info cli (header header' <> fullDesc))
    >>= \Cli {..} -> runReaderT c_cmd c_env
    >> do
      readIORef emit >>= T.putStrLn
      unless (e_emitRaw c_env)
        $ exitImmediately $ ExitFailure 42

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
