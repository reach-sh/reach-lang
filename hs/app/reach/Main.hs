{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module Main (main) where

import Control.Monad (join)
import Control.Monad.Shell
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Posix.IO (stdError)

import qualified Data.Text         as TS
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL
import qualified NeatInterpolation as N

import qualified Reach.Version     as V

default (TL.Text)


type Subcommand = ParserInfo (Script ())


reachImages :: [TL.Text]
reachImages =
  [ "reach"
  , "ethereum-devnet"
  , "algorand-devnet"
  , "devnet-cfx"
  , "runner"
  , "react-runner"
  , "rpc-server"
  ]


--------------------------------------------------------------------------------
echo :: CmdParams p => p
echo = cmd "echo"


exit :: Int -> Script ()
exit i = cmd "exit" $ static i


docker :: CmdParams p => p
docker = cmd "docker"


rm :: CmdParams p => p
rm = cmd "rm"


stdErrDevNull :: Script () -> Script ()
stdErrDevNull f = f |> (stdError, TL.unpack "/dev/null")


-- | Squelch @stderr@ and continue even if @f@ returns non-zero exit code
regardless :: Script () -> Script ()
regardless f = stdErrDevNull f -||- cmd ":"


-- | A completely silent 'regardless'
regardless' :: Script () -> Script ()
regardless' f = regardless $ toStderr f


--------------------------------------------------------------------------------
clean :: Subcommand
clean = info f $ fullDesc <> desc <> fdoc where
  desc = progDesc "Delete 'build/$MODULE.$IDENT.mjs'"
  fdoc = footerDoc . Just
     $  text "MODULE is \"index\" by default"
   <$$> text "IDENT  is \"main\"  by default"
   <$$> text ""
   <$$> text "If:"
   <$$> text " * MODULE is a directory then `cd $MODULE && rm -f \"build/index.$IDENT.mjs\";"
   <$$> text " * MODULE is <something-else> then `rm -f \"build/$MODULE.$IDENT.mjs\""

  go m i = do
    let f' m' = rm "-f" $ "build/" <> TL.pack m' <> "." <> i <> ".mjs"

    case m of
      "index" -> f' m
      _       -> ifCmd (test $ TDirExists m)
        (cmd "cd" m -||- exit 1 *> f' "index")
        (f' m)

  f = go
    <$> strArgument (metavar "MODULE" <> value "index" <> showDefault)
    <*> strArgument (metavar "IDENT"  <> value "main"  <> showDefault)


--------------------------------------------------------------------------------
compile :: Subcommand
compile = info f d where
  d = progDesc "Compile an app"
  f = undefined


--------------------------------------------------------------------------------
reachVersionShort :: TS.Text -- TODO
reachVersionShort = TS.pack V.compatibleVersionStr


initRsh :: TS.Text -> TL.Text
initRsh v = TL.fromStrict [N.text|
  'reach ${v}';

  export const main = Reach.App(() => {
    const Alice = Participant('Alice', {});
    const Bob   = Participant('Bob', {});
    deploy();
    // write your program here

  });
|]


initMjs :: TS.Text -> TL.Text
initMjs app = TL.fromStrict [N.text|
  import {loadStdlib} from '@reach-sh/stdlib';
  import * as backend from './build/${app}.main.mjs';

  (async () => {
    const stdlib = await loadStdlib(process.env);
    const startingBalance = stdlib.parseCurrency(100);

    const alice = await stdlib.newTestAccount(startingBalance);
    const bob = await stdlib.newTestAccount(startingBalance);

    const ctcAlice = alice.deploy(backend);
    const ctcBob = bob.attach(backend, ctcAlice.getInfo());

    await Promise.all([
      backend.Alice(ctcAlice, {
        ...stdlib.hasRandom
      }),
      backend.Bob(ctcBob, {
        ...stdlib.hasRandom
      }),
    ]);

    console.log('Hello, Alice and Bob!');
  })();
|]


init' :: Subcommand
init' = info f $ d <> foot where
  d = progDesc "Set up source files for a simple app"
  f = go <$> strArgument (metavar "APP" <> value "index" <> showDefault)

  foot = footerDoc . Just
     $  text "APP is \"index\" by default"
   <$$> text ""
   <$$> text "Aborts if $APP.rsh or $APP.mjs already exist"

  go :: FilePath -> Script ()
  go app = do
    let rsh = app <> ".rsh"
    let mjs = app <> ".mjs"

    whenCmd (test $ TRegularFileExists rsh) $ do
      echo $ rsh <> " already exists"
      exit 1

    whenCmd (test $ TRegularFileExists mjs) $ do
      echo $ mjs <> " already exists"
      exit 1

    echo $ "Writing " <> rsh
    cmd "cat" |> rsh `hereDocument` initRsh reachVersionShort

    echo $ "Writing " <> mjs
    cmd "cat" |> mjs `hereDocument` initMjs (TS.pack app)


--------------------------------------------------------------------------------
run' :: Subcommand
run' = info f d where
  d = progDesc "Run a simple app"
  f = undefined


--------------------------------------------------------------------------------
down :: Subcommand
down = info f d where
  d = progDesc "Halt any Dockerized devnets for this app"
  f = undefined


--------------------------------------------------------------------------------
scaffold :: Subcommand
scaffold = info f d where
  d = progDesc "Set up Docker scaffolding for a simple app"
  f = undefined


--------------------------------------------------------------------------------
react :: Subcommand
react = info f d where
  d = progDesc "Run a simple React app"
  f = undefined


--------------------------------------------------------------------------------
rpcServer :: Subcommand
rpcServer = info f d where
  d = progDesc "Run a simple Reach RPC server"
  f = undefined


--------------------------------------------------------------------------------
rpcRun :: Subcommand
rpcRun = info f d where
  d = progDesc "Run an RPC server + frontend with development configuration"
  f = undefined


--------------------------------------------------------------------------------
devnet :: Subcommand
devnet = info f d where
  d = progDesc "Run only the devnet"
  f = undefined


--------------------------------------------------------------------------------
upgrade :: Subcommand
upgrade = info f d where
  d = progDesc "Upgrade Reach"
  f = undefined


--------------------------------------------------------------------------------
update :: Subcommand
update = info f d where
  d = progDesc "Update Reach Docker images"
  f = undefined


--------------------------------------------------------------------------------
dockerReset :: Subcommand
dockerReset = info f d where
  d = progDesc "Docker kill and rm all images"
  f = pure $ do
    echo "Docker kill all the things..."
    regardless' $ docker "kill" (Output $ docker "ps" "-q" )
    echo "Docker rm   all the things..."
    regardless' $ docker "rm"   (Output $ docker "ps" "-qa")
    echo "...done"


--------------------------------------------------------------------------------
version :: Subcommand
version = info f d where
  d = progDesc "Display version"
  f = pure $ echo V.versionHeader


--------------------------------------------------------------------------------
help' :: Subcommand
help' = info f d where
  d = progDesc "Show usage"
  f = undefined


--------------------------------------------------------------------------------
hashes :: Subcommand
hashes = info f d where
  d = progDesc "Display git hashes used to build each Docker image"
  f = pure $ flip mapM_ reachImages $ \i -> do
    let t = "reachsh/" <> i <> ":" <> TL.pack V.compatibleVersionStr
    let s = docker "run" "--entrypoint" "/bin/sh" t "-c" (quote "echo $REACH_GIT_HASH")
    echo (i <> ":") (Output s)


--------------------------------------------------------------------------------
whoami :: Subcommand
whoami = info f fullDesc where
  f = pure . stdErrDevNull $ docker "info" "--format" "{{.ID}}"


--------------------------------------------------------------------------------
numericVersion :: Subcommand
numericVersion = info f fullDesc where
  f = pure $ echo V.compatibleVersionStr


--------------------------------------------------------------------------------
reactDown :: Subcommand
reactDown = info f fullDesc where
  f = undefined


--------------------------------------------------------------------------------
rpcServerDown :: Subcommand
rpcServerDown = info f fullDesc where
  f = undefined


--------------------------------------------------------------------------------
unscaffold :: Subcommand
unscaffold = info f fullDesc where
  f = undefined


--------------------------------------------------------------------------------
-- TODO better header
header' :: String
header' = "https://reach.sh"


main :: IO ()
main = join . fmap sh $ customExecParser (prefs showHelpOnError) cmds where
  s |?| p = command s p

  cs = "compile"      |?| compile
    <> "clean"        |?| clean
    <> "init"         |?| init'
    <> "run"          |?| run'
    <> "down"         |?| down
    <> "scaffold"     |?| scaffold
    <> "react"        |?| react
    <> "rpc-server"   |?| rpcServer
    <> "rpc-run"      |?| rpcRun
    <> "devnet"       |?| devnet
    <> "upgrade"      |?| upgrade
    <> "update"       |?| update
    <> "docker-reset" |?| dockerReset
    <> "version"      |?| version
    <> "hashes"       |?| hashes
    <> "help"         |?| help'

  hs = internal
    <> commandGroup "hidden subcommands"
    <> "numeric-version" |?| numericVersion
    <> "react-down"      |?| reactDown
    <> "rpc-server-down" |?| rpcServerDown
    <> "unscaffold"      |?| unscaffold
    <> "whoami"          |?| whoami

  im   = header header' <> fullDesc
  cmds = info (hsubparser cs <|> hsubparser hs <**> helper) im where

  sh f = TL.putStrLn . script $ do
    stopOnFailure True
    f
