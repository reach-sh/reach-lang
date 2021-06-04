{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

import Control.Monad (join)
import Control.Monad.Shell
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Posix.IO (stdError)

import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

default (T.Text)

-- TODO usage help currently prints `reach-cli` instead of `reach`, which may
-- be confusing to end-users who shouldn't have to care that their CLI session
-- is being forwarded to this binary... Consider renaming

type Subcommand = ParserInfo (Script ())


-- TODO
reachVersion :: T.Text
reachVersion = "0.1"

reachImages :: [T.Text]
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


docker :: CmdParams p => p
docker = cmd "docker"


rm :: CmdParams p => p
rm = cmd "rm"


stdErrDevNull :: Script () -> Script ()
stdErrDevNull f = f |> (stdError, T.unpack "/dev/null")


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
    let f' m' = rm "-f" $ "build/" <> T.pack m' <> "." <> i <> ".mjs"

    case m of
      "index" -> f' m
      _       -> ifCmd (test $ TDirExists m)
        (cmd "cd" m -||- cmd "exit" "1" *> f' "index")
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
init' :: Subcommand
init' = info f d where
  d = progDesc "Set up source files for a simple app"
  f = undefined


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
  f = pure $ echo (quote $ "reach " <> reachVersion)


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
    let t = "reachsh/" <> i <> ":" <> reachVersion
    let s = docker "run" "--entrypoint" "/bin/sh" t "-c" (quote "echo $REACH_GIT_HASH")
    echo (i <> ":") (Output s)


--------------------------------------------------------------------------------
whoami :: Subcommand
whoami = info f fullDesc where
  f = pure . stdErrDevNull $ docker "info" "--format" "{{.ID}}"


--------------------------------------------------------------------------------
numericVersion :: Subcommand
numericVersion = info f fullDesc where
  f = pure $ echo reachVersion


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

  sh f = T.putStrLn . script $ do
    stopOnFailure True
    f
