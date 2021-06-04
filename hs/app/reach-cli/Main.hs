{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

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


data Cli
  = Clean FilePath T.Text
  | Compile
  | Devnet
  | DockerReset
  | Down
  | Hashes
  | Help
  | Init
  | NumericVersion
  | React
  | ReactDown
  | RpcRun
  | RpcServer
  | RpcServerDown
  | Run
  | Scaffold
  | Unscaffold
  | Update
  | Upgrade
  | Version
  | Whoami


--------------------------------------------------------------------------------
cmdClean :: Mod CommandFields Cli
cmdClean = command "clean" . info opts $ fullDesc <> desc <> fdoc
 where
  desc = progDesc "Delete 'build/$MODULE.$IDENT.mjs'"
  fdoc = footerDoc . Just
     $  text "MODULE is \"index\" by default"
   <$$> text "IDENT  is \"main\"  by default"
   <$$> text ""
   <$$> text "If:"
   <$$> text " * MODULE is a directory then `cd $MODULE && rm -f \"build/index.$IDENT.mjs\";"
   <$$> text " * MODULE is <something-else> then `rm -f \"build/$MODULE.$IDENT.mjs\""
  opts = Clean
    <$> strArgument (metavar "MODULE" <> value "index" <> showDefault)
    <*> strArgument (metavar "IDENT"  <> value "main"  <> showDefault)


-- | > rm -f "build/$m.$i.mjs
clean :: FilePath -> T.Text -> Script ()
clean m i = do
  let f m' = cmd "rm" "-f" $ "build/" <> T.pack m' <> "." <> i <> ".mjs"
  if m == "index"
    then f m
    else ifCmd (test $ TDirExists m)
      (cmd "cd" m -||- cmd "exit" "1" *> f "index")
      (f m)


--------------------------------------------------------------------------------
cmdCompile :: Mod CommandFields Cli
cmdCompile = command "compile" $ info (pure Compile) desc where
  desc = progDesc "Compile an app"


compile :: Script ()
compile = undefined


--------------------------------------------------------------------------------
cmdInit :: Mod CommandFields Cli
cmdInit = command "init" $ info (pure Init) desc where
  desc = progDesc "Set up source files for a simple app"


init' :: Script ()
init' = undefined


--------------------------------------------------------------------------------
cmdRun :: Mod CommandFields Cli
cmdRun = command "run" $ info (pure Run) desc where
  desc = progDesc "Run a simple app"


run' :: Script ()
run' = undefined


--------------------------------------------------------------------------------
cmdDown :: Mod CommandFields Cli
cmdDown = command "down" $ info (pure Down) desc where
  desc = progDesc "Halt any dockerized devnets for this app"


down :: Script ()
down = undefined


--------------------------------------------------------------------------------
cmdScaffold :: Mod CommandFields Cli
cmdScaffold = command "scaffold" $ info (pure Scaffold) desc where
  desc = progDesc "Set up Docker scaffolding for a simple app"


scaffold :: Script ()
scaffold = undefined


--------------------------------------------------------------------------------
cmdReact :: Mod CommandFields Cli
cmdReact = command "react" $ info (pure React) desc where
  desc = progDesc "Run a simple React app"


react :: Script ()
react = undefined


--------------------------------------------------------------------------------
cmdRpcServer :: Mod CommandFields Cli
cmdRpcServer = command "rpc-server" $ info (pure RpcServer) desc where
  desc = progDesc "Run a simple Reach RPC server"


rpcServer :: Script ()
rpcServer = undefined


--------------------------------------------------------------------------------
cmdRpcRun :: Mod CommandFields Cli
cmdRpcRun = command "rpc-run" $ info (pure RpcRun) desc where
  desc = progDesc "Run an RPC server + frontend with development configuration"


rpcRun :: Script ()
rpcRun = undefined


--------------------------------------------------------------------------------
cmdDevnet :: Mod CommandFields Cli
cmdDevnet = command "devnet" $ info (pure Devnet) desc where
  desc = progDesc "Run only the devnet"


devnet :: Script ()
devnet = undefined


--------------------------------------------------------------------------------
cmdUpgrade :: Mod CommandFields Cli
cmdUpgrade = command "upgrade" $ info (pure Upgrade) desc where
  desc = progDesc "Upgrade Reach"


upgrade :: Script ()
upgrade = undefined


--------------------------------------------------------------------------------
cmdUpdate :: Mod CommandFields Cli
cmdUpdate = command "update" $ info (pure Update) desc where
  desc = progDesc "Update Reach Docker images"


update :: Script ()
update = undefined


--------------------------------------------------------------------------------
cmdDockerReset :: Mod CommandFields Cli
cmdDockerReset = command "docker-reset" $ info (pure DockerReset) desc where
  desc = progDesc "Docker kill and rm all images"


dockerReset :: Script ()
dockerReset = undefined


--------------------------------------------------------------------------------
cmdVersion :: Mod CommandFields Cli
cmdVersion = command "version" $ info (pure Version) desc where
  desc = progDesc "Display version"


version :: Script ()
version = cmd "echo" (quote $ "reach " <> reachVersion)


--------------------------------------------------------------------------------
cmdHelp :: Mod CommandFields Cli
cmdHelp = command "help" $ info (pure Help) desc where
  desc = progDesc "Show usage"


help' :: Script ()
help' = undefined


--------------------------------------------------------------------------------
cmdHashes :: Mod CommandFields Cli
cmdHashes = command "hashes" $ info (pure Hashes) desc where
  desc = progDesc "Display git hashes used to build each Docker image"


hashes :: Script ()
hashes = flip mapM_ reachImages $ \i -> do
  let t = "reachsh/" <> i <> ":" <> reachVersion
      s = cmd "docker" "run" "--entrypoint" "/bin/sh" t "-c" (quote "echo $REACH_GIT_HASH")
  cmd "echo" (i <> ":") (Output s)


--------------------------------------------------------------------------------
cmdWhoami :: Mod CommandFields Cli
cmdWhoami = command "whoami" $ info (pure Whoami) fullDesc


whoami :: Script ()
whoami = cmd "docker" "info" "--format" "{{.ID}}"
  |> (stdError, T.unpack "/dev/null")


--------------------------------------------------------------------------------
cmdNumericVersion :: Mod CommandFields Cli
cmdNumericVersion = command "numeric-version" $ info (pure NumericVersion) fullDesc


numericVersion :: Script ()
numericVersion = cmd "echo" reachVersion


--------------------------------------------------------------------------------
cmdReactDown :: Mod CommandFields Cli
cmdReactDown = command "react-down" $ info (pure ReactDown) fullDesc


reactDown :: Script ()
reactDown = undefined


--------------------------------------------------------------------------------
cmdRpcServerDown :: Mod CommandFields Cli
cmdRpcServerDown = command "rpc-server-down" $ info (pure RpcServerDown) fullDesc


rpcServerDown :: Script ()
rpcServerDown = undefined


--------------------------------------------------------------------------------
cmdUnscaffold :: Mod CommandFields Cli
cmdUnscaffold = command "unscaffold" $ info (pure Unscaffold) fullDesc


unscaffold :: Script ()
unscaffold = undefined


--------------------------------------------------------------------------------
-- TODO better header
header' :: String
header' = "https://reach.sh"


cmds :: ParserInfo Cli
cmds = info (hsubparser cs <|> hsubparser hs <**> helper) im where
  im = header header' <> fullDesc
  cs = cmdCompile
    <> cmdClean
    <> cmdInit
    <> cmdRun
    <> cmdDown
    <> cmdScaffold
    <> cmdReact
    <> cmdRpcServer
    <> cmdRpcRun
    <> cmdDevnet
    <> cmdUpgrade
    <> cmdUpdate
    <> cmdDockerReset
    <> cmdVersion
    <> cmdHashes
    <> cmdHelp
  hs = internal
    <> commandGroup "hidden subcommands"
    <> cmdNumericVersion
    <> cmdReactDown
    <> cmdRpcServerDown
    <> cmdUnscaffold
    <> cmdWhoami


main :: IO ()
main = customExecParser (prefs showHelpOnError) cmds >>= \case
  Clean m i      -> sh $ clean m i
  Compile        -> sh compile
  Devnet         -> sh devnet
  DockerReset    -> sh dockerReset
  Down           -> sh down
  Hashes         -> sh hashes
  Help           -> sh help'
  Init           -> sh init'
  React          -> sh react
  RpcRun         -> sh rpcRun
  RpcServer      -> sh rpcServer
  Run            -> sh run'
  Scaffold       -> sh scaffold
  Update         -> sh update
  Upgrade        -> sh upgrade
  Version        -> sh version

  -- Hidden
  NumericVersion -> sh numericVersion
  ReactDown      -> sh reactDown
  RpcServerDown  -> sh rpcServerDown
  Unscaffold     -> sh unscaffold
  Whoami         -> sh whoami

 where
  sh f = T.putStrLn . script $ do
    stopOnFailure True
    f
