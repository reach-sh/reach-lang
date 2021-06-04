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
-- TODO better header
header' :: String
header' = "https://reach.sh"


cmds :: ParserInfo Cli
cmds = info (hsubparser cs <|> hsubparser hs <**> helper) im where
  im = header header' <> fullDesc
  cs = cmdClean
    <> cmdHashes
  hs = internal
    <> commandGroup "hidden subcommands"
    <> cmdWhoami


main :: IO ()
main = customExecParser (prefs showHelpOnError) cmds >>= \case
  Clean m i -> sh $ clean m i
  Hashes    -> sh hashes
  Whoami    -> sh whoami

  Compile        -> undefined
  Devnet         -> undefined
  DockerReset    -> undefined
  Down           -> undefined
  Init           -> undefined
  NumericVersion -> undefined
  React          -> undefined
  ReactDown      -> undefined
  RpcRun         -> undefined
  RpcServer      -> undefined
  RpcServerDown  -> undefined
  Run            -> undefined
  Scaffold       -> undefined
  Unscaffold     -> undefined
  Update         -> undefined
  Upgrade        -> undefined
  Version        -> undefined

 where
  sh f = T.putStrLn . script $ do
    stopOnFailure True
    f
