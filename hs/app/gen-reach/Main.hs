{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Monad.Shell hiding (run)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TLIO
import GenReach.EmbeddedFiles
import Reach.Version (versionStr)
import Prelude hiding (init)

default (Text)

----- Helpers

data Globals = Globals
  { gREACH :: Term Var Text
  , gREACH_VERSION :: Term Var Text
  , gHERE :: Term Var Text
  }

exit :: Int -> Script ()
exit i = cmd "exit" (static i)

echo :: CmdParams params => params
echo = cmd "echo"

docker :: CmdParams params => params
docker = cmd "docker"

curl :: CmdParams params => params
curl = cmd "curl"

-- TODO
realpath :: CmdParams params => params
realpath = cmd "realpath"

dirname :: CmdParams params => params
dirname = cmd "dirname"

cp :: CmdParams params => params
cp = cmd "cp"

chmod :: CmdParams params => params
chmod = cmd "chmod"

----- Subcommands

lint :: Globals -> Script ()
lint _ = pure ()

compile :: Globals -> Script ()
compile Globals {..} = do
  hs <- newVarFrom (WithVar gHERE (<> "/..")) (NamedLike "HS") -- TODO
  echo "hs=" hs
  echo "args=" positionalParameters
  pure ()

usage :: Globals -> Script ()
usage _ = echo usage_txt

version :: Globals -> Script ()
version Globals {..} = echo "reach" gREACH_VERSION

numericVersion :: Globals -> Script ()
numericVersion Globals {..} = echo gREACH_VERSION

update :: Globals -> Script ()
update Globals {..} = do
  let reachImage img = WithVar gREACH_VERSION (("reachsh/" <> img <> ":") <>)
  mapM_
    (docker "pull" . reachImage)
    [ "reach"
    , "stdlib"
    , "runner"
    , "ethereum-devnet"
    , "algorand-devnet"
    ]

-- TODO: non-master url?
reachUrl :: Text
reachUrl = "https://raw.githubusercontent.com/reach-sh/reach-lang/hs/sbin/master/reach"

upgrade :: Globals -> Script ()
upgrade Globals {..} = do
  new <- newVarFrom "reach.new" (NamedLike "NEW")
  curl reachUrl "-o" new
    -&&- chmod "+x" new
    -&&- cp "-f" new gREACH
  pure ()

scaffold :: Globals -> Script ()
scaffold _ = do
  pure () -- XXX

unscaffold :: Globals -> Script ()
unscaffold _ = do
  pure () -- XXX

run :: Globals -> Script ()
run _ = do
  pure () -- XXX

init :: Globals -> Script ()
init _ = do
  pure () -- XXX

----- Pulling it all together

-- | Sets global vars:
-- * HERE
-- * REACH
-- * REACH_VERSION
-- Then dispatches to subcommand
mainScript :: Script ()
mainScript = do
  comment "Generated with gen-reach"
  stopOnFailure True
  reach <- scriptPath (NamedLike "REACH")
  realReach <- newVarFrom (Output (realpath reach)) (NamedLike "REAL_REACH")

  gREACH <- globalVar "REACH"
  gREACH_VERSION <- globalVar "REACH_VERSION"
  gHERE <- globalVar "HERE"
  let globals = Globals {..}

  setVar gREACH realReach
  setVar gHERE $ Output (dirname realReach)
  setVar gREACH_VERSION versionStr

  let gFunc n f = func (NamedLike n) (f globals)

  do_lint <- gFunc "lint" lint
  do_compile <- gFunc "compile" compile
  do_usage <- gFunc "usage" usage
  do_version <- gFunc "version" version
  do_numericVersion <- gFunc "numericVersion" numericVersion
  do_update <- gFunc "update" update
  do_upgrade <- gFunc "upgrade" upgrade
  do_scaffold <- gFunc "scaffold" scaffold
  do_unscaffold <- gFunc "unscaffold" unscaffold
  do_run <- gFunc "run" run
  do_init <- gFunc "init" init

  paramsLen <- lengthVar positionalParameters
  zero <- newVarContaining (0 :: Int) (NamedLike "ZERO")
  whenCmd (test (TEqual paramsLen zero)) (do_usage >> exit 1)
  subcommand <- takeParameter (NamedLike "SUBCOMMAND")
  echo reach subcommand

  caseOf
    subcommand
    [ ("compile", do_compile positionalParameters >> exit 0)
    , ("run", do_run positionalParameters >> exit 0)
    , ("init", do_init >> exit 0)
    , ("lint", do_lint positionalParameters >> exit 0)
    , ("update", do_update >> exit 0)
    , ("upgrade", do_upgrade >> exit 0)
    , ("version|--version", do_version >> exit 0)
    , ("numeric-version|--numeric-version", do_numericVersion >> exit 0)
    , ("help|--help", do_usage >> exit 0)
    , ("scaffold", do_scaffold >> exit 0)
    , ("unscaffold", do_unscaffold >> exit 0)
    , (glob "*", do_usage >> exit 1)
    ]
  pure ()

main :: IO ()
main = TLIO.putStrLn $ script mainScript
