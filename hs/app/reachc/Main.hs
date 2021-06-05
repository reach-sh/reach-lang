module Main (main) where

import Control.Exception
import Control.Monad
import Reach.CommandLine
import Reach.CompilerTool
import Reach.Report
import Reach.Version
import System.Environment
import System.Exit
import System.FilePath

data CompilerToolEnv = CompilerToolEnv
  { cte_REACHC_ID :: Maybe String
  , cte_REACHC_HASH :: Maybe String
  , cte_CI :: Maybe String -- most CI services
  , cte_GITHUB_ACTIONS :: Maybe String -- Github Actions
  , cte_TF_BUILD :: Maybe String -- Azure Pipelines
  }

makeCompilerToolOpts :: CompilerToolArgs -> CompilerToolEnv -> CompilerToolOpts
makeCompilerToolOpts CompilerToolArgs {..} CompilerToolEnv {} =
  CompilerToolOpts
    { cto_outputDir = maybe defaultOutputDir id cta_outputDir
    , cto_dirDotReach = maybe defaultDirDotReach id cta_dirDotReach
    , cto_source = cta_source
    , cto_tops = cta_tops
    , cto_intermediateFiles = cta_intermediateFiles
    , cto_installPkgs = cta_installPkgs
    }
  where
    defaultOutputDir = takeDirectory cta_source </> "build"
    defaultDirDotReach = takeDirectory cta_source </> ".reach"

getCompilerEnv :: IO CompilerToolEnv
getCompilerEnv = do
  cte_REACHC_ID <- lookupEnv "REACHC_ID"
  cte_REACHC_HASH <- lookupEnv "REACHC_HASH"
  cte_CI <- lookupEnv "CI"
  cte_GITHUB_ACTIONS <- lookupEnv "GITHUB_ACTIONS"
  cte_TF_BUILD <- lookupEnv "TF_BUILD"
  return CompilerToolEnv {..}

shouldReport :: CompilerToolArgs -> CompilerToolEnv -> Bool
shouldReport CompilerToolArgs {..} CompilerToolEnv {..} =
  not cta_disableReporting
    && all emptyish [cte_CI, cte_GITHUB_ACTIONS, cte_TF_BUILD]
  where
    emptyish Nothing = True
    emptyish (Just x) = x == ""

main :: IO ()
main = do
  env <- getCompilerEnv
  let hashStr = case cte_REACHC_HASH env of
        Just hash -> " (" <> hash <> ")"
        Nothing -> ""
  let versionCliDisp = ("reachc " <> versionStr <> hashStr <> " - Reach compiler")
  rawArgs <- getArgs
  when ("--version" `elem` rawArgs) $ do
    putStrLn versionCliDisp
    exitSuccess
  when ("--numeric-version" `elem` rawArgs) $ do
    putStrLn versionStr
    exitSuccess
  when ("--hash" `elem` rawArgs) $ do
    maybe exitFailure putStrLn $ cte_REACHC_HASH env
    exitSuccess
  args <- getCompilerArgs versionCliDisp
  report <-
    case shouldReport args env of
      False -> return $ const $ return ()
      True -> startReport (cte_REACHC_ID env)
  let ctool_opts = makeCompilerToolOpts args env
  (e :: Either SomeException ()) <-
    try $ compilerToolMain ctool_opts
  report e
  case e of
    Left exn -> throwIO exn
    Right () -> return ()
