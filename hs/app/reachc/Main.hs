module Main (main) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Either.Extra
import Data.Typeable (cast)
import Reach.AST.Base
import Reach.CommandLine
import Reach.Compiler
import Reach.Report
import Reach.Version
import Safe
import System.Environment
import System.Exit
import System.Process

shouldReport :: CompilerToolArgs -> CompilerToolEnv -> Bool
shouldReport CompilerToolArgs {..} CompilerToolEnv {..} =
  not cta_disableReporting
    && all emptyish [cte_CI, cte_GITHUB_ACTIONS, cte_TF_BUILD]
  where
    emptyish Nothing = True
    emptyish (Just x) = x == ""

apMA :: Monad m => m (a -> m b) -> a -> m b
apMA f = join . ap f . pure

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
  when ("--report" `elem` rawArgs) $ do
    rid <- lookupEnv "REACHC_ID"
    (startReport rid "compile" `apMA`) =<< case atMay rawArgs 2 of
      Nothing -> do
        putStrLn "Missing reportable compilation result."
        exitWith $ ExitFailure 1
      Just e' -> (pure . eitherDecode' $ pack e') >>= \case
        Right (e'' :: Either CompileErrorException ()) ->
          pure $ mapLeft SomeException e''
        Left x -> do
          print x
          exitWith $ ExitFailure 1
    exitSuccess
  args <- getCompilerArgs versionCliDisp
  (e :: Either SomeException ()) <-
    try $ compile env $ cta_co args
  case shouldReport args env of
    False -> pure ()
    True -> case e of
      Right _ -> report $ Right ()
      Left (SomeException i) -> case (cast i :: Maybe CompileErrorException) of
        Just i' -> report $ Left i'
        Nothing -> startReport (cte_REACHC_ID env) "compile" `apMA` e
  case e of
    Left exn -> throwIO exn
    Right () -> return ()
 where
  report :: Either CompileErrorException () -> IO ()
  report a = do
    x <- getExecutablePath
    void . spawnCommand $ x <> " --error-format-json --report '" <> (unpack $ encode a) <> "'"
