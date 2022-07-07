module Main (main) where

import Data.List (intercalate)
import Network.HTTP.Client
import Reach.Proto
import Safe
import Servant.Client
import System.Console.Pretty
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  m <- newManager defaultManagerSettings
  getArgs >>= \case
    ["serve"] -> do
      putStrLn . color Magenta $ "Serving on port: " <> (style Bold $ show stubPort) <> "..."
      runStubServer
    c -> cmd' m
      (maybe "help" id $ (drop 1 c) `atMay` 0)
      (Just $ XProject "matt" "rps")
      $ Req (drop 2 c) Nothing mempty
 where
  err e = putStrLn ("Failed with:\n" <> e) >> exitWith (ExitFailure 1)
  env m = mkClientEnv m $ BaseUrl Http "localhost" stubPort ""

  exitWith' n = exitWith (if n == 0 then ExitSuccess else ExitFailure n)

  msg = \case
    Stdout n m -> putStrLn $ "id# " <> show n <> " (stdout): " <> BL.toString m
    Stderr n m -> putStrLn $ "id# " <> show n <> " (stderr): " <> BL.toString m
    Keepalive  -> putStrLn $ "(keepalive)"
    ExitCode' n m -> putStrLn ("id# " <> show n <> " (exit code): " <> show m) *> exitWith' m

  listen' m p = do
    putStrLn . color Magenta
      $ "Listening to stub server on port: " <> (style Bold $ show stubPort) <> "..."
    listen (env m) err msg p Stdboth'

  cmd' m c mp r@Req{..} = do
    let as = if null req_args then "" else " " <> intercalate " " req_args
    putStrLn . color Magenta $ (style Bold $ "`reach " <> c <> as <> "`")
      <> " to stub server on port: " <> show stubPort <> "..."
    cmd (env m) err c mp r $ \case
      -- TODO redirects mean we'll need to swap the "manager" that lives in our `ClientEnv`
      Redirect h -> putStrLn $ "Received redirect to host: " <> h
      Interpret i -> case i of
        ExitStdout n t -> T.hPutStrLn stdout (t <> "\n") *> exitWith' n
        ExitStderr n t -> T.hPutStrLn stderr (t <> "\n") *> exitWith' n
        AttachStreamJustListen p _n -> listen' m p
        AttachStreamListenSpeak _ _ -> do
          -- TODO: this will involve a multi-threaded system whereby we wait for
          -- input from local stdin and immediately `say` it while simultaneously
          -- `listen`ing for std(out|err) coming from the cloud and piping that
          -- to console in an async-safe way
          pure ()
        PutProjectUpdates _ -> maybe (fail "You must specify a project!") pure mp >>= \p -> do
          putStrLn . color Magenta
            $ "Syncing project `"
           <> T.unpack (fromXProject p)
           <> "` to stub server on port: " <> (style Bold $ show stubPort) <> "..."
          projPut (env m) p . ReqProjPut $ M.fromList [] -- TODO
