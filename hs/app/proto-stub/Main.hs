module Main (main) where

import Network.HTTP.Client
import Reach.Proto
import Safe
import Servant.Client
import System.Environment
import System.Exit
import qualified Data.Text as T

main :: IO ()
main = do
  m <- newManager defaultManagerSettings
  getArgs >>= \case
    ["serve"] -> putStrLn ("Serving on port: " <> show stubPort <> "...") >> runStubServer
    ["say"] -> say' m
    ["listen"] -> listen' m
    c -> cmd' m (maybe "help" id $ (drop 1 c) `atMay` 0) $ Req (T.pack <$> drop 2 c) Nothing mempty
 where
  pid = "54321"
  err e = putStrLn ("Failed with:\n" <> e) >> exitWith (ExitFailure 1)
  env m = mkClientEnv m $ BaseUrl Http "localhost" stubPort ""

  msg = \case
    Stdout n m -> putStrLn $ "id# " <> show n <> " (stdout): " <> show m
    Stderr n m -> putStrLn $ "id# " <> show n <> " (stderr): " <> show m
    Keepalive  -> putStrLn $ "(keepalive)"
    ExitCode' n m -> putStrLn ("id# " <> show n <> " (exit code): " <> show m)
      *> exitWith (if m == 0 then ExitSuccess else ExitFailure m)

  say' m = do
    putStrLn $ "Say `hello` to PID# " <> T.unpack pid <> " at stub server on port: " <> show stubPort <> "..."
    say (env m) pid "hello"

  listen' m = do
    putStrLn $ "Listening to stub server on port: " <> show stubPort <> "..."
    listen (env m) err msg pid Stdboth'

  cmd' m c r = do
    putStrLn $ "`reach " <> c <> "` to stub server on port: " <> show stubPort <> "..."
    cmd (env m) c r
