module Reach.Util where

import System.Exit
import Control.Monad

maybeDie :: IO ExitCode -> IO ()
maybeDie ma = do
  ec <- ma
  unless (ec == ExitSuccess)
    (do (exitWith ec))
  return ()

impossible :: String -> b
impossible msg = error $ "impossible situation (i.e. compiler error): " ++ msg
