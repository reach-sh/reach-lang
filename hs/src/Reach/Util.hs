module Reach.Util where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit
import Control.Monad


-- | A simple substitute for Data.ByteString.Char8.pack that handles unicode
bpack :: String -> ByteString
bpack = TE.encodeUtf8 . T.pack

maybeDie :: IO ExitCode -> IO ()
maybeDie ma = do
  ec <- ma
  unless (ec == ExitSuccess)
    (do (exitWith ec))
  return ()

impossible :: String -> b
impossible msg = error $ "impossible situation (i.e. compiler error): " ++ msg

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- Note: drop 1 is safer than init/tail on empty strings
trimQuotes :: String -> String
trimQuotes = reverse . drop 1 . reverse . drop 1
