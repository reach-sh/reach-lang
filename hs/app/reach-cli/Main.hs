{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}

module Main (main) where

import Control.Monad.Shell
import Options.Generic
import System.Posix.IO (stdError)

import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

default (T.Text)


data Cli w
  = Clean
    (w ::: Maybe FilePath <?> "Module name")
    (w ::: Maybe T.Text   <?> "Ident")

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
  deriving Generic

deriving instance Show (Cli Unwrapped)

instance ParseRecord (Cli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers


-- | > rm -f "build/$m.$i.mjs
--
-- If:
--  * @m@ is @Just "directory"@ then @cd directory@ and use @"index"@ in @m@'s place;
--  * @m@ is @Just "not-index"@ then @rm -f "build/not-index.$i.mjs"@;
--  * @m@ is @Nothing@ then use @"index"@ in its place;
--  * @i@ is @Nothing@ then use @"main"@ in its place
clean :: Maybe FilePath -> Maybe T.Text -> Script ()
clean m i = do
  let i'   = maybe "main" id i
      f m' = cmd "rm" "-f" $ "build/" <> m' <> "." <> i' <> ".mjs"

  case m of
    Nothing -> f "index"
    Just m' -> ifCmd (test $ TDirExists m')
      (cmd "cd" m' -||- cmd "exit" "1" *> f "index")
      (f $ T.pack m')


whoami :: Script ()
whoami = cmd "docker" "info" "--format" "{{.ID}}"
  |> (stdError, T.unpack "/dev/null")


-- TODO better header
header :: T.Text
header = "https://reach.sh"


main :: IO ()
main = unwrapRecord (T.toStrict header) >>= \case
  Clean m i -> sh $ clean m i
  Whoami    -> sh whoami

  Compile        -> undefined
  Devnet         -> undefined
  DockerReset    -> undefined
  Down           -> undefined
  Hashes         -> undefined
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
