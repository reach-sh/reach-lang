module Reach.Report (Report, startReport) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Time
import Network.HTTP.Client.Conduit (httpNoBody)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import System.Environment
import Reach.CommandLine
import Reach.Version

--- TODO maybe have each part collect some information and report it back through a (Map String String)
type Report = Either SomeException ()

startReport :: Maybe String -> String -> IO (Report -> IO ())
startReport mwho i = do
  startTime <- getCurrentTime
  cm <- lookupEnv "REACH_CONNECTOR_MODE" >>= maybe (pure "") pure
  vse <- truthyEnv <$> lookupEnv "REACH_IDE"
  req <- parseRequest $ "https://log.reach.sh/submit"
  manager <- newManager tlsManagerSettings
  let send log_req = async $ runReaderT (httpNoBody log_req) manager

  --- Prime the connection to the server
  _ignored <- send (setRequestMethod "OPTIONS" req)

  return $ \what -> do
    endTime <- getCurrentTime
    let rep =
          object
            [ "userId" .= maybe "Numerius Negidius" id mwho
            , "startTime" .= startTime
            , "version" .= version
            , "elapsed" .= diffUTCTime endTime startTime
            , "result" .= show what
            , "connectorMode" .= cm
            , "usingVisualStudioExtension" .= vse
            , "initiator" .= i
            ]
    m <- send (setRequestBodyJSON rep $ setRequestMethod "POST" req)
    let block = waitCatch m
    case what of
      Left {} -> void block
      Right () -> race_ block (threadDelay 1_000_000)
