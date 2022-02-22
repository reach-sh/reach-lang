module Reach.Report (Report, startReport) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Time
import Data.Typeable (cast)
import Network.HTTP.Client.Conduit (httpNoBody)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import Reach.CommandLine
import Reach.Version
import Reach.AST.Base (CompileErrorException(..), encodeJSONString)
import System.Environment

--- TODO maybe have each part collect some information and report it back through a (Map String String)
type Report = Either SomeException ()

reportResult :: Report -> String
reportResult = \case
  Right () -> "Right ()"
  report@(Left (SomeException inner)) ->
    case (cast inner :: Maybe CompileErrorException) of
      Just cee -> encodeJSONString cee
      Nothing -> show report

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

  return $ \report -> do
    endTime <- getCurrentTime
    let reportJson =
          object
            [ "userId" .= maybe "Numerius Negidius" id mwho
            , "startTime" .= startTime
            , "version" .= version
            , "elapsed" .= diffUTCTime endTime startTime
            , "result" .= reportResult report
            , "connectorMode" .= cm
            , "usingVisualStudioExtension" .= vse
            , "initiator" .= i
            ]
    m <- send (setRequestBodyJSON reportJson $ setRequestMethod "POST" req)
    let block = waitCatch m
    case report of
      Left {} -> void block
      Right () -> race_ block (threadDelay 1_000_000)
