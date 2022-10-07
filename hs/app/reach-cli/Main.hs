{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.HTTP.Simple (setRequestBodyJSON)
import qualified Network.HTTP.Types as HTTP
import System.Environment

data Config = Config
  { cfgApiSite :: String
  } deriving (Generic, Show)

instance A.FromJSON Config where
    parseJSON = A.genericParseJSON $
      A.defaultOptions
        { A.fieldLabelModifier = drop 3
        }

data ReachPConfig = ReachPConfig
  { rpcVer :: T.Text
  , rpcUid :: T.Text
  , rpcKey :: T.Text
  } deriving (Show)

t2b :: T.Text -> BS.ByteString
t2b = BC.pack . T.unpack

data AMeth
  = AGet (M.Map BS.ByteString BS.ByteString)
  | APost A.Value

reqMod :: Request -> AMeth -> Request
reqMod req = \case
  AGet m -> req
    { method = "GET"
    , queryString = BS.intercalate "&" $ map go $ M.toList m 
    }
    where
      go (key, val) = mconcat [ e key, "=", e val ]
      e = HTTP.urlEncode True
  APost v -> setRequestBodyJSON v $ req { method = "POST" }

apiJson_ :: A.FromJSON a => Manager -> Request -> BS.ByteString -> AMeth -> IO (Either String a)
apiJson_ m req p meth = do
  let req' = flip reqMod meth $ req { path = p }
  putStrLn $ show req'
  res <- httpLbs req' m
  putStrLn $ show res
  case (responseStatus res == ok200) of
    False -> return $ Left $ BC.unpack $ statusMessage $ responseStatus res
    True -> do
      let b = responseBody res
      case A.eitherDecode b of
        Left s -> return $ Left $ "Decoding error: " <> s
        Right v -> return $ Right v

data API = API
  { apManager :: Manager
  , areq :: Request
  }

makeAPI :: ReachPConfig -> IO API
makeAPI (ReachPConfig {..}) = do
  m <- newManager tlsManagerSettings
  let mver = if rpcVer == "" then "" else ("-" <> rpcVer)
  let curl = (T.unpack $ "https://dev" <> mver <> ".reach.sh")
  creq <- parseRequest curl
  Config {..} <- apiJson_ m creq "config.json" (AGet mempty) >>= \case
    Left s -> error $ "Could not get Reach Cloud configuration: " <> show s
    Right c -> return c
  let url = cfgApiSite
  areq <- applyBearerAuth (t2b $ rpcUid <> "#" <> rpcKey) <$> parseRequest url
  return $ API m areq

apiJson :: (A.ToJSON dom, A.FromJSON rng) => API -> BS.ByteString -> dom -> IO (Either String rng)
apiJson (API {..}) p dv = apiJson_ apManager areq p (APost $ A.toJSON dv)

data ReachInstance = ReachInstance
  { riEndpoint :: String
  , riKey :: String
  } deriving (Generic, Show)

down1 :: String -> String
down1 = \case
  c : v -> Char.toLower c : v
  v -> v

instance A.FromJSON ReachInstance where
    parseJSON = A.genericParseJSON $
      A.defaultOptions
        { A.fieldLabelModifier = down1 . drop 2
        }

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let (inVER, inUID, inKEY) =
        case args of
          [x, y, z] -> (x, y, z)
          _ -> error "XXX"
  a <- makeAPI $ ReachPConfig inVER inUID inKEY
  r <- apiJson a "/ReachInstanceP" $ A.object mempty
  case r of
    Left s -> error $ "Could not get Reach Cloud instance: " <> show s
    Right ri@(ReachInstance {}) -> do
      putStrLn $ show ri
      return ()
