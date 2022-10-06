{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import System.Environment

data Config = Config
  { cfgApiSite :: T.Text
  } deriving (Generic, Show)

instance A.FromJSON Config where
    parseJSON = A.genericParseJSON $
      A.defaultOptions
        { A.fieldLabelModifier = drop 3
        }

getConfig :: T.Text -> IO Config
getConfig ver = do
  let mver = if ver == "" then "" else ("-" <> ver)
  e <- A.eitherDecode <$> simpleHttp (T.unpack $ "https://dev" <> mver <> ".reach.sh/config.json")
  case e of
    Right v -> return v
    Left s -> do
      error $ "Could not decode Reach Cloud configuration: " <> s

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let (inVER, inUID, inKEY) =
        case args of
          [x, y, z] -> (x, y, z)
          _ -> error "XXX"
  cfg@Config {..} <- getConfig inVER
  putStrLn $ show cfg
  putStrLn $ show (inUID, inKEY)
  putStrLn $ show cfgApiSite
