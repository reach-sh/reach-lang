{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE QuasiQuotes #-}

module ReachPC.ConfigDir (readOrCreateConfigToml, Config) where

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist)
import qualified Toml as T
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import NeatInterpolation (text)
import Control.Monad (when)

-- Represents ~/.config/reach/config.toml
data Config = Config
  { cfg_cloudOrLocal :: String
  , cfg_connectorMode :: String
  }

readOrCreateConfigToml :: IO Config
readOrCreateConfigToml = do
  configTomlPath_ <- configTomlPath
  configTomlMissing <- not <$> doesFileExist configTomlPath_
  when configTomlMissing $ TextIO.writeFile configTomlPath_ defaultConfigToml
  readConfigToml

readConfigToml :: IO Config 
readConfigToml = inConfigDir "reach.toml" >>= T.decodeFile configCodec

configTomlPath :: IO FilePath
configTomlPath = inConfigDir "reach.toml"

authTokenPath :: IO FilePath
authTokenPath = inConfigDir "auth_token"

inConfigDir :: FilePath -> IO FilePath
inConfigDir path = (</> path) <$> getXdgDirectory XdgConfig "reach"

configCodec :: T.TomlCodec Config
configCodec = Config 
  <$> T.string "cloud-or-local" T..= cfg_cloudOrLocal
  <*> T.string "connector-mode" T..= cfg_connectorMode

defaultConfigToml :: Text
defaultConfigToml = [text|
# This config file contains default settings for Reach
# They can be overridden with environment variables

# Whether to use Reach Cloud or Reach Local. Can be "cloud" or "local"
# Can be overridden by REACH_CLOUD_OR_LOCAL
cloud-or-local = "cloud"

# Which connector to use. Can be "algo" or "cfg" or "eth"
# Can be overridden by REACH_CONNECTOR_MODE
connector-mode = "algo"
|]