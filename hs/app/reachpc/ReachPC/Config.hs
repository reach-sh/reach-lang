{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE TemplateHaskell #-}

module ReachPC.Config
  ( readOrCreateGlobalReachToml
  , discoverProjectReachToml
  , Config(..)
  , getProjectConfig
  ) where

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List.Split (splitOn, split, onSublist, keepDelimsR)
import Data.Maybe (fromMaybe)
import qualified Toml as T
import Control.Monad (when)
import System.Environment (getEnvironment)
import Data.Char (toLower, toUpper)
import Text.Read (readMaybe)
import qualified ReachPC.CommandLine as Cli
import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.ByteString as BS (writeFile, ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)

data CloudOrLocal = Cloud | Local deriving (Show, Read, Enum, Bounded)
data Connector = ALGO | CFX | ETH deriving (Show, Read, Enum, Bounded)

-- Utility functions to read the above types
readCloudOrLocal :: String -> CloudOrLocal
readCloudOrLocal = readEnum "CloudOrLocal" . capitalFirst
 where
  capitalFirst (c:s) = toUpper c : map toLower s
  capitalFirst [] = []
  
readConnector :: String -> Connector
readConnector = readEnum "Connector" . capitalAll
 where 
  capitalAll = map toUpper

readEnum :: forall a. (Show a, Read a, Enum a, Bounded a) => String -> String -> a
readEnum what s = fromMaybe (error errMsg) $ readMaybe s
 where
  errMsg = "Invalid " <> what <> ", valid options are: " <> validOpts
  validOpts = intercalate ", " $ map show $ enumFrom (minBound :: a)
 
-- Project config, potentially created from multiple sources. See getProjectConfig for more details.
data Config = Config
  { cfg_cloudOrLocal :: CloudOrLocal
  , cfg_connector :: Connector
  , cfg_forwardEnvVars :: [String]
  , cfg_project :: String
  } deriving (Show)

-- Represents a reach.toml file; Same fields as Config, except every field is optional (because they
-- may also be specified by flags or env vars).
data RchToml = RchToml
  { rtml_cloudOrLocal :: Maybe CloudOrLocal
  , rtml_connector :: Maybe Connector
  , rtml_forwardEnvVars :: Maybe [String]
  , rtml_project :: Maybe String
  } deriving (Show)

-- Generates a Config by reading various sources. If a project reach.toml isn't found, crash. 
-- If a global reach.toml isn't found,  make one with default entries.
getProjectConfig :: Cli.CliOptions -> IO Config
getProjectConfig cliOpts = do
  projectRchToml <- readOrErrorProjectReachToml
  globalRchToml <- readOrCreateGlobalReachToml
  envVars <- getEnvironment
  let envVar = flip lookup envVars
  let unwrapConfigOption = fromMaybe . error . (++ "\nCheck <reach.sh/docs/project-config TODO page> for help\n") 
  
  -- The pattern here is that for every part of the project config, we check (and prioritize)
  -- env var, command line flag, project reach.toml, global reach.toml.
  
  -- Whether to use reachd-cloud or reachd-local
  -- REACH_CLOUD_OR_LOCAL / --cloud or --local / cloud-or-local = "cloud" or "local"
  let env_cloudOrLocal = readCloudOrLocal <$> envVar "REACH_CLOUD_OR_LOCAL"
  let cli_cloudOrLocal = (\b -> if b then Cloud else Local) <$> Cli.cli_cloudOrLocal cliOpts
  let prj_cloudOrLocal = rtml_cloudOrLocal projectRchToml
  let glb_cloudOrLocal = rtml_cloudOrLocal globalRchToml
  let cfg_cloudOrLocal = unwrapConfigOption "Unspecified whether to use Reach Cloud or Reach Local." $
                         env_cloudOrLocal <|> cli_cloudOrLocal <|> prj_cloudOrLocal <|> glb_cloudOrLocal

  -- What connector to use (algo/cfx/eth/...)
  -- REACH_CONNECTOR / --connector=... / connector = "algo" or "eth" or ...
  let env_connector = readConnector <$> envVar "REACH_CONNECTOR"
  let cli_connector = readConnector <$> Cli.cli_connector cliOpts
  let prj_connector = rtml_connector projectRchToml
  let glb_connector = rtml_connector globalRchToml
  let cfg_connector = unwrapConfigOption "Unspecified what connector to use." $
                      env_connector <|> cli_connector <|> prj_connector <|> glb_connector

  -- What env vars to forward to the remote (sources are combined, not prioritized)
  -- REACH_FORWARD_ENV_VARS / --env / forward-env-vars = [...]
  let env_forwardEnvVars = maybe [] (splitOn ",") $ envVar "REACH_FORWARD_ENV_VARS"
  let cli_forwardEnvVars = fromMaybe [] $ Cli.cli_forwardEnvVars cliOpts
  let prj_forwardEnvVars = fromMaybe [] $ rtml_forwardEnvVars projectRchToml
  let gbl_forwardEnvVars = fromMaybe [] $ rtml_forwardEnvVars globalRchToml
  let cfg_forwardEnvVars = env_forwardEnvVars <> cli_forwardEnvVars <> prj_forwardEnvVars <> gbl_forwardEnvVars
  
  -- Remote project identifer
  -- REACH_PROJECT / --project=.. / project = ".."
  let env_project = envVar "REACH_PROJECT"
  let cli_project = Cli.cli_project cliOpts
  let prj_project = rtml_project projectRchToml
  let glb_project = rtml_project globalRchToml
  let cfg_project = unwrapConfigOption "Unspecified remote project identifier." $
                    env_project <|> cli_project <|> prj_project <|> glb_project

  return Config{..}

readOrCreateGlobalReachToml :: IO RchToml
readOrCreateGlobalReachToml = do
  configTomlPath <- inConfigDir "reach.toml"
  configTomlMissing <- not <$> doesFileExist configTomlPath
  when configTomlMissing $ do
    putStrLn $ "Generated global reach.toml at \"" <> configTomlPath <> "\""
    putStrLn "You may want to run `reach config` or edit it yourself."
    BS.writeFile configTomlPath defaultReachToml
  inConfigDir "reach.toml" >>= readReachToml

readOrErrorProjectReachToml :: IO RchToml
readOrErrorProjectReachToml = fromMaybe (error "No reach.toml for a project was found.") <$> discoverProjectReachToml

discoverProjectReachToml :: IO (Maybe RchToml)
discoverProjectReachToml = getCurrentDirectory >>= go . tomlPaths
 where
  tomlPaths = map (</> "reach.toml") . reverse . scanl1 (</>) . splitPath
  splitPath = split . keepDelimsR . onSublist $ "/"
  go [] = return Nothing
  go (path:paths) = do
    exists <- doesFileExist path
    if exists then Just <$> readReachToml path else go paths

inConfigDir :: FilePath -> IO FilePath
inConfigDir path = (</> path) <$> getXdgDirectory XdgConfig "reach"

readReachToml :: FilePath -> IO RchToml
readReachToml = T.decodeFile reachTomlCodec

-- TODO: currently this will translate a bad setting (eg cloud-or-local = "nonsense") into Nothing
-- instead of throwing an error and printing valid options. This can probably be fixed with T.match
reachTomlCodec :: T.TomlCodec RchToml
reachTomlCodec = RchToml
  <$> T.dioptional (T.enumBounded "cloud-or-local") T..= rtml_cloudOrLocal
  <*> T.dioptional (T.enumBounded "connector") T..= rtml_connector
  <*> T.dioptional (T.arrayOf T._String "forward-env-vars") T..= rtml_forwardEnvVars
  <*> T.dioptional (T.string "project") T..= rtml_project

-- TODO: put into separate file and include at compile time
defaultReachToml :: ByteString
defaultReachToml = $(makeRelativeToProject "app/reachpc/ReachPC/default_reach.toml" >>= embedFile)
