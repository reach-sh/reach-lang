{- HLINT ignore "Use if" -}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ReachPC.Config
  ( discoverProjectReachToml
  , Config(..)
  , getProjectConfig
  , interactiveConfigCreate
  ) where

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List.Split (splitOn, split, onSublist, keepDelimsR)
import Data.Maybe (fromMaybe)
import qualified Toml as T
import Control.Monad (forM_)
import System.Environment (getEnvironment)
import Data.Char (toLower, toUpper, isSpace)
import Text.Read (readMaybe)
import qualified ReachPC.CommandLine as Cli
import Control.Applicative ((<|>))
import Data.List (intercalate, dropWhileEnd)
-- import qualified Data.ByteString as BS
-- import Data.FileEmbed (embedFile, makeRelativeToProject)
import Control.Exception (IOException, catch)
import System.IO (hFlush, stdout)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

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
  validOpts = intercalate ", " $ showEnum @a

showEnum :: forall a. (Show a, Enum a, Bounded a) => [String]
showEnum = map show $ enumFrom (minBound :: a)

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

emptyRchToml :: RchToml
emptyRchToml = RchToml Nothing Nothing Nothing Nothing Nothing

-- helper for interactiveConfigCreate
data PromptAnswer = Unset | Unchanged | Str String

-- Code for the `reach config` subcommand
interactiveConfigCreate :: IO ()
interactiveConfigCreate = do
  which <- askUserEnum "Which config file do you want to edit?" ["Global", "Project"]
  let global = which == "Global"
  (rchTmlPath, rchTml) <- if global then readOrEmptyGlobalReachToml else readOrErrorProjectReachToml
  let showCurrent field = "Currently " <> maybe "unset" (\f -> "\"" <> show f <> "\"") (field rchTml)

  project <- case global of
              True -> return Nothing
              False -> askUserString ("\nWhat is the remote name of the project? " <> showCurrent rtml_project) "Project name: "
                       <&> \case
                         Unset -> Nothing
                         Unchanged -> rtml_project rchTml
                         Str s -> Just s
  organization <- askUserString ("\nWhat is your organization? " <> showCurrent rtml_organization) "Organization: "
                  <&> \case
                    Unset -> Nothing
                    Unchanged -> rtml_organization rchTml
                    Str s -> Just s
  cloudOrLocal <- askUserEnum' ("\nDo you want to use Reach Cloud or Reach Local? " <> showCurrent rtml_cloudOrLocal) (showEnum @CloudOrLocal)
                  <&> \case
                    Unset -> Nothing
                    Unchanged -> rtml_cloudOrLocal rchTml
                    Str s -> Just $ readCloudOrLocal s
  connector <- askUserEnum' ("\nWhat connector do you want to use? " <> showCurrent rtml_connector) (showEnum @Connector)
               <&> \case
                 Unset -> Nothing
                 Unchanged -> rtml_connector rchTml
                 Str s -> Just $ readConnector s
  forwardEnvVars <- askUserString ("\nWhat environment variables do you want to forward? " <> showCurrent rtml_cloudOrLocal) "Env vars (comma separated list, e.g. VAR1,VAR2,VAR3): "
                    <&> \case
                      Unset -> Nothing
                      Unchanged -> rtml_forwardEnvVars rchTml
                      Str s -> Just . map (dropWhileEnd isSpace . dropWhile isSpace) . splitOn "," $ s
  let newRchTml = RchToml
        { rtml_project = project
        , rtml_organization = organization
        , rtml_cloudOrLocal = cloudOrLocal
        , rtml_connector = connector
        , rtml_forwardEnvVars = forwardEnvVars
        }

  writeFile rchTmlPath "# See this webpage <TODO LINK> for more information\n"
  let encoded = encodeReachToml newRchTml
  TextIO.appendFile rchTmlPath encoded
  putStrLn $ "\nThis is your new config, located at \"" <> rchTmlPath <> "\""
  TextIO.putStr encoded
 where
  askUserString prompt prompt2 = do
    ans <- askUserEnum' prompt ["Enter new"]
    case ans of
      Str _ -> do
        putStr prompt2
        hFlush stdout
        Str <$> getLine
      _ -> return ans
  askUserEnum' prompt answers = do
    ans <- askUserEnum prompt $ "Leave unchanged" : "Unset" : answers
    return $ if | ans == "Leave unchanged" -> Unchanged
                | ans == "Unset" -> Unset
                | otherwise -> Str ans
  askUserEnum prompt answers = do
    let (enumAnswers :: [(Int, String)]) = zip [1..] answers
    putStrLn prompt
    forM_ enumAnswers $ \(n, ans) -> putStrLn $ show n <> ") " <> ans
    putStr "> "
    hFlush stdout
    (n :: Int) <- catch readLn (\(_ :: IOException) -> return (-1))
    case lookup n enumAnswers of
      Just ans -> return ans
      Nothing -> askUserEnum prompt answers

-- Generates a Config by reading various sources. If a project reach.toml isn't found, crash. 
-- If a global reach.toml isn't found,  make one with default entries.
getProjectConfig :: Cli.CliOptions -> IO Config
getProjectConfig cliOpts = do
  (_, projectRchToml) <- readOrErrorProjectReachToml
  (_, globalRchToml) <- readOrEmptyGlobalReachToml
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

readOrEmptyGlobalReachToml :: IO (FilePath, RchToml)
readOrEmptyGlobalReachToml = do
  path <- globalReachTomlPath
  missing <- not <$> doesFileExist path
  tml <- if missing then return emptyRchToml else readReachToml path
  return (path, tml)
  -- TODO which behavior do we want? Automatically create? Or user must create?

  -- when missing $ do
    -- putStrLn $ "Generated global reach.toml at \"" <> path <> "\""
    -- putStrLn "You may want to run `reach config` or edit it yourself."
    -- BS.writeFile path defaultReachToml
  -- inGlobalConfigDir "reach.toml" >>= readReachToml

readOrErrorProjectReachToml :: IO (FilePath, RchToml)
readOrErrorProjectReachToml = discoverProjectReachToml >>= maybe (error "No reach.toml for a project was found.")
                                                                 (\p -> sequence (p, readReachToml p))

-- Up-scan for a file called reach.toml (check ./reach.toml, ../reach.toml, ../../reach.toml etc)
discoverProjectReachToml :: IO (Maybe FilePath)
discoverProjectReachToml = getCurrentDirectory >>= go . tomlPaths
 where
  tomlPaths = map (</> "reach.toml") . reverse . scanl1 (</>) . splitPath
  splitPath = split . keepDelimsR . onSublist $ "/"
  go [] = return Nothing
  go (path:paths) = do
    exists <- doesFileExist path
    if exists then return $ Just path else go paths

globalReachTomlPath :: IO FilePath
globalReachTomlPath = inGlobalConfigDir "reach.toml"

inGlobalConfigDir :: FilePath -> IO FilePath
inGlobalConfigDir path = (</> path) <$> getXdgDirectory XdgConfig "reach"

readReachToml :: FilePath -> IO RchToml
readReachToml = T.decodeFile reachTomlCodec

encodeReachToml :: RchToml -> Text
encodeReachToml = T.encode reachTomlCodec

-- TODO: currently this will translate a bad setting (eg cloud-or-local = "nonsense") into Nothing
-- instead of throwing an error and printing valid options. This can probably be fixed with T.match
reachTomlCodec :: T.TomlCodec RchToml
reachTomlCodec = RchToml
  <$> T.dioptional (T.enumBounded "cloud-or-local") T..= rtml_cloudOrLocal
  <*> T.dioptional (T.enumBounded "connector") T..= rtml_connector
  <*> T.dioptional (T.arrayOf T._String "forward-env-vars") T..= rtml_forwardEnvVars
  <*> T.dioptional (T.string "project") T..= rtml_project

-- defaultReachToml :: BS.ByteString
-- defaultReachToml = $(makeRelativeToProject "app/reachpc/ReachPC/default_reach.toml" >>= embedFile)
