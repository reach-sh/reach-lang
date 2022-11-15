{- HLINT ignore "Use if" -}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ReachPC.Config
  ( Config(..)
  , getProjectConfig
  , interactiveCreateReachToml
  , interactiveCreateGlobalsToml
  ) where

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List.Split (splitOn, split, onSublist, keepDelimsR)
import Data.Maybe (fromMaybe, fromJust)
import qualified Toml as T
import Control.Monad (forM_)
import System.Environment (getEnvironment)
import Data.Char (toLower, toUpper, isSpace)
import Text.Read (readMaybe)
import qualified ReachPC.CommandLine as Cli
import Control.Applicative ((<|>))
import Data.List (intercalate, dropWhileEnd)
import Control.Exception (IOException, catch)
import System.IO (hFlush, stdout)
import Data.Functor ((<&>))
import qualified Data.Text.IO as TextIO

data CloudOrLocal = Cloud | Local deriving (Show, Read, Enum, Bounded)
data Connector = ALGO | ETH deriving (Show, Read, Enum, Bounded)

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

-- Project config, created from multiple sources. See getProjectConfig for more details.
data Config = Config
  { cfg_cloudOrLocal :: CloudOrLocal
  , cfg_connector :: Connector
  , cfg_forwardEnvVars :: [String]
  , cfg_project :: String
  } deriving (Show)

-- Represents a project's reach.toml file
data ReachToml = ReachToml
  { rtml_cloudOrLocal :: Maybe CloudOrLocal
  , rtml_connector :: Maybe Connector
  , rtml_forwardEnvVars :: Maybe [String]
  , rtml_project :: Maybe String -- TODO split into (organization :: String, project_name :: String)
  } deriving (Show)

-- TODO: currently this will translate a bad setting (eg cloud-or-local = "nonsense") into Nothing
-- instead of throwing an error and printing valid options. This can probably be fixed with T.match
reachTomlCodec :: T.TomlCodec ReachToml
reachTomlCodec = ReachToml
  <$> T.dioptional (T.enumBounded "cloud-or-local") T..= rtml_cloudOrLocal
  <*> T.dioptional (T.enumBounded "connector") T..= rtml_connector
  <*> T.dioptional (T.arrayOf T._String "forward-env-vars") T..= rtml_forwardEnvVars
  <*> T.dioptional (T.string "project") T..= rtml_project

-- Represents ~/.config/reach/config.toml
data GlobalsToml = GlobalsToml
  { gtml_cloudOrLocal :: Maybe CloudOrLocal
  , gtml_connector :: Maybe Connector
  }

globalsTomlCodec :: T.TomlCodec GlobalsToml
globalsTomlCodec = GlobalsToml
  <$> T.dioptional (T.enumBounded "cloud-or-local") T..= gtml_cloudOrLocal
  <*> T.dioptional (T.enumBounded "connector") T..= gtml_connector

-- Generates a Config by reading various sources. If a project reach.toml isn't found, crash.
-- If a global reach.toml isn't found,  make one with global entries.
getProjectConfig :: Cli.CliOptions -> IO Config
getProjectConfig cliOpts = do
  (_, ReachToml{..}) <- readOrErrorReachToml
  GlobalsToml{..} <- readOrEmptyGlobalsToml
  envVars <- getEnvironment
  let envVar = flip lookup envVars
  let unwrapConfigOption = fromMaybe . error . (++ "\nCheck <reach.sh/docs/project-config TODO page> for help\n")

  -- Whether to use reachd-cloud or reachd-local
  -- REACH_CLOUD_OR_LOCAL / --cloud or --local / cloud-or-local = "cloud" or "local" in reach.toml or config.toml
  let env_cloudOrLocal = readCloudOrLocal <$> envVar "REACH_CLOUD_OR_LOCAL"
  let cli_cloudOrLocal = (\b -> if b then Cloud else Local) <$> Cli.cli_cloudOrLocal cliOpts
  let cfg_cloudOrLocal = unwrapConfigOption "Unspecified whether to use Reach Cloud or Reach Local." $
                         env_cloudOrLocal <|> cli_cloudOrLocal <|> rtml_cloudOrLocal <|> gtml_cloudOrLocal

  -- What connector to use (algo/eth/...)
  -- REACH_CONNECTOR_MODE / --connector=... / connector = "algo" or "eth" or ...
  let env_connector = readConnector <$> envVar "REACH_CONNECTOR_MODE"
  let cli_connector = readConnector <$> Cli.cli_connector cliOpts
  let cfg_connector = unwrapConfigOption "Unspecified what connector to use." $
                      env_connector <|> cli_connector <|> rtml_connector <|> gtml_connector

  -- What env vars to forward to the remote (sources are combined, not prioritized)
  -- REACH_FORWARD_ENV_VARS / --env / forward-env-vars = [...] in reach.toml
  let env_forwardEnvVars = maybe [] (splitOn ",") $ envVar "REACH_FORWARD_ENV_VARS"
  let cli_forwardEnvVars = fromMaybe [] $ Cli.cli_forwardEnvVars cliOpts
  let prj_forwardEnvVars = fromMaybe [] $ rtml_forwardEnvVars
  let cfg_forwardEnvVars = env_forwardEnvVars <> cli_forwardEnvVars <> prj_forwardEnvVars

  -- Remote project identifer
  -- project = "..." in reach.toml
  let cfg_project = unwrapConfigOption "Unspecified project name." rtml_project

  return Config{..}

readOrErrorReachToml :: IO (FilePath, ReachToml)
readOrErrorReachToml = reachTomlPath >>= maybe notFound (\p -> sequence (p, T.decodeFile reachTomlCodec p))
  where
    notFound = error (error "No reach.toml was found.")

readOrEmptyGlobalsToml :: IO GlobalsToml
readOrEmptyGlobalsToml = do
  path <- globalsTomlPath
  missing <- not <$> doesFileExist path
  tml <- if missing then return (GlobalsToml Nothing Nothing) else T.decodeFile globalsTomlCodec path
  return tml

-- Up-scan for a file called reach.toml (check ./reach.toml, ../reach.toml, ../../reach.toml etc)
reachTomlPath :: IO (Maybe FilePath)
reachTomlPath = go . tomlPaths =<< getCurrentDirectory
 where
  tomlPaths = map (</> "reach.toml") . reverse . scanl1 (</>) . splitPath
  splitPath = split . keepDelimsR . onSublist $ "/"
  go [] = return Nothing
  go (path:paths) = do
    exists <- doesFileExist path
    if exists then return $ Just path else go paths

globalsTomlPath :: IO FilePath
globalsTomlPath = getXdgDirectory XdgConfig "reach" <&> (</> "config.toml")

-- helper for interactiveCreate*
data PromptAnswer = Unset | Unchanged | Str String

parseAnswer :: (Maybe a) -> (String -> a) -> PromptAnswer -> Maybe a
parseAnswer unchanged readFn = \case
  Unset -> Nothing
  Unchanged -> unchanged
  Str s -> Just $ readFn s

interactiveCreateReachToml :: Bool -> IO ()
interactiveCreateReachToml alreadyExists = do
  oldToml <- if alreadyExists then Just <$> readOrErrorReachToml else return Nothing
  let (oldTomlPath, oldToml') = fromJust oldToml
  let showOld field = case alreadyExists of
        True -> " Currently " <> maybe "unset" (\f -> "\"" <> show f <> "\"") (field oldToml')
        False -> ""
  let parseAnswer' field = parseAnswer $ field oldToml'

  -- TODO make a read function for splitting project into organization and project name
  let projectPrompt = ("\nWhat is the remote name of the project?" <> showOld rtml_project)
  project <- parseAnswer' rtml_project id <$> askUserString alreadyExists False projectPrompt "Project name: "

  let cloudOrLocalPrompt = ("\nDo you want to use Reach Cloud or Reach Local?" <> showOld rtml_cloudOrLocal)
  cloudOrLocal <- parseAnswer' rtml_cloudOrLocal readCloudOrLocal <$> askUserEnum' alreadyExists True cloudOrLocalPrompt (showEnum @CloudOrLocal)

  let connectorPrompt = ("\nWhat connector do you want to use?" <> showOld rtml_connector)
  connector <- parseAnswer' rtml_connector readConnector <$> askUserEnum' alreadyExists True connectorPrompt (showEnum @Connector)

  let forwardEnvVarsPrompt = ("\nWhat environment variables do you want to forward?" <> showOld rtml_forwardEnvVars)
  let readVars = map (dropWhileEnd isSpace . dropWhile isSpace) . splitOn ","
  forwardEnvVars <- parseAnswer' rtml_forwardEnvVars readVars <$> askUserString alreadyExists True forwardEnvVarsPrompt "Env vars (comma separated list, e.g. VAR1,VAR2,VAR3): "

  let newReachToml = ReachToml
        { rtml_project = project
        , rtml_cloudOrLocal = cloudOrLocal
        , rtml_connector = connector
        , rtml_forwardEnvVars = forwardEnvVars
        }
  path <- if alreadyExists then return oldTomlPath else getCurrentDirectory <&> (</> "reach.toml")
  writeToml path reachTomlCodec newReachToml

interactiveCreateGlobalsToml :: IO ()
interactiveCreateGlobalsToml = do
  oldToml <- readOrEmptyGlobalsToml
  let showOld field = " Currently " <> maybe "unset" (\f -> "\"" <> show f <> "\"") (field oldToml)
  let parseAnswer' field = parseAnswer $ field oldToml

  let cloudOrLocalPrompt = ("\nDo you want to use Reach Cloud or Reach Local by default?" <> showOld gtml_cloudOrLocal)
  cloudOrLocal <- parseAnswer' gtml_cloudOrLocal readCloudOrLocal <$> askUserEnum' False True cloudOrLocalPrompt (showEnum @CloudOrLocal)

  let connectorPrompt = ("\nWhat connector do you want to use by default?" <> showOld gtml_connector)
  connector <- parseAnswer' gtml_connector readConnector <$> askUserEnum' False True connectorPrompt (showEnum @Connector)

  let newGlobalsToml = GlobalsToml
        { gtml_cloudOrLocal = cloudOrLocal
        , gtml_connector = connector
        }
  path <- globalsTomlPath
  writeToml path globalsTomlCodec newGlobalsToml

writeToml :: FilePath -> T.TomlCodec a -> a -> IO ()
writeToml path codec toml = do
  let encoded = T.encode codec toml
  TextIO.appendFile path encoded
  putStrLn $ "\nThis is your new config, located at \"" <> path <> "\""
  TextIO.putStr encoded

askUserString :: Bool -> Bool -> String -> String -> IO PromptAnswer
askUserString canBeUnchanged canBeUnset prompt prompt2 = do
  let mustEnter = not (canBeUnchanged || canBeUnset)
  ans <- case mustEnter of
    True -> return $ Str ""
    False -> askUserEnum' canBeUnchanged canBeUnset prompt ["Enter new"]
  case ans of
    Str _ -> do
      putStr prompt2
      hFlush stdout
      Str <$> getLine
    _ -> return ans

askUserEnum' :: Bool -> Bool -> String -> [String] -> IO PromptAnswer
askUserEnum' canBeUnchanged canBeUnset prompt answers = do
  let canBe cond val = if cond then (val :) else id
  let answers' = canBe canBeUnchanged "Leave unchanged" $ canBe canBeUnset "Unset" $ answers
  ans <- askUserEnum prompt answers'
  return $ if | canBeUnchanged && ans == "Leave unchanged" -> Unchanged
              | canBeUnset     && ans == "Unset" -> Unset
              | otherwise -> Str ans

askUserEnum :: String -> [String] -> IO String
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
