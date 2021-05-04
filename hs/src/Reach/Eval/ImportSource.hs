{- ORMOLU_DISABLE -}

{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Reach.Eval.ImportSource
  ( ImportSource(..)
  , LockFile(..)
  , LockModule(..)

  , PkgError
  , PkgT
  , HasPkgT(..)
  , runPkgT

  , dirDotReach
  , dirGitClones
  , dirLockModules
  , pathLockFile
  , removeMe

  , HostGit(..)
  , HostGitAcct(..)
  , HostGitRef(..)
  , HostGitRepo(..)
  , HostGitDir(..)
  , HostGitFile(..)

  , importSource
  , gitUriOf

  , GitUri(..)
  ) where

import Text.Parsec

import Control.Monad           (unless, when)
import Control.Monad.Except    (MonadError, ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.State     (MonadState, StateT(..), get, modify)
import Crypto.Hash             (Digest, SHA256(..), hashWith, digestFromByteString)
import Data.Aeson              (ToJSONKey, FromJSONKey)
import Data.ByteArray          (ByteArrayAccess, ByteArray, convert)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import Data.Functor.Identity   (Identity)
import Data.List               (intercalate)
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Data.Yaml               (ToJSON(..), FromJSON(..), prettyPrintParseException)
import Data.Yaml               (withText, encode, decodeEither')
import GHC.Generics            (Generic)
import System.Directory        (createDirectoryIfMissing, doesDirectoryExist)
import System.Directory        (doesFileExist, getCurrentDirectory)
import System.Exit             (ExitCode(..))
import System.FilePath         ((</>), isValid, takeFileName, pathSeparator)
import System.Process          (readCreateProcessWithExitCode, shell, cwd)
import Text.Printf             (printf)

import Reach.Util (uncurry5)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text       as T


newtype G a = G a
  deriving (Eq, Show, Generic)

instance (ToJSON   a) => ToJSON   (G a)
instance (FromJSON a) => FromJSON (G a)

newtype HostGitAcct = HostGitAcct  String    deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G  String)
newtype HostGitRepo = HostGitRepo  String    deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G  String)
newtype HostGitRef  = HostGitRef   String    deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G  String)
newtype HostGitDir  = HostGitDir  [FilePath] deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G [FilePath])
newtype HostGitFile = HostGitFile  FilePath  deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G  FilePath)
newtype GitUri      = GitUri       String    deriving (Eq, Show) deriving (ToJSON, FromJSON) via (G  String)


toBase16 :: ByteArrayAccess b => b -> T.Text
toBase16 a = decodeUtf8
  . convertToBase Base16
  $ (convert a :: B.ByteString)


fromBase16 :: ByteArray b => T.Text -> Either T.Text b
fromBase16 = either (Left . T.pack) Right
  . convertFromBase Base16
  . encodeUtf8


newtype SHA = SHA (Digest SHA256)
  deriving (Eq, Show, Ord)

instance ToJSON SHA where
  toJSON (SHA d) = toJSON $ toBase16 d

instance FromJSON SHA where
  parseJSON = withText "SHA" $ \a -> maybe
    (fail $ "Invalid SHA: " <> show a)
    (pure . SHA)
    (digestFromByteString =<< either (const Nothing) Just (fromBase16 @B.ByteString a))

instance ToJSONKey   SHA
instance FromJSONKey SHA


data HostGit
  = GitHub    HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  | BitBucket HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ImportSource
  = ImportLocal     FilePath
  | ImportRemoteGit HostGit
  deriving (Eq, Show)


data LockModule = LockModule
  { host :: HostGit
  , uri  :: GitUri
  , deps :: [ LockModule ]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data LockFile = LockFile
  { version :: Int
  , modules :: M.Map SHA LockModule
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


lockFileEmpty :: LockFile
lockFileEmpty =  LockFile
  { version = 1
  , modules = mempty
  }


--------------------------------------------------------------------------------

data PkgError
  = PkgFileDoesNotExist  FilePath
  | PkgGitCloneFailed    String
  | PkgGitCheckoutFailed String
  | PkgLockHashUnknownFS FilePath
  | PkgLockFileParseEx   String
  deriving (Eq, Show)


newtype PkgT m a = PkgT
  { runPkgT' :: ExceptT PkgError m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError PkgError
             ) via (ExceptT PkgError m)


class Monad m => HasPkgT m where
  dirExists   :: FilePath ->                 PkgT m Bool
  fileExists  :: FilePath ->                 PkgT m Bool
  fileRead    :: FilePath ->                 PkgT m B.ByteString
  fileUpsert  :: FilePath -> B.ByteString -> PkgT m ()
  mkdirP      :: FilePath ->                 PkgT m ()
  pwd         ::                             PkgT m FilePath
  gitClone'   :: String   -> FilePath     -> PkgT m ()
  gitFileRead :: HostGit  ->                 PkgT m B.ByteString


runPkgT :: PkgT m a -> m (Either PkgError a)
runPkgT = runExceptT . runPkgT'


deriving newtype instance MonadIO m => MonadIO (PkgT m)

instance (Monad m, MonadIO (PkgT m)) => HasPkgT m where
  dirExists     = liftIO . doesDirectoryExist
  fileExists    = liftIO . doesFileExist
  fileRead      = liftIO . B.readFile
  fileUpsert f  = liftIO . B.writeFile f
  mkdirP        = liftIO . createDirectoryIfMissing True
  pwd           = liftIO $ getCurrentDirectory

  gitClone' u d = do
    let cmd = printf "git clone %s %s" u d
    liftIO (readCreateProcessWithExitCode (shell cmd) "") >>= \case
      (ExitSuccess,   _, _) -> pure ()
      (ExitFailure _, _, e) -> throwError $ PkgGitCloneFailed e

  gitFileRead h = do
    clone <- (</> gitCloneDirOf h) <$> dirGitClones

    let cmd  = printf "git checkout %s" (gitRefOf h)
        cmd' = (shell cmd) { cwd = Just clone }

    liftIO (readCreateProcessWithExitCode cmd' "") >>= \case
      (ExitSuccess,   _, _) -> pure ()
      (ExitFailure _, _, e) -> throwError $ PkgGitCheckoutFailed e

    fileRead $ clone </> gitFilePathOf h


--------------------------------------------------------------------------------

dirDotReach :: HasPkgT m => PkgT m FilePath
dirDotReach = (</> ".reach") <$> pwd


dirGitClones :: HasPkgT m => PkgT m FilePath
dirGitClones = (</> "warehouse" </> "git") <$> dirDotReach


dirLockModules :: HasPkgT m => PkgT m FilePath
dirLockModules = (</> "sha256") <$> dirDotReach


pathLockFile :: HasPkgT m => PkgT m FilePath
pathLockFile = (</> "reach-lock.yaml") <$> pwd


mkdirPdotReach :: HasPkgT m => PkgT m ()
mkdirPdotReach = do
  warehouse <- dirGitClones
  lockMods  <- dirLockModules
  mkdirP warehouse
  mkdirP lockMods


gitClone :: HasPkgT m => HostGit -> PkgT m ()
gitClone h = do
  mkdirPdotReach

  dest          <- (</> gitCloneDirOf h) <$> dirGitClones
  alreadyCloned <- dirExists dest

  unless alreadyCloned $ gitClone' (gitUriOf h) dest


lockFileRead :: HasPkgT m => PkgT m LockFile
lockFileRead = decodeEither' <$> (pathLockFile >>= fileRead) >>= either
  (throwError . PkgLockFileParseEx . prettyPrintParseException)
  pure


-- TODO fail when module not in lockfile manifest
_lockModuleContents :: HasPkgT m => FilePath -> PkgT m B.ByteString
_lockModuleContents p = do
  mkdirPdotReach

  let sha = takeFileName p

  p' <- (</> (show sha)) <$> dirLockModules

  hashUnknownFS <- not <$> fileExists p'

  when hashUnknownFS
    $ throwError $ PkgLockHashUnknownFS p'

  fileRead p


lockModuleFix :: HasPkgT m => HostGit -> PkgT m (SHA, LockModule)
lockModuleFix h = do
  mkdirPdotReach

  gitClone h

  reach <- gitFileRead h
  lockf <- pathLockFile
  lmods <- dirLockModules

  lock <- fileExists lockf >>= \case
    True  -> lockFileRead
    False -> pure lockFileEmpty

  let sha  = hashWith SHA256 reach
      dest = lmods </> (T.unpack $ toBase16 sha)
      lmod = LockModule { host = h
                        , uri  = GitUri . gitUriOf $ h
                        , deps = [] -- TODO
                        }

  fileUpsert dest reach

  -- TODO YAML comment about committing to source control but not editing
  fileUpsert lockf . encode
    $ lock { modules = M.insert (SHA sha) lmod (modules lock) }

  pure (SHA sha, lmod)


removeMe :: FilePath -> HasPkgT m => PkgT m (SHA, LockModule)
removeMe f = case importSource f of
  Left  e                   -> throwError . PkgGitCloneFailed $ show e
  Right (ImportLocal _)     -> throwError $ PkgGitCheckoutFailed "nope"
  Right (ImportRemoteGit h) -> lockModuleFix h


--------------------------------------------------------------------------------

data PkgTestEnv = PkgTestEnv
  { filesystem :: M.Map FilePath B.ByteString
  } deriving Show


newtype PkgTest a = PkgTest (StateT PkgTestEnv Identity a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState PkgTestEnv
           ) via (StateT PkgTestEnv Identity)

deriving newtype instance MonadState PkgTestEnv (PkgT PkgTest)


instance HasPkgT PkgTest where
  dirExists  f = get >>= pure . (/= Nothing) . M.lookup f . filesystem
  fileExists f = get >>= pure . (/= Nothing) . M.lookup f . filesystem

  fileRead f = do
    PkgTestEnv {..} <- get
    pure (M.lookup f filesystem) >>= \case
      Nothing -> throwError $ PkgFileDoesNotExist f
      Just c  -> pure c

  fileUpsert f c = modify
    $ \s -> s { filesystem = M.insert f c (filesystem s) }

  -- TODO
  gitClone' _ _ = pure ()
  gitFileRead _ = pure ""
  mkdirP      _ = pure ()
  pwd           = pure ""


--------------------------------------------------------------------------------

mostHosts :: Parsec String () (HostGitAcct, HostGitRepo, HostGitRef, HostGitDir, HostGitFile)
mostHosts = do
  (,,,,) <$> (HostGitAcct <$> manyTill (allowed "host account") (char '/'))
         <*> (HostGitRepo <$> manyTill (allowed "repo")         (char '#'))
         <*> (HostGitRef  <$> manyTill (allowed "ref")          (char '/'))
         <*> (HostGitDir  <$> many dir)
         <*> (HostGitFile <$> (filename <|> pure "index.rsh"))

 where
  allowed t = alphaNum <|> oneOf "-_."
    <?> "valid git " <> t <> " character (alphanumeric, -, _, .)"

  dir = try $ manyTill (allowed "directory") (char '/')

  filename = do
    n <- manyTill (allowed "file") (try . lookAhead $ string ".rsh" <* eof)
    pure $ n <> ".rsh"


remoteGit :: Parsec FilePath () ImportSource
remoteGit = do
  let h |?| p   = uncurry5 h <$> p; infixr 3 |?|
      bitbucket = BitBucket |?|      (try $ string "bitbucket.org:") *> mostHosts
      github    = GitHub    |?| (optional $ string    "github.com:") *> mostHosts

  _ <- string "@"
  h <- bitbucket <|> github <?> "git host"

  pure $ ImportRemoteGit h


localPath :: Parsec FilePath () ImportSource
localPath = do
  p <- manyTill anyChar eof
  if isValid p then pure $ ImportLocal p
               else fail $ "Invalid local path: " <> p


importSource :: FilePath -> Either ParseError ImportSource
importSource = runParser (remoteGit <|> localPath) () ""


--------------------------------------------------------------------------------

-- TODO: better data structures will negate the need for these

gitUriOf :: HostGit -> String
gitUriOf = \case
  GitHub    a r _ _ _ -> f "https://github.com/%s/%s.git"    a r
  BitBucket a r _ _ _ -> f "https://bitbucket.org/%s/%s.git" a r
 where f fmt (HostGitAcct a) (HostGitRepo r) = printf fmt a r


gitRefOf :: HostGit -> String
gitRefOf = \case
  GitHub    _ _ (HostGitRef r) _ _ -> r
  BitBucket _ _ (HostGitRef r) _ _ -> r


gitCloneDirOf :: HostGit -> String
gitCloneDirOf = \case
  GitHub    a r _ _ _ -> f "@github.com:%s:%s"    a r
  BitBucket a r _ _ _ -> f "@bitbucket.org:%s:%s" a r
 where f fmt (HostGitAcct a) (HostGitRepo r) = printf fmt a r


gitFilePathOf :: HostGit -> FilePath
gitFilePathOf = \case
  GitHub    _ _ _ (HostGitDir d) (HostGitFile f) -> (intercalate (pathSeparator : "") d) </> f
  BitBucket _ _ _ (HostGitDir d) (HostGitFile f) -> (intercalate (pathSeparator : "") d) </> f
