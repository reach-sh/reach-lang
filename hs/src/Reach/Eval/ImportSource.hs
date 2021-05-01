{- ORMOLU_DISABLE -}

{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Reach.Eval.ImportSource
  ( ImportSource(..)
  , LockFile(..)
  , EntryModule(..)

  , PkgError
  , PkgT
  , HasPkgT(..)
  , runPkgT

  , dirDotReach
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
  ) where

import Text.Parsec

import Control.Monad.Except   (MonadError, ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State    (MonadState, StateT(..), get, modify)
import Data.Aeson.Types       (ToJSON, FromJSON)
import Data.Functor.Identity  (Identity)
import GHC.Generics           (Generic)
import System.Directory       (createDirectoryIfMissing, doesDirectoryExist)
import System.Directory       (doesFileExist, getCurrentDirectory)
import System.FilePath        ((</>), isValid)
import System.Exit            (ExitCode(..))
import System.Process         (readCreateProcessWithExitCode, shell, cwd)
import Text.Printf            (printf)

import Reach.Util             (uncurry5)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M


newtype G a = G a
  deriving (Eq, Show, Generic)

instance (ToJSON   a) => ToJSON   (G a)
instance (FromJSON a) => FromJSON (G a)

newtype HostGitAcct = HostGitAcct  String    deriving (Eq, Show, ToJSON, FromJSON) via (G HostGitAcct)
newtype HostGitRepo = HostGitRepo  String    deriving (Eq, Show, ToJSON, FromJSON) via (G HostGitRepo)
newtype HostGitRef  = HostGitRef   String    deriving (Eq, Show, ToJSON, FromJSON) via (G HostGitRef)
newtype HostGitDir  = HostGitDir  [FilePath] deriving (Eq, Show, ToJSON, FromJSON) via (G HostGitDir)
newtype HostGitFile = HostGitFile  FilePath  deriving (Eq, Show, ToJSON, FromJSON) via (G HostGitFile)
newtype GitUri      = GitUri       String    deriving (Eq, Show, ToJSON, FromJSON) via (G GitUri)


data HostGit
  = GitHub    HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  | BitBucket HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ImportSource
  = ImportLocal     FilePath
  | ImportRemoteGit HostGit
  deriving (Eq, Show)


data EntryModule = EntryModule
  { host :: HostGit
  , uri  :: GitUri
  } deriving (Eq, Show, ToJSON, FromJSON) via (G EntryModule)


data LockFile = LockFile
  { version :: Int
  , modules :: M.Map String EntryModule
  } deriving (Eq, Show, ToJSON, FromJSON) via (G LockFile)


--------------------------------------------------------------------------------

data PkgError
  = PkgFileDoesNotExist FilePath
  | PkgGitCloneFailed   String
  deriving (Eq, Show)


newtype PkgT m a = PkgT
  { runPkgT' :: ExceptT PkgError m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError PkgError
             ) via (ExceptT PkgError m)


class Monad m => HasPkgT m where
  dirExists  :: FilePath ->                 PkgT m Bool
  fileExists :: FilePath ->                 PkgT m Bool
  fileRead   :: FilePath ->                 PkgT m B.ByteString
  fileUpsert :: FilePath -> B.ByteString -> PkgT m ()
  gitClone   :: HostGit  ->                 PkgT m ()
  mkdirP     :: FilePath ->                 PkgT m ()
  pwd        ::                             PkgT m FilePath


runPkgT :: PkgT m a -> m (Either PkgError a)
runPkgT = runExceptT . runPkgT'


instance (Monad m, MonadIO (PkgT m)) => HasPkgT m where
  dirExists    = liftIO . doesDirectoryExist
  fileExists   = liftIO . doesFileExist
  fileRead     = liftIO . B.readFile
  fileUpsert f = liftIO . B.writeFile f
  mkdirP       = liftIO . createDirectoryIfMissing True
  pwd          = liftIO $ getCurrentDirectory

  gitClone h = do
    warehouse <- dirGitClones
    mkdirP warehouse

    let uri = gitUriOf h
        cmd = printf "git clone %s %s" uri $ cloneDirOf h

    (c, o, e) <- liftIO $ readCreateProcessWithExitCode
      ((shell cmd) { cwd = Just warehouse }) ""

    case c of
      ExitSuccess   -> liftIO     $ putStrLn o
      ExitFailure _ -> throwError $ PkgGitCloneFailed e


dirDotReach :: HasPkgT m => PkgT m FilePath
dirDotReach = (</> ".reach") <$> pwd

dirGitClones :: HasPkgT m => PkgT m FilePath
dirGitClones = (</> "warehouse" </> "git") <$> dirDotReach

pathLockFile :: HasPkgT m => PkgT m FilePath
pathLockFile = (</> "reach-lock.yaml") <$> pwd

removeMe :: FilePath -> HasPkgT m => PkgT m ()
removeMe f = case importSource f of
  Left  e                   -> throwError . PkgGitCloneFailed $ show e
  Right (ImportLocal _)     -> pure ()
  Right (ImportRemoteGit h) -> gitClone h


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
  gitClone _ = pure ()
  mkdirP   _ = pure ()
  pwd        = pure ""


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

gitUriOf :: HostGit -> String
gitUriOf = \case
  GitHub    a r _ _ _ -> f "https://github.com/%s/%s.git"    a r
  BitBucket a r _ _ _ -> f "https://bitbucket.org/%s/%s.git" a r
 where f fmt (HostGitAcct a) (HostGitRepo r) = printf fmt a r


cloneDirOf :: HostGit -> String
cloneDirOf = \case
  GitHub    a r _ _ _ -> f "@github.com:%s:%s"    a r
  BitBucket a r _ _ _ -> f "@bitbucket.org:%s:%s" a r
 where f fmt (HostGitAcct a) (HostGitRepo r) = printf fmt a r
