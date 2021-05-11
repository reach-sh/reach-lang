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

  , HostGit(..)
  , HostGitAcct(..)
  , HostGitRef(..)
  , HostGitRepo(..)
  , HostGitDir(..)
  , HostGitFile(..)

  , gitUriOf
  , importSource
  , lockModuleAbsPath
  , lockModuleAbsPathGitLocalDep
  , runPkgT

  , GitUri(..)
  ) where

import Text.Parsec

import Control.Monad.Except    (MonadError, ExceptT(..), runExceptT, throwError)
import Control.Monad.Extra     (unlessM, whenM, ifM)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Reader    (ReaderT(..), MonadReader, asks)
import Control.Monad.State     (MonadState, StateT(..), get, modify)
import Crypto.Hash             (Digest, SHA256(..), hashWith, digestFromByteString)
import Data.Aeson              (ToJSONKey, FromJSONKey)
import Data.ByteArray          (ByteArrayAccess, ByteArray, convert)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import Data.Functor.Identity   (Identity)
import Data.List               (find, intercalate)
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Data.Yaml               (ToJSON(..), FromJSON(..), prettyPrintParseException)
import Data.Yaml               (withText, encode, decodeEither')
import GHC.Generics            (Generic)
import System.Directory        (createDirectoryIfMissing, doesDirectoryExist)
import System.Directory        (doesFileExist)
import System.Exit             (ExitCode(..))
import System.FilePath         ((</>), isValid, pathSeparator)
import System.Process          (readCreateProcessWithExitCode, shell, cwd)
import Text.Printf             (printf)

import Reach.Parser.Common (ParserOpts(..))
import Reach.Util          (uncurry5)

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
  { host  :: HostGit
  , uri   :: GitUri
  , ldeps :: M.Map FilePath SHA
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
  = PkgGitCloneFailed         String
  | PkgGitCheckoutFailed      String
  | PkgGitFetchFailed         String
  | PkgLockFileParseEx        String
  | PkgLockModuleDoesNotExist FilePath
  | PkgLockModuleShaMismatch  FilePath
  | PkgLockModuleUnknown      HostGit
  | PkgLockModifyUnauthorized
  deriving (Eq, Show)


newtype PkgT m a = PkgT
  { runPkgT' :: ReaderT ParserOpts (ExceptT PkgError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader ParserOpts
             , MonadError  PkgError
             ) via ReaderT ParserOpts (ExceptT PkgError m)


class Monad m => HasPkgT m where
  dirExists    :: FilePath -> PkgT m Bool
  fileExists   :: FilePath -> PkgT m Bool
  fileRead     :: FilePath -> PkgT m B.ByteString
  mkdirP       :: FilePath -> PkgT m ()

  fileUpsert   :: FilePath -> B.ByteString -> PkgT m ()
  gitCheckout' :: FilePath -> String       -> PkgT m ()

  gitClone'    :: FilePath -> String -> FilePath -> PkgT m ()


runPkgT :: PkgT m a -> ReaderT ParserOpts m (Either PkgError a)
runPkgT a = ReaderT $ \po -> runExceptT $ runReaderT (runPkgT' a) po


deriving newtype instance MonadIO m => MonadIO (PkgT m)


orFail :: Monad m => (b -> PkgT m ()) -> (ExitCode, a, b) -> PkgT m ()
orFail err = \case
  (ExitSuccess,   _, _) -> pure ()
  (ExitFailure _, _, e) -> err e


runGit :: MonadIO m => FilePath -> String -> m (ExitCode, String, String)
runGit cwd c = liftIO
  $ readCreateProcessWithExitCode ((shell ("git " <> c)){ cwd = Just cwd }) ""


instance (Monad m, MonadIO m) => HasPkgT m where
  dirExists     = liftIO . doesDirectoryExist
  fileExists    = liftIO . doesFileExist
  fileRead      = liftIO . B.readFile
  fileUpsert f  = liftIO . B.writeFile f
  mkdirP        = liftIO . createDirectoryIfMissing True

  gitClone' b u d = runGit b (printf "clone %s %s" u d)
    >>= orFail (throwError . PkgGitCloneFailed)

  gitCheckout' b r = check fetch where
    check e = runGit b ("checkout " <> r) >>= orFail e
    fetch _ = runGit b "fetch"
      >>= orFail (throwError . PkgGitFetchFailed)
      >>   check (throwError . PkgGitCheckoutFailed)


--------------------------------------------------------------------------------

dirGitClones :: HasPkgT m => PkgT m FilePath
dirGitClones = (</> "warehouse" </> "git") <$> asks dirDotReach


dirLockModules :: HasPkgT m => PkgT m FilePath
dirLockModules = (</> "sha256") <$> asks dirDotReach


pathLockFile :: HasPkgT m => PkgT m FilePath
pathLockFile = (</> "lock.yaml") <$> asks dirDotReach


mkdirPdotReach :: HasPkgT m => PkgT m ()
mkdirPdotReach = do
  warehouse <- dirGitClones
  lockMods  <- dirLockModules
  mkdirP warehouse
  mkdirP lockMods


gitClone :: HasPkgT m => HostGit -> PkgT m ()
gitClone h = do
  mkdirPdotReach

  dirClones <- dirGitClones
  let dest = dirClones </> gitCloneDirOf h

  unlessM (dirExists dest) $ gitClone' dirClones (gitUriOf h) dest


lockFileRead :: HasPkgT m => PkgT m LockFile
lockFileRead = pathLockFile >>= fileRead >>= pure . decodeEither' >>= either
  (throwError . PkgLockFileParseEx . prettyPrintParseException)
  pure


withLock :: HasPkgT m => ((LockFile, FilePath) -> PkgT m a) -> PkgT m a
withLock m = do
  mkdirPdotReach
  lockf <- pathLockFile
  lock  <- ifM (fileExists lockf) lockFileRead (pure lockFileEmpty)
  m (lock, lockf)


lockFileUpsert :: HasPkgT m => LockFile -> PkgT m ()
lockFileUpsert a = withLock $ \(_, lockf) ->
  fileUpsert lockf $ B.intercalate "\n"
    [ "# Lockfile automatically generated by Reach. Don't edit!"
    , "# This file is meant to be included in source control.\n"
    , encode a
    ]


lockModuleFix
  :: HasPkgT m
  => HostGit
  -> PkgT m (FilePath, LockModule)
lockModuleFix h = withLock $ \(lock, _) -> do
  gitClone h

  dirClone <- (</> gitCloneDirOf h) <$> dirGitClones
  gitCheckout' dirClone (gitRefOf h)

  reach <- fileRead $ dirClone </> gitFilePathOf h
  lmods <- dirLockModules

  let sha  = hashWith SHA256 reach
      dest = lmods </> (T.unpack $ toBase16 sha)
      lmod = LockModule { host  = h
                        , uri   = GitUri . gitUriOf $ h
                        , ldeps = mempty
                        }

  whenM (fileExists dest)
    $ whenM (fileRead dest >>= pure . (sha /=) . hashWith SHA256)
      $ throwError $ PkgLockModuleShaMismatch dest

  fileUpsert dest reach

  lockFileUpsert
    $ lock { modules = M.insert (SHA sha) lmod (modules lock) }

  pure (dest, lmod)


(@!!) :: LockFile -> HostGit -> Maybe (SHA, LockModule)
(@!!) l h = find ((== h) . host . snd) (M.toList $ modules l)


failIfMissingOrMismatched :: HasPkgT m => FilePath -> SHA -> PkgT m ()
failIfMissingOrMismatched f (SHA s) = do
  whenM (not <$> fileExists f)
    $ throwError $ PkgLockModuleDoesNotExist f

  whenM (((/= s) . hashWith SHA256) <$> fileRead f)
    $ throwError $ PkgLockModuleShaMismatch f

  pure ()


lockModuleAbsPath
  :: HasPkgT m
  => HostGit
  -> PkgT m FilePath
lockModuleAbsPath h = withLock $ \(lock, _) -> do
  case lock @!! h of
    Just (SHA k, _) -> do
      modPath  <- (</> (T.unpack $ toBase16 k)) <$> dirLockModules
      failIfMissingOrMismatched modPath (SHA k)
      pure modPath

    Nothing -> ifM (asks canGit)
      (lockModuleFix h >>= pure . fst)
      (throwError PkgLockModifyUnauthorized)


lockModuleAbsPathGitLocalDep
  :: HasPkgT m
  => HostGit
  -> FilePath
  -> PkgT m FilePath
lockModuleAbsPathGitLocalDep h ldep = withLock $ \(lock, _) -> do
  canGit' <- asks canGit

  let relPath = gitDirPathOf h </> ldep

      fix shaParent lm = if not canGit'
        then throwError PkgLockModifyUnauthorized
        else do
          dirClones <- dirGitClones
          gitCheckout' (dirClones </> gitCloneDirOf h ) (gitRefOf h)

          reach <- fileRead $ dirClones </> gitCloneDirOf h </> relPath
          lmods <- dirLockModules

          let sha  = hashWith SHA256 reach
              dest = lmods </> (T.unpack $ toBase16 sha)
              lmod = lm { ldeps = M.insert relPath (SHA sha) (ldeps lm) }

          whenM (fileExists dest)
            $ whenM (fileRead dest >>= pure . (sha /=) . hashWith SHA256)
              $ throwError $ PkgLockModuleShaMismatch dest

          fileUpsert dest reach

          lockFileUpsert
            $ lock { modules = M.insert shaParent lmod (modules lock) }

          pure dest

  case lock @!! h of
    Nothing -> throwError $ PkgLockModuleUnknown h

    Just (shaParent, lm) -> case M.lookup relPath (ldeps lm) of
      Nothing      -> fix shaParent lm
      Just (SHA s) -> do
        dest <- (</> (T.unpack $ toBase16 s)) <$> dirLockModules
        failIfMissingOrMismatched dest (SHA s)
        pure dest


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
      Nothing -> throwError $ PkgLockModuleDoesNotExist f
      Just c  -> pure c

  fileUpsert f c = modify
    $ \s -> s { filesystem = M.insert f c (filesystem s) }

  -- TODO
  gitClone'    _ _ _ = pure ()
  mkdirP           _ = pure ()
  gitCheckout'   _ _ = pure ()


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

gitDirPathOf :: HostGit -> FilePath
gitDirPathOf = \case
  GitHub    _ _ _ (HostGitDir d) _ -> intercalate (pathSeparator : "") d
  BitBucket _ _ _ (HostGitDir d) _ -> intercalate (pathSeparator : "") d

gitFilePathOf :: HostGit -> FilePath
gitFilePathOf = \case
  h@(GitHub    _ _ _ _ (HostGitFile f)) -> gitDirPathOf h </> f
  h@(BitBucket _ _ _ _ (HostGitFile f)) -> gitDirPathOf h </> f
