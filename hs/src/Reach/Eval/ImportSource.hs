{- ORMOLU_DISABLE -}

{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Reach.Eval.ImportSource
  ( ImportSource(..)
  , SHA
  , LockFile
  , LockModule
  , PkgError

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
  ) where

import Text.Parsec

import Control.Exception        (SomeException, catch)
import Control.Monad.Extra      (unlessM, whenM, ifM)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Reader     (ReaderT(..), ask, asks)
import Crypto.Hash              (Digest, SHA256(..), hashWith, digestFromByteString)
import Data.Aeson               (ToJSONKey, FromJSONKey)
import Data.ByteArray           (ByteArrayAccess, ByteArray, convert)
import Data.ByteArray.Encoding  (Base(Base16), convertFromBase, convertToBase)
import Data.List                (find, intercalate)
import Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import Data.Yaml                (ToJSON(..), FromJSON(..), withText, encode, decodeFileThrow)
import GHC.Generics             (Generic)
import System.Directory         (createDirectoryIfMissing, doesDirectoryExist)
import System.Directory         (doesFileExist)
import System.Directory.Extra   (listFilesRecursive)
import System.Exit              (ExitCode(..))
import System.FilePath          ((</>), isValid, pathSeparator)
import System.Process           (readCreateProcessWithExitCode, shell, cwd)
import Text.Printf              (printf)
import System.PosixCompat.Files (setFileMode, accessModes, stdFileMode)

import Reach.AST.Base           (ErrorMessageForJson, ErrorSuggestions, SrcLoc, expect_thrown)
import Reach.Util               (listDirectoriesRecursive, uncurry5)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text       as T


data Env = Env
  { srcloc      :: SrcLoc
  , canGit      :: Bool
  , dirDotReach :: FilePath
  }

type App = ReaderT Env IO


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


-- | Represents an entry indexed by 'SHA' in 'LockFile' capturing details by
-- which a directly-imported package module may be fetched.
--
-- This type tracks parsed but unevaluated @git@ imports in its 'host' field.
--
-- __The /unevaluated/ distinction here is important:__ e.g. if the user asks
-- to import a module from the package's @master@ branch, in which case we need
-- to perform some additional steps to fix the import to a specific @git SHA@.
-- This is why the 'refsha' field exists.
--
-- Transitive dependencies which are local to the direct import in 'host' and
-- which belong to the same 'refsha' are discovered and added the 'ldeps'
-- field.
data LockModule = LockModule
  { host   :: HostGit            -- ^ Raw result of parsing import statement
  , refsha :: HostGitRef         -- ^ Git SHA to which @host@ refers at time of locking
  , uri    :: GitUri             -- ^ A @git clone@-able URI
  , ldeps  :: M.Map FilePath SHA -- ^ Repo-local deps discovered during @gatherDeps_*@ phase
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
  | PkgGitRevParseFailed      String
  | PkgLockModuleDoesNotExist FilePath
  | PkgLockModuleShaMismatch  FilePath
  | PkgLockModuleUnknown      HostGit
  | PkgLockModifyUnauthorized
  deriving (Eq, Show, ErrorMessageForJson, ErrorSuggestions)


expect_thrown' :: (Show e, ErrorMessageForJson e, ErrorSuggestions e) => e -> App a
expect_thrown' e = asks srcloc >>= flip expect_thrown e


orFail :: (b -> App a) -> (ExitCode, a, b) -> App a
orFail err = \case
  (ExitSuccess  , a, _) -> pure a
  (ExitFailure _, _, b) -> err b


orFail_ :: (b -> App a) -> (ExitCode, a, b) -> App ()
orFail_ err r = orFail err r >> pure ()


runGit :: FilePath -> String -> App (ExitCode, String, String)
runGit cwd c = liftIO
  $ readCreateProcessWithExitCode ((shell ("git " <> c)){ cwd = Just cwd }) ""


fileExists :: FilePath -> App Bool
fileExists = liftIO . doesFileExist


fileRead :: FilePath -> App B.ByteString
fileRead = liftIO . B.readFile


fileUpsert :: FilePath -> B.ByteString -> App ()
fileUpsert f  = liftIO . B.writeFile f


mkdirP :: FilePath -> App ()
mkdirP = liftIO . createDirectoryIfMissing True


gitClone' :: FilePath -> String -> FilePath -> App ()
gitClone' b u d = runGit b (printf "clone %s %s" u d)
  >>= orFail_ (expect_thrown' . PkgGitCloneFailed)


gitCheckout' :: FilePath -> String -> App ()
gitCheckout' b r = check fetch >> pure () where
  check e = runGit b ("checkout " <> r) >>= orFail e
  fetch _ = runGit b "fetch"
    >>= orFail_ (expect_thrown' . PkgGitFetchFailed)
    >>    check (expect_thrown' . PkgGitCheckoutFailed)


gitRevParse' :: FilePath -> String -> App String
gitRevParse' b r = runGit b ("rev-parse " <> r)
  >>= orFail (expect_thrown' . PkgGitRevParseFailed)


-- | Allow `sudo`-less directory traversal/deletion for Docker users but
-- disable execute bit on individual files
applyPerms :: App ()
applyPerms = do
  dr <- asks dirDotReach
  ds <- liftIO $ listDirectoriesRecursive dr
  fs <- liftIO $ listFilesRecursive       dr
  liftIO $ setFileMode dr accessModes
  liftIO $ mapM_ (flip setFileMode accessModes) ds
  liftIO $ mapM_ (flip setFileMode stdFileMode) fs


--------------------------------------------------------------------------------

dirGitClones :: App FilePath
dirGitClones = (</> "warehouse" </> "git") <$> asks dirDotReach


dirLockModules :: App FilePath
dirLockModules = (</> "sha256") <$> asks dirDotReach


pathLockFile :: App FilePath
pathLockFile = (</> "lock.yaml") <$> asks dirDotReach


withDotReach :: ((LockFile, FilePath) -> App a) -> App a
withDotReach m = do
  warehouse    <- dirGitClones
  lockMods     <- dirLockModules
  lockf        <- pathLockFile
  gitignore    <- (</> ".gitignore")    <$> asks dirDotReach
  dockerignore <- (</> ".dockerignore") <$> asks dirDotReach

  mkdirP warehouse
  mkdirP lockMods

  unlessM (fileExists gitignore)
    $ fileUpsert gitignore $ B.intercalate "\n"
      [ "warehouse/"
      ]

  unlessM (fileExists dockerignore)
    $ fileUpsert dockerignore $ B.intercalate "\n"
      [ "warehouse/"
      ]

  lock <- ifM (fileExists lockf) lockFileRead (pure lockFileEmpty)
  res  <- m (lock, lockf)

  applyPerms
  pure res


gitClone :: HostGit -> App ()
gitClone h = withDotReach $ \_ -> do
  dirClones <- dirGitClones
  let dest = dirClones </> gitCloneDirOf h

  unlessM (liftIO $ doesDirectoryExist dest) $ gitClone' dirClones (gitUriOf h) dest


lockFileRead :: App LockFile
lockFileRead = pathLockFile >>= decodeFileThrow


lockFileUpsert :: LockFile -> App ()
lockFileUpsert a = withDotReach $ \(_, lockf) ->
  fileUpsert lockf $ B.intercalate "\n"
    [ "# Lockfile automatically generated by Reach. Don't edit!"
    , "# This file is meant to be included in source control.\n"
    , encode a
    ]


byGitRefSha :: HostGit -> FilePath -> App (HostGitRef, B.ByteString)
byGitRefSha h fp = withDotReach $ \_ -> do
  case gitRefOf h of
    "master" -> f "master" `orTry` f "main"
    ref      -> f ref

 where
  f ref = do
    dirClone <- (</> gitCloneDirOf h) <$> dirGitClones
    ref'     <- gitRevParse' dirClone ref

    gitCheckout' dirClone ref'

    whenM (not <$> fileExists fp)
      $ expect_thrown' $ PkgLockModuleDoesNotExist fp

    reach <- fileRead fp
    pure (HostGitRef ref', reach)

  orTry a b = do
    env <- ask
    liftIO $ (runReaderT a env) `catch` (\(_ :: SomeException) -> runReaderT b env)


lockModuleFix :: HostGit -> App (FilePath, LockModule)
lockModuleFix h = withDotReach $ \(lock, _) -> do
  gitClone h

  dirClone      <- (</> gitCloneDirOf h)   <$> dirGitClones
  (rsha, reach) <- byGitRefSha h (dirClone </> gitFilePathOf h)
  lmods         <- dirLockModules

  let hash = hashWith SHA256 reach
      dest = lmods </> (T.unpack $ toBase16 hash)
      lmod = LockModule { host   = h
                        , refsha = rsha
                        , uri    = GitUri . gitUriOf $ h
                        , ldeps  = mempty
                        }

  whenM (fileExists dest)
    $ whenM (fileRead dest >>= pure . (hash /=) . hashWith SHA256)
      $ expect_thrown' $ PkgLockModuleShaMismatch dest

  fileUpsert dest reach

  lockFileUpsert
    $ lock { modules = M.insert (SHA hash) lmod (modules lock) }

  pure (dest, lmod)


(@!!) :: LockFile -> HostGit -> Maybe (SHA, LockModule)
(@!!) l h = find ((== h) . host . snd) (M.toList $ modules l)
infixl 9 @!!


failIfMissingOrMismatched :: FilePath -> SHA -> App ()
failIfMissingOrMismatched f (SHA s) = do
  whenM (not <$> fileExists f)
    $ expect_thrown' $ PkgLockModuleDoesNotExist f

  whenM (((/= s) . hashWith SHA256) <$> fileRead f)
    $ expect_thrown' $ PkgLockModuleShaMismatch f

  pure ()


lockModuleAbsPath :: SrcLoc -> Bool -> FilePath -> HostGit -> IO FilePath
lockModuleAbsPath srcloc canGit dirDotReach h =
  flip runReaderT (Env {..}) $ withDotReach $ \(lock, _) -> do
    case lock @!! h of
      Just (SHA k, _) -> do
        modPath <- (</> (T.unpack $ toBase16 k)) <$> dirLockModules
        failIfMissingOrMismatched modPath (SHA k)
        pure modPath

      Nothing -> if canGit
        then lockModuleFix h >>= pure . fst
        else expect_thrown' PkgLockModifyUnauthorized


lockModuleAbsPathGitLocalDep :: SrcLoc -> Bool -> FilePath -> HostGit -> FilePath -> IO FilePath
lockModuleAbsPathGitLocalDep srcloc canGit dirDotReach h ldep =
  flip runReaderT (Env {..}) $ withDotReach $ \(lock, _) -> do

  let relPath = gitDirPathOf h </> ldep

      fix shaParent lm = if not canGit
        then expect_thrown' PkgLockModifyUnauthorized
        else do
          let HostGitRef refsha' = refsha lm

          dirClones <- dirGitClones
          gitCheckout' (dirClones </> gitCloneDirOf h) refsha'

          reach <- fileRead $ dirClones </> gitCloneDirOf h </> relPath
          lmods <- dirLockModules

          let hash = hashWith SHA256 reach
              dest = lmods </> (T.unpack $ toBase16 hash)
              lmod = lm { ldeps = M.insert relPath (SHA hash) (ldeps lm) }

          whenM (fileExists dest)
            $ whenM (fileRead dest >>= pure . (hash /=) . hashWith SHA256)
              $ expect_thrown' $ PkgLockModuleShaMismatch dest

          fileUpsert dest reach

          lockFileUpsert
            $ lock { modules = M.insert shaParent lmod (modules lock) }

          pure dest

  case lock @!! h of
    Nothing -> expect_thrown' $ PkgLockModuleUnknown h

    Just (shaParent, lm) -> case M.lookup relPath (ldeps lm) of
      Nothing      -> fix shaParent lm
      Just (SHA s) -> do
        dest <- (</> (T.unpack $ toBase16 s)) <$> dirLockModules
        failIfMissingOrMismatched dest (SHA s)
        pure dest


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


data Err_Parse_InvalidImportSource
  = Err_Parse_InvalidImportSource FilePath ParseError
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions)

instance Show Err_Parse_InvalidImportSource where
  show (Err_Parse_InvalidImportSource fp e) =
    "Invalid import: " <> fp <> "\n" <> show e


importSource :: SrcLoc -> FilePath -> IO ImportSource
importSource srcloc fp = either
  (expect_thrown srcloc . Err_Parse_InvalidImportSource fp)
  pure
  (runParser (remoteGit <|> localPath) () "" fp)


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
