module Reach.PackageImport (packageImport, PkgError(..)) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Aeson hiding (encode, (<?>))
import qualified Data.ByteString as B
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Yaml
import GHC.Generics
import GHC.Stack (HasCallStack)
import Reach.AST.Base
import Reach.Util
import System.Directory
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.PosixCompat.Files
import System.Process
import Text.Parsec
import Text.Printf

data Env = Env
  { e_at :: SrcLoc
  , e_install :: Bool
  , e_dreachp :: FilePath
  }

type App = ReaderT Env IO

data PkgError
  = Err_Clone String
  | Err_Checkout String
  | Err_NoRev (Maybe String)
  | Err_Unauthorized
  | Err_InvalidImportSource FilePath ParseError
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions, Generic)

instance HasErrorCode PkgError where
  errPrefix = const "RI"
  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If you delete a constructor, do NOT re-allocate the number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_Clone {} -> 0
    Err_Checkout {} -> 1
    Err_NoRev {} -> 2
    Err_Unauthorized {} -> 3
    Err_InvalidImportSource {} -> 4

instance Show PkgError where
  show = \case
    Err_Clone s ->
      "`git clone` failed: " <> s
    Err_Checkout s ->
      "`git checkout` failed: " <> s
    Err_NoRev Nothing ->
      "Repository does not contain 'master' or 'main' revision"
    Err_NoRev (Just r) ->
      "Repository does not contain '" <> r <> "' revision"
    Err_Unauthorized ->
      "Cannot install packages; did you mean to run with `--install-pkgs`?"
    Err_InvalidImportSource fp e ->
      "Invalid import: " <> fp <> "\n" <> show e

-- Library

expect_ :: (HasErrorCode e, HasCallStack, Show e, ErrorMessageForJson e, ErrorSuggestions e) => e -> App a
expect_ e = asks e_at >>= flip expect_thrown e

runGit :: FilePath -> [String] -> App (Either String String)
runGit cwd args = do
  ensureInstall
  let p = (proc "git" args) {cwd = Just cwd}
  (ec, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode p ""
  case ec of
    ExitSuccess -> return $ Right stdout
    ExitFailure _ -> return $ Left stderr

runGit_ :: (String -> PkgError) -> FilePath -> [String] -> App ()
runGit_ err cwd args =
  runGit cwd args >>= \case
    Right _ -> return ()
    Left e -> expect_ $ err e

dirExists :: FilePath -> App Bool
dirExists = liftIO . doesDirectoryExist

fileExists :: FilePath -> App Bool
fileExists = liftIO . doesFileExist

fileUpsert :: FilePath -> B.ByteString -> App ()
fileUpsert f c = do
  ensureInstall
  liftIO $ B.writeFile f c
  applyPerms

dirBare :: App FilePath
dirBare = (</> "bare") <$> asks e_dreachp

dirRev :: App FilePath
dirRev = (</> "rev") <$> asks e_dreachp

data GitRepo = GitRepo
  { gr_host :: String
  , gr_user :: String
  , gr_repo :: String
  , gr_rev :: Maybe String
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

gitUri :: GitRepo -> String
gitUri (GitRepo {..}) =
  printf "https://%s/%s/%s.git" gr_host gr_user gr_repo

gitDir :: Bool -> GitRepo -> String
gitDir incRef (GitRepo {..}) =
  printf "@%s:%s:%s%s" gr_host gr_user gr_repo gr_rev'
  where
    gr_rev' =
      case incRef of
        True -> maybe "" (":" <>) gr_rev
        False -> ""

gitBareDir :: GitRepo -> App FilePath
gitBareDir gr = (</> gitDir False gr) <$> dirBare

gitRevDir :: GitRepo -> App FilePath
gitRevDir gr = (</> gitDir True gr) <$> dirRev

ensureInstall :: HasCallStack => App ()
ensureInstall =
  (e_install <$> ask) >>= \case
    True -> return ()
    False -> expect_ Err_Unauthorized

ensureBare :: GitRepo -> App ()
ensureBare gr = do
  bdir <- gitBareDir gr
  unlessM (dirExists bdir) $ do
    p <- dirBare
    let u = gitUri gr
    runGit_ Err_Clone p $
      ["clone", "--bare", u, bdir]

ensureRev :: GitRepo -> App ()
ensureRev gr = do
  rdir <- gitRevDir gr
  unlessM (dirExists rdir) $ do
    ensureBare gr
    bdir <- gitBareDir gr
    let r = fromMaybe (impossible "ensureRev") $ gr_rev gr
    mkdirP rdir
    runGit_ Err_Checkout rdir $
      ["--git-dir=" <> bdir, "--work-tree=.", "checkout", r]

-- Lockfile

data LockFile = LockFile
  { lf_vers :: Int
  , lf_revs :: M.Map GitRepo GitRepo
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

lockFileEmpty :: LockFile
lockFileEmpty =
  LockFile
    { lf_vers = 1
    , lf_revs = mempty
    }

pathLockFile :: App FilePath
pathLockFile = (</> "lock.yaml") <$> asks e_dreachp

-- | Allow `sudo`-less directory traversal/deletion for Docker users but
-- disable execute bit on individual files
applyPerms :: App ()
applyPerms = do
  ensureInstall
  dr <- asks e_dreachp
  ds <- liftIO $ listDirectoriesRecursive dr
  fs <- liftIO $ listFilesRecursive dr
  liftIO $ setFileMode dr accessModes
  liftIO $ mapM_ (flip setFileMode accessModes) ds
  liftIO $ mapM_ (flip setFileMode stdFileMode) fs

mkdirP :: FilePath -> App ()
mkdirP d = do
  unlessM (dirExists d) $ do
    ensureInstall
    liftIO $ createDirectoryIfMissing True d
    applyPerms

withDotReach :: ((LockFile, FilePath) -> App a) -> App a
withDotReach m = do
  barep <- dirBare
  revp <- dirRev
  lockf <- pathLockFile
  mkdirP barep
  mkdirP revp
  let initIgnore f = do
        unlessM (fileExists f) $ do
          fileUpsert f $
            B.intercalate
              "\n"
              [ "bare/"
              , "rev/"
              ]
  initIgnore =<< (</> ".gitignore") <$> asks e_dreachp
  initIgnore =<< (</> ".dockerignore") <$> asks e_dreachp
  let lockFileRead = pathLockFile >>= decodeFileThrow
  lock <- ifM (fileExists lockf) lockFileRead (pure lockFileEmpty)
  res <- m (lock, lockf)
  pure res

lockFileUpsert :: LockFile -> App ()
lockFileUpsert a = withDotReach $ \(_, lockf) ->
  fileUpsert lockf $
    B.intercalate
      "\n"
      [ "# Lockfile automatically generated by Reach. Don't edit!"
      , "# This file is meant to be included in source control.\n"
      , encode a
      ]

-- Resolver

consultLock :: GitRepo -> App GitRepo
consultLock gr@(GitRepo {..}) = withDotReach $ \(lf@(LockFile {..}), _) ->
  case M.lookup gr lf_revs of
    Just gr' -> return $ gr'
    Nothing -> do
      ensureBare gr
      bdir <- gitBareDir gr
      let getM :: String -> App (Maybe String)
          getM r =
            runGit bdir ["rev-parse", r] >>= \case
              Left _ -> return $ Nothing
              Right ls ->
                case lines ls of
                  [x] -> return $ Just x
                  _ -> return $ Nothing
      mrev' <-
        case gr_rev of
          Just r -> getM r
          Nothing ->
            getM "master" >>= \case
              Nothing -> getM "main"
              x -> return x
      case mrev' of
        Nothing ->
          expect_ $ Err_NoRev gr_rev
        Just rev' -> do
          let gr' = gr {gr_rev = Just rev'}
          let lf' = lf {lf_revs = M.insert gr gr' lf_revs}
          lockFileUpsert lf'
          return $ gr'

resolve :: Normal -> App FilePath
resolve (Normal {..}) = do
  lgr <- consultLock n_gr
  ensureRev lgr
  lgr' <- gitRevDir lgr
  let n_dir' = intercalate (pathSeparator : "") n_dir
  return $ lgr' </> n_dir' </> n_file

-- Normalizer

data Normal = Normal
  { n_gr :: GitRepo
  , n_dir :: [FilePath]
  , n_file :: FilePath
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

normalize :: Parsed -> App Normal
normalize (Parsed {..}) = do
  let gr_host = fromMaybe "github.com" p_host
  let gr_user = p_user
  let gr_repo = p_repo
  let gr_rev = p_ref
  let n_gr = GitRepo {..}
  let n_dir = p_dir
  let n_file = fromMaybe "index.rsh" p_file
  return $ Normal {..}

-- Parser

data Parsed = Parsed
  { p_host :: Maybe String
  , p_user :: String
  , p_repo :: String
  , p_ref :: Maybe String
  , p_dir :: [FilePath]
  , p_file :: Maybe FilePath
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parser :: Parsec String () Parsed
parser = do
  _ <- string "@"
  host <- optionMaybe $ try $ "server" `terminatedBy` (char ':')
  user <- "account" `terminatedBy` (char '/')
  repo <- "repo" `terminatedBy` endRepo
  ref <- optionMaybe $ try $ (char '#') *> "ref" `terminatedBy` endRef
  optionMaybe (char ':') >>= \case
    Nothing -> do
      eof
      return $ Parsed host user repo ref [] Nothing
    Just _ -> do
      ds <- many $ try $ manyTill (allowed "directory") (char '/')
      file <- optionMaybe $ try $ "file" `terminatedBy` eof
      return $ Parsed host user repo ref ds file
  where
    allowed t =
      alphaNum <|> oneOf "-_."
        <?> "valid git " <> t <> " character (alphanumeric, -, _, .)"
    f `terminatedBy` x = do
      h <- allowed f
      t <- manyTill (allowed f) x
      pure $ h : t
    tlac a = try (lookAhead $ char a) *> pure ()
    endRepo = eof <|> tlac '#' <|> tlac ':'
    endRef = eof <|> tlac ':' *> pure ()

parseIt :: String -> App Parsed
parseIt s = do
  let pr = runParser parser () "" s
  case pr of
    Left pe ->
      expect_ $ Err_InvalidImportSource s pe
    Right v ->
      return $ v

-- Put it together

packageImport :: SrcLoc -> Bool -> FilePath -> String -> IO FilePath
packageImport e_at e_install e_dreachp is =
  flip runReaderT (Env {..}) $ do
    pd <- parseIt is
    nd <- normalize pd
    fp <- resolve nd
    return $ fp
