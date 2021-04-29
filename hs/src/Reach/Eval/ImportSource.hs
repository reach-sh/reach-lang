{- ORMOLU_DISABLE -}

module Reach.Eval.ImportSource
  ( ImportSource(..)
  , HostGit(..)
  , HostGitAcct(..)
  , HostGitRef(..)
  , HostGitRepo(..)
  , HostGitDir(..)
  , HostGitFile(..)
  , importSource
  ) where

import Text.Parsec
import System.FilePath (isValid)
import Reach.Util      (uncurry5)


newtype HostGitAcct = HostGitAcct  String    deriving (Eq, Show)
newtype HostGitRepo = HostGitRepo  String    deriving (Eq, Show)
newtype HostGitRef  = HostGitRef   String    deriving (Eq, Show)
newtype HostGitDir  = HostGitDir  [FilePath] deriving (Eq, Show)
newtype HostGitFile = HostGitFile  FilePath  deriving (Eq, Show)

data HostGit
  = GitHub    HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  | BitBucket HostGitAcct HostGitRepo HostGitRef HostGitDir HostGitFile
  deriving (Eq, Show)


data ImportSource
  = ImportLocal     FilePath
  | ImportRemoteGit HostGit
  deriving (Eq, Show)

-- @github.com:reach-sh/reach-lang#6c3dd0f/examples/exports/index.rsh


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
