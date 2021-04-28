{- ORMOLU_DISABLE -}

module Reach.Eval.ImportSource
  ( ImportSource(..)
  , HostGit(..)
  , HostGitAcct(..)
  , HostGitRef(..)
  , HostGitRepo(..)
  , from
  ) where

import Text.Parsec
import Reach.Util (uncurry3)


newtype HostGitAcct = HostGitAcct String deriving (Eq, Show)
newtype HostGitRepo = HostGitRepo String deriving (Eq, Show)
newtype HostGitRef  = HostGitRef  String deriving (Eq, Show)

data HostGit
  = GitHub    HostGitAcct HostGitRepo HostGitRef
  | BitBucket HostGitAcct HostGitRepo HostGitRef
  deriving (Eq, Show)


data ImportSource
  = ImportLocal     FilePath
  | ImportRemoteGit HostGit (Maybe FilePath)
  deriving (Eq, Show)

-- @github.com:reach-sh/reach-lang#6c3dd0f/examples/exports/index.rsh


mostHosts :: Parsec String () (HostGitAcct, HostGitRepo, HostGitRef)
mostHosts = do
  _ <- notFollowedBy (char ':')
  (,,) <$> (HostGitAcct <$> manyTill anyChar (char '/'))
       <*> (HostGitRepo <$> manyTill anyChar (char '#'))
       <*> (HostGitRef  <$> manyTill anyChar (char '/'))


bitbucket :: Parsec String () HostGit
bitbucket = fmap (uncurry3 BitBucket) $ string "bitbucket.org:" *> mostHosts


github :: Parsec String () HostGit
github = fmap (uncurry3 GitHub) $ (optional $ string "github.com:") *> mostHosts


gitHost :: Parsec String () HostGit
gitHost = bitbucket <|> github


remoteGit :: Parsec FilePath () ImportSource
remoteGit = do
  _ <- string "@"
  g <- gitHost
  f <- manyTill anyChar eof
  pure $ ImportRemoteGit g (Just f)


from :: FilePath -> ImportSource
from p = either (const $ ImportLocal p) id
  $ runParser remoteGit () "" p
