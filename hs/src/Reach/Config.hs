{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Reach.Config
  ( RC(..)
  , RCParsed(..)
  , rcOption
  ) where

import Data.Aeson
import Data.ByteArray.Encoding
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import GHC.Generics

import Reach.Connector
import Reach.Util

import qualified Data.ByteString as BS
import qualified Options.Applicative as A

-- TODO friendlier layout for human-editing
data RC = RC
  { rcDefaultConnector :: Maybe ConnectorName
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RCParsed
  = RCParseFailedBase64 String String
  | RCParseFailedInvalidConfig BS.ByteString ParseException
  | RCParsed BS.ByteString RC
  deriving Show

rcOption :: A.Parser (Maybe RCParsed)
rcOption = A.optional $ A.option r o where
  o = A.long "config-contents" <> A.metavar "CONFIG_YAML_CONTENTS" <> A.internal
  r = A.eitherReader $ \a -> case convertFromBase Base64 . encodeUtf8 . pack $ trimQuotes a of
    Left e -> Right $ RCParseFailedBase64 a e
    Right a' -> either (Right . RCParseFailedInvalidConfig a') (Right . RCParsed a') $ decodeEither' a'
