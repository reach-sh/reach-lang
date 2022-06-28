{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Reach.AST.ID.Req where

import           Data.Aeson
import           GHC.Generics
import           Servant.Auth.JWT

data CreateAccountFormInfo = CreateAccountFormInfo
  { ca_firstName   :: String
  , ca_lastName    :: String
  , ca_username    :: String
  , ca_email       :: String
  , ca_password    :: String
  , ca_phoneNumber :: String
  , ca_mfaSms      :: Maybe String
  , ca_mfaEmail    :: Maybe String
  , ca_mfaTotp     :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance ToJSON CreateAccountFormInfo
instance FromJSON CreateAccountFormInfo

data LoginFormInfo = LoginFormInfo
  { lf_username :: String
  , lf_password :: String
  } deriving (Eq, Show, Generic)

instance ToJSON LoginFormInfo
instance FromJSON LoginFormInfo

data LogoutFormInfo = LogoutFormInfo
  { lo_id :: String
  } deriving (Eq, Show, Generic)

instance ToJSON LogoutFormInfo
instance FromJSON LogoutFormInfo

data VerifyMfaFormInfo = VerifyMfaFormInfo
  { vm_id      :: String
  , vm_verCode :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON VerifyMfaFormInfo
instance FromJSON VerifyMfaFormInfo

data UpdateAccountFormInfo = UpdateAccountFormInfo
  { ua_firstName   :: String
  , ua_lastName    :: String
  , ua_phoneNumber :: String
  , ua_mfaSms      :: Maybe String
  , ua_mfaEmail    :: Maybe String
  , ua_mfaTotp     :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateAccountFormInfo
instance FromJSON UpdateAccountFormInfo

data MfaInfo = MfaInfo
  { mi_userId   :: String
  , mi_mfaSms   :: Maybe String
  , mi_mfaEmail :: Maybe String
  , mi_mfaTotp  :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance ToJSON MfaInfo
instance FromJSON MfaInfo

data UpdateValueFormInfo = UpdateValueFormInfo
  { uv_newVal :: String
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateValueFormInfo
instance FromJSON UpdateValueFormInfo

data User = User
  { userId      :: String
  , firstName   :: String
  , lastName    :: String
  , username    :: String
  , email       :: String
  , phoneNumber :: String
  , mfaSms      :: Maybe String
  , mfaEmail    :: Maybe String
  , mfaTotp     :: Maybe Bool
 } deriving (Eq, Ord, Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User
