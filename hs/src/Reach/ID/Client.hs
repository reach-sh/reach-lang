module Reach.ID.Client
  ( App
  , AuthResult(..)
  , createAccount
  , defaultEnv
  , getAccountInfo
  , JWT
  , login
  , logout
  , Password(..)
  , sendVerificationCode
  , updateAccount
  , updateEmail
  , updatePassword
  , UserId(..)
  , Username(..)
  , VerCode(..)
  , verifyMfa
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString        (ByteString)
import           Data.IORef
import           Data.Maybe
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Reach.ID.Types
import           Reach.Util


-- Types

type JWT = ByteString

data Env = Env
  { e_useEnv  :: Bool
  , e_jwt     :: IORef (Maybe JWT)
  , e_user    :: IORef (Maybe User)
  , e_mfaInfo :: IORef (Maybe MfaInfo)
  } deriving (Eq)

type App = ReaderT Env IO

newtype Username = Username String
newtype Password = Password String

newtype UserId = UserId String
newtype VerCode = VerCode (Maybe String)

data AuthResult a = AuthResult
  { ar_val :: a
  , ar_jwt :: Maybe JWT }
  deriving (Eq, Show)

type QueryParam = (ByteString, Maybe ByteString)


-- Helpers

mt :: Value
mt = object []

whenUsingEnv :: App () -> App ()
whenUsingEnv m = asks e_useEnv >>= flip when m

ensureJWT :: Maybe a -> App (Maybe a)
ensureJWT mJWT = do
  when (isNothing mJWT) $ error "Need JWT but none was provided or found in environment"
  return mJWT

ensureUserId :: Maybe a -> App a
ensureUserId = \case
  Nothing -> error "Need User ID but none was provided or found in environment"
  Just i  -> return i

useOrFindJWT :: Maybe JWT -> App (Maybe JWT)
useOrFindJWT = \case
  Nothing  -> ensureJWT =<< (liftIO . readIORef) =<< asks e_jwt
  Just jwt -> return $ Just jwt

findUserId :: App (Maybe String)
findUserId = do
  iCanHazEnv <- asks e_useEnv
  case iCanHazEnv of
    True -> do
      e_user <- (liftIO . readIORef) =<< asks e_user
      e_mfa  <- (liftIO . readIORef) =<< asks e_mfaInfo
      return $  fmap mi_userId e_mfa
            <|> fmap userId e_user
    False -> return Nothing

useOrFindUserId :: Maybe UserId -> App String
useOrFindUserId = \case
  Nothing           -> ensureUserId =<< findUserId
  Just (UserId uid) -> return uid

baseUrl :: String
baseUrl = "http://127.0.0.1:8080"

makeReq :: (ToJSON a, FromJSON b) => ByteString -> String -> Maybe JWT -> [QueryParam] -> a -> App (b, Maybe ByteString)
makeReq meth endpoint mJWT queryParams body = do
  initReq <- liftIO $ parseRequest $ baseUrl <> "/" <> endpoint
  let authHeaders = maybe [] (\ jwt -> [("Authorization", "Bearer " <> jwt)]) mJWT
  response <- httpJSON $
        setQueryString queryParams $
          initReq
            { method = meth
            , requestHeaders = [ ("Accept", "application/json")
                              , ("Content-type", "application/json") ]
                              <> authHeaders
            , requestBody = RequestBodyLBS $ encode body
            }
  let rBody = getResponseBody response
  let headers = getResponseHeaders response
  case lookup "Authorization" headers of
    Just jwt -> do
      whenUsingEnv $ do
        liftIO . flip writeIORef (Just jwt) =<< asks e_jwt
      return (rBody, Just jwt)
    Nothing  -> return (rBody, Nothing)

makePost :: (ToJSON a, FromJSON b) => String -> Maybe JWT -> [QueryParam] -> a -> App (b, Maybe ByteString)
makePost = makeReq "POST"

makeGet :: (ToJSON a, FromJSON b) => String -> Maybe JWT -> [QueryParam] -> a -> App (b, Maybe ByteString)
makeGet = makeReq "GET"

save :: (Env -> IORef (Maybe a)) -> a -> App a
save f a = do
  whenUsingEnv $ do
    liftIO . flip writeIORef (Just a) =<< asks f
  return a

saveMfaInfo :: MfaInfo -> App MfaInfo
saveMfaInfo = save e_mfaInfo

saveUser :: User -> App User
saveUser = save e_user

-- Methods

login :: Username -> Password -> App MfaInfo
login (Username username) (Password password) = do
  mfaInfo <- fmap fst $ makePost "login" Nothing mempty lfi
  saveMfaInfo mfaInfo
  where
    lfi = LoginFormInfo
            { lf_username = username
            , lf_password = password }

verifyMfa :: Maybe UserId -> VerCode -> App (AuthResult User)
verifyMfa mUserID (VerCode vm_verCode) = do
  vm_id <- useOrFindUserId mUserID
  (result, mJWT) <- makePost "verifyMfa" Nothing mempty $ VerifyMfaFormInfo {..}
  return $ AuthResult result mJWT

logout :: Maybe JWT -> App ()
logout mUserJWT = do
  mJWT <- useOrFindJWT mUserJWT
  (_ :: Value, _) <- makePost "logout" mJWT mempty mt
  return ()

createAccount :: CreateAccountFormInfo -> App MfaInfo
createAccount cafi = do
  mfaInfo <- fmap fst $ makePost "createAccount" Nothing mempty $ cafi
  saveMfaInfo mfaInfo

sendVerificationCode :: Maybe UserId -> App MfaInfo
sendVerificationCode mUserID = do
  userId <- useOrFindUserId mUserID
  mfaInfo <- fmap fst $ makePost "sendVerificationCode" Nothing [("id", Just $ bpack userId)] mt
  saveMfaInfo mfaInfo

getAccountInfo :: Maybe JWT -> App User
getAccountInfo mUserJWT = do
  mJWT <- useOrFindJWT mUserJWT
  user <- fmap fst $ makeGet "getAccountInfo" mJWT mempty mt
  saveUser user

updateAccount :: UpdateAccountFormInfo -> Maybe JWT -> App User
updateAccount uafi mUserJWT = do
  mJWT <- useOrFindJWT mUserJWT
  user <- fmap fst $ makePost "updateAccount" mJWT mempty uafi
  saveUser user

updateEmail :: String -> Maybe JWT -> App ()
updateEmail newEmailAddress mUserJWT = do
  mJWT <- useOrFindJWT mUserJWT
  let uvfi = UpdateValueFormInfo newEmailAddress
  fmap fst $ makePost "updateEmail" mJWT mempty uvfi

updatePassword :: String -> Maybe JWT -> App MfaInfo
updatePassword newPassword mUserJWT = do
  mJWT <- useOrFindJWT mUserJWT
  let uvfi = UpdateValueFormInfo newPassword
  mfaInfo <- fmap fst $ makePost "updatePassword" mJWT mempty uvfi
  saveMfaInfo mfaInfo

defaultEnv :: IO Env
defaultEnv = do
  let e_useEnv = True
  e_jwt     <- newIORef Nothing
  e_user    <- newIORef Nothing
  e_mfaInfo <- newIORef Nothing
  return $ Env {..}
