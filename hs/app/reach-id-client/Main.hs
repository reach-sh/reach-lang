module Main (main) where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap    as KM
import           Options.Applicative
import           Reach.AST.Base
import           Reach.ID.Client
import           Reach.ID.Types
import           Reach.Util

data Method
  = MCreateAccount CreateAccountFormInfo
  | MGetAccountInfo
  | MLogin LoginFormInfo
  | MLogout
  | MSendVerification String
  | MUpdateAccount UpdateAccountFormInfo
  | MUpdateEmail String
  | MUpdatePassword String
  | MVerifyMfa String (Maybe String)
  deriving (Eq, Show)

data Opts = Opts
  { o_method :: Method
  , o_jwt    :: Maybe String
  , o_output :: Maybe String
  , o_values :: [String]
  } deriving (Eq, Show)

loginInfo :: ParserInfo Method
loginInfo = info parser $ progDesc "Begin the login process (triggers MFA)"
  where
    parser =
      fmap MLogin $ LoginFormInfo
        <$> strArgument (metavar "USERNAME")
        <*> strArgument (metavar "PASSWORD")

createAccountInfo :: ParserInfo Method
createAccountInfo = info parser $ progDesc "Creates an account"
  where
    parser =
      fmap MCreateAccount $ CreateAccountFormInfo
      <$> strArgument (metavar "FIRST_NAME")
      <*> strArgument (metavar "LAST_NAME")
      <*> strArgument (metavar "USERNAME")
      <*> strArgument (metavar "EMAIL")
      <*> strArgument (metavar "PASSWORD")
      <*> strArgument (metavar "PHONE_NUMBER")
      <*> optional (strOption (long "mfa-sms" <> metavar "MFA_SMS_NUMBER"))
      <*> optional (strOption (long "mfa-email" <> metavar "MFA_EMAIL"))
      <*> optional (switch $ long "mfa-totp")

updateAccountInfo :: ParserInfo Method
updateAccountInfo = info parser $ progDesc "Updates the account information for the logged in user"
  where
    parser =
      fmap MUpdateAccount $ UpdateAccountFormInfo
      <$> strArgument (metavar "FIRST_NAME")
      <*> strArgument (metavar "LAST_NAME")
      <*> strArgument (metavar "PHONE_NUMBER")
      <*> optional (strOption (long "mfa-sms" <> metavar "MFA_SMS_NUMBER"))
      <*> optional (strOption (long "mfa-email" <> metavar "MFA_EMAIL"))
      <*> optional (switch $ long "mfa-totp")

verifyMfaInfo :: ParserInfo Method
verifyMfaInfo = info parser $ progDesc "Verifies a login with an MFA code"
  where
    parser =
      MVerifyMfa
      <$> strArgument (metavar "USER_ID")
      <*> optional (strArgument (metavar "MFA_CODE"))

logoutInfo :: ParserInfo Method
logoutInfo = info (pure MLogout) $ progDesc "Logout a user"

getAccountInfoInfo :: ParserInfo Method
getAccountInfoInfo = info (pure MGetAccountInfo) $ progDesc "Get the account information for the logged in user"

sendVerificationInfo :: ParserInfo Method
sendVerificationInfo = info parser $ progDesc "Sends a verification code to the given user"
  where
    parser = MSendVerification <$> strArgument (metavar "USER_ID")

updateEmailInfo :: ParserInfo Method
updateEmailInfo = info parser $ progDesc "Updates the email for the logged in user"
  where
    parser = MUpdateEmail <$> strArgument (metavar "NEW_EMAIL")

updatePasswordInfo :: ParserInfo Method
updatePasswordInfo = info parser $ progDesc "Updates the password for the logged in user"
  where
    parser = MUpdatePassword <$> strArgument (metavar "NEW_PASSWORD")

cmds :: Mod CommandFields Method
cmds = command "createAccount" createAccountInfo
    <> command "getAccountInfo" getAccountInfoInfo
    <> command "login" loginInfo
    <> command "logout" logoutInfo
    <> command "sendVerification" sendVerificationInfo
    <> command "updateAccount" updateAccountInfo
    <> command "updateEmail" updateEmailInfo
    <> command "updatePassword" updatePasswordInfo
    <> command "verifyMfa" verifyMfaInfo

optsParser :: Parser Opts
optsParser = Opts
  <$> hsubparser cmds
  <*> (optional $ strOption
        (metavar "<JWT>"
          <> long "jwt"
          <> help "Specify the JWT for the API call"
          <> showDefault))
  <*> (optional $ strOption
        (long "output"
        <> short 'o'
        <> help "The specific JSON field to output"))
  <*> (many
        (strArgument
          (metavar "VALUE"
          <> help "Values to pass to the API call")))

addJWT :: Maybe JWT -> Value -> Value
addJWT (Just jwt) (Object vs) = Object $ KM.insert "jwt" (String $ b2t jwt) vs
addJWT _ v                    = v

go :: Opts -> App Value
go opts = case o_method opts of
  MCreateAccount ci -> do
    createAccount ci >>= return . toJSON
  MGetAccountInfo -> do
    getAccountInfo (bpack <$> o_jwt opts) >>= return . toJSON
  MLogin li -> do
    login (Username $ lf_username li) (Password $ lf_password li) >>= return . toJSON
  MLogout -> do
    logout (bpack <$> o_jwt opts) >>= return . toJSON
  MSendVerification uid -> do
    sendVerificationCode (Just $ UserId uid) >>= return . toJSON
  MUpdateAccount ai -> do
    updateAccount ai (bpack <$> o_jwt opts) >>= return . toJSON
  MUpdateEmail v -> do
    updateEmail v (bpack <$> o_jwt opts) >>= return . toJSON
  MUpdatePassword v -> do
    updatePassword v (bpack <$> o_jwt opts) >>= return . toJSON
  MVerifyMfa uid mc -> do
    AuthResult user jwt <- verifyMfa (Just $ UserId uid) (VerCode mc)
    return $ addJWT jwt $ toJSON user

getOutput :: Opts -> Value -> Value
getOutput (Opts {..}) result =
  case (o_output, result) of
    (Just f, Object vs)
      | Just v <- KM.lookup (fromString f) vs -> v
    (_, _) -> result

main :: IO ()
main = do
  let opts = info (optsParser <**> helper) (fullDesc)
  args <- execParser opts
  env  <- defaultEnv
  result <- flip runReaderT env $ go args
  liftIO . putStrLn . encodeJSONString $
    getOutput args result
