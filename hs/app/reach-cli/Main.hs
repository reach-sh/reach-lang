{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Amazonka
import Amazonka.CognitoIdentityProvider.InitiateAuth
import Amazonka.CognitoIdentityProvider.Types.AuthFlowType
import Amazonka.CognitoIdentityProvider.Types.ChallengeNameType
import Amazonka.CognitoIdentityProvider.Types.AuthenticationResultType
import Amazonka.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Amazonka.CognitoIdentityProvider.RespondToAuthChallenge
import Control.Monad.Reader
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import System.Environment

data Config = Config
  { cfgStage :: Text
  , cfgRegion :: Region
  , cfgUserPoolClientId :: Text
  } deriving (Generic, Show)

instance FromJSON Config where
    parseJSON = A.genericParseJSON $
      A.defaultOptions
        { A.fieldLabelModifier = drop 3
        }

getConfig :: IO Config
getConfig = do
  e <- A.eitherDecode <$> simpleHttp "https://dev-pr36.reach.sh/config.json"
  case e of
    Right v -> return v
    Left s -> do
      error $ "Could not decode Reach Cloud configuration: " <> s

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  -- XXX this is really stupid, the username and password should be in the
  -- config file
  -- XXX the mfa result should be something else, perhaps the OTP secret and we
  -- can internally generate the current value, or just ask the user to provide
  -- it?
  let (inUSERNAME, inPASSWORD, inMFA) =
        case args of
          [x, y, z] -> (x, y, z)
          _ -> error "XXX"
  cfg@Config {..} <- getConfig
  putStrLn $ show cfg
  -- XXX include analyticsMetadata
  let am = Nothing
  runResourceT $ do
    env_ <- newEnvNoAuth
    let env = env_ { envRegion = cfgRegion }
    r0 <- sendUnsigned env $
      (newInitiateAuth AuthFlowType_USER_PASSWORD_AUTH cfgUserPoolClientId)
        & initiateAuth_analyticsMetadata .~ am
        & initiateAuth_authParameters .~ (Just $ HM.fromList
            [ ("PASSWORD", inPASSWORD)
            , ("USERNAME", inUSERNAME)
            ])
    liftIO $ putStrLn $ "XXX " <> show r0
    let ses = r0 ^. initiateAuthResponse_session
    case r0 ^. initiateAuthResponse_challengeName of
      Just cn@ChallengeNameType_SOFTWARE_TOKEN_MFA -> do
        r1 <- sendUnsigned env $
          (newRespondToAuthChallenge cfgUserPoolClientId cn)
            & respondToAuthChallenge_analyticsMetadata .~ am
            & respondToAuthChallenge_session .~ ses
            & respondToAuthChallenge_challengeResponses .~ (Just $ HM.fromList
              [ ("USERNAME", inUSERNAME)
              , ("SOFTWARE_TOKEN_MFA_CODE", inMFA)
              ])
        liftIO $ putStrLn $ "XXX " <> show r1
        case r1 ^. respondToAuthChallengeResponse_authenticationResult of
          Just (AuthenticationResultType'
            { accessToken = Just (Sensitive tok)
            , refreshToken = Just (Sensitive ref)
            , newDeviceMetadata' = Just (NewDeviceMetadataType'
              { deviceKey = Just devk
              })
            }) -> do
            liftIO $ putStrLn $ "tok is " <> show tok
            liftIO $ putStrLn $ "ref is " <> show ref
            liftIO $ putStrLn $ "devk is " <> show devk
            -- XXX this doesn't work... don't know why, always get that it is
            -- wrong
            r2 <- sendUnsigned env $
              (newInitiateAuth AuthFlowType_REFRESH_TOKEN_AUTH cfgUserPoolClientId)
                & initiateAuth_analyticsMetadata .~ am
                & initiateAuth_authParameters .~ (Just $ HM.fromList
                    [ ("USERNAME", inUSERNAME)
                    , ("REFRESH_TOKEN", ref)
                    , ("DEVICE_KEY", devk)
                    ])
            liftIO $ putStrLn $ "XXX " <> show r2
          _ ->
            error $ "Expected successful authentication"
      _ ->
        error $ "Expected software token MFA, but did not receive it"
