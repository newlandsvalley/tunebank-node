module Tunebank.Tools.Mail
  ( sendRegistrationMail
  , sendNewPasswordOTPMail
  , sendUserNameMail
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Exception (message) as Exception
import NodeMailer (Message, MessageInfo, Transporter, TransportConfig, createTransporter, getTestMessageUrl, sendMailMessage)
import Tunebank.Config (MailConfig)
import Tunebank.Environment (Env)
import Tunebank.Logging.Winston (logError, logInfo)
import Tunebank.HTTP.Response (ResponseError(..))
import Tunebank.Types (Email)

-- | the email subject
type Subject = String
-- | the email text
type Text = String

sendRegistrationMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> m (Either ResponseError Unit)
sendRegistrationMail toAddress uuid = do
  config :: MailConfig <- asks _.mail
  let
    -- this is the validation URL we would use were we to have direct communication from the frontend 
    -- but we intend to proxy requests from the frontend 'https://frontend-server/tunebank' to here
    -- validationUrl = "http://" <> serverConfig.host <> ":" <> show serverConfig.port <> "/user/validate/" <> uuid
    validationUrl = "https://" <> config.frontend <> "/tunebank/user/validate/" <> uuid
    subject = "Tunebank user registration"
    text = "click on this link to complete your user registration: " <> validationUrl
  sendMail toAddress subject text

-- | send a mail message to a user who has requested a change of password which includes the One-Time-Password
sendNewPasswordOTPMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> m (Either ResponseError Unit)
sendNewPasswordOTPMail toAddress otp = do
  let
    subject = "Tunebank change password authorization"
    text = "copy this one-time-password to the change password page: " <> otp
  sendMail toAddress subject text

-- | send a mail message to a user who has requested his user name by supplying his email address
sendUserNameMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> m (Either ResponseError Unit)
sendUserNameMail toAddress name = do
  let
    subject = "Tunebank retrieve user name"
    text = "Your user name is: " <> name
  sendMail toAddress subject text

-- | send a mail message to a user with the given subject and text
sendMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> String -> m (Either ResponseError Unit)
sendMail toAddress subject text = do
  logger <- asks _.logger
  config :: MailConfig <- asks _.mail

  let
    transportConfig = getTransportConfig config

  _ <- liftEffect $ log ("trying to email user at " <> toAddress <> " using email provider " <> config.auth.user <> " pw: " <> config.auth.pass)

  _ <- liftEffect $ logInfo logger
    ("trying to email user at " <> toAddress <> " using email provider " <> config.auth.user <> " pw: " <> config.auth.pass)

  transporter <- liftEffect $ createTransporter transportConfig

  message <- liftEffect $ createMessage toAddress subject text
  eResult <- liftAff $ sendMailMessage message transporter

  case eResult of
    Left e -> do
      let
        errorMessage = Exception.message e
        errorText = "Sending " <> subject <> " mail message to " <> toAddress <> " failed: " <> errorMessage
      _ <- liftEffect $ log errorText
      _ <- liftEffect $ logError logger errorText
      pure $ Left $ InternalServerError errorMessage
    Right info -> do
      let
        successText = subject <> " mail message sent to " <> toAddress
      _ <- liftEffect $ log successText
      _ <- liftEffect $ logInfo logger successText
      when (config.host == "smtp.ethereal.email") do
        let 
          mMessageUrl :: Maybe String 
          mMessageUrl = getTestMessageUrl info
        case mMessageUrl of 
          Just messageUrl -> do
            let 
              confirmationText = "You can confirm " <> subject <> " mail at: " <> messageUrl
            _ <- liftEffect $ log confirmationText
            liftEffect $ logInfo logger confirmationText
          Nothing -> 
            pure unit
      pure $ Right unit

-- | create an email message 
createMessage :: Email -> String -> String -> Effect Message
createMessage toAddress subject text = do
  let
    recipient = "Recipient <" <> toAddress <> ">"
  pure
    { from: "noreply@tunebank.org.uk"
    , to: [ recipient ]
    , cc: []
    , bcc: []
    , subject: subject
    , text: text
    , attachments: []
    }

-- | build the transport config from the mail section of our server config
getTransportConfig :: MailConfig -> TransportConfig
getTransportConfig config =
  { host: config.host
  , port: config.port
  , secure: config.secure
  , auth: config.auth
  }

