module Tunebank.Tools.Mail 
  ( sendRegistrationMail)
  
  where

import Prelude


import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import NodeMailer (Message, MessageInfo, Transporter, TransportConfig, createTransporter, getTestMessageUrl, sendMail_)
import Tunebank.Config (MailConfig, ServerConfig)
import Tunebank.Environment (Env)
import Tunebank.Logging.Winston (logError, logInfo)
import Tunebank.Types (Email)

sendRegistrationMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> m (Either Error MessageInfo)
sendRegistrationMail toAddress uuid = do 
  logger <- asks _.logger
  config :: MailConfig <- asks _.mail
  serverConfig :: ServerConfig <- asks _.server
  let
    transportConfig :: TransportConfig 
    transportConfig = 
      { host : config.host
      , port : config.port
      , secure : config.secure
      , auth : config.auth
      , web : ""
      , mxEnabled : false
      } 

    -- this is the validation URL we would use were we to have direct communication from the frontend 
    -- but we intend to proxy requests from the frontend 'https://frontend-server/tunebank' to here
    -- validationUrl = "http://" <> serverConfig.host <> ":" <> show serverConfig.port <> "/user/validate/" <> uuid
    validationUrl = "https://" <> config.frontend <> "/tunebank/user/validate/" <> uuid

  _ <- liftEffect $ log ("trying to email user at " <> toAddress <> " using email provider " <> config.auth.user <> " pw: " <> config.auth.pass)

  _ <- liftEffect $ logInfo logger 
         ("trying to email user at " <> toAddress <> " using email provider " <> config.auth.user <> " pw: " <> config.auth.pass)
  
  transporter :: Transporter <- liftEffect $ createTransporter transportConfig  
  message <- liftEffect $ createMessage toAddress validationUrl
  eResult <- liftAff $ sendMailMessage message transporter

  case eResult of 
    Left e -> do
      _ <- liftEffect $ log ("Sending registration email to " <> toAddress <> " failed: " <> show e)
      liftEffect $ logError logger ("Sending registration email to " <> toAddress <> " failed: " <> show e)
    Right info -> do
      _ <- liftEffect $ log ("Registration email message sent to " <> toAddress)
      _ <- liftEffect $ logInfo logger ("Registration email message sent to " <> toAddress)
      when (config.host == "smtp.ethereal.email") do
        liftEffect $ log $ "You can confirm a mail at: " <> (show $ getTestMessageUrl info)
  pure eResult

-- | create a message for the registering recipient containing the url to complete registration
createMessage :: Email -> String -> Effect Message
createMessage toAddress url = do
  let 
    recipient = "Recipient <" <> toAddress <> ">"
  pure
    { from: "noreply@tunebank.org.uk"
    , to: [ recipient ]
    , cc: []
    , bcc: []
    , subject: "Tunebank user registration"
    , text: ("click on this link to complete your user registration: " <> url)
    , attachments: []
    }

-- | a version  of sendMail_ which discriminates berween success and failure
sendMailMessage :: Message -> Transporter -> Aff (Either Error MessageInfo)
sendMailMessage message transporter = do
  mySendMail `catchError` \e  -> pure $ Left e

  where

  mySendMail :: Aff (Either Error MessageInfo)
  mySendMail = do 
    info <- sendMail_ message transporter 
    pure $ Right info
  