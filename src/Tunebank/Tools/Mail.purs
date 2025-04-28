module Tunebank.Tools.Mail 
  ( sendRegistrationMail)
  
  where

import Prelude


import Control.Monad.Reader (class MonadAsk, asks)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import NodeMailer (Message, createTransporter, getTestMessageUrl, sendMail_)
import NodeMailer (TransportConfig, Transporter) as NM
import Tunebank.Config (MailConfig, ServerConfig)
import Tunebank.Environment (Env)
import Tunebank.Types (Email)

sendRegistrationMail :: forall m. MonadAff m => MonadAsk Env m => Email -> String -> m Unit
sendRegistrationMail toAddress uuid = do 
  config :: MailConfig <- asks _.mail
  serverConfig :: ServerConfig <- asks _.server
  let
    transportConfig :: NM.TransportConfig 
    transportConfig = 
      { host : config.host
      , port : config.port
      , secure : config.secure
      , auth : config.auth
      , web : ""
      , mxEnabled : false
      } 

    validationUrl = "http://" <> serverConfig.host <> ":" <> show serverConfig.port <> "/user/validate/" <> uuid

  liftEffect $ log ("trying to email user " <> config.auth.user <> " pw: " <> config.auth.pass)
  
  transporter :: NM.Transporter <- liftEffect $ createTransporter transportConfig  
  message <- liftEffect $ createMessage toAddress validationUrl
  info <- liftAff $  sendMail_ message transporter
  when (config.host == "smtp.ethereal.email)") do
    liftEffect $ log $ "You can confirm a mail at: " <> (show $ getTestMessageUrl info)
  pure unit


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