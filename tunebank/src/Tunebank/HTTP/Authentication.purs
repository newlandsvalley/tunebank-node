module Tunebank.HTTP.Authentication 
  ( getAuthorization
  , getCredentials
  , parseCredentials
  , validateCorsOrigin
  , withAdminAuthorization
  , withAnyAuthorization
  ) where

import Prelude


import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (drop, indexOf, splitAt)
import Data.String.Base64 (decode)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTPurple (RequestHeaders)
import HTTPurple (lookup) as Headers
import HTTPurple.Response (Response, unauthorized)
import StringParser (Parser, ParseError, runParser)
import StringParser.CodePoints (skipSpaces, regex)
import Tunebank.Database.User (validateCredentials)
import Tunebank.Environment (Env)
import Tunebank.HTTP.Response (customForbidden)
import Tunebank.Types (Authorization, Credentials, isAdministrator)
import Yoga.Postgres (Client)


-- | run a process that requires the user to be authorized, 
-- | returning an HTTP unauthorized response instead if not so
withAnyAuthorization :: forall m. MonadAff m
  => (Either String Authorization)
  -> (Authorization -> m Response)
  -> (m Response)
withAnyAuthorization eAuth p = do
  case eAuth of 
    Left _err -> 
      unauthorized
    Right auth ->
      p auth  

-- | run a process that requires the user to be authorized as an administrator, 
-- | returning an HTTP unauthorized response instead if not so
withAdminAuthorization :: forall m. MonadAff m
  => (Either String Authorization)
  -> (Authorization -> m Response)
  -> (m Response)
withAdminAuthorization eAuth p = do
  case eAuth of 
    Left _err -> 
      unauthorized
    Right auth ->
      if (isAdministrator auth.role ) then
        p auth  
      else 
        customForbidden "This requires administrator authorization"

-- | get the credentials from the HTTP request and check them against the database
-- | returning either the Authorization or an error
getAuthorization :: forall m. MonadAff m => RequestHeaders -> Client -> m (Either String Authorization)
getAuthorization headers c = do
  liftAff $ case getCredentials headers of 
    Right credentials -> do
      validateCredentials credentials c
    Left _ -> 
      pure $ Left "missing credentials"      

-- | get the basic auth credentials from the HTTP request
getCredentials :: RequestHeaders -> Either String Credentials
getCredentials headers = 
  case (Headers.lookup headers "Authorization") of
    Nothing -> 
      Left "no auth header"
    Just value -> 
      parseCredentials value

-- | parse and decode the credentials
parseCredentials :: String -> Either String Credentials
parseCredentials value = 
  case parse value of  
    Left _ -> 
      Left "badly formed auth"
    Right hash ->
      case decode hash of 
        Left _ -> 
          Left "not base 64 encoded"
        Right decoded -> 
          case indexOf (Pattern ":") decoded of
            Nothing -> 
              Left ("not a user password pair - " <> decoded)
            Just colonPos -> 
              let 
                pair = splitAt colonPos decoded 
              in 
                Right { user: pair.before, password: (drop 1 pair.after)}
  
  where
  -- | Parse a basic auth expression
  parse :: String -> Either ParseError String
  parse =
    runParser basicAuth 

    where
    basicAuth :: Parser String 
    basicAuth =
      basic *> hash

    basic :: Parser String
    basic =
      skipSpaces *> regex "[Bb][Aa][Ss][Ii][Cc]"

    hash :: Parser String
    hash =
      skipSpaces *> regex "[A-Za-z0-9._~+/-]+=*" <* skipSpaces

defaultOrigin :: String 
defaultOrigin = 
  "http://localhost"

-- | validate an HTTP request origin against the allowable CORS origins
-- | returning either that origin (if valid) or the default if not
validateCorsOrigin :: forall m. MonadAff m => MonadAsk Env m => Maybe String -> m String
validateCorsOrigin mOrigin = do 
  allowedOrigins <- asks _.corsOrigins
  case mOrigin of  
    Nothing -> pure defaultOrigin
    Just origin ->
      if (elem origin allowedOrigins) then 
        pure origin
      else 
        pure defaultOrigin



