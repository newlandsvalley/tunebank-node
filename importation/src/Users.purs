module Users 
  ( arbitrageUser
  , decodeUser
  )
  where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.String (length)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Aff (Aff, catchError)
import Tunebank.Database.User (insertExportedUser)
import Tunebank.HTTP.Response (ResponseError)
import Tunebank.Types (UserRecordExported)
import Yoga.Postgres (Client)
import Utils (handleException)


decodeJsonExportedUser :: Json -> Either JsonDecodeError UserRecordExported
decodeJsonExportedUser json = do
    obj <- decodeJson json
    name <- obj .: "username"
    password <- obj .: "passwd"
    role <- obj .: "rolename"
    email <- obj .: "email"
    valid <- obj .: "valid"
    registrationId <- obj .: "registrationid"
    timestamp <- obj .: "ts"
    pure $ { name, password, role, email, valid , registrationId, timestamp }

decodeUser :: String -> Either String UserRecordExported
decodeUser jsonString = 
  case (parseJson jsonString) of 
    Left err -> 
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err 
      else 
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonExportedUser json


-- inspect the result of decoding an exported user JSON string as a User 
-- and then attempting to load the user to the Tunebank database
-- we ignore the administrator user and any non-validated userss
arbitrageUser :: Client -> Either String UserRecordExported -> Aff Unit
arbitrageUser c eUser = do 
  case eUser of 
    Left err -> 
      liftEffect $ log $ "decoding error: " <> err
    Right user -> do 
      if (user.name == "administrator") || (user.valid == "N") then 
        liftEffect $ log $ "skipping " <> user.name 
      else do
        eResult <- importUser user c
        liftEffect $ either logShow (const $ log $ "user " <> user.name <> " inserted") eResult


-- import a successfully decoded user
importUser :: UserRecordExported -> Client -> Aff (Either ResponseError Unit)
importUser user c = do
  catchError 
    (insertExportedUser user c)
    handleException 

