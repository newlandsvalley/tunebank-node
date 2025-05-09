module Users 
  ( MusicrestUser 
  , arbitrageUser
  , decodeUser
  )
  where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe)
import Data.String (length)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Aff (Aff)
import Tunebank.Database.User (UserValidity(..), insertUser)
import Tunebank.HTTP.Response (ResponseError)
import Tunebank.Types (NewUser)
import Yoga.Postgres (Client)

type MusicrestUser = 
  { name :: String 
  , password :: String 
  , email :: String 
  , uuid :: Maybe String 
  , valid :: String
  }


decodeJsonMusicrestUser :: Json -> Either JsonDecodeError MusicrestUser
decodeJsonMusicrestUser json = do
    obj <- decodeJson json
    name <- obj .: "_id"
    password <- obj .: "password"
    email <- obj .: "email"
    uuid <- obj .:? "uuid"
    valid <- obj .: "valid"
    pure $ { name, password, email, uuid, valid }


decodeUser :: String -> Either String MusicrestUser
decodeUser jsonString = 
  case (parseJson jsonString) of 
    Left err -> 
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err 
      else 
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonMusicrestUser json



-- inspect the result of decoding a Musicrest JSON string as a User 
-- and then attempting to load the user to the Tunebank database
-- we ignore the administrator user and any non-validated userss
arbitrageUser :: Client -> Either String MusicrestUser -> Aff Unit
arbitrageUser c eUser = do 
  case eUser of 
    Left err -> 
      liftEffect $ log $ "decoding error: " <> err
    Right musicrestUser -> do 
      if (musicrestUser.name == "administrator") || (musicrestUser.valid == "N") then 
        liftEffect $ log $ "skipping " <> musicrestUser.name 
      else do
        eResult <- migrateUser musicrestUser c
        liftEffect $ either logShow logShow eResult


-- migrate a successfully decoded user
migrateUser :: MusicrestUser -> Client -> Aff (Either ResponseError String)
migrateUser musicrestUser c = do
  let 
    newUser :: NewUser 
    newUser = 
      { name : musicrestUser.name
      , email : musicrestUser.email
      , password : musicrestUser.password
      }
  insertUser newUser Prevalidated c

