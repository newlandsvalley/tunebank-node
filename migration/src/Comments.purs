module Comments where

import Prelude

import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Tunebank.Database.Comment (insertCommentWithTs)
import Tunebank.HTTP.Response (ResponseError(..))
import Tunebank.Types (Genre(..), Title(..), UserName(..))
import Args.Types (IncomingGenre)
import Utils (handleException, mongoObjectIdToDate)
import Yoga.Postgres (Client)

{- this is what an object in a Mongo export looks like
type ObjectId = 
 { id :: String }
-}

type MusicrestComment =
  { objectId :: String
  , submitter :: String
  , subject :: String
  , text :: String
  , tune :: Maybe String
  }

decodeJsonObjectId :: Json -> Either JsonDecodeError String
decodeJsonObjectId json = do
  obj <- decodeJson json
  id <- obj .: "$oid"
  pure id

decodeJsonMusicrestComment :: Json -> Either JsonDecodeError MusicrestComment
decodeJsonMusicrestComment json = do
  obj <- decodeJson json
  oid <- obj .: "_id"
  objectId <- decodeJsonObjectId oid
  submitter <- obj .: "user"
  subject <- obj .: "subject"
  text <- obj .: "text"
  tune <- obj .:? "tune"
  pure $ { objectId, submitter, subject, text, tune }

decodeComment :: String -> Either String MusicrestComment
decodeComment jsonString =
  case (parseJson jsonString) of
    Left err ->
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err
      else
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonMusicrestComment json

-- | migrate a comment and log the results
migrateComment :: IncomingGenre -> Client -> Either String MusicrestComment -> Aff Unit
migrateComment incomingGenre c eComment = do
  case eComment of
    Left err ->
      liftEffect $ log $ "decoding error: " <> err
    Right musicrestComment -> do
      eResult <- migrateComment' incomingGenre musicrestComment c
      liftEffect $ either logShow (\i -> log ("comment inserted id: " <> show i)) eResult

-- migrate a successfully decoded comment
migrateComment' :: IncomingGenre -> MusicrestComment -> Client -> Aff (Either ResponseError Int)
migrateComment' incomingGenre musicrestComment c = do
  let
    genre = Genre $ show incomingGenre
    timestamp = mongoObjectIdToDate musicrestComment.objectId
    comment =
      { subject: musicrestComment.subject
      , text: musicrestComment.text
      }
  case musicrestComment.tune of
    Nothing ->
      pure $ Left $ BadRequest "skipping comment with no tune attached"
    Just title ->
      catchError
        (insertCommentWithTs genre (Title title) comment (UserName musicrestComment.submitter) timestamp c)
        handleException

