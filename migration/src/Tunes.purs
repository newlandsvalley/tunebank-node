module Tunes
  ( decodeTune
  , migrateTune
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Number (fromString)
import Data.Maybe (fromMaybe)
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Tunebank.HTTP.Response (ResponseError)
import Tunebank.Logic.Api (upsertValidatedTuneWithTs)
import Tunebank.Types (Genre(..), Role(..), UserName(..))
import Args.Types (IncomingGenre)
import Utils (mongoTsToDateTimeString)
import Yoga.Postgres (Client)

type MusicrestTune =
  { abc :: String
  , abcHeaders :: String
  , submitter :: String
  , ts :: String
  }

decodeJsonMusicrestTune :: Json -> Either JsonDecodeError MusicrestTune
decodeJsonMusicrestTune json = do
  obj <- decodeJson json
  abc <- obj .: "abc"
  abcHeaders <- obj .: "abcHeaders"
  submitter <- obj .: "submitter"
  ts <- obj .: "ts"
  pure $ { abc, abcHeaders, submitter, ts }

decodeTune :: String -> Either String MusicrestTune
decodeTune jsonString =
  case (parseJson jsonString) of
    Left err ->
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err
      else
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonMusicrestTune json

-- | migrate a tune and log the results
migrateTune :: IncomingGenre -> Client -> Either String MusicrestTune -> Aff Unit
migrateTune incomingGenre c eTune = do
  case eTune of
    Left err ->
      liftEffect $ log $ "decoding error: " <> err
    Right musicrestTune -> do
      eResult <- migrateTune' incomingGenre musicrestTune c
      liftEffect $ either logShow logShow eResult

-- migrate a successfully decoded tune
migrateTune' :: IncomingGenre -> MusicrestTune -> Client -> Aff (Either ResponseError String)
migrateTune' incomingGenre musicrestTune c = do
  let
    abc = musicrestTune.abcHeaders <> musicrestTune.abc
    genre = Genre $ show incomingGenre
    musicrestTs = fromMaybe 0.0 $ fromString musicrestTune.ts
    timestamp = mongoTsToDateTimeString musicrestTs
    auth =
      if (musicrestTune.submitter == "administrator ") then
        { user: UserName "administrator"
        , role: Role "administrator"
        }
      else
        { user: UserName musicrestTune.submitter
        , role: Role "normaluser"
        }
  -- _ <- liftEffect $ log $ "abc: " <> abc
  upsertValidatedTuneWithTs auth c genre timestamp abc

