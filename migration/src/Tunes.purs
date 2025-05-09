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
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Tunebank.HTTP.Response (ResponseError)
import Tunebank.Logic.Api (upsertValidatedTuneWithTs)
import Tunebank.Types (Genre(..), Role(..), UserName(..))
import Types (IncomingGenre)
import Utils (tsToDateTimeString)
import Yoga.Postgres (Client)

{-}
type MongoId = 
  { oid :: String }
-}

type MusicrestTune =
  { abc :: String
  , abcHeaders :: String
  , submitter :: String 
  , ts :: Int
  }

{-}
decodeJsonMongoId :: Json -> Either JsonDecodeError MongoId
decodeJsonMongoId json = do
-} 

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
    abc = musicrestTune.abc <> musicrestTune.abcHeaders
    genre = Genre $ show incomingGenre
    timestamp = tsToDateTimeString musicrestTune.ts
    auth = 
      if (musicrestTune.submitter  == "administrator ") then 
        { user : UserName musicrestTune.submitter
        , role : Role "administrator" 
        }
      else 
        { user : UserName musicrestTune.submitter
        , role : Role "normaluser" 
        }
  upsertValidatedTuneWithTs auth c genre timestamp abc


  
