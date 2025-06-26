module Tunes 
  ( decodeTune
  , importTune
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
import Args.Types (IncomingGenre)
import Yoga.Postgres (Client)
import Utils (fixDoubleBackslash)


type ExportedTune =
  { abc :: String
  , submitter :: String 
  , ts :: String
  }

decodeJsonExportedTune :: Json -> Either JsonDecodeError ExportedTune
decodeJsonExportedTune json = do
    obj <- decodeJson json
    abc <- obj .: "abc"
    submitter <- obj .: "submitter"
    ts <- obj .: "ts"
    pure $ { abc, submitter, ts }

decodeTune :: String -> Either String ExportedTune
decodeTune jsonString = 
  case (parseJson jsonString) of 
    Left err -> 
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err 
      else 
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonExportedTune json

-- | import a tune and log the results
importTune :: IncomingGenre -> Client -> Either String ExportedTune -> Aff Unit
importTune incomingGenre c eTune = do 
  case eTune of 
    Left err -> 
      liftEffect $ log $ "decoding error: " <> err
    Right exportedTune -> do 
      eResult <- importTune' incomingGenre exportedTune c
      liftEffect $ either logShow logShow eResult

-- import a successfully decoded tune
importTune' :: IncomingGenre -> ExportedTune -> Client -> Aff (Either ResponseError String)
importTune' incomingGenre exportedTune c = do
  let 
    genre = Genre $ show incomingGenre
    auth = 
      if (exportedTune.submitter  == "administrator ") then 
        { user : UserName "administrator"
        , role : Role "administrator" 
        }
      else 
        { user : UserName exportedTune.submitter
        , role : Role "normaluser" 
        }
    fixedAbc = fixDoubleBackslash exportedTune.abc
  upsertValidatedTuneWithTs auth c genre exportedTune.ts fixedAbc


  
