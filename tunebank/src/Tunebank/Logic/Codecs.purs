module Tunebank.Logic.Codecs
  ( decodeNewComment
  , decodeNewUser
  , encodeUserRecords
  , encodeUserRecord
  , encodeUserRecordsPage
  , encodeGenres
  , encodeRhythms
  , encodeTuneMetadata
  , encodeTuneRefs
  , encodeTunesPage
  , encodeComments
  , encodeComment
  , encodeMessage
  ) where

import Prelude

import Tunebank.Types
import Tunebank.Pagination (TuneRefsPage, UserRecordsPage)

import Data.Argonaut.Decode (JsonDecodeError, parseJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either)

-- | encoding

encodeGenres :: Array Genre -> Json
encodeGenres genres =
  "genres" := genres
    ~> jsonEmptyObject

encodeRhythms :: Array Rhythm -> Json
encodeRhythms rhythms =
  "rhythm" := rhythms
    ~> jsonEmptyObject

encodeTuneRefs :: Array TuneRef -> Json
encodeTuneRefs =
  encodeJson

encodeTuneMetadata :: TuneMetadata -> Json
encodeTuneMetadata =
  encodeJson

encodeTunesPage :: TuneRefsPage -> Json
encodeTunesPage =
  encodeJson

encodeUserRecords :: Array UserRecord -> Json
encodeUserRecords =
  encodeJson

encodeUserRecord :: UserRecord -> Json
encodeUserRecord =
  encodeJson

encodeUserRecordsPage :: UserRecordsPage -> Json
encodeUserRecordsPage =
  encodeJson

encodeMessage :: String -> Json
encodeMessage message =
  "message" := message
    ~> jsonEmptyObject

encodeComments :: Array Comment -> Json
encodeComments =
  encodeJson

encodeComment :: Comment -> Json
encodeComment =
  encodeJson

-- decoding 

decodeNewUser :: String -> Either JsonDecodeError NewUser
decodeNewUser s =
  decodeJson =<< parseJson s

decodeNewComment :: String -> Either JsonDecodeError NewComment
decodeNewComment s =
  decodeJson =<< parseJson s

