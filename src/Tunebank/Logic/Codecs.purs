module Tunebank.Logic.Codecs
  ( encodeUserRecords
  , encodeUserRecord
  , encodeGenres
  , encodeRhythms
  , encodeTuneRefs 
  , encodeComments
  , encodeComment
  , encodeMessage)
   where

import Prelude

import Tunebank.Types

import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode.Class (encodeJson)


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

encodeUserRecords :: Array UserRecord -> Json 
encodeUserRecords =
  encodeJson


encodeUserRecord :: UserRecord -> Json 
encodeUserRecord = 
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



  
  
  
  
