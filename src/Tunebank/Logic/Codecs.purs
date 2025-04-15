module Tunebank.Logic.Codecs
  ( encodeUserRecords
  , encodeUserRecord
  , encodeGenres
  , encodeRhythms
  , encodeTuneRefs 
  , encodeMessage)
   where

import Prelude

import Tunebank.Types

import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Core (Json, jsonEmptyObject)


encodeGenres :: Array Genre -> Json 
encodeGenres genres = 
    "genres" := genres       
      ~> jsonEmptyObject

encodeRhythms :: Array Rhythm -> Json 
encodeRhythms rhythms = 
    "rhythm" := rhythms       
      ~> jsonEmptyObject

encodeTuneRefs :: Array TuneRef -> Json 
encodeTuneRefs tuneRefs = 
    "tunes" := tuneRefs     
      ~> jsonEmptyObject

encodeUserRecords :: Array UserRecord -> Json 
encodeUserRecords userRecs = 
    "users" := userRecs    
      ~> jsonEmptyObject

encodeUserRecord :: UserRecord -> Json 
encodeUserRecord userRec = 
    "user" := userRec    
      ~> jsonEmptyObject

encodeMessage :: String -> Json 
encodeMessage message = 
    "message" := message    
      ~> jsonEmptyObject

  
  
  
  
