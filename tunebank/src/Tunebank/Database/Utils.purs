module Tunebank.Database.Utils 
  ( maybeIntResult
  , maybeStringResult
  , singleIntResult
  , stringResult
  , read') where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Error, message)
import Effect.Console (logShow)
import Effect.Exception (error)
import Foreign (Foreign, readArray, readInt, readString, unsafeFromForeign, typeOf)
import Foreign.Keys (keys)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafeCrashWith)

read' ∷ ∀ (t ∷ Type). DecodeJson t ⇒ Foreign → Either Error t
read' = toJson >>> decodeJson >>> lmap toError
  where
  toJson :: Foreign -> Json
  toJson = unsafeCoerce

  toError = printJsonDecodeError >>> error

-- | to be used when we are querying for a single integer value where we know we'll always get a value
-- | even if no matching rows exist - for example resulting from a count(*) query
singleIntResult :: Foreign -> Either Error Int
singleIntResult = toInt >>> Right
  where 
  toInt :: Foreign -> Int 
  toInt = unsafeCoerce

-- | to be used when we might get an Int or Nothing if no matching row exists
maybeIntResult ::  Foreign -> Either Error (Maybe Int)
maybeIntResult f = do
  ans <- runExceptT $ readInt f
  case ans of 
    Right i -> 
      pure (Just i)
    Left _ ->
      Right Nothing

-- | to be used when we are querying for a single string value where we will get a null result
-- | if no matching rows exist - for example resulting from a select name using primary key query
maybeStringResult ::  Foreign -> Either Error (Maybe String)
maybeStringResult f = do
  ans <- runExceptT $ readString f
  case ans of 
    Right str -> 
      pure (Just str)
    Left _ ->
      Right Nothing

stringResult ::  Foreign -> Either Error String
stringResult f = do
  ans <- runExceptT $ readString f
  case ans of 
    Right str -> 
      Right str
    Left _ ->
      Left $ error "String expected"





