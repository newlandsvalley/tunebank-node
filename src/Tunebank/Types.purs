module Tunebank.Types
  ( Authorization
  , Comment
  , Credentials
  , Email
  , Genre(..)
  , GenreRecord
  , Role(..)
  , NewComment
  , Password
  , Rhythm(..)
  , RhythmRecord
  , Title
  , TuneMetadata
  , TuneRef
  , UserRecord
  , UserName(..)
  , isAdministrator
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Either (note)
import Data.Argonaut ((:=), (~>), jsonEmptyObject)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Yoga.Postgres.SqlValue (class IsSqlValue, toSql)

-- | A user role
newtype Role = Role String

derive instance genericRole :: Generic Role _
derive instance newtypeRole :: Newtype Role _
derive newtype instance eqRole :: Eq Role
derive newtype instance ordRole :: Ord Role
instance showRole :: Show Role where
  show (Role s) = s

instance sqlValueRole :: IsSqlValue Role where
  toSql (Role r) = toSql r

instance encodeJsonRole :: EncodeJson Role where
  encodeJson (Role r) = encodeJson r

instance decodeJsonRole :: DecodeJson Role where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Role") (Just $ Role string)

-- | A valid user
newtype UserName = UserName String

derive instance genericUserName :: Generic UserName _
derive instance newtypeUserName :: Newtype UserName _
derive newtype instance eqUserName :: Eq UserName
derive newtype instance ordUserName :: Ord UserName
instance showUserName :: Show UserName where
  show (UserName s) = s

instance sqlValueUserName :: IsSqlValue UserName where
  toSql (UserName r) = toSql r

instance encodeJsonUserName :: EncodeJson UserName where
  encodeJson (UserName r) = encodeJson r

instance decodeJsonUserName :: DecodeJson UserName where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "UserName") (Just $ UserName string)


type Password = String
type Email = String

-- | unvalidated user credentials
type Credentials = 
  { user :: String 
  , password :: Password 
  }


-- | validated user authorization
type Authorization = 
  { user :: UserName
  , role :: Role
  }

isAdministrator :: Role -> Boolean 
isAdministrator (Role role) = role == "administrator"

newtype Genre = Genre String

derive instance genericGenre :: Generic Genre _
derive instance newtypeGenre :: Newtype Genre _
derive newtype instance eqGenre :: Eq Genre
derive newtype instance ordGenre :: Ord Genre
instance showGenre :: Show Genre where
  show (Genre s) = s

instance sqlValueGenre :: IsSqlValue Genre where
  toSql (Genre r) = toSql r

instance encodeJsonGenre :: EncodeJson Genre where
  encodeJson (Genre r) = encodeJson r

instance decodeJsonGenre :: DecodeJson Genre where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Genre") (Just $ Genre string)

newtype Rhythm = Rhythm String

derive newtype instance eqRhythm :: Eq Rhythm
instance encodeJsonRhythm :: EncodeJson Rhythm where
  encodeJson (Rhythm r) = encodeJson r


instance showRhythm :: Show Rhythm where
  show (Rhythm s) = s

instance decodeJsonRhythm :: DecodeJson Rhythm where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Rhythm") (Just $ Rhythm string)

instance sqlValueRhythm :: IsSqlValue Rhythm where
  toSql (Rhythm r) = toSql r

-- | the tune title 
type Title = String

type GenreRecord = 
  { genre :: Genre }


type RhythmRecord = 
  { genre :: Genre 
  , rhythm :: Rhythm 
  }


-- type returned from a select query of the Users table
type UserRecord =
  { username :: UserName
  , email :: Email
  , role :: Role
  , valid :: String
  }

type TuneRef =
  { title :: Title
  , rhythm :: Rhythm
  , timestamp :: Int
  , abc :: String
  }

type TuneMetadata =
  { title :: Title
  , source :: Maybe String
  , composer :: Maybe String
  , origin :: Maybe String
  , transcriber :: Maybe String
  , submitter :: UserName
  , id :: Int
  , timestamp :: Int
  , abc :: String
  }

type NewComment = 
  { subject :: String 
  , text :: String 
  , submitter :: UserName
  }


type Comment = 
  { subject :: String 
  , text :: String 
  , submitter :: UserName
  , id  :: Int
  , timestamp :: Int
  }



