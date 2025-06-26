module Utils
  ( handleException
  , readMigrationFile
  , mongoObjectIdToDate
  , mongoTsToDateTimeString
  ) where

import Prelude

import Data.Maybe (maybe, fromMaybe)
import Data.Either (Either(..), either)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (fromStringAs, hexadecimal, toNumber)
import Data.Time.Duration (Milliseconds(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.String.Utils (lines, startsWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Data.String (take)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Tunebank.HTTP.Response (ResponseError(..))

readMigrationFile :: FilePath -> Aff (Array String)
readMigrationFile filePath = do
  _ <- liftEffect $ log ("trying to read file: " <> filePath)
  text <- readTextFile UTF8 filePath
  pure $ lines text

mongoTsToDateTimeString :: Number -> String
mongoTsToDateTimeString ts =
  let
    mInstant = instant $ Milliseconds ts
    dateTime = maybe (bottom) (toDateTime) mInstant
  in
    either (const "bad date") identity $ formatDateTime "YYYY-MM-DD hh:mm:ss" dateTime

mongoObjectIdToDate :: String -> String
mongoObjectIdToDate oid =
  mongoTsToDateTimeString number

  where
  instant = decodeObjectId
  number = toNumber instant * 1000.0

  decodeObjectId :: Int
  decodeObjectId =
    fromMaybe 0 $ fromStringAs hexadecimal $ take 8 oid

handleException :: forall r. Error -> Aff (Either ResponseError r)
handleException err = do
  let
    errorText = show err
  if (startsWith "error: duplicate key value violates unique constraint" errorText) then
    pure $ Left $ BadRequest "skipping entry which has already been inserted"
  else do
    liftEffect $ log errorText
    pure $ Left $ BadRequest "skipping entry which is in error"

