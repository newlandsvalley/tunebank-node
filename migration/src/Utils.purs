module Utils 
  ( readMigrationFile
  , mongoObjectIdToDate
  , mongoTsToDateTimeString
  ) where

import Prelude

import Data.Maybe (maybe, fromMaybe)
import Data.Either (either)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (fromStringAs, hexadecimal, toNumber)
import Data.Time.Duration (Milliseconds(..))
import Data.Formatter.DateTime (formatDateTime)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.String (take)
import Data.String.Utils (lines)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

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
