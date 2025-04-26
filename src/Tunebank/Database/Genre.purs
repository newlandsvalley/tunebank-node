module Tunebank.Database.Genre 
  ( existsGenre
  , getGenres
  , getGenreStrings
  , validateGenre) where

import Prelude

import Data.Either (Either, note)
import Effect.Console (logShow)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Data.Maybe (Maybe, maybe)
import Yoga.Postgres (Query(Query), Client, queryValue, query_)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Database.Utils (singleIntResult, maybeStringResult, read')
import Tunebank.Types (Genre(..), GenreRecord)
import Tunebank.HTTP.Response (ResponseError(..))

existsGenre :: Genre -> Client -> Aff Boolean
existsGenre genre c = do
  _ <- liftEffect $ logShow ("trying to match " <> (show genre))
  matchCount <- queryValue singleIntResult (Query "select count(*) from genres where genre = $1" :: Query Int) [ toSql genre ] c
  pure $ maybe false ((_ > 0)) matchCount

validateGenre :: Genre -> Client -> Aff (Either ResponseError Genre)
validateGenre genre c = do
  _ <- liftEffect $ logShow ("trying to match " <> (show genre))
  mGenre <- queryValue maybeStringResult (Query "select genre from genres where genre = $1" :: Query (Maybe String)) [ toSql genre ] c
  pure $ map Genre $ note (BadRequest $ "Unknown genre: " <> (show genre)) $ join mGenre

-- get the array of all Genre records
getGenreRecords :: Client -> Aff (Array GenreRecord)
getGenreRecords = do
  query_ read' (Query "select genre from genres order by genre" :: Query GenreRecord) 

-- | get the list of genres as strings
getGenreStrings :: Client -> Aff (Array String)
getGenreStrings c = do
  genreRecords <- getGenreRecords c
  pure $ map (_.genre >>> show) genreRecords

getGenres :: Client -> Aff (Array Genre)
getGenres c = do
  genreRecords <- getGenreRecords c
  pure $ map (_.genre ) genreRecords

