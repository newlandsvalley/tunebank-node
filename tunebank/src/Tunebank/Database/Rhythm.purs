module Tunebank.Database.Rhythm 
  ( existsRhythm
  , getRhythmsForGenre
  , getRhythmStrings
  , validateRhythm) where

import Prelude

import Data.Either (Either, note)
import Data.Maybe (Maybe, maybe)
import Effect.Console (logShow)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Yoga.Postgres (Query(Query), Client, queryValue, query)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Database.Utils (singleIntResult, maybeStringResult, read')
import Tunebank.Types (Genre, Rhythm(..), RhythmRecord)
import Tunebank.HTTP.Response (ResponseError(..))

existsRhythm :: Genre -> Rhythm -> Client -> Aff Boolean
existsRhythm genre rhythm c = do
  _ <- liftEffect $ logShow ("trying to match genre " <> (show genre) <> " and rhythm " <> (show rhythm))
  mCount <- queryValue singleIntResult (Query "select count(*) from rhythms where genre = $1 and rhythm = $2" :: Query Int) 
     [ toSql genre, toSql rhythm ] c
  pure $ maybe false ((_ > 0)) mCount

validateRhythm :: Genre -> Rhythm -> Client -> Aff (Either ResponseError Rhythm)
validateRhythm genre rhythm c = do
  _ <- liftEffect $ logShow ("trying to match genre " <> (show genre) <> " and rhythm " <> (show rhythm))
  mRhythm <- queryValue maybeStringResult (Query "select rhythm from rhythms where genre = $1 and rhythm = $2" :: Query (Maybe String))
     [ toSql genre, toSql rhythm]  c  
  pure $ map Rhythm $ note (BadRequest $ "Unknown rhythm: " <> (show rhythm) <> " for the " <> (show genre) <> " genre") $ join mRhythm

-- | get all the rhythm records for a given genre
getRhythmRecords :: Genre -> Client -> Aff (Array RhythmRecord)
getRhythmRecords genre = do
  query read' 
     (Query "select genre, rhythm from rhythms where genre = $1 order by rhythm " :: Query RhythmRecord) 
     [ toSql genre ] 

-- | get all the rhythms for the requested genre as a string array
getRhythmStrings :: Genre -> Client -> Aff (Array String)
getRhythmStrings genre c = do
  rhythmRecords <- getRhythmRecords genre c
  pure $ map (_.rhythm >>> show) rhythmRecords


getRhythmsForGenre :: Genre -> Client -> Aff (Array Rhythm)
getRhythmsForGenre genre c = do
  rhythmRecords <- getRhythmRecords genre c
  pure $ map (_.rhythm) rhythmRecords

