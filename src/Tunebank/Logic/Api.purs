module Tunebank.Logic.Api 
   ( getTuneMidi
   , getTuneRefsPage
   , getUserRecordsPage
   , upsertValidatedTune) where

import Prelude
import Data.Abc.Midi (toMidi)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, fromArray)
import Yoga.Postgres (Client)
import Tunebank.Database.Tune (countSelectedTunes, getTuneAbc, getTuneRefs, upsertTune)
import Tunebank.Database.User (getUserRecords, getUserCount)
import Tunebank.Database.Search (SearchExpression)
import Tunebank.Types (Authorization, Genre, Title)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Pagination (PaginationExpression, PaginationResponse, TuneRefsPage, UserRecordsPage)
import Tunebank.HTTP.Response (ResponseError(..))


-- | upsert a tune - insert or update the database as appropriate
upsertValidatedTune :: Authorization -> Client -> Genre -> String -> Aff (Either ResponseError String)
upsertValidatedTune auth c genre tuneString =    
  -- make sure the tune is terminated before we parse it
  case (buildMetadata (tuneString <> "\n")) of 
    Right abcMetadata -> do
      upsertTune genre auth abcMetadata c          
    Left err -> do
      pure $ Left $ BadRequest err

-- | decorate a tune page returned from the database with its paging information
getTuneRefsPage :: Genre -> SearchExpression ->  PaginationExpression -> Int -> Client -> Aff TuneRefsPage
getTuneRefsPage genre searchExpression paginationExpression pageSize c = do
  tunes <- getTuneRefs genre searchExpression paginationExpression c
  count <- countSelectedTunes genre searchExpression c
  let 
    maxPages = (count / pageSize ) + 1
    pagination = paginate maxPages

  pure { tunes, pagination}

  where
  page :: Int 
  page = (paginationExpression.offset / pageSize) + 1

  paginate :: Int -> PaginationResponse
  paginate maxPages = { page,  maxPages, size: pageSize }


-- | decorate a tune page returned from the database with its paging information
getUserRecordsPage :: PaginationExpression -> Int -> Client -> Aff UserRecordsPage
getUserRecordsPage paginationExpression pageSize c = do
  users <- getUserRecords paginationExpression c
  count <- getUserCount c
  let 
    maxPages = (count / pageSize ) + 1
    pagination = paginate maxPages

  pure { users, pagination}

  where
  page :: Int 
  page = (paginationExpression.offset / pageSize) + 1

  paginate :: Int -> PaginationResponse
  paginate maxPages = { page,  maxPages, size: pageSize }

getTuneMidi :: Genre -> Title -> Client -> Aff (Either ResponseError Buffer)
getTuneMidi genre title c = do
  mAbc :: Maybe String <- getTuneAbc genre title c
  case mAbc of 
    Nothing -> 
      pure $ Left $ BadRequest $ "not found - tune: " <> show title
    Just abcString -> 
      case (parse abcString) of 
        Left { error, pos } -> 
          pure $ Left $ InternalServerError $ "Invalid ABC: " <> error <> " at position " <> show pos
        Right tune -> do
          let 
            intArray = (toUnfoldable <<< toMidi) tune
          buffer <- liftEffect $ fromArray intArray
          pure $ Right buffer




