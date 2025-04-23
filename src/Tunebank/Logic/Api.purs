module Tunebank.Logic.Api 
   ( getTuneRefsPage
   , upsertValidatedTune) where

import Prelude
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Yoga.Postgres (Client)
import Tunebank.Database.Tune (countSelectedTunes, getTuneRefs, upsertTune)
import Tunebank.Database.Search (SearchExpression)
import Tunebank.Types (Authorization, Genre)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Pagination (PaginationExpression, PaginationResponse, TuneRefsPage)


-- | upsert a tune - insert or update the database as appropriate
upsertValidatedTune :: Authorization -> Client -> Genre -> String -> Aff (Either String String)
upsertValidatedTune auth c genre tuneString =    
  -- make sure the tune is terminated before we parse it
  case (buildMetadata (tuneString <> "\n")) of 
    Right abcMetadata -> do
      upsertTune genre auth abcMetadata c          
    eErr -> do
      pure $ rmap (const "") eErr

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
  paginate maxPages = { page,  maxPages }




