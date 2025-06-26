module Tunebank.Pagination
  ( PaginationExpression
  , PaginationResponse
  , PageType(..)
  , SortKey
  , TuneRefsPage
  , PagingParams
  , UserRecordsPage
  , buildPaginationExpression
  , buildSearchPaginationExpression
  , buildPaginationExpressionString
  , defaultPaginationExpression
  , defaultUserPagingParams
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (toLower)
import Tunebank.Database.Search (SearchParams)
import Tunebank.Types (TuneRef, UserRecord)

data PageType
  = TunesPage
  | UsersPage

-- | supported sort keys for the tunes or users tables
-- | these have different interpretations for the two tables
data SortKey
  = ByAlphaKey
  | ByDateKey

derive instance eqSortKey :: Eq SortKey

instance showSortKey :: Show SortKey where
  show ByAlphaKey = "sort by alpha"
  show ByDateKey = "sort by date"

interpretSortKey :: PageType -> SortKey -> String
interpretSortKey page k =
  case page of
    TunesPage ->
      case k of
        ByAlphaKey -> "order by title ASC"
        ByDateKey -> "order by ts DESC"
    UsersPage ->
      case k of
        ByAlphaKey -> "order by username ASC"
        ByDateKey -> "order by ts DESC"

-- | a pagination request to the database
type PaginationExpression =
  { sort :: SortKey
  , limit :: Int
  , offset :: Int
  }

-- | a pagination response to the user, associated with the result page
type PaginationResponse =
  { page :: Int -- e.g. page 1
  , maxPages :: Int -- of 10
  , size :: Int -- number of entries per page
  }

-- | a page of tune references
type TuneRefsPage =
  { tunes :: Array TuneRef
  , pagination :: PaginationResponse
  }

-- | parameters supplied by the user for a search users request
type PagingParams =
  { page :: Maybe Int
  , sort :: Maybe String
  }

-- | a page of user records
type UserRecordsPage =
  { users :: Array UserRecord
  , pagination :: PaginationResponse
  }

defaultPaginationExpression :: PaginationExpression
defaultPaginationExpression =
  { sort: ByAlphaKey, limit: 15, offset: 0 }

defaultUserPagingParams :: PagingParams
defaultUserPagingParams =
  { page: Just 1, sort: Just "alpha" }

-- | build a pagination expression from the search params in place for tune search
buildSearchPaginationExpression :: SearchParams -> Int -> PaginationExpression
buildSearchPaginationExpression searchParams limit =
  let
    params :: PagingParams
    params = { page: searchParams.page, sort: searchParams.sort }
  in
    buildPaginationExpression params limit

-- | build a database pagination request expression from the incoming GET URL parameters
buildPaginationExpression :: PagingParams -> Int -> PaginationExpression
buildPaginationExpression params limit =
  { sort, limit, offset }

  where

  page = fromMaybe 1 params.page
  offset = (page - 1) * limit
  sort = maybe ByAlphaKey decodeKey params.sort

  decodeKey :: String -> SortKey
  decodeKey s =
    case (toLower s) of
      "date" -> ByDateKey
      _ -> ByAlphaKey

-- | build an expression string from the pagination request expression for the tail end of a query defining 
-- | the ORDER BY clause and also OFFSET and LIMIT pagination (which we'll try before possibly moving to cursors)
buildPaginationExpressionString :: PageType -> PaginationExpression -> String
buildPaginationExpressionString pageType req =
  " "
    <> interpretSortKey pageType req.sort
    <> " limit "
    <> show req.limit
    <> " offset "
    <> show req.offset
