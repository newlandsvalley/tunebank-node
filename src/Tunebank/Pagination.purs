module Tunebank.Pagination 
   ( PaginationExpression
   , PaginationResponse   
   , SortKey
   , TuneRefsPage
   , buildPaginationExpression
   , buildPaginationExpressionString
   , defaultPaginationExpression
   )
   where

import Prelude

import Data.Maybe (fromMaybe, maybe)
import Data.String.Common (toLower)
import Tunebank.Database.Search (SearchParams)
import Tunebank.Types (TuneRef)

-- | supported sort keys for the tunes table
data SortKey 
  = ByTitleKey
  | ByDateKey

derive instance eqSortKey :: Eq SortKey

instance showSortKey :: Show SortKey where
  show ByTitleKey = "order by title ASC"
  show ByDateKey = "order by ts DESC"

-- | a pagination request to the database
type PaginationExpression = 
  { sort :: SortKey 
  , limit :: Int 
  , offset :: Int 
  }

-- | a pagination response to the user, associated with the result page
type PaginationResponse = 
  { page :: Int          -- e.g. page 1
  , maxPages :: Int      -- of 10
  }

type TuneRefsPage = 
  { tunes :: Array TuneRef 
  , pagination :: PaginationResponse
  }

defaultPaginationExpression :: PaginationExpression 
defaultPaginationExpression = 
  { sort: ByTitleKey, limit: 15, offset: 0 } 


-- | build a database pagination request expression from the incoming GET URL parameters
buildPaginationExpression :: SearchParams -> Int -> PaginationExpression
buildPaginationExpression params limit = 
  { sort, limit, offset }
  
  where
  
  page = fromMaybe 1 params.page
  offset = (page - 1) * limit 
  sort = maybe ByTitleKey decodeKey params.sort

  decodeKey :: String -> SortKey
  decodeKey s = 
    case (toLower s) of
      "date" -> ByDateKey
      _ -> ByTitleKey

-- | build an expression string from the pagination request expression for the tail end of a query defining 
-- | the ORDER BY clause and also OFFSET and LIMIT pagination (which we'll try before possibly moving to cursors)
buildPaginationExpressionString :: PaginationExpression -> String 
buildPaginationExpressionString req = 
  " " 
  <> show req.sort 
  <> " limit " 
  <> show req.limit 
  <> " offset " 
  <> show req.offset
