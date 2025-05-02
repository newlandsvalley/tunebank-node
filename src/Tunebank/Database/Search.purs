module Tunebank.Database.Search where

import Prelude

import Data.Abc (KeySignature)
import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.Parser (parseKeySignature)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Map (Map, catMaybes, empty, insert, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data SearchCriterion
  = ByTitle
  | ByRhythm
  | ByKeySig
  | BySource
  | ByComposer
  | ByOrigin
  | ByTranscriber


instance showSearchCriterion :: Show SearchCriterion where
  show ByTitle  = "title"
  show ByRhythm = "rhythm"
  show ByKeySig = "keysig"
  show BySource = "source"
  show ByComposer = "composer"
  show ByOrigin = "origin"
  show ByTranscriber = "transcriber"


derive instance eqSearchCriterion:: Eq SearchCriterion
derive instance ordSearchCriterion:: Ord SearchCriterion

data SearchOperator
  = Equals 
  | Like

instance showSearchOperator :: Show SearchOperator where 
  show Equals = " = "
  show Like = " ilike "


derive instance eqSearchOperator:: Eq SearchOperator

type SearchTerm = 
  { criterion :: SearchCriterion
  , operator  :: SearchOperator
  , target    :: String
  }

type SearchExpression = Array SearchTerm


buildTermString :: SearchTerm -> String 
buildTermString searchTerm = 
  " and " 
  <> show searchTerm.criterion 
  <> show searchTerm.operator 
  <> embedTarget searchTerm.target (searchTerm.operator == Like)

  where
  embedTarget :: String -> Boolean -> String 
  embedTarget target isLike = 
    if isLike then 
      "'%" <> target <> "%'"
    else
      "'" <> target <> "'"

buildSearchExpressionString :: SearchExpression -> String 
buildSearchExpressionString = 
  map buildTermString >>> fold

normaliseKeySignature :: String -> String
normaliseKeySignature s = 
  case (parseKeySignature s) of 
    -- try to normalise if we can
    Right mks -> 
      normalise mks.keySignature
    -- otherwise leave it alone, meaning search on key will fail
    _ ->
      s
  where    
  
  normalise :: KeySignature -> String
  normalise k =
      show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode
      

-- | search params as they arrive from the request
type SearchParams =
  { title :: Maybe String
  , key :: Maybe String
  , rhythm :: Maybe String
  , source :: Maybe String
  , composer :: Maybe String
  , origin :: Maybe String
  , transcriber :: Maybe String
  , page :: Maybe Int
  , sort :: Maybe String
  }

defaultSearchParams :: SearchParams
defaultSearchParams =
  { title : Nothing
  , key: Nothing
  , rhythm: Nothing
  , source : Nothing
  , composer : Nothing
  , origin : Nothing
  , transcriber : Nothing
  , page: (Just 1)
  , sort: (Just "alpha")
  }

type SearchTermMaybeMap = Map.Map SearchCriterion (Maybe String)
type SearchTermMap = Map.Map SearchCriterion String
type SearchTermTuples = Array ( Tuple SearchCriterion String )


-- | build a search expression from the search parameters coming from the client
buildSearchExpression :: SearchParams -> SearchExpression 
buildSearchExpression p = 
  map f buildSearchTuples 

  where

  -- transform the tuple array into full search criteria
  f :: (Tuple SearchCriterion String ) -> SearchTerm 
  f (Tuple c t ) = 
    let 
      operator = 
        case c of 
          -- we treat key signature differently.  We attempt to normalise both the key on the database
          -- and the key supplied in the search and attempt to match them exactly
          ByKeySig -> Equals
          -- everything else is case-insensitive Like (ILike in Postgres)
          _ -> Like  
      target = 
        case c of 
          ByKeySig -> normaliseKeySignature t 
          _ -> t
    in  { criterion: c
        , operator : operator
        , target : target
        }

  -- build an array of SearchCriterion / target tuples from the possible search parameters
  -- eliminating any absent search terms
  buildSearchTuples :: SearchTermTuples
  buildSearchTuples = 
    let
      fullMap :: SearchTermMaybeMap
      fullMap = 
        (   Map.insert ByTitle p.title 
        >>> Map.insert ByRhythm p.rhythm
        >>> Map.insert ByKeySig p.key
        >>> Map.insert BySource p.source
        >>> Map.insert ByComposer p.composer
        >>> Map.insert ByOrigin p.origin 
        >>> Map.insert ByTranscriber p.transcriber) Map.empty
    in
      Map.toUnfoldable $ Map.catMaybes fullMap


