module Tunebank.Database.Tune where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Yoga.Postgres (Query(Query), Client, query_, queryOne, queryValue, queryValue_, execute)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Logic.AbcMetadata (ValidatedAbc)
import Tunebank.Pagination (PaginationExpression, PageType(..), buildPaginationExpressionString)
import Tunebank.Types (Authorization, UserName(..), TimestampString, Title(..), TuneMetadata, TuneRef, Genre, Role(..), isAdministrator)
import Tunebank.Database.Search (SearchExpression, buildSearchExpressionString)
import Tunebank.Database.User (getUserRole)
import Tunebank.Database.Utils (maybeIntResult, maybeStringResult, read', singleIntResult)
import Tunebank.HTTP.Response (ResponseError(..))

-- | insert a tune to the database, allowing the database to add the timestamp
insertTune :: Genre -> UserName -> ValidatedAbc -> Client -> Aff String
insertTune genre user vAbc c = do
  let 
    query = "insert into tunes ( title, genre, rhythm, submitter, keysig, composer, origin, source, transcriber, abc) "
            <> " values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10 )"
    params = [toSql vAbc.title, toSql genre, toSql vAbc.rhythm, toSql user, toSql vAbc.keysig] <>
              [toSql vAbc.composer, toSql vAbc.origin, toSql vAbc.source, toSql vAbc.transcriber, toSql vAbc.abc] 
  -- _ <- liftEffect $ logShow ("trying to insert tune " <> vAbc.title)
  _ <- execute (Query query) params c
  pure vAbc.title

-- | update a tune in the database
updateTune :: Genre -> ValidatedAbc -> Client -> Aff String
updateTune genre vAbc c = do
  let 
    query = "update tunes set " <> 
            " keysig = $1, composer = $2, origin = $3, source = $4, transcriber = $5, abc = $6 " <>
            " where genre = $7 and rhythm = $8 and title = $9"
    params = [toSql vAbc.keysig, toSql vAbc.composer, toSql vAbc.origin] <>
              [toSql vAbc.source, toSql vAbc.transcriber, toSql vAbc.abc] <>
              [toSql genre, toSql vAbc.rhythm, toSql vAbc.title]
  -- _ <- liftEffect $ logShow ("trying to update tune " <> vAbc.title)
  _ <- execute (Query query) params c 
  pure vAbc.title


-- | insert a tune to the database including the timestamp
-- | this is useful in migrating from MusicRest
insertTuneWithTs :: Genre -> UserName -> TimestampString -> ValidatedAbc -> Client -> Aff String
insertTuneWithTs genre user timestamp vAbc c = do
  -- _ <- liftEffect $ log ("trying to upsert tune with ts using owner " <> (show user))
  let 
    query = "insert into tunes ( title, genre, rhythm, submitter, keysig, composer, origin, source, transcriber, abc, ts) "
            <> " values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11 )"
    params = [toSql vAbc.title, toSql genre, toSql vAbc.rhythm, toSql user, toSql vAbc.keysig, toSql vAbc.composer] <>
              [toSql vAbc.origin, toSql vAbc.source, toSql vAbc.transcriber, toSql vAbc.abc, toSql timestamp] 
  -- _ <- liftEffect $ log ("query: " <> query)
  _ <- execute (Query query) params c
  pure vAbc.title


getTuneOwner :: Genre -> Title -> Client -> Aff (Maybe UserName)
getTuneOwner genre title c = do
  -- _ <- liftEffect $ logShow ("trying to get owner (if she exists) of tune: " <> title)
  let 
    query = "select submitter from tunes where genre = $1 and title = $2" 
    params = [toSql genre, toSql title]
  mOwner <- queryValue maybeStringResult (Query query :: Query (Maybe String)) params c
  pure $ (join >>> map UserName) mOwner


getTuneId :: Genre -> Title -> Client -> Aff (Maybe Int)
getTuneId genre title c = do
  -- _ <- liftEffect $ logShow ("trying to get id of tune: " <> title)
  let 
    query = "select id from tunes where genre = $1 and title = $2" 
    params = [toSql genre, toSql title]
  mId <- queryValue maybeIntResult (Query query :: Query (Maybe Int)) params c
  pure $ join mId


-- | upsert a tune, allowing the database to add the timestamp
upsertTune :: Genre -> Authorization -> ValidatedAbc -> Client -> Aff (Either ResponseError String)
upsertTune genre auth vAbc c = do
  -- userRole <- getUserRole user c
  mOwner <- getTuneOwner genre (Title vAbc.title) c
  result <- case mOwner of 
    (Just owner) -> 
      if (auth.user == owner || isAdministrator auth.role) then do
        title <- updateTune genre vAbc c
        pure $ Right title
      else 
        pure $ Left $ Forbidden "Only the original tune submitter or an administrator can update a tune"
    _ -> do
      title <- insertTune genre auth.user vAbc c
      pure $ Right title

  pure result

-- | upset a tune where we supply the timestamp for insert - useful for migration
upsertTuneWithTs :: Genre -> Authorization -> TimestampString -> ValidatedAbc -> Client -> Aff (Either ResponseError String)
upsertTuneWithTs genre auth timestamp vAbc c = do
  -- userRole <- getUserRole user c
  mOwner <- getTuneOwner genre (Title vAbc.title) c
  result <- case mOwner of 
    (Just owner) -> 
      if (auth.user == owner || isAdministrator auth.role) then do
        title <- updateTune genre vAbc c
        pure $ Right title
      else 
        pure $ Left $ Forbidden "Only the original tune submitter or an administrator can update a tune"
    _ -> do
      title <- insertTuneWithTs genre auth.user timestamp vAbc c
      pure $ Right title

  pure result


deleteTune :: Genre -> Title -> UserName -> Client -> Aff (Either ResponseError Unit)
deleteTune genre title user c = do
  -- _ <- liftEffect $ logShow ("trying to delete tune " <> show title)
  mOwner <- getTuneOwner genre title c
  mRole <- getUserRole user c
  let 
    userRole = maybe (Role "normaluser") identity mRole
  case mOwner of 
    (Just owner) -> do
      if (user == owner || isAdministrator userRole) then do
        _ <- execute (Query "delete from tunes where genre = $1 and title = $2") 
           [ toSql genre, toSql title ] c
        pure $ Right unit
      else 
        pure $ Left $ Forbidden "Only the original tune submitter or an administrator can delete a tune"
    Nothing -> 
      pure $ Left $ BadRequest ("tune: " <> show title <> " not found")


countSelectedTunes :: Genre -> SearchExpression -> Client -> Aff Int 
countSelectedTunes genre searchExpression c = do 
  let 
    query = "select count(*) from tunes where genre = '" <> (show genre) <> "'" 
            <> buildSearchExpressionString searchExpression
  -- _ <- liftEffect $ log ("query: " <> query)
  -- _ <- liftEffect $ logShow ("trying to count selected tunes")
  matchCount <- queryValue_ singleIntResult (Query query :: Query Int) c
  pure $ maybe 0 identity matchCount

getTuneRefs :: Genre -> SearchExpression -> PaginationExpression -> Client -> Aff (Array TuneRef)
getTuneRefs genre searchExpression paginationExpression c = do
  let 
    queryText = "select title, rhythm, floor(extract (epoch from ts))::integer as timestamp, abc " 
            <> "from tunes where genre = '" <> (show genre) <> "'" 
            <> buildSearchExpressionString searchExpression
            <> buildPaginationExpressionString TunesPage paginationExpression
  -- _ <- liftEffect $ log ("query: " <> queryText)
  -- _ <- liftEffect $ logShow ("trying to get tune refs for selected tunes")
  query_ read' (Query queryText :: Query TuneRef ) c

getTuneMetadata :: Genre -> Title -> Client -> Aff (Maybe TuneMetadata)
getTuneMetadata genre title c = do
  -- _ <- liftEffect $ logShow ("trying to get metadata for tune " <> show title)
  let 
    queryText = "select title, source, composer, origin, transcriber, submitter, id, " <>
                " floor(extract (epoch from ts))::integer as timestamp, abc " <>
                " from tunes where genre = $1 and title = $2 "    
    params = [toSql genre, toSql title]
  queryOne read' (Query queryText :: Query TuneMetadata) params c 

getTuneAbc :: Genre -> Title -> Client -> Aff (Maybe String)
getTuneAbc genre title c = do
  -- _ <- liftEffect $ logShow ("trying to get abc for tune " <> show title <> " and genre " <> show genre)
  let 
    queryText = "select abc from tunes where genre = $1 and title = $2 "    
    params = [toSql genre, toSql title]
  mmResult <- queryValue maybeStringResult (Query queryText :: Query (Maybe String)) params c 
  pure $ join mmResult


  

