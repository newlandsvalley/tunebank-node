module Tunebank.Database.Comment 
  ( getComments
  , getComment
  , deleteComment
  , deleteComments
  , updateComment
  , insertComment ) 
  
  where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), note)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Yoga.Postgres (Query(Query), Client, query, queryValue, queryOne, execute)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Database.Tune (getTuneId)
import Tunebank.Types (Authorization, Title, UserName(..), Genre, Comment, NewComment, isAdministrator)
import Tunebank.Database.Utils (maybeStringResult, read', singleIntResult)
import Tunebank.HTTP.Response (ResponseError(..))


insertComment :: Genre -> Title -> NewComment -> UserName -> Client -> Aff (Either ResponseError Int)
insertComment genre title comment (UserName user) c = do
  mTuneId <- getTuneId genre title c
  case mTuneId of 
    Nothing -> do 
      pure $ Left $ BadRequest ("Tune not found: " <> show title) 
    Just tuneId -> do
      let 
        queryText = "insert into comments ( tuneid, subject, comment, submitter) " <>
                    " values ($1, $2, $3, $4 ) returning id"
        params = [toSql tuneId, toSql comment.subject, toSql comment.text, toSql user] 
      -- _ <- liftEffect $ logShow ("trying to insert tune " <> vAbc.title)
      (mCommentId :: (Maybe Int))  <- queryValue singleIntResult (Query queryText :: Query Int) params c
      pure $ maybe (Left $ InternalServerError "comment insertion seems to have failed") Right mCommentId

getComments :: Genre -> Title -> Client -> Aff (Either ResponseError (Array Comment))
getComments genre title  c = do
  mTuneId <- getTuneId genre title c
  case mTuneId of 
    Nothing -> do 
      pure $ Left $ BadRequest ("Tune not found: " <> show title) 
    Just tuneId -> do
      let 
        queryText = "select subject, comment as text, submitter, id, " <>
                     " floor(extract (epoch from ts))::integer as timestamp " <>
                     "from comments where tuneid = $1"
      comments <- query read' (Query queryText :: Query Comment ) [toSql tuneId] c
      pure $ Right comments

getComment :: Int -> Client -> Aff (Either ResponseError Comment)
getComment commentId c = do
  let 
    queryText = "select subject, comment as text, submitter, id, " <>
                " floor(extract (epoch from ts))::integer as timestamp " <>
                "from comments where id = $1"
  mComment <- queryOne read' (Query queryText :: Query Comment ) [toSql commentId] c
  pure $ note (BadRequest $ "Comment not found: " <> (show commentId))  mComment


getCommentOwner :: Int -> Client -> Aff (Maybe UserName)
getCommentOwner commentId c = do
  -- _ <- liftEffect $ logShow ("trying to get owner (if she exists) of comment: " <> (show commentId))
  let 
    query = "select submitter from comments where id = $1 " 
    params = [toSql commentId]
  mOwner <- queryValue maybeStringResult (Query query :: Query (Maybe String)) params c
  pure $ (join >>> map UserName) mOwner


deleteComment :: Int -> Authorization -> Client -> Aff (Either ResponseError Unit)
deleteComment commentId auth c = do
  _ <- liftEffect $ logShow ("trying to delete comment " <> (show commentId))
  mOwner <- getCommentOwner commentId c
  case mOwner of 
    (Just owner) -> do
      if (auth.user == owner || isAdministrator auth.role) then do
        _ <- execute (Query "delete from comments where id = $1 ") 
           [ toSql commentId ] c
        pure $ Right unit
      else 
        pure $ Left $ Forbidden "Only the original comment submitter or an administrator can delete a comment"
    Nothing -> 
      pure $ Left $ BadRequest ("comment: " <> (show commentId) <> " not found")

-- | delete all comments for a tune
deleteComments :: Genre -> Title -> Client -> Aff Unit
deleteComments genre title  c = do
  _ <- liftEffect $ logShow ("trying to delete all comments for tune " <> show title)
  let 
    query = "delete from comments where tuneid in (select id from tunes where genre = $1 and title = $2)"
  _ <- execute (Query query) [toSql genre, toSql title] c
  pure unit

updateComment :: Int -> NewComment -> Authorization -> Client -> Aff (Either ResponseError Unit)
updateComment commentId comment auth c = do
  _ <- liftEffect $ logShow ("trying to update comment " <> (show commentId))
  mOwner <- getCommentOwner commentId c
  case mOwner of 
    (Just owner) -> do
      if (auth.user == owner || isAdministrator auth.role) then do
        _ <- execute (Query "update comments set subject = $1, comment = $2 where id = $3 ") 
           [ toSql comment.subject, toSql comment.text, toSql commentId ] c
        pure $ Right unit
      else 
        pure $ Left $ Forbidden "Only the original comment submitter or an administrator can update a comment"
    Nothing -> 
      pure $ Left $ BadRequest ("comment: " <> (show commentId) <> " not found")