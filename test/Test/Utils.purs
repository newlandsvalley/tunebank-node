module Test.Utils 
  ( getTestCommentId 
  , getInitialCommentId
  , getRegistrationId
  , removeCommentsFrom
  , removeUser
  , withDBConnection) 
  where

import Prelude
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Yoga.Postgres (Query(Query), Client, queryValue_, queryValue, mkPool, withClient)
import Yoga.Postgres.SqlValue (toSql)
import Tunebank.Database.Utils (maybeIntResult, maybeStringResult)
import Tunebank.Database.User (deleteUser)
import Tunebank.Environment (connectionInfo)
import Tunebank.Database.Comment (getComments, deleteComments)
import Tunebank.Types (Genre(..), UserName(..))
import Partial.Unsafe (unsafeCrashWith)

-- | get the id of the test comment set up by the installation script
-- | this is necessary because the comment id is not static - it changes with every test run
getTestCommentId :: Client -> Aff (Maybe Int)
getTestCommentId c = do
  let 
    query = "select id from comments where comment = 'This is a horrible tune' " 
  mId <- queryValue_ maybeIntResult (Query query :: Query (Maybe Int)) c
  pure $ join mId


-- | get the id of the first of the comments we added to griffenfeldt when we set up the test database
getInitialCommentId :: Client -> Aff Int
getInitialCommentId c = do
  eComments <- getComments (Genre "scandi") "griffenfeldt" c
  case eComments of 
    Left _err -> unsafeCrashWith "Test setup failure - couldn't find griffenfeldt"
    Right comments -> 
      case (head comments) of
        Nothing -> 
          unsafeCrashWith "Test setup failure - couldn't find any comments for griffenfeldt"
        Just comment -> 
          pure comment.id

-- | get the registration id of a user 
-- | (the tests re-register John, which is an idempotent operation)
getRegistrationId :: String -> Client -> Aff String
getRegistrationId user c = do
  let query = "select CAST(registrationid AS CHAR(36)) from users where username = $1 "
  registrationId <- 
    queryValue maybeStringResult (Query query :: Query (Maybe String)) [toSql user] c
  case (join registrationId) of 
    Nothing -> 
      unsafeCrashWith ("Test setup failure - couldn't find a user registrationId for " <> user)
    Just uuid -> 
      pure uuid

removeUser :: String -> Aff Unit 
removeUser user = 
  withDBConnection do 
    deleteUser (UserName user)


-- | remove all comments from the named tune
removeCommentsFrom :: String -> Aff Unit
removeCommentsFrom title = 
  withDBConnection do 
    deleteComments (Genre "scandi") title


-- | query the test database
withDBConnection :: forall a. (Client -> Aff a) -> Aff a
withDBConnection execution = do
  pool <- liftEffect $ mkPool connectionInfo
  withClient pool execution