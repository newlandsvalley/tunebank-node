module Tunebank.Database.User
  ( UserValidity(..)
  , assertKnownUser
  , assertIsAdministrator
  , deleteUser
  , getUserCount
  , getUserName
  , getUserPassword
  , getUserRecord
  , getUserRecords
  , getUserRole
  , existsUser
  , existsValidatedUser
  , insertExportedUser
  , insertUser
  , validateUser
  , changeUserPassword
  , validateCredentials
  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw, throwException)
import Tunebank.Database.Utils (read', maybeStringResult, singleIntResult)
import Tunebank.HTTP.Response (ResponseError(..))
import Tunebank.Pagination (PaginationExpression, PageType(..), buildPaginationExpressionString)
import Tunebank.Types (Authorization, Credentials, Email, NewUser, Password, UserName(..), Role(..), UserRecord, UserRecordExported)
import Yoga.Postgres (Query(Query), Client, execute, query_, queryOne, queryValue, queryValue_)
import Yoga.Postgres.SqlValue (toSql)

-- | Validity of a user about to be inserted
data UserValidity
  = Unvalidated
  | Prevalidated

-- | return true if the user exists and is validated
existsValidatedUser :: UserName -> Client -> Aff Boolean
existsValidatedUser userName c = do
  -- _ <- liftEffect $ logShow ("trying to match " <> userName)
  matchCount <- queryValue singleIntResult (Query "select count(*) from users where username = $1 and valid = 'Y'" :: Query Int) [ toSql userName ] c
  pure $ maybe false ((_ > 0)) matchCount

-- | return true if the user exists (irrespective of her validation)
existsUser :: UserName -> Client -> Aff Boolean
existsUser userName c = do
  -- _ <- liftEffect $ logShow ("trying to match " <> userName)
  matchCount <- queryValue singleIntResult (Query "select count(*) from users where username = $1" :: Query Int) [ toSql userName ] c
  pure $ maybe false ((_ > 0)) matchCount

-- | get the user validity from the email address if she exists
-- | result options are:
-- |   Nothing  - no user is using the email
-- |   Just "Y" - a validated user is using the email
-- |   Just "N" - a user is using the email who has not, for whatever reason, completed the registration
getValidityFromEmail :: Email -> Client -> Aff (Maybe String)
getValidityFromEmail email c = do
  -- _ <- liftEffect $ logShow ("trying to get user validity for email" <> email)
  mValidity <- queryValue maybeStringResult (Query "select valid from users where email = $1" :: Query (Maybe String)) [ toSql email ] c
  pure $ join mValidity

validateCredentials :: Credentials -> Client -> Aff (Either String Authorization)
validateCredentials credentials c = do
  let
    queryText = "select username as user, rolename as role from users where username = $1 and passwd = $2 and valid = 'Y'"
    params = [ toSql credentials.user, toSql credentials.password ]
  -- _ <- liftEffect $ logShow ("trying to match " <> user)
  mAuth <- queryOne read' (Query queryText :: Query (Maybe Authorization)) params c
  pure $ note ("Invalid credentials: " <> credentials.user) (join mAuth)

checkKnownUser :: UserName -> Client -> Aff (Either String UserName)
checkKnownUser user c = do
  -- _ <- liftEffect $ logShow ("trying to match " <> user)
  mUser <- queryValue maybeStringResult (Query "select username from users where username = $1 and valid = 'Y'" :: Query (Maybe String)) [ toSql user ] c
  -- pure $ maybe (Left $ error ("Unknown user: " <> user)) Right (join $ mResult)
  pure $ map UserName $ note ("Unknown user: " <> (show user)) (join mUser)

-- | get the user role, returning Nothing if the user doesn't exist
getUserRole :: UserName -> Client -> Aff (Maybe Role)
getUserRole user c = do
  -- _ <- liftEffect $ logShow ("trying to get role for " <> user)
  mRole <- queryValue maybeStringResult (Query "select rolename from users where username = $1 and valid = 'Y'" :: Query (Maybe String)) [ toSql user ] c
  pure $ (map Role) $ join mRole

getUserRecord :: UserName -> Client -> Aff (Maybe UserRecord)
getUserRecord (UserName user) c = do
  -- _ <- liftEffect $ logShow ("trying to get user record for user: " <> user)
  let
    query =
      "select username, email, rolename as role, valid, floor(extract (epoch from ts))::integer as timestamp"
        <> " from users where username = $1"
  queryOne read' (Query query :: Query UserRecord) [ toSql user ] c

getUserRecords :: PaginationExpression -> Client -> Aff (Array UserRecord)
getUserRecords paginationExpression c = do
  let
    queryText = "select username, email, rolename as role, valid,"
      <> " floor(extract (epoch from ts))::integer as timestamp from users "
      <> buildPaginationExpressionString UsersPage paginationExpression
  -- _ <- liftEffect $ log "trying to get all user records "
  query_ read' (Query queryText :: Query UserRecord) c

-- | validate a user by setting the valid flag if the hash corresponds
validateUser :: String -> Client -> Aff Unit
validateUser uuid c = do
  -- _ <- liftEffect $ logShow ("trying to authorise user with uuid " <> uuid)
  let
    query = "update users set valid = 'Y' where CAST(registrationid AS CHAR(36)) = $1"
  execute (Query query) [ toSql uuid ] c

-- | change the users's password
changeUserPassword :: UserName -> Password -> Client -> Aff Unit
changeUserPassword user newPassword c = do
  -- _ <- liftEffect $ logShow ("trying to change password for user " <> (show user))
  execute (Query "update users set passwd = $1 where username = $2") [ toSql newPassword, toSql user ] c

-- | delete the user
deleteUser :: UserName -> Client -> Aff Unit
deleteUser user c = do
  -- _ <- liftEffect $ logShow ("trying to delete user " <> user)
  execute (Query "delete from users where username = $1") [ toSql user ] c

-- | insert an as yet unvalidated user, returning the UUID needed for the eventual validation
insertUser :: NewUser -> UserValidity -> Client -> Aff (Either ResponseError String)
insertUser newUser userValidity c = do
  userAlreadyExists <- existsUser (UserName newUser.name) c
  mExistingValidity <- getValidityFromEmail newUser.email c
  if (userAlreadyExists) then do
    -- _ <- liftEffect $ logShow ("username " <> newUser.name <> " is already taken")
    pure $ Left $ BadRequest ("username " <> newUser.name <> " is already taken")
  else if (mExistingValidity == Just "Y") then do
    pure $ Left $ BadRequest ("email " <> newUser.email <> " is already taken by another user")
  else do
    let
      valid = validity userValidity
      queryText =
        case mExistingValidity of
          Nothing ->
            ( "insert into users (username, rolename, passwd, email, valid) "
                <> " values ($1, 'normaluser', $2, $3, $4 )"
                <> " returning CAST(registrationid AS CHAR(36))"
            )
          _ {- Just "N" -} ->
            ( "update users set username = $1, passwd = $2 where email = $3 "
                <> " returning CAST(registrationid AS CHAR(36))"
            )
    -- _ <- liftEffect $ logShow ("trying to insert an as yet unregistered user " <> newUser.name)
    mResult <- case mExistingValidity of
      Nothing ->
        queryValue maybeStringResult (Query queryText :: Query (Maybe String))
          [ toSql newUser.name, toSql newUser.password, toSql newUser.email, toSql valid ]
          c
      _ ->
        queryValue maybeStringResult (Query queryText :: Query (Maybe String))
          [ toSql newUser.name, toSql newUser.password, toSql newUser.email ]
          c
    pure $ note (InternalServerError $ "user creation failed for " <> newUser.name) (join mResult)

  where
  validity :: UserValidity -> String
  validity Unvalidated = "N"
  validity Prevalidated = "Y"

-- | insert a full user record (with all fields generated by the database after an export)
-- | used for import
insertExportedUser :: UserRecordExported -> Client -> Aff (Either ResponseError Unit)
insertExportedUser user c = do
  userAlreadyExists <- existsUser (UserName user.name) c
  if (userAlreadyExists) then do
    pure $ Left $ BadRequest ("username " <> user.name <> " is already taken")
  else do
    let
      query = "insert into users (username, rolename, passwd, email, valid, registrationid, ts) " <>
        " values ($1, $2, $3, $4, $5, $6, $7)"
      params = [ toSql user.name, toSql user.role, toSql user.password, toSql user.email ] <>
        [ toSql user.valid, toSql user.registrationId, toSql user.timestamp ]
    -- _ <- liftEffect $ logShow ("trying to insert a previously exported user " <> user.name)
    _ <- execute (Query query) params c
    pure $ Right unit

assertKnownUser :: UserName -> Client -> Aff Unit
assertKnownUser user c = do
  eUser <- checkKnownUser user c
  case eUser of
    Right (UserName _username) -> do
      -- liftEffect $ logShow ("user: " <> username <> " is OK")
      pure unit
    Left err -> do
      liftEffect $ throwException $ error err

assertIsAdministrator :: UserName -> Client -> Aff Unit
assertIsAdministrator user c = do
  mRole <- getUserRole user c
  case mRole of
    Just (Role "administrator") ->
      pure unit
    _ ->
      liftEffect $ throw ("user " <> (show user) <> " has insufficient authority")

getUserCount :: Client -> Aff Int
getUserCount c = do
  -- _ <- liftEffect $ logShow ("trying to count total number of users")
  mCount <- queryValue_ singleIntResult (Query "select count(*) from users" :: Query Int) c
  pure $ maybe 0 identity mCount

getUserName :: Email -> Client -> Aff (Maybe String)
getUserName email c = do
  -- _ <- liftEffect $ logShow ("trying to find user name from password " <> email)
  mName <- queryValue maybeStringResult (Query "select username from users where email = $1" :: Query (Maybe String)) [ toSql email ] c
  pure $ join mName

getUserPassword :: UserName -> Client -> Aff (Maybe String)
getUserPassword userName c = do
  -- _ <- liftEffect $ logShow ("trying to find password for user of name " <> (show userName))
  mPassword <- queryValue maybeStringResult (Query "select passwd from users where username = $1 and valid = 'Y'" :: Query (Maybe String)) [ toSql userName ] c
  pure $ join mPassword

