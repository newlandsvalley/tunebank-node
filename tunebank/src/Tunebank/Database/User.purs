module Tunebank.Database.User
  ( assertKnownUser
  , assertIsAdministrator
  , deleteUser
  , getUserCount
  , getUserName
  , getUserPassword
  , getUserRecord
  , getUserRecords
  , getUserRole
  , getUserValidity
  , existsUser
  , existsValidatedUser
  , insertExportedUser
  , upsertUser
  , upsertPrevalidatedUser
  , validateUserFromHash
  , updateUserValidity
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


-- | get the user name from the email address if she exists
-- | result options are:
-- |   Nothing  - no user is using the email
-- |   Just "<name>£ - a user is using the email
getUserNameFromEmail :: Email -> Client -> Aff (Maybe String)
getUserNameFromEmail email c = do
  -- _ <- liftEffect $ logShow ("trying to get user validity for email" <> email)
  mUserName <- queryValue maybeStringResult (Query "select username from users where email = $1" :: Query (Maybe String)) [ toSql email ] c
  pure $ join mUserName


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
-- | this happens when the user clicks on a link that incorporates the hash code (UUID)
validateUserFromHash :: String -> Client -> Aff Unit
validateUserFromHash uuid c = do
  -- _ <- liftEffect $ logShow ("trying to authorise user with uuid " <> uuid)
  let
    query = "update users set valid = 'Y' where CAST(registrationid AS CHAR(36)) = $1"
  execute (Query query) [ toSql uuid ] c


-- | set/reset the valid flag if the user name corresponds
-- | this is used when if an administrator validates the user
updateUserValidity :: UserName -> Boolean -> Client -> Aff Unit
updateUserValidity user isValid c = do
  -- _ <- liftEffect $ logShow ("trying to change user validation with name " <> user)
  let
    validity = if isValid then "Y" else "N"
    query = "update users set valid = $1 where username = $2"
  execute (Query query) [ toSql validity, toSql user ] c

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

-- | upsert a new user.  This is intended to be called from the server module for registering a new user
-- | A variety of cases can occur:
-- |  - the new user is already properly registered - no need to do anything
-- |  - the new user name has been taken by another user - error
-- |  - the new user email address has been taken by another user - error
-- |  - the current user has not managed to complete the validation and is trying again - OK
-- |  - this is a completely new user and email address - OK - the default case
upsertUser :: NewUser -> Client -> Aff (Either ResponseError String)
upsertUser newUser c = do
  mExistingUserValidity <- getUserValidity (UserName newUser.name) c
  mExistingUserName <- getUserNameFromEmail newUser.email c
  case mExistingUserValidity of 
    Just "Y" -> do
      -- this user has already registered - no need to alter the validity
      pure $ Left $ BadRequest ("username " <> newUser.name <> " is already taken")
    Just _ -> do           
      case mExistingUserName of 
        Nothing -> 
          -- no-one has claimed this email address (which may differ from the one currently on record)
          -- we have a user record which has not been validated, so update the record
          updateNewUser
        Just existingName -> 
          -- no user has bagged this email address
          if (existingName ==  newUser.name) then do
            -- our user has bagged this email address  before but not completed his registration  
            updateNewUser
            -- pure $ Left $ BadRequest (" update user to do")
          else 
            -- a different user has bagged this email address
            pure $ Left $ BadRequest ("email " <> newUser.email <> " is already taken by another user")
    Nothing -> do     
      case mExistingUserName of 
        Nothing -> 
          -- no user has bagged this email address
          insertNewUser
          -- pure $ Left $ BadRequest (" insert user to do")
        Just existingName -> 
          if (existingName ==  newUser.name) then do
            -- shouldn't happen.  At this stage, there's no user record because there is no validity flag for this user name
            -- so we shouldn't find that this email address belongs to the new user name
            -- let's just insert, but really it's a no-op
            insertNewUser
            -- pure $ Left $ BadRequest (" update user to do")
          else 
            -- a different user has bagged this email address
            pure $ Left $ BadRequest ("email " <> newUser.email <> " is already taken by another user")
      -- pure $ Left $ BadRequest (" insert user to do")

    where 
      insertNewUser :: Aff (Either ResponseError String)
      insertNewUser = do
        let queryText =  ( "insert into users (username, rolename, passwd, email, valid) "
                <> " values ($1, 'normaluser', $2, $3, 'N')"
                <> " returning CAST(registrationid AS CHAR(36))" )
        mResult <- queryValue maybeStringResult (Query queryText :: Query (Maybe String))
                     [ toSql newUser.name, toSql newUser.password, toSql newUser.email ]
                     c
        pure $ note (InternalServerError $ "user creation failed for " <> newUser.name) (join mResult)


      updateNewUser :: Aff (Either ResponseError String)
      updateNewUser = do
        let queryText =  ( "update users set email = $1, passwd = $2 where username = $3 "
                          <> " returning CAST(registrationid AS CHAR(36))" )
        mResult <- queryValue maybeStringResult (Query queryText :: Query (Maybe String))
                     [ toSql newUser.email, toSql newUser.password, toSql newUser.name ]
                     c
        pure $ note (InternalServerError $ "user creation failed for " <> newUser.name) (join mResult)


-- | upsert a pre-validate user, returning the UUID needed from the validation
-- | This is called from the migration module where the user does pre-exist and 
-- | so the validity is set to 'Y' and is also used in test setup code.
upsertPrevalidatedUser :: NewUser -> Client -> Aff (Either ResponseError String)
upsertPrevalidatedUser newUser c = do
  userAlreadyExists <- existsUser (UserName newUser.name) c
  mExistingValidity <- getValidityFromEmail newUser.email c
  if (userAlreadyExists) then do
    pure $ Left $ BadRequest ("username " <> newUser.name <> " is already taken")
  else if (mExistingValidity == Just "Y") then do
    pure $ Left $ BadRequest ("email " <> newUser.email <> " is already taken by another user")
  else do
    let
      queryText =
        case mExistingValidity of
          Nothing ->
            ( "insert into users (username, rolename, passwd, email, valid) "
                <> " values ($1, 'normaluser', $2, $3, 'Y' )"
                <> " returning CAST(registrationid AS CHAR(36))"
            )
          _ {- Just "N" -} ->
            ( "update users set username = $1, passwd = $2 where email = $3 "
                <> " returning CAST(registrationid AS CHAR(36))"
            )
    -- _ <- liftEffect $ logShow ("trying to insert an as yet unregistered user " <> newUser.name)
    mResult <-  queryValue maybeStringResult (Query queryText :: Query (Maybe String))
                  [ toSql newUser.name, toSql newUser.password, toSql newUser.email ]
                  c
    pure $ note (InternalServerError $ "user creation failed for " <> newUser.name) (join mResult)


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

getUserValidity :: UserName -> Client -> Aff (Maybe String)
getUserValidity userName c = do
  -- _ <- liftEffect $ logShow ("trying to find validity for user of name " <> (show userName))
  mValidity <- queryValue maybeStringResult (Query "select valid from users where username = $1 " :: Query (Maybe String)) [ toSql userName ] c
  pure $ join mValidity

