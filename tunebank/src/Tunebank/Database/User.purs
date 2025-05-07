module Tunebank.Database.User
  ( UserValidity(..)
  , assertKnownUser
  , assertIsAdministrator
  , deleteUser
  , getUserCount
  , getUserName
  , getUserRecord
  , getUserRecords
  , getUserRole
  , existsUser
  , existsValidatedUser
  , insertUser
  , validateUser
  , changeUserPassword
  , validateCredentials) where

import Prelude

import Data.Either (Either(..), note)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, error)
import Effect.Exception (throw, throwException)
import Data.Maybe (Maybe(..), maybe)
import Yoga.Postgres (Query(Query), Client, execute, query_, queryOne, queryValue, queryValue_)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Database.Utils (read', maybeStringResult, singleIntResult)
import Tunebank.Pagination (PaginationExpression, PageType(..), buildPaginationExpressionString)
import Tunebank.Types (Authorization, Credentials, NewUser, Password, UserName(..), Role (..), UserRecord)
import Tunebank.HTTP.Response (ResponseError(..))

-- | Validity of a user about to be inserted
data UserValidity =
    Unvalidated
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

validateCredentials :: Credentials -> Client -> Aff (Either String Authorization)
validateCredentials credentials c = do
  let 
    queryText = "select username as user, rolename as role from users where username = $1 and passwd = $2 and valid = 'Y'"
    params = [ toSql credentials.user, toSql credentials.password ]
  -- _ <- liftEffect $ logShow ("trying to match " <> user)
  mAuth <- queryOne read' (Query queryText :: Query (Maybe Authorization)) params c
  pure $ note ("Invalid credentials: " <> (show credentials.user)) (join mAuth)

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
  _ <- liftEffect $ logShow ("trying to get user record for user: " <> user)
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
  _ <- liftEffect $ log "trying to get all user records "
  query_ read' (Query queryText :: Query UserRecord) c

-- | validate a user by setting the valid flag if the hash corresponds
validateUser :: String -> Client -> Aff Unit
validateUser uuid c = do
  _ <- liftEffect $ logShow ("trying to authorise user with uuid " <> uuid)
  let 
    query = "update users set valid = 'Y' where CAST(registrationid AS CHAR(36)) = $1"
  execute (Query query) [ toSql uuid ] c

-- | register a user by setting the valid flag
changeUserPassword :: UserName -> Password -> Client -> Aff Unit
changeUserPassword user newPassword c = do
  _ <- liftEffect $ logShow ("trying to change password for user " <> (show user))
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
  if (userAlreadyExists) then do
    _ <- liftEffect $ logShow ("username " <> newUser.name <> " is already taken")
    pure $ Left $ BadRequest ("username " <> newUser.name <> " is already taken")
  else do
    let 
      valid = validity userValidity
      queryText = ("insert into users (username, rolename, passwd, email, valid) " <>
                  " values ($1, 'normaluser', $2, $3, $4 )" <> 
                  " returning CAST(registrationid AS CHAR(36))")
    _ <- liftEffect $ logShow ("trying to insert an as yet unregistered user " <> newUser.name)
    mResult <- queryValue maybeStringResult (Query queryText :: Query (Maybe String)) 
      [ toSql newUser.name, toSql newUser.password, toSql newUser.email, toSql valid ] c
    pure $ note (InternalServerError $ "user insert failed for " <> newUser.name) (join mResult)

    where      
    validity :: UserValidity -> String 
    validity Unvalidated = "N"
    validity Prevalidated = "Y"

assertKnownUser :: UserName -> Client -> Aff Unit
assertKnownUser user c = do
  eUser <- checkKnownUser user c 
  case eUser of 
    Right (UserName username) -> do
      liftEffect $ logShow ("user: " <> username <> " is OK")
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
  _ <- liftEffect $ logShow ("trying to count total number of users")
  matchCount <- queryValue_ singleIntResult (Query "select count(*) from users" :: Query Int) c
  pure $ maybe 0 identity matchCount


getUserName :: String -> Client -> Aff (Maybe String)
getUserName userName c = do
  _ <- liftEffect $ logShow ("trying to find users of name " <> userName)
  matchName <- queryValue maybeStringResult (Query "select username from users where username = $1 and valid = 'Y'" :: Query (Maybe String)) [ toSql userName ] c
  pure $ join matchName 







