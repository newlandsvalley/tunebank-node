module Tunebank.Database.User
  ( assertKnownUser
  , assertIsAdministrator
  , deleteUser
  , getUserCount
  , getUserName
  , getUserRecord
  , getUserRecords
  , getUserRole
  , existsUser
  , existsRegisteredUser
  , insertUnregisteredUser
  , registerUser
  , changeUserPassword
  , validateUser
  , validateCredentials
  , withNormalUserAuth
  , withAdminAuth) where

import Prelude

import Data.Either (Either(..), note)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, Error, error)
import Effect.Exception (throw, throwException)
import Data.Maybe (Maybe(..), maybe)
import Yoga.Postgres (Query(Query), Client, execute, query_, queryOne, queryValue)
import Yoga.Postgres.SqlValue (toSql)

import Tunebank.Database.Utils (read', maybeStringResult, singleIntResult)
import Tunebank.Types (Authorization, Credentials, Email, Password, UserName(..), Role (..), UserRecord)

-- | return true if the user exists and is registered
existsRegisteredUser :: UserName -> Client -> Aff Boolean
existsRegisteredUser userName c = do
  -- _ <- liftEffect $ logShow ("trying to match " <> userName)
  matchCount <- queryValue singleIntResult (Query "select count(*) from users where username = $1 and valid = 'Y'" :: Query Int) [ toSql userName ] c
  pure $ maybe false ((_ > 0)) matchCount

-- | return true if the user exists (irrespective of her registration)
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

validateUser :: UserName -> Client -> Aff (Either String UserName)
validateUser user c = do
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
  _ <- liftEffect $ logShow ("trying to get user record for " <> user)
  queryOne read' (Query "select username, email, rolename as role, valid from users where username = $1" :: Query UserRecord) [ toSql user ] c


getUserRecords :: Client -> Aff (Array UserRecord)
getUserRecords c = do
  _ <- liftEffect $ log "trying to get all user records "
  query_ read' (Query "select username, email, rolename as role, valid from users" :: Query UserRecord) c

-- | register a user by setting the valid flag
registerUser :: UserName -> Client -> Aff Unit
registerUser (UserName user) c = do
  _ <- liftEffect $ logShow ("trying to authorise user " <> user)
  execute (Query "update users set valid = 'Y' where username = $1") [ toSql user ] c

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

-- | insert an as yet unregistered user
insertUnregisteredUser :: UserName -> Password -> Email -> Client -> Aff (Either String String)
insertUnregisteredUser (UserName user) password email c = do
  userAlreadyExists <- existsUser (UserName user) c
  if (userAlreadyExists) then do
    _ <- liftEffect $ logShow ("username " <> user <> " is already taken")
    pure $ Left ("username " <> user <> " is already taken")
  else do
    let 
      query = ("insert into users (username, rolename, passwd, email, valid) " <>
               " values ($1, 'normaluser', $2, $3, 'N')")
    _ <- liftEffect $ logShow ("trying to insert an as yet unregistered user " <> user)
    _ <- execute (Query query) [ toSql user, toSql password, toSql email ] c
    pure $ Right ("user " <> user <> " inserted")

assertKnownUser :: UserName -> Client -> Aff Unit
assertKnownUser user c = do
  eUser <- validateUser user c 
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


isAdministrator :: UserName -> Client -> Aff Boolean
isAdministrator user c = do
  mRole <- getUserRole user c 
  let 
    result = maybe false (_ == (Role "administrator")) mRole
  pure result

withAdminAuth :: forall a. UserName -> Client -> (UserName -> Aff (Either Error a)) -> Aff (Either Error a)
withAdminAuth user c p = do
  isAdmin <- isAdministrator user c
  if isAdmin then 
    p user
  else do
    pure $ Left $ error ("No admin authority for user " <> (show user))
  
withNormalUserAuth :: forall a. UserName -> Client -> (UserName -> Aff (Either Error a)) -> Aff (Either Error a)
withNormalUserAuth user c p = do
  isUser <- existsUser user c
  if isUser then 
    p user
  else do
    pure $ Left $ error ("No such user " <> (show user))


getUserCount :: String -> Client -> Aff Int
getUserCount userName c = do
  _ <- liftEffect $ logShow ("trying to count users of name " <> userName)
  matchCount <- queryValue singleIntResult (Query "select count(*) from users where username = $1 and valid = 'Y'" :: Query Int) [ toSql userName ] c
  pure $ maybe 0 identity matchCount


getUserName :: String -> Client -> Aff (Maybe String)
getUserName userName c = do
  _ <- liftEffect $ logShow ("trying to find users of name " <> userName)
  matchName <- queryValue maybeStringResult (Query "select username from users where username = $1 and valid = 'Y'" :: Query (Maybe String)) [ toSql userName ] c
  pure $ join matchName 







