module Test.Utils 
  ( adminAuth
  , fastan
  , generatePostgresTimestamp
  , getTestCommentId 
  , getInitialCommentId
  , getRegistrationId
  , removeCommentsFrom
  , removeUser
  , withDBConnection) 
  where

import Prelude
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.DateTime.Instant (toDateTime)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Now (now)
import Yoga.Postgres (Query(Query), Client, ClientConfig, queryValue_, queryValue, mkPool, withClient)
import Yoga.Postgres.SqlValue (toSql)
import Tunebank.Database.Utils (maybeIntResult, maybeStringResult)
import Tunebank.Database.User (deleteUser)
import Tunebank.Environment (connectionInfo)
import Tunebank.Database.Comment (getComments, deleteComments)
import Tunebank.Types (Authorization, Genre(..), UserName(..), Role(..), Title(..))
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
  eComments <- getComments (Genre "scandi") (Title "Griffenfeldt") c
  case eComments of 
    Left _err -> unsafeCrashWith "Test setup failure - couldn't find Griffenfeldt"
    Right comments -> 
      case (head comments) of
        Nothing -> 
          unsafeCrashWith "Test setup failure - couldn't find any comments for Griffenfeldt"
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
removeCommentsFrom :: Title -> Aff Unit
removeCommentsFrom title = 
  withDBConnection do 
    deleteComments (Genre "scandi") title


-- | query the test database using the default test user and database configuration
withDBConnection :: forall a. (Client -> Aff a) -> Aff a
withDBConnection execution = do
  pool <- liftEffect $ mkPool $ connectionInfo testClientConfig
  withClient pool execution

  where 

  testClientConfig :: ClientConfig
  testClientConfig =
    { host: "localhost"
    , database: "tunedbtest"
    , port: 5432
    , user: "test_database_user"
    , password: "changeit"
    , ssl: false
    }

adminAuth :: Authorization 
adminAuth = 
  { user : UserName "administrator"
  , role : Role "administrator"
  }

generatePostgresTimestamp :: Effect String 
generatePostgresTimestamp = do 
  inst <- now
  let
    eTimestamp = formatDateTime "YYYY-MM-DD hh:mm:ss" $ toDateTime inst
  pure $ either (const "bad date") identity eTimestamp



-- sample tune
fastan :: String
fastan =
  "X: 1\r\n"
  <> "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | ge3 c4 A4- | (3:5:3A4B4cBA2 B2d2 | de3 c8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | (3:4:3g2a2f4g4 e4- | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"