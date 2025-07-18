module Test.Integration 
  (integrationSpec) 
    where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Base64 (encode)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Foreign.Object (Object)
import Foreign.Object (empty, singleton) as Object
import Test.HTTPurple.TestHelpers (Test, awaitStarted, delete, get, getStatus, post, (?=))
import Test.Spec (before_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Assertions.String (shouldStartWith)
import Test.Utils (getInitialCommentId, getTestCommentId, removeUser, withDBConnection)

-- | Integration tests requre that there is a running tunebank server on localhost:8080

integrationSpec :: Test
integrationSpec =
  describe "Integration" do
    getRequestsSpec
    postRequestsSpec
    deleteRequestsSpec

getRequestsSpec :: Test
getRequestsSpec = 
  describe "Get requests" do
    getHome
    getGenres
    getRhythms
    getTuneAbc
    getTuneMetadata
    getTuneMidi
    getComments
    getComment
    getUsers
    getUsersPage2
    getUsersForbidden
    registerUser
    checkUser
    checkUnknownUser
    getUserName


postRequestsSpec :: Test
postRequestsSpec = before_ prepareDB do
  describe "Post requests" do
    insertNewUser
    insertExistingUser
    updateExistingUser
    enforceUniqueEmail
    emailUserOTP
    changeUserPassword
    insertComment
    updateComment
    forbidUpdateToComment

  where 
    prepareDB = do 
      _ <- removeUser "Albert"
      removeUser "IvankaTrump"


deleteRequestsSpec :: Test
deleteRequestsSpec = do
  describe "Delete requests" do
    forbidDeleteComment

-- GET request tests

getHome :: Test
getHome =
  it "finds the home route" do
    -- close <- liftEffect main
    awaitStarted 8080
    response <- get 8080 Object.empty "/"
    -- liftEffect $ close $ pure unit
    response ?= "tunebank 0.0.1"

getGenres :: Test
getGenres =
  it "finds the genre route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre"
    response ?= """{"genres":["english","irish","klezmer","scandi","scottish"]}"""

getRhythms :: Test
getRhythms =
  it "finds the rhythms route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/rhythm"
    response ?= """{"rhythm":["brudmarsch","engelska","gånglåt","halling","hambo","långdans",""" <> 
      """"marsch","polka","polska","schottis","sekstur","skänklåt","slängpolska","waltz"]}"""  


getTuneAbc :: Test
getTuneAbc =
  it "finds tune abc route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/Elverumspols/abc"
    response `shouldStartWith` "X: 1"
    response `shouldSatisfy` contains (Pattern "Elverumspols")


getTuneMetadata :: Test
getTuneMetadata =
  it "finds tune metadata route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/Elverumspols"
    response `shouldSatisfy` contains (Pattern "Elverumspols")

getTuneMidi :: Test
getTuneMidi =
  it "finds tune MIDI route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/Elverumspols/midi"
    response `shouldStartWith` "MTh"
    -- response `shouldSatisfy` contains (Pattern "Elverumspols")


getComments :: Test
getComments =
  it "finds comments route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/Elverumspols/comments"
    -- response `shouldStartWith` """{"comments":"""
    response `shouldSatisfy` contains (Pattern "horrible tune")

getComment :: Test
getComment =
  it "finds comment route" do
    mId <- withDBConnection getTestCommentId
    case mId of
      Nothing -> fail "test framework error"
      Just id -> do
        awaitStarted 8080
        response <- get 8080 Object.empty ("/comment/" <> show id)
        -- response `shouldStartWith` """{"comments":"""
        response `shouldSatisfy` contains (Pattern "horrible tune")


getUsers :: Test
getUsers =
  it "finds users route  if admin auth" do
    awaitStarted 8080
    response <- get 8080 adminAuthHeaders "/user"
    response `shouldSatisfy` contains (Pattern "john.watson@gmx.co.uk")
    response `shouldStartWith` """{"users":[{"valid":"""

getUsersPage2 :: Test
getUsersPage2 =
  it "finds empty page of users on page 2" do
    awaitStarted 8080
    response <- get 8080 adminAuthHeaders "/user?page=2"
    response `shouldStartWith` """{"users":[]"""

getUsersForbidden :: Test
getUsersForbidden =
  it "forbids users route if normal auth" do
    awaitStarted 8080
    responseStatus <- getStatus 8080 normalAuthHeaders "/user"
    response <- get 8080 normalAuthHeaders "/user"
    responseStatus ?= 403
    response ?= """{"message":"This requires administrator authorization"}"""


registerUser :: Test
registerUser =
  it "registers the user" do
    awaitStarted 8080
    -- use Jim's registration id from the static SQL used to initialise the test database
    _ <- get 8080 Object.empty "/user/register/c78d39ac-5620-4b16-8c72-7a88e1dedfe8"
    response <- get 8080 Object.empty "/user/Jim"
    response `shouldStartWith` """{"valid":"Y"""

checkUser :: Test
checkUser =
  it "checks a normal users credentials" do
    awaitStarted 8080
    response <- get 8080 normalAuthHeaders "/user/check"
    response ?= "normaluser"

checkUnknownUser :: Test
checkUnknownUser =
  it "rejects an unknown users credentials" do
    awaitStarted 8080
    responseStatus <- getStatus 8080 unknownAuthHeaders "/user/check"
    responseStatus ?= 401 -- unauthorized

getUserName :: Test
getUserName = 
  it "gets the user name from her email address" do
  response <- post 8080 Object.empty "/user/getName" "john.watson@gmx.co.uk"
  response `shouldEqual` "user name emailed to user: John"

-- POST request tests

insertNewUser :: Test
insertNewUser = 
  it "inserts a genuine new user" do
  let 
    newUser = """{"name":"Albert","password":"changeit","email":"princealbert@gmail.com"}"""  
  response <- post 8080 Object.empty "/user" newUser 
  response `shouldEqual` "User: Albert created."

insertExistingUser :: Test
insertExistingUser = 
  it "forbids insertion of an existing user if she's validated" do
  let 
    newUser = """{"name":"John","password":"changeit","email":"john.doe@gmail.com"}"""  
  response <- post 8080 Object.empty "/user" newUser 
  response ?= """{"message":"username John is already taken"}"""

updateExistingUser :: Test
updateExistingUser = 
  it "allows update of an existing user if she's not yet validated" do
  let 
    newUser = """{"name":"DonaldTrump","password":"changeit","email":"donald@truthsocial.com"}"""  
    updatedUser = """{"name":"IvankaTrump","password":"changeit","email":"donald@truthsocial.com"}"""  
  _ <- post 8080 Object.empty "/user" newUser 
  response <- post 8080 Object.empty "/user" updatedUser 
  response `shouldEqual` "User: IvankaTrump created."

enforceUniqueEmail :: Test
enforceUniqueEmail = 
  it "forbids user insert if the email is taken by a validated user" do
  -- we use a different name from Tont Blair but use his email address
  let 
    newUser = """{"name":"CherieBlair","password":"changeit","email":"tony@blairfoundation.com"}"""  
  response <- post 8080 Object.empty "/user" newUser 
  response `shouldEqual` """{"message":"email tony@blairfoundation.com is already taken by another user"}"""

emailUserOTP :: Test
emailUserOTP = 
  it "sends an email with an OTP to the user" do
    awaitStarted 8080
    let 
      userPasswordOTP = """{"name":"TonyBlair","otp":"123-456-789"}"""   
    response <- post 8080 Object.empty "/user/newPasswordOTP" userPasswordOTP
    response `shouldEqual` "A one-time-password (OTP) has been sent to tony@blairfoundation.com."

changeUserPassword :: Test
changeUserPassword = 
  it "changes a user password" do
  let 
    userPassword = """{"name":"TonyBlair","password":"changedPassword"}"""   
  response <- post 8080 Object.empty "/user/newPassword" userPassword
  response `shouldEqual` "User: TonyBlair password updated."

insertComment :: Test 
insertComment = 
  it "inserts a comment if authorized" do
  let   
    newComment = """{"subject":"This is Bert's new comment to Elverumspols",""" <> 
                 """"text":"the comment has been inserted through the integration tests"}"""
    bertHeaders = authHeadersFor "Bert"
    url = "/genre/scandi/tune/Getingen/comments" 
  _ <- liftEffect $ logShow $ "insert comment - trying POST to " <> url
  _ <- delete 8080 adminAuthHeaders url 
  response <- post 8080 bertHeaders url newComment
  -- we should get an integer comment id
  response `shouldSatisfy` (fromString >>> isJust)

updateComment :: Test 
updateComment = 
  it "updates a comment if authorized" do
  let   
    updatedComment = """{"subject":"This is John's update to John's comment",""" <> 
                     """"text":"the comment has been updated through the integration tests"}"""
    johnHeaders = authHeadersFor "John"
  commentId <- withDBConnection $ getInitialCommentId
  let 
    url = "/comment/" <> (show commentId)
  -- _ <- liftEffect $ logShow $ "update comment - trying POST to " <> url
  response <- post 8080 johnHeaders url updatedComment
  response `shouldEqual` "unit"


forbidUpdateToComment :: Test 
forbidUpdateToComment = 
  it "forbids a normal user from updating another user's comment" do
  let   
    updatedComment = """{"subject":"This is Bert's updated to John's comment",""" <> 
                     """"text":"the comment has been updated through the integration tests"}"""
    bertHeaders = authHeadersFor "Bert"
  commentId <- withDBConnection $ getInitialCommentId
  let 
    url = "/comment/" <> (show commentId)
  -- _ <- liftEffect $ logShow $ "update comment - trying POST to " <> url
  response <- post 8080 bertHeaders url updatedComment
  response `shouldEqual` """{"message":"Only the original comment submitter or an administrator can update a comment"}""" 

-- DELETE request tests

forbidDeleteComment :: Test
forbidDeleteComment =
  it "forbids a normal user from deleting another user's comment" do
  let   
    bertHeaders = authHeadersFor "Bert"
  commentId <- withDBConnection $ getInitialCommentId
  let 
    url = "/comment/" <> (show commentId)
  response <- delete 8080 bertHeaders url
  response `shouldEqual` """{"message":"Only the original comment submitter or an administrator can delete a comment"}""" 

-- Basic Auth headers for different users
adminAuthHeaders :: Object String
adminAuthHeaders = 
  Object.singleton "authorization" $ "Basic " <> (encode "administrator:changeit")

normalAuthHeaders :: Object String
normalAuthHeaders = 
  authHeadersFor "John"

unknownAuthHeaders :: Object String
unknownAuthHeaders = 
  authHeadersFor "UnknownUser"

authHeadersFor :: String -> Object String
authHeadersFor user = 
  Object.singleton "authorization" $ "Basic " <> (encode (user <> ":changeit"))