module Test.Integration 
  (integrationSpec) 
    where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Base64 (encode)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Foreign.Object (Object)
import Foreign.Object (empty, singleton) as Object
import Test.Utils (getTestCommentId , withDBConnection)
import Test.HTTPurple.TestHelpers (Test, awaitStarted, awaitStartedSecure, get, get', getBinary, getHeader, getStatus, post, postBinary, (?=))
import Test.Spec (describe, it)
import Test.Spec.Assertions.String (shouldStartWith)
import Test.Spec.Assertions (fail, shouldSatisfy)

-- | Integration tests requre that there is a running tunebank server on localhost:8080

integrationSpec :: Test
integrationSpec =
  describe "Integration" do
    getRequestsSpec

getRequestsSpec :: Test
getRequestsSpec =
  describe "Get requests" do
    getHome
    getGenres
    getRhythms
    getTuneAbc
    getComments
    getComment
    getUsers
    getUsersForbidden

getHome :: Test
getHome =
  it "finds the home route" do
    -- close <- liftEffect main
    awaitStarted 8080
    response <- get 8080 Object.empty "/"
    -- liftEffect $ close $ pure unit
    response ?= "tunebank 0.0.1 Joe"

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
    response <- get 8080 Object.empty "/genre/scandi"
    response ?= """{"rhythm":["brudmarsch","engelska","gånglåt","halling","hambo","långdans",""" <> 
      """"marsch","polka","polska","schottis","sekstur","skänklåt","slängpolska","waltz"]}"""  


getTuneAbc :: Test
getTuneAbc =
  it "finds tune abc route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/elverumspols"
    response `shouldStartWith` "X: 1"
    response `shouldSatisfy` contains (Pattern "Elverumspols")

getComments :: Test
getComments =
  it "finds comments route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi/tune/elverumspols/comment"
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
  it "finds users route if admin auth" do
    awaitStarted 8080
    response <- get 8080 adminAuthHeaders "/user"
    response `shouldSatisfy` contains (Pattern "john.watson@gmx.co.uk")
    response `shouldStartWith` """[{"valid":"""

getUsersForbidden :: Test
getUsersForbidden =
  it "forbids users route if normal auth" do
    awaitStarted 8080
    responseStatus <- getStatus 8080 normalAuthHeaders "/user"
    response <- get 8080 normalAuthHeaders "/user"
    responseStatus ?= 403
    response ?= """{"message":"This requires administrator authorization"}"""

adminAuthHeaders :: Object String
adminAuthHeaders = 
  Object.singleton "authorization" $ "Basic " <> (encode "Administrator:changeit")

normalAuthHeaders :: Object String
normalAuthHeaders = 
  Object.singleton "authorization" $ "Basic " <> (encode "John:changeit")