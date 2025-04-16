module Test.Integration 
  (integrationSpec) 
    where

import Prelude

import Data.String.Base64 (encode)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Foreign.Object (Object)
import Foreign.Object (empty, fromFoldable, singleton) as Object
import Test.HTTPurple.TestHelpers (Test, awaitStarted, awaitStartedSecure, get, get', getBinary, getHeader, getStatus, post, postBinary, (?=))
import Test.Spec (describe, it)
import Test.Spec.Assertions.String (shouldStartWith)
import Test.Spec.Assertions (shouldSatisfy)

-- | Integration tests requre that there is a running tunebank server on localhost:8080

integrationSpec :: Test
integrationSpec =
  describe "Integration" do
    getHomeSpec
    getGenresSpec
    getRhythmsSpec
    getUsersSpec
    getUsersForbiddenSpec

getHomeSpec :: Test
getHomeSpec =
  it "finds the home route" do
    -- close <- liftEffect main
    awaitStarted 8080
    response <- get 8080 Object.empty "/"
    -- liftEffect $ close $ pure unit
    response ?= "tunebank 0.0.1 Joe"

getGenresSpec :: Test
getGenresSpec =
  it "finds the genre route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre"
    response ?= """{"genres":["english","irish","klezmer","scandi","scottish"]}"""

getRhythmsSpec :: Test
getRhythmsSpec =
  it "finds the rhythms route" do
    awaitStarted 8080
    response <- get 8080 Object.empty "/genre/scandi"
    response ?= """{"rhythm":["brudmarsch","engelska","gånglåt","halling","hambo","långdans",""" <> 
      """"marsch","polka","polska","schottis","sekstur","skänklåt","slängpolska","waltz"]}"""  

getUsersSpec :: Test
getUsersSpec =
  it "finds users route if admin auth" do
    awaitStarted 8080
    response <- get 8080 adminAuthHeaders "/user"
    response `shouldSatisfy` contains (Pattern "john.watson@gmx.co.uk")

getUsersForbiddenSpec :: Test
getUsersForbiddenSpec =
  it "forbids users route if normal auth" do
    awaitStarted 8080
    responseStatus <- getStatus 8080 normalAuthHeaders "/user"
    response <- get 8080 normalAuthHeaders "/user"
    responseStatus ?= 403
    response ?= """{"message":"This requires admninistrator authorization"}"""

adminAuthHeaders :: Object String
adminAuthHeaders = 
  Object.singleton "authorization" $ "Basic " <> (encode "Administrator:changeit")

normalAuthHeaders :: Object String
normalAuthHeaders = 
  Object.singleton "authorization" $ "Basic " <> (encode "John:changeit")