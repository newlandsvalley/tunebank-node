module Test.Authorization where

import Data.Either (Either(..))
import Data.String.Base64 (encode)
import Prelude (Unit, (<>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tunebank.HTTP.Authentication (parseCredentials)

authorizationSpec :: Spec Unit
authorizationSpec = do
  describe "authorization" do
    it "decodes basic auth credentials" do
      let 
        cred = "basic " <> encode "John:changeit"
      parseCredentials cred `shouldEqual` Right { user: "John", password: "changeit"}
  