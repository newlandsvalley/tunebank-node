module Test.Authorization where

import Data.Either (Either(..))
import Data.String.Base64 (encode)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>))
import Test.Spec (Spec, before_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Tunebank.HTTP.Authentication (parseCredentials)


authorizationSpec :: Spec Unit
authorizationSpec = do
  describe "authorization" do
    it "decodes basic auth credentials" do
      let 
        cred = "basic " <> encode "John:changeit"
      parseCredentials cred `shouldEqual` Right { user: "John", password: "changeit"}
  