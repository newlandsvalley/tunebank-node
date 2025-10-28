module Test.Validation where

import Prelude

import Data.Either (Either(..), isRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (backslashTitle, fastan, withDBConnection)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Types (Genre(..))


validationSpec :: Spec Unit
validationSpec =
  describe "validation spec" do
    it "accepts a well-formed tune" do
      res <- withDBConnection $ buildMetadata fastan (Genre "scandi")
      res `shouldSatisfy` isRight
    it "rejects a tune with an invalid rhythm for the genre" do
      res <- withDBConnection $ buildMetadata fastan (Genre "irish")
      res `shouldEqual` (Left "Unknown rhythm: polska for the irish genre")
    it "rejects a tune with a title containing a backslash" do
      res <- withDBConnection $ buildMetadata backslashTitle (Genre "scandi")
      res `shouldEqual` (Left "Please use Unicode for all titles and don't use backslashes - you have submitted new\\tune")
