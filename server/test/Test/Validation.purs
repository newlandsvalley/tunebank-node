module Test.Validation where

import Prelude

import Data.Either (Either(..), isRight)
import Effect (Effect)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (fastan, withDBConnection)
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
