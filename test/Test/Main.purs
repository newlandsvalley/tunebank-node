module Test.Main where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (describe)
import Test.Authorization (authorizationSpec)
import Test.Integration (integrationSpec)

main :: Effect Unit
main = do 
  -- rebuild the DB and then run the tests
  runAff_ runSpec doNothing

runSpec :: forall a. Either Error a -> Effect Unit 
runSpec _ =
  runSpecAndExitProcess [ specReporter] do
    describe "all tunebank tests" do
      authorizationSpec
      integrationSpec

doNothing :: Aff Unit 
doNothing = pure unit
    
