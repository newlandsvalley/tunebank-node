module Test.Naming where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Tunebank.Logic.Naming (safeFileName)

namingSpec :: Spec Unit
namingSpec =
  describe "naming spec" do
    it "leaves simple ASCII names unaffected" do
      safeFileName "Elvermumspols" `shouldEqual` "Elvermumspols"
    it "handles Å" do
      safeFileName "Årepolskan" `shouldEqual` "Arepolskan"
    it "handles å and ä" do
      safeFileName "Gånglåt från Mockfjärd" `shouldEqual` "Ganglat fran Mockfjard"
    it "handles ø" do
      safeFileName "Gammel reinlender frå Sønndala" `shouldEqual` "Gammel reinlender fra Sonndala"
    it "handles ö and -" do
      safeFileName "Höök-Olles storpolska" `shouldEqual` "Hook-Olles storpolska"
    it "removes punctuation" do 
      safeFileName "my {best} tune!"`shouldEqual` "my best tune"
