module Tunebank.Logic.Api 
   ( upsertValidatedTune) where

import Prelude
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Yoga.Postgres (Client)
import Tunebank.Database.Tune (upsertTune)
import Tunebank.Types (Authorization, Genre)
import Tunebank.Logic.AbcMetadata (buildMetadata)

upsertValidatedTune :: Authorization -> Client -> Genre -> String -> Aff (Either String String)
upsertValidatedTune auth c genre tuneString =    
  -- make sure the tune is terminated before we parse it
  case (buildMetadata (tuneString <> "\n")) of 
    Right abcMetadata -> do
      upsertTune genre auth abcMetadata c          
    eErr -> do
      pure $ rmap (const "") eErr


