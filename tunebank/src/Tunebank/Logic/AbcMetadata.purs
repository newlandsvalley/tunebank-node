module Tunebank.Logic.AbcMetadata
  ( ValidatedAbc
  , buildMetadata
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Abc (ModifiedKeySignature, KeySignature)
import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (getKeySig)
import Data.Abc.Optics (_headers, _Composer, _Rhythm, _Origin, _Source, _Transcription)
import Data.Abc.Parser (parse)
import Data.Abc.Utils (getTitle)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid, toEither)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
import Data.String (contains, joinWith)
import Data.String.Common (toLower, trim)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Tunebank.Types (Genre, Rhythm(..))
import Tunebank.Database.Rhythm (validateRhythmForGenre)
import Yoga.Postgres (Client)

type UnvalidatedAbc =
  { title :: Maybe String
  , rhythm :: Maybe String
  , modifiedKeysig :: Maybe ModifiedKeySignature
  , composer :: Maybe String
  , origin :: Maybe String
  , source :: Maybe String
  , transcriber :: Maybe String
  , abc :: String
  }

type ValidatedAbc =
  { title :: String
  , rhythm :: Rhythm
  , keysig :: String
  , composer :: Maybe String
  , origin :: Maybe String
  , source :: Maybe String
  , transcriber :: Maybe String
  , abc :: String
  }

type Errors = Array String

validatedAbc :: String -> Rhythm -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> ValidatedAbc
validatedAbc title rhythm keysig composer origin source transcriber abc =
  { title, rhythm, keysig, composer, origin, source, transcriber, abc }

buildMetadata :: String -> Genre -> Client -> Aff (Either String ValidatedAbc)
buildMetadata abc genre c = do
  validation :: V Errors ValidatedAbc <- validate 
  pure $ (toEither >>> lmap (joinWith "\n")) validation

  where
  validate :: Aff (V Errors ValidatedAbc)
  validate =
    case (parse abc) of
      Left { error, pos } ->
        pure $ invalid [ "Invalid ABC: " <> error <> " at position " <> show pos ]
      Right abcTune -> do
        let
          unvalidatedAbc :: UnvalidatedAbc
          unvalidatedAbc =
            { title: getTitle abcTune
            , rhythm: map trim $ firstOf (_headers <<< traversed <<< _Rhythm) abcTune
            , modifiedKeysig: getKeySig abcTune
            , composer: map trim $ firstOf (_headers <<< traversed <<< _Composer) abcTune
            , origin: map trim $ firstOf (_headers <<< traversed <<< _Origin) abcTune
            , source: map trim $ firstOf (_headers <<< traversed <<< _Source) abcTune
            , transcriber: map trim $ firstOf (_headers <<< traversed <<< _Transcription) abcTune
            , abc: abc
            }
        constructMetadata unvalidatedAbc

  constructMetadata :: UnvalidatedAbc -> Aff (V Errors ValidatedAbc)
  constructMetadata u = do
    validatedRhythm <- validateRhythm u.rhythm      
    pure $ validatedAbc <$> validateTitle u.title
      <*> validatedRhythm
      <*> normalisedKeysig u.modifiedKeysig
      <*> pure u.composer
      <*> pure u.origin
      <*> pure u.source
      <*> pure u.transcriber
      <*> pure u.abc
  
  validateRhythm :: Maybe String -> Aff (V Errors Rhythm)
  validateRhythm Nothing = pure $ invalid [ "rhythm cannot be empty" ]
  validateRhythm (Just "") = pure $ invalid [ "rhythm cannot be empty" ]
  validateRhythm (Just value) = do
    eRhythm <- validateRhythmForGenre genre (Rhythm $ toLower value) c 
    case eRhythm of 
      Left errorText -> 
        pure $ invalid [ errorText ]
      Right r ->
        pure $ pure $ r
  
  validateTitle :: Maybe String -> V Errors String
  validateTitle Nothing = invalid [ "title cannot be empty" ]
  validateTitle (Just "") = invalid [ "title cannot be empty" ]
  validateTitle (Just value) = 
    if (contains (Pattern "\\")) value then 
      invalid [ "Please use Unicode for all titles and don't use backslashes - you have submitted " <> value ]
    else
      pure value

  normalisedKeysig :: Maybe ModifiedKeySignature -> V Errors String
  normalisedKeysig Nothing = invalid [ "key signature cannot be empty" ]
  normalisedKeysig (Just mKsig) = pure $ normalise mKsig.keySignature
    where
    normalise :: KeySignature -> String
    normalise k =
      show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode

