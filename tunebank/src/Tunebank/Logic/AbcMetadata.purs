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
import Data.String (joinWith)
import Data.String.Common (toLower, trim)
import Tunebank.Types (Rhythm(..))

type UnvalidatedAbc =
  { title :: Maybe String
  , rhythm :: Maybe Rhythm
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

buildMetadata :: String -> Either String ValidatedAbc
buildMetadata =
  validate >>> toEither >>> lmap (joinWith "\n")

  where
  validate :: String -> V Errors ValidatedAbc
  validate abc =
    case (parse abc) of
      Left { error, pos } ->
        invalid [ "Invalid ABC: " <> error <> " at position " <> show pos ]
      Right abcTune ->
        let
          unvalidatedAbc :: UnvalidatedAbc
          unvalidatedAbc =
            { title: getTitle abcTune
            , rhythm: map Rhythm $ map trim $ firstOf (_headers <<< traversed <<< _Rhythm) abcTune
            , modifiedKeysig: getKeySig abcTune
            , composer: map trim $ firstOf (_headers <<< traversed <<< _Composer) abcTune
            , origin: map trim $ firstOf (_headers <<< traversed <<< _Origin) abcTune
            , source: map trim $ firstOf (_headers <<< traversed <<< _Source) abcTune
            , transcriber: map trim $ firstOf (_headers <<< traversed <<< _Transcription) abcTune
            , abc: abc
            }
        in
          constructMetadata unvalidatedAbc

  constructMetadata :: UnvalidatedAbc -> V Errors ValidatedAbc
  constructMetadata u =
    validatedAbc <$> nonEmpty "title" u.title
      <*> nonEmptyRhythm u.rhythm
      <*> normalisedKeysig u.modifiedKeysig
      <*> pure u.composer
      <*> pure u.origin
      <*> pure u.source
      <*> pure u.transcriber
      <*> pure u.abc

nonEmpty :: String -> Maybe String -> V Errors String
nonEmpty fieldName Nothing = invalid [ fieldName <> " cannot be empty" ]
nonEmpty fieldName (Just "") = invalid [ fieldName <> " cannot be empty" ]
nonEmpty _ (Just value) = pure $ value

nonEmptyRhythm :: Maybe Rhythm -> V Errors Rhythm
nonEmptyRhythm Nothing = invalid [ "rhythm cannot be empty" ]
nonEmptyRhythm (Just (Rhythm "")) = invalid [ "rhythm cannot be empty" ]
nonEmptyRhythm (Just (Rhythm value)) = pure $ Rhythm $ toLower value

normalisedKeysig :: Maybe ModifiedKeySignature -> V Errors String
normalisedKeysig Nothing = invalid [ "key signature cannot be empty" ]
normalisedKeysig (Just mKsig) = pure $ normalise mKsig.keySignature
  where
  normalise :: KeySignature -> String
  normalise k =
    show k.pitchClass <> (keySignatureAccidental k.accidental) <> show k.mode

