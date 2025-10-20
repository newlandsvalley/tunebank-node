module Tunebank.Tools.Loader
  ( uploadTunes
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Data.Array (length)
import Data.Either (Either, either)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.FS.Aff (readdir, readTextFile)
import Node.Encoding (Encoding(..))
import Node.Path (FilePath, concat, normalize)
import Tunebank.Logic.Api (upsertValidatedTune)
import Tunebank.Logging.Winston (Logger)
import Tunebank.Types (Genre, UserName(..), Role(..))
import Tunebank.HTTP.Response (ResponseError)
import Yoga.Postgres (Client)

-- | I think we should move this to test - it's only used by the test framework

uploadTunes :: Genre -> FilePath -> Logger -> Client -> Aff Unit
uploadTunes genre dirPath logger c = do
  _ <- liftEffect $ logShow ("importing from " <> dirPath)
  files <- readdir $ normalize dirPath
  let
    message = "found " <> (show $ length files) <> " files"
  _ <- liftEffect $ log message

  _ <- traverse (uploadFile >>> logResult) files
  pure unit

  where
  uploadFile :: FilePath -> Aff (Either ResponseError String)
  uploadFile filePath = do
    let
      fullPath = concat [ dirPath, filePath ]
    _ <- liftEffect $ log ("trying to read file: " <> filePath)
    text <- readTextFile UTF8 fullPath
    upsertValidatedTune ({ user: (UserName "John"), role: (Role "normaluser") }) c genre text logger

  logResult :: forall a. Show a => Aff (Either ResponseError a) -> Aff Unit
  logResult aff = do
    eResult <- aff
    _ <- liftEffect $ either logShow logShow eResult
    pure unit

