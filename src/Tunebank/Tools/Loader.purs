module Tunebank.Tools.Loader 
  (uploadTunes) where

-- | WARNING - Just uses the Test database connection at the moment
-- | And curently only used in testing

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
import Yoga.Postgres (Client, mkPool, withClient)
import Tunebank.Types (Genre, UserName(..), Role(..))
import Tunebank.Config (testClientConfig)
import Tunebank.Environment (connectionInfo)


uploadTunes :: Genre -> FilePath -> Aff Unit
uploadTunes genre dirPath = do
  _ <- liftEffect $ logShow ("importing from " <> dirPath)
  files <- readdir $ normalize dirPath
  let 
    message = "found " <> (show $ length files) <> " files"
  _ <- liftEffect $ log message

  pool <- liftEffect $ mkPool $ connectionInfo testClientConfig
  withClient pool $ \c -> do
    _ <- traverse (uploadFile c >>> logResult) files
    pure unit
  pure unit

  where 
  uploadFile :: Client -> FilePath -> Aff (Either String String)
  uploadFile c filePath = do 
    let 
      fullPath = concat [dirPath, filePath]
    _ <- liftEffect $ log ("trying to read file: " <> filePath)
    text <- readTextFile UTF8 fullPath
    upsertValidatedTune ( { user: (UserName "John"), role: (Role "normaluser") }) c genre text

logResult :: forall a. Show a => Aff (Either String a) -> Aff Unit 
logResult aff = do
  eResult <- aff  
  _ <- liftEffect $ either logShow logShow eResult
  pure unit


