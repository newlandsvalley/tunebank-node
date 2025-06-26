module Utils 
  ( handleException
  , readExportedFile
  , fixDoubleBackslash
  ) where

import Prelude

import Data.Either (Either(..))
import Data.String (replaceAll)
import Data.String.CodeUnits (singleton)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Utils (lines, startsWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Tunebank.HTTP.Response (ResponseError(..))

readExportedFile :: FilePath -> Aff (Array String)
readExportedFile filePath = do
  _ <- liftEffect $ log ("trying to read file: " <> filePath)
  text <- readTextFile UTF8 filePath
  pure $ lines text

handleException :: forall r.Error -> Aff (Either ResponseError r)
handleException err = do
  let 
    errorText = show err 
  if (startsWith  "error: duplicate key value violates unique constraint" errorText) then 
    pure $ Left $ BadRequest "skipping entry which has already been inserted"
  else do
    liftEffect $ log errorText
    pure $ Left $ BadRequest "skipping entry which is in error"

-- | We need to post-process any String values exported by postgres which contain control characters
-- | because postgres will preface each occurrence with an extra backslash. (This is because it uses
-- | the backslash as its own escape character as a row or column delimiter.)  We have a good deal
-- | of newlines in the ABC itself and in comments which need to be cleansed - \\n -> \n.
fixDoubleBackslash :: String -> String
fixDoubleBackslash = 
  replaceAll (Pattern "\\n") (Replacement $ singleton '\n')


