module Comments where

import Prelude

import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.String (length)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Tunebank.Database.Comment (insertCommentWithTs)
import Tunebank.HTTP.Response (ResponseError)
import Tunebank.Types (Genre(..), Title(..), UserName(..))
import Args.Types (IncomingGenre)
import Utils (fixDoubleBackslash, handleException)
import Yoga.Postgres (Client)


type ExportedComment =
  { tune :: String
  , subject :: String
  , text :: String
  , submitter :: String 
  , ts :: String
  }

decodeJsonExportedComment :: Json -> Either JsonDecodeError ExportedComment
decodeJsonExportedComment json = do
    obj <- decodeJson json
    tune <- obj .: "title"
    subject <- obj .: "subject"
    text <- obj .: "comment"
    submitter <- obj .: "submitter"
    ts <- obj .: "ts"
    pure $ { tune, subject, text, submitter, ts}

decodeComment :: String -> Either String ExportedComment
decodeComment jsonString = 
  case (parseJson jsonString) of 
    Left err -> 
      if (length jsonString > 0) then
        Left $ printJsonDecodeError err 
      else 
        Left ""
    Right json ->
      bimap printJsonDecodeError identity $ decodeJsonExportedComment json


-- | import a comment and log the results
importComment :: IncomingGenre -> Client -> Either String ExportedComment -> Aff Unit
importComment incomingGenre c eComment = do 
  case eComment of 
    Left err -> 
      liftEffect $ log $ "decoding error: " <> err
    Right exportedComment -> do 
        eResult <- importComment' incomingGenre exportedComment c
        liftEffect $ either logShow (\i -> log ("comment inserted id: " <> show i)) eResult

-- import a successfully decoded comment
importComment' :: IncomingGenre -> ExportedComment -> Client -> Aff (Either ResponseError Int)
importComment' incomingGenre exportedComment c = do
  let 
    genre = Genre $ show incomingGenre
    title = Title exportedComment.tune
    user = UserName exportedComment.submitter
    ts = exportedComment.ts
    comment =  
      {  subject : fixDoubleBackslash exportedComment.subject
       , text : fixDoubleBackslash exportedComment.text
      }
  catchError 
    (insertCommentWithTs genre title comment user ts c)
    handleException 



