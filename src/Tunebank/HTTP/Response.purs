module Tunebank.HTTP.Response 
  ( customBadRequest
  , customForbidden
  ) where

import Prelude

import Data.Argonaut (stringify)
import Effect.Aff.Class (class MonadAff)
import HTTPurple.Response (Response, badRequest', response')
import HTTPurple.Status (forbidden) as Status
import HTTPurple.Json (jsonHeaders)
import Tunebank.Logic.Codecs (encodeMessage)


-- | 400 Bad Request with a json message in the body
customBadRequest :: forall m. MonadAff m => String -> m Response
customBadRequest message = do
  let 
    body = stringify $ encodeMessage message
  badRequest' jsonHeaders body

-- | 403 Forbidden with a json message in the body
customForbidden :: forall m. MonadAff m => String -> m Response
customForbidden message = do
  let 
    body = stringify $ encodeMessage message
  response' Status.forbidden jsonHeaders body


