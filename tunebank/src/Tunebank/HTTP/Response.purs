module Tunebank.HTTP.Response
  ( ResponseError(..)
  , customBadRequest
  , customForbidden
  , customErrorResponse
  , customInternalServerError
  ) where

import Prelude

import Data.Argonaut (stringify)
import Effect.Aff.Class (class MonadAff)
import HTTPurple.Json (jsonHeaders)
import HTTPurple.Response (Response(..), badRequest', response', internalServerError, internalServerError', unauthorized)
import HTTPurple.Status (forbidden) as Status
import Tunebank.HTTP.Headers (corsHeadersAllOrigins)
import Tunebank.Logic.Codecs (encodeMessage)

data ResponseError
  = BadRequest String -- usually resource not found
  | Forbidden String
  | NotAuthorized
  | InternalServerError String

instance showResponseError :: Show ResponseError where
  show (BadRequest r) = "BadRequest: " <> r
  show (Forbidden r) = "Forbidden: " <> r
  show NotAuthorized = "NotAuthorized"
  show (InternalServerError r) = "InternalServerError: " <> r

derive instance eqResponseError :: Eq ResponseError

customErrorResponse :: forall m. MonadAff m => ResponseError -> m Response
customErrorResponse e =
  case e of
    BadRequest reason ->
      customBadRequest reason
    Forbidden reason ->
      customForbidden reason
    NotAuthorized ->
      unauthorized
    InternalServerError reason ->
      internalServerError reason

-- | 400 Bad Request with a json message in the body
customBadRequest :: forall m. MonadAff m => String -> m Response
customBadRequest message = do
  let
    body = stringify $ encodeMessage message
  badRequest' (jsonHeaders <> corsHeadersAllOrigins) body

-- | 403 Forbidden with a json message in the body
customForbidden :: forall m. MonadAff m => String -> m Response
customForbidden message = do
  let
    body = stringify $ encodeMessage message
  response' Status.forbidden (jsonHeaders <> corsHeadersAllOrigins) body

-- | 500 Internal Server Error with a json message in the body
customInternalServerError :: forall m. MonadAff m => String -> m Response
customInternalServerError message = do
  let
    body = stringify $ encodeMessage message
  internalServerError' (jsonHeaders <> corsHeadersAllOrigins) body

