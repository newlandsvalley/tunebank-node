module Tunebank.HTTP.Headers
  ( abcHeaders
  , midiHeaders
  , corsHeadersAllOrigins
  , corsHeadersOrigin
  , preflightAllOrigins
  , preflightOrigin
  ) where

import Prelude
import HTTPurple.Headers (ResponseHeaders, headers)

-- | Custom response headers

-- | ABC content type
abcHeaders :: ResponseHeaders
abcHeaders = headers { "Content-Type": "text/vnd.abc" }

-- | MIDI content type
midiHeaders :: ResponseHeaders
midiHeaders = headers { "Content-Type": "audio/midi" }

-- | Basic access request (Get etc) from any javascript HTTPRequest
corsHeadersAllOrigins :: ResponseHeaders
corsHeadersAllOrigins =
  corsAllowAllOrigins
    <> corsAllowCredentials

-- | Basic access request from a javascript HTTPRequest from a specific host
corsHeadersOrigin :: String -> ResponseHeaders
corsHeadersOrigin origin =
  corsAllowOrigin origin
    <> corsAllowCredentials

-- | Preflight HTTP option responses for Post etc from any origin
preflightAllOrigins :: ResponseHeaders
preflightAllOrigins =
  corsAllowAllOrigins
    <> corsAllowMethods
    <> corsAllowAuthorization
    <> corsMaxAge

-- | Preflight HTTP option responses for Post etc from a specific host
preflightOrigin :: String -> ResponseHeaders
preflightOrigin origin =
  corsAllowOrigin origin
    <> corsAllowMethods
    <> corsAllowAuthorization
    <> corsMaxAge

corsAllowAllOrigins :: ResponseHeaders
corsAllowAllOrigins = headers { "Access-Control-Allow-Origin": "*" }

corsAllowOrigin :: String -> ResponseHeaders
corsAllowOrigin origin =
  headers { "Access-Control-Allow-Origin": origin }

corsAllowCredentials :: ResponseHeaders
corsAllowCredentials =
  headers { "Access-Control-Allow-Credentials": "true" }

corsAllowMethods :: ResponseHeaders
corsAllowMethods =
  headers { "Access-Control-Allow-Methods": "POST, GET, OPTIONS, DELETE" }

corsAllowAuthorization :: ResponseHeaders
corsAllowAuthorization =
  headers { "Access-Control-Allow-Headers": "Authorization, Content-type" }

corsMaxAge :: ResponseHeaders
corsMaxAge =
  headers { "Access-Control-Allow-Max-Age": "86400" }

