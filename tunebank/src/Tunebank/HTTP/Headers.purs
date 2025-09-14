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

-- | ABC content type together with a suggestion for the download file name
abcHeaders :: String -> ResponseHeaders
abcHeaders fileName = 
  headers { "Content-Type": "text/vnd.abc" }
    <> utf8ContentDispositionHeaders fileName

-- | MIDI content type together with a suggestion for the download file name
midiHeaders :: String -> ResponseHeaders
midiHeaders fileName =
  headers { "Content-Type": "audio/midi" }
    <> simpleContentDispositionHeaders fileName

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

-- | simple content disposition headers suggesting a download file of the given name
simpleContentDispositionHeaders :: String -> ResponseHeaders
simpleContentDispositionHeaders fileName = 
  let 
    headerValue = "attachment; filename=" <> fileName
  in
    headers { "Content-Disposition": headerValue }

-- | content disposition headers suggesting a download file of type UTF-8 with the given name
utf8ContentDispositionHeaders :: String -> ResponseHeaders
utf8ContentDispositionHeaders fileName = 
  let 
    headerValue = "attachment; filename*=UTF-8''" <> fileName
  in
    headers { "Content-Disposition": headerValue }


