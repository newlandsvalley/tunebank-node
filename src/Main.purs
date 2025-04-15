module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks, runReaderT)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (ServerM, ok, serve')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

import Tunebank.HTTP.Route (Route, route, router)
import Yoga.Postgres (Client, Query(Query), execute_, mkPool, withClient)
import Tunebank.Environment (Env, buildEnv)




-- | Boot up the server
main :: ServerM
main = do
  env <- liftEffect buildEnv
  serve' (\a -> runReaderT a env ) { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080             │"
    log " │    # => hello, joe                    │"
    log " └───────────────────────────────────────┘"




{-}
-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                 │"
    log " │                                            │"
    log " │ To test, run:                              │"
    log " │  > curl localhost:8080   # => hello world! │"
    log " └────────────────────────────────────────────┘"


-}


