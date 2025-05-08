module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (serve')

import Tunebank.HTTP.Route (route, router)
import Tunebank.Config (TunebankConfig, loadConfig)
import Tunebank.Environment (buildEnv)

type ServerAffM = Aff (Effect Unit -> Effect Unit)

main :: Effect Unit
main = launchAff_ do 
  -- load the config file tunebank.yaml which must be in the conf subdirectory
  eConfig <- loadConfig "conf"
  case eConfig of 
    Right config -> do
      _ <- runServer config
      pure unit
    Left err -> 
      liftEffect $ log err 


-- | Boot up the server
runServer :: TunebankConfig -> ServerAffM
runServer config = do
  env <- liftEffect $ buildEnv config
  liftEffect $ serve' (\a -> runReaderT a env ) { hostname: config.server.host , port: config.server.port, onStarted } { route, router }
  where
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl -v localhost:8080             │"
    log " │    # => tunebank 0.0.1                │"
    log " └───────────────────────────────────────┘"


