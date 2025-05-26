module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import HTTPurple (serve')

import Tunebank.HTTP.Route (route, router)
import Tunebank.Config (TunebankConfig, loadConfig)
import Tunebank.Environment (buildEnv)
import Tunebank.Logging.Winston (logInfo)

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
      liftEffect $ Console.log err 


-- | Boot up the server
runServer :: TunebankConfig -> ServerAffM
runServer config = do
  env <- liftEffect $ buildEnv config  
  _ <- liftEffect $ logInfo env.logger "Tunebank starting"

  liftEffect $ serve' (\a -> runReaderT a env ) { hostname: config.server.host , port: config.server.port, onStarted } { route, router }
  where
  onStarted = do
    Console.log " ┌───────────────────────────────────────┐"
    Console.log " │ Server now up on port 8080            │"
    Console.log " │                                       │"
    Console.log " │ To test, run:                         │"
    Console.log " │  > curl -v localhost:8080             │"
    Console.log " │    # => tunebank 0.0.1                │"
    Console.log " └───────────────────────────────────────┘"


