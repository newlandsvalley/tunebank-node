module Main where
  
import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Path (concat, normalize)
import Tunebank.Config (loadConfig)
import Types (IncomingGenre(..))
import Migrate (migrateComments, stagingServer)


main :: Effect Unit
main = launchAff_ $ do
  eConfig <- loadConfig $ concat [normalize stagingServer, "conf"]
  case eConfig of 
    Right config -> do
      -- migrateUsers config
      -- migrateTunes Klezmer config
      -- migrateTunes English config
      -- migrateTunes Irish config
      -- migrateTunes Scandi config
      -- migrateTunes Scottish config
      migrateComments English config
    Left err -> 
      liftEffect $ log err 
