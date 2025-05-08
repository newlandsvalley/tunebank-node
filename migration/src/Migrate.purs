module Migrate where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Path (FilePath, concat, normalize)
import Users (arbitrageUser, decodeUser, readMigrationFile)
import Tunebank.Environment (connectionInfo)
import Tunebank.Config (TunebankConfig, loadConfig)
import Yoga.Postgres (withClient, mkPool)

stagingServer :: FilePath 
stagingServer = "/home/john/services/tunebank-node/"

main :: Effect Unit
main = launchAff_ $ do
  eConfig <- loadConfig $ concat [normalize stagingServer, "conf"]
  case eConfig of 
    Right config -> do
      migrateUsers config
    Left err -> 
      liftEffect $ log err 

migrateUsers :: TunebankConfig -> Aff Unit
migrateUsers config = do
  users <- readMigrationFile $ concat  [normalize stagingServer, "migration", "users.json"]
  let 
    userList = map decodeUser users 
  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (arbitrageUser c) userList


