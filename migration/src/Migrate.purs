module Migrate
  ( migrateTunes
  , migrateComments
  , migrateUsers
  ) where

import Prelude

import Data.Traversable (sequence_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Path (concat)
import Comments (migrateComment, decodeComment)
import Tunes (migrateTune, decodeTune)
import Users (arbitrageUser, decodeUser)
import Tunebank.Environment (connectionInfo)
import Tunebank.Config (TunebankConfig)
import Yoga.Postgres (withClient, mkPool)
import Args.Types (IncomingGenre)
import Utils (readMigrationFile)

migrateUsers :: TunebankConfig -> Aff Unit
migrateUsers config = do
  _ <- liftEffect $ log "migrating users"
  users <- readMigrationFile $ concat [ "migration", "users.json" ]
  let
    userList = map decodeUser users
  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (arbitrageUser c) userList

migrateTunes :: IncomingGenre -> TunebankConfig -> Aff Unit
migrateTunes incomingGenre config = do
  _ <- liftEffect $ log $ "migrating " <> (show incomingGenre) <> " tunes"
  let
    filename = (show incomingGenre) <> "tunes.json"
  tunes <- readMigrationFile $ concat [ "migration", filename ]
  let
    tuneList = map decodeTune tunes

  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (migrateTune incomingGenre c) tuneList

migrateComments :: IncomingGenre -> TunebankConfig -> Aff Unit
migrateComments incomingGenre config = do
  _ <- liftEffect $ log $ "migrating " <> (show incomingGenre) <> " comments"
  let
    filename = (show incomingGenre) <> "comments.json"
  comments <- readMigrationFile $ concat [ "migration", filename ]
  let
    commentList = map decodeComment comments

  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (migrateComment incomingGenre c) commentList

