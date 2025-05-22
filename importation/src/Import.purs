module Import
  ( importTunes
  , importComments
  , importUsers
  )
  where


import Prelude

import Data.Traversable (sequence_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Path (concat)
import Comments (importComment, decodeComment)
import Tunes (importTune, decodeTune)
import Users (arbitrageUser, decodeUser)
import Tunebank.Environment (connectionInfo)
import Tunebank.Config (TunebankConfig)
import Yoga.Postgres (withClient, mkPool)
import Args.Types (IncomingGenre)
import Utils (readExportedFile)

importUsers :: TunebankConfig -> Aff Unit
importUsers config = do
  _ <- liftEffect $ log "importing users"
  users <- readExportedFile $ concat  ["export", "usersexport.json"]
  let 
    userList = map decodeUser users 
  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (arbitrageUser c) userList

importTunes :: IncomingGenre -> TunebankConfig -> Aff Unit
importTunes incomingGenre config = do
  _ <- liftEffect $ log $ "importing " <> (show incomingGenre) <> " tunesexport"
  let 
    filename = (show incomingGenre) <> "tunesexport.json"
  tunes <- readExportedFile $ concat  [ "export", filename]
  let 
    tuneList = map decodeTune tunes 

  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (importTune incomingGenre c) tuneList

importComments :: IncomingGenre -> TunebankConfig -> Aff Unit
importComments incomingGenre config = do
  _ <- liftEffect $ log $ "importing " <> (show incomingGenre) <> " commentsexport"
  let 
    filename = (show incomingGenre) <> "commentsexport.json"
  comments <- readExportedFile $ concat  [ "export", filename]
  let 
    commentList = map decodeComment comments 

  dbpool <- liftEffect $ mkPool $ connectionInfo config.db
  withClient dbpool $ \c -> do
    sequence_ $ map (importComment incomingGenre c) commentList



