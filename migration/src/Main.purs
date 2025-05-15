module Main where
  
import Prelude

import Args.Parser (parse)
import Args.Types (Target(..))
import Data.Array (drop, intercalate)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Migrate (migrateComments, migrateTunes, migrateUsers, stagingServer)
import Node.Path (concat, normalize)
import Node.Process (argv)
import Tunebank.Config (loadConfig)

main :: Effect Unit
main = do
  args :: Array String <- argv
  let 
    -- drop node and path to migrate.js
    argLine = intercalate " " $ drop 2 args
  _ <- log $ "args are: " <> argLine
  case (parse argLine) of 
    Left _ -> 
      log showHint 
    Right target -> 
      runMigration target

showHint :: String 
showHint = 
  "command line options are: \r\n" <> 
    "  users\r\n" <>
    "  english tunes\r\n" <>
    "  irish tunes\r\n" <>
    "  klezmer tunes\r\n" <>
    "  scandi tunes\r\n" <>
    "  scottish tunes\r\n" <>
    "  english comments\r\n" <>
    "  irish comments\r\n" <>
    "  klezmer comments\r\n" <>
    "  scandi comments\r\n" <>
    "  scottish comments\r\n" <> 
    "\r\n" <>
    "This should be run from the home directory of your tunebank server " <>
    "and expects subdirectories of conf (tunebank configuration) and " <>
    "migration (Json format files exported from Musicrest's MongoDB)."

runMigration :: Target -> Effect Unit
runMigration target = launchAff_ $ do
  eConfig <- loadConfig $ concat [normalize stagingServer, "conf"]
  case eConfig of 
    Right config -> do
      case target of 
        Users -> 
          migrateUsers config
        Tunes genre -> 
          migrateTunes genre config
        Comments genre -> 
          migrateComments genre config
    Left err -> 
      liftEffect $ log err 

