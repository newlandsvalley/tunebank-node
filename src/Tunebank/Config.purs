module Tunebank.Config 
  ( ServerConfig
  , PagingConfig
  , MailConfig
  , SecurityConfig
  , TunebankConfig
  , loadConfig
  , testClientConfig)
  
  where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (concat)
import Yoga.Postgres (ClientConfig) as Postgres

type ServerConfig = 
  { host :: String 
  , port :: Int
  }

type PagingConfig = 
  { defaultSize :: Int }

type MailConfig = 
  { host :: String 
  , port :: Int
  , login :: String
  , password :: String 
  , fromAddress :: String
  }

type SecurityConfig = 
  { corsOrigins:: Array String }

type TunebankConfig = 
  { server :: ServerConfig
  , db :: Postgres.ClientConfig 
  , paging :: PagingConfig
  , mail :: MailConfig
  , security :: SecurityConfig
  }

decodeYamlConfig :: String -> Effect (Either String TunebankConfig)
decodeYamlConfig s = case runExcept $ parseYAMLToJson s of
  Right json -> do
    -- _ <- logShow $ stringify json
    pure $ lmap printJsonDecodeError $ decodeDBConfig json
  Left _err ->
    pure $ Left "Could not parse YAML"

  where
  decodeDBConfig :: Json -> Either JsonDecodeError TunebankConfig
  decodeDBConfig = 
    decodeJson 

loadConfig :: Aff (Either String TunebankConfig)
loadConfig = do
  let
    fullPath = concat ["conf", "tunebank.yaml"]
  _ <- liftEffect $ logShow ("reading config from " <> fullPath)
  text <- readTextFile UTF8 fullPath
  liftEffect $ decodeYamlConfig text

-- | temporary - the Postgres client config just used for testing
testClientConfig :: Postgres.ClientConfig
testClientConfig =
  { host: "localhost"
  , database: "tunedbtest"
  , port: 5432
  , user: "tunebank_api"
  , password: "Brudmarsch"
  , ssl: false
  }