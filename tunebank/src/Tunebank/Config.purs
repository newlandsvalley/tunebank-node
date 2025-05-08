module Tunebank.Config 
  ( ServerConfig
  , PagingConfig
  --, MailAuth
  , MailConfig
  , SecurityConfig
  , TunebankConfig
  , loadConfig
  )
  
  where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
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
import Node.Path (FilePath, concat)
import Yoga.Postgres (ClientConfig) as Postgres
import NodeMailer (AuthConfig) as NM

type ServerConfig = 
  { host :: String 
  , port :: Int
  }

type PagingConfig = 
  { defaultSize :: Int }


type MailConfig = 
  { host :: String 
  , port :: Int
  , secure :: Boolean
  , auth :: NM.AuthConfig
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

loadConfig :: FilePath -> Aff (Either String TunebankConfig)
loadConfig dirPath = do
  let
    fullPath = concat [dirPath, "tunebank.yaml"]
  _ <- liftEffect $ logShow ("reading config from " <> fullPath)
  text <- readTextFile UTF8 fullPath
  liftEffect $ decodeYamlConfig text
