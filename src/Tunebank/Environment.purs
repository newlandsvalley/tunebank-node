module Tunebank.Environment where

import Prelude
import Effect (Effect)
import Yoga.Postgres (Pool, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool)


-- | A type to hold the environment for our ReaderT
type Env = { name :: String
           , dbpool :: Pool
           }

buildEnv :: Effect Env 
buildEnv = do
  dbpool <- mkPool connectionInfo
  pure $ { name : "Joe"
         , dbpool
         }


clientConfig :: ClientConfig
clientConfig =
  { host: "localhost"
  , database: "tunedbtest"
  , port: 5432
  , user: "tunebank_api"
  , password: "Brudmarsch"
  , ssl: false
  }

connectionInfo :: ConnectionInfo
connectionInfo = connectionInfoFromConfig clientConfig defaultPoolConfig


