module Tunebank.Environment where

import Prelude
import Effect (Effect)
import Tunebank.Config (TunebankConfig, PagingConfig)
import Yoga.Postgres (Pool, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool)


-- | A type to hold the environment for our ReaderT
type Env = { paging :: PagingConfig
           , dbpool :: Pool
           }

buildEnv :: TunebankConfig -> Effect Env 
buildEnv config = do
  dbpool <- mkPool $ connectionInfo config.db
  pure $ { paging : config.paging
         , dbpool
         }

{-}
clientConfig :: ClientConfig
clientConfig =
  { host: "localhost"
  , database: "tunedbtest"
  , port: 5432
  , user: "tunebank_api"
  , password: "Brudmarsch"
  , ssl: false
  }
-}

connectionInfo :: ClientConfig -> ConnectionInfo
connectionInfo clientConfig = 
  connectionInfoFromConfig clientConfig defaultPoolConfig


