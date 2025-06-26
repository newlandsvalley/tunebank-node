module Tunebank.Environment where

import Prelude
import Effect.Console (log) as Console
import Effect (Effect)
import Tunebank.Config (TunebankConfig, MailConfig, PagingConfig, ServerConfig)
import Tunebank.Logging.Winston (Logger, createLogger)
import Yoga.Postgres (Pool, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool)

-- | A type to hold the environment for our ReaderT
type Env =
  { server :: ServerConfig
  , paging :: PagingConfig
  , mail :: MailConfig
  , corsOrigins :: Array String
  , dbpool :: Pool
  , logger :: Logger
  }

buildEnv :: TunebankConfig -> Effect Env
buildEnv config = do
  dbpool <- mkPool $ connectionInfo config.db
  logger <- createLogger config.logging.dir
  pure $
    { server: config.server
    , paging: config.paging
    , mail: config.mail
    , corsOrigins: config.security.corsOrigins
    , dbpool
    , logger
    }

connectionInfo :: ClientConfig -> ConnectionInfo
connectionInfo clientConfig =
  connectionInfoFromConfig clientConfig defaultPoolConfig

