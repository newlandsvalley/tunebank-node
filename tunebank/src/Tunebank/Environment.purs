module Tunebank.Environment where

import Prelude
import Control.Logger.Journald.Types (JournaldLogger)
import Control.Logger.Journald (logger)
import Effect (Effect)
import Node.Systemd.Journald (Journald, journald)
import Tunebank.Config (TunebankConfig, MailConfig, PagingConfig, ServerConfig)
import Yoga.Postgres (Pool, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool)


-- | A type to hold the environment for our ReaderT
type Env = { server :: ServerConfig
           , paging :: PagingConfig
           , mail :: MailConfig
           , corsOrigins :: Array String
           , dbpool :: Pool
           , journal :: Journald
           }

buildEnv :: TunebankConfig -> Effect Env 
buildEnv config = do
  dbpool <- mkPool $ connectionInfo config.db
  journal <- journald { syslog_identifier: "tunebank-server" }
  pure $ { server : config.server
         , paging : config.paging
         , mail : config.mail
         , corsOrigins : config.security.corsOrigins
         , dbpool
         , journal
         }

connectionInfo :: ClientConfig -> ConnectionInfo
connectionInfo clientConfig = 
  connectionInfoFromConfig clientConfig defaultPoolConfig


