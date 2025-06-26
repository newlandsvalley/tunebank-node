module Tunebank.Logging.Winston
  ( Logger
  , createLogger
  , logError
  , logInfo
  , logWarn
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

-- | the Winston logger
foreign import data Logger :: Type

foreign import createLoggerImpl :: EffectFn1 String Logger

createLogger :: String -> Effect Logger
createLogger = runEffectFn1 createLoggerImpl

foreign import logErrorImpl :: EffectFn2 Logger String Unit

logError :: Logger -> String -> Effect Unit
logError = runEffectFn2 logErrorImpl

foreign import logInfoImpl :: EffectFn2 Logger String Unit

logInfo :: Logger -> String -> Effect Unit
logInfo = runEffectFn2 logInfoImpl

foreign import logWarnImpl :: EffectFn2 Logger String Unit

logWarn :: Logger -> String -> Effect Unit
logWarn = runEffectFn2 logWarnImpl
