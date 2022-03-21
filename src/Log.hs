module Log
  ( logMessage,
    LogLevel (..),
    HasLog (getLog),
    HasLogLevel (getLogLevel),
  )
where

import Control.Monad.Reader (MonadIO, MonadReader (ask), liftIO, when)

data LogLevel = Error | Info | Debug deriving (Show, Eq, Ord)

logMessage :: (MonadReader e m, MonadIO m, HasLog e) => LogLevel -> String -> m ()
logMessage logLevel msg = do
  env <- ask
  let envLevel = getLogLevel env
  let rMsg = show logLevel ++ ": " ++ msg
  when (logLevel <= envLevel) $
    liftIO $ getLog env rMsg

class (HasLogLevel e) => HasLog e where
  getLog :: e -> (String -> IO ())

class HasLogLevel e where
  getLogLevel :: e -> LogLevel
