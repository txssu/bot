module Bot.Base.Log
  ( logMessage,
    LogLevel (..),
    HasLog (getLog),
    HasLogLevel (getLogLevel),
    logger
  )
where

import Control.Monad.Reader (MonadIO, MonadReader (ask), liftIO, when)

data LogLevel = Error | Info | Debug deriving (Show, Eq, Ord)

logger :: LogLevel -> (String -> IO ()) -> LogLevel -> String -> IO ()
logger envLevel logFunc msgLevel msg = do
  let rMsg = show msgLevel ++ ": " ++ msg
  when
    (msgLevel <= envLevel)
    ( do
        logFunc rMsg
    )

logMessage :: (MonadReader e m, MonadIO m, HasLog e) => LogLevel -> String -> m ()
logMessage msgLevel msg = do
  env <- ask
  let rMsg = show msgLevel ++ ": " ++ msg
  liftIO $ getLog env msgLevel rMsg

class (HasLogLevel e) => HasLog e where
  getLog :: e -> (LogLevel -> String -> IO ())

class HasLogLevel e where
  getLogLevel :: e -> LogLevel
