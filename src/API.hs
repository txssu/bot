module API
  ( API (..),
    HasAPI (getAPI),
    APIException (..),
    HasManager (..),
  )
where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import Data.ByteString.Lazy (ByteString)
import Data.Data (Typeable)
import Log (HasLog, LogLevel (Debug), logMessage)
import Network.HTTP.Client (Manager, Request, Response (responseBody, responseStatus), httpLbs)
import Network.HTTP.Types.Status (Status (statusCode))

class HasManager api => API api where
  newRequestWithMethod :: (MonadReader (env api) m, HasAPI env, MonadIO m, MonadThrow m) => String -> [(String, String)] -> m Request
  sendRequest :: (MonadReader (env api) m, HasLog (env api), HasAPI env, MonadIO m, MonadThrow m) => Request -> m ByteString
  sendRequest req = do
    env <- ask
    let api = getAPI env
    logMessage Debug "Send request"
    let manager = getManager api
    res <- liftIO (httpLbs req manager)
    let status = responseStatus res
    case statusCode status of
      200 -> do
        return $ responseBody res
      code -> do
        throwM APIException

  sendAPIMethod ::
    ( MonadReader (env api) m,
      HasAPI env,
      HasLog (env api),
      MonadThrow m,
      MonadIO m
    ) =>
    String ->
    [(String, String)] ->
    m ByteString
  sendAPIMethod method params = do
    env <- ask
    let api = getAPI env
    let manager = getManager api
    req <- newRequestWithMethod method params
    sendRequest req

class HasManager e where
  getManager :: e -> Manager

data APIException = APIException
  deriving (Show, Typeable)

instance Exception APIException

class HasAPI e where
  getAPI :: (API a) => e a -> a
