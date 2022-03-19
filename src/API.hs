module API
  ( API (..),
    APIException (..),
    HasManager (..)
  )
where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString)
import Data.Data (Typeable)
import Network.HTTP.Client (Manager, Request, Response (responseBody, responseStatus), httpLbs)
import Network.HTTP.Types.Status (Status (statusCode))

class HasManager api => API api where
  newRequestWithMethod :: (MonadThrow m) => api -> String -> [(String, String)] -> m Request
  sendRequest :: (MonadIO m, MonadThrow m) => api -> Request -> m ByteString
  sendRequest api req = do
    let manager = getManager api
    res <- liftIO (httpLbs req manager)
    let status = responseStatus res
    case statusCode status of
      200 -> do
        return $ responseBody res
      code -> do
        throwM APIException
  sendAPIMethod ::
    ( MonadThrow m,
      MonadIO m
    ) =>
    api ->
    String ->
    [(String, String)] ->
    m ByteString
  sendAPIMethod api method params = do
    let manager = getManager api
    req <- newRequestWithMethod api method params
    sendRequest api req

class HasManager e where
  getManager :: e -> Manager

data APIException = APIException
  deriving (Show, Typeable)

instance Exception APIException
