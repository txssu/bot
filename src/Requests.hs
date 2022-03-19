module Requests (sendRequest, envSendAPIMethdod) where

import API (API (newRequestWithMethod), APIException (APIException), newRequestWithMethod)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Env (HasEnv, getAPI, getManager)
import Network.HTTP.Client (Manager, Request, Response (responseBody, responseStatus), httpLbs)
import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as HStatus

sendRequest ::
  (MonadIO m, MonadThrow m) =>
  Manager ->
  Request ->
  m ByteString
sendRequest manager req = do
  let url = show $ HClient.host req <> HClient.path req <> HClient.queryString req
  res <- liftIO (httpLbs req manager)
  let status = responseStatus res
  case HStatus.statusCode status of
    200 -> do
      return $ responseBody res
    code -> do
      throwM APIException

envSendAPIMethdod ::
  ( MonadReader (env api) m,
    Control.Monad.Catch.MonadThrow m,
    MonadIO m,
    HasEnv env api,
    API api
  ) =>
  String ->
  [(String, String)] ->
  m L.ByteString
envSendAPIMethdod method params = do
  env <- ask
  let api = getAPI env
  let manager = getManager env
  req <- newRequestWithMethod api method params
  sendRequest manager req