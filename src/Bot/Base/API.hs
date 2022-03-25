{-# LANGUAGE ConstraintKinds #-}

module Bot.Base.API
  ( API (..),
    HasAPI (getAPI),
    APIException (..),
    HasManager (..),
    LongPoll (..),
  )
where

import Bot.Base.Log (HasLog, LogLevel (Debug, Error), logMessage)
import Bot.Base.Types (GenericUpdate (NewMessage))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import Data.ByteString.Lazy (ByteString)
import Data.Data (Typeable)
import Network.HTTP.Client (Manager, Request, Response (responseBody, responseStatus), httpLbs)
import Network.HTTP.Types.Status (Status (statusCode))

type IsAPI env api m = (MonadReader (env api) m, HasAPI env, MonadIO m, MonadThrow m)

class HasManager api => API api where
  newRequestWithMethod :: (IsAPI env api m) => String -> [(String, String)] -> m Request
  sendRequest :: (IsAPI env api m, HasLog (env api)) => Request -> m ByteString
  sendRequest req = do
    env <- ask
    let api = getAPI env
    let manager = getManager api
    res <- liftIO (httpLbs req manager)
    let status = responseStatus res
    case statusCode status of
      200 -> do
        return $ responseBody res
      code -> do
        logMessage Error $ "Error status code: " ++ show res
        throwM APIException

  sendAPIMethod ::
    ( IsAPI env api m,
      HasLog (env api)
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

  replyMessage ::
    ( IsAPI env api m,
      HasLog (env api)
    ) =>
    GenericUpdate ->
    String ->
    m ByteString

class HasManager e where
  getManager :: e -> Manager

data APIException = APIException
  deriving (Show, Typeable)

instance Exception APIException

class HasAPI e where
  getAPI :: (API a) => e a -> a

class LongPoll lp where
  initLongPoll ::
    ( MonadReader (env api) m,
      HasLog (env api),
      HasAPI env,
      MonadIO m,
      MonadThrow m,
      API api
    ) =>
    m lp

  awaitLongPoll ::
    ( MonadReader (env api) m,
      HasLog (env api),
      HasAPI env,
      MonadIO m,
      MonadThrow m,
      API api,
      HasManager api
    ) =>
    lp ->
    m ([GenericUpdate], lp)

  handleLongPoll ::
    ( MonadReader (env api) m,
      HasLog (env api),
      HasAPI env,
      MonadIO m,
      MonadThrow m,
      API api,
      HasManager api
    ) =>
    lp ->
    (GenericUpdate -> m ()) ->
    m ()
  handleLongPoll lp handler = do
    env <- ask
    let api = getAPI env
    let manager = getManager api
    (ups, newLP) <- awaitLongPoll lp
    mapM_ handler ups
    handleLongPoll newLP handler
