{-# LANGUAGE ConstraintKinds #-}

module Bot.Base.API
  ( API (..),
    HasAPI (getAPI),
    HasManager (..),
    LongPoll (..),
    WithAPI,
  )
where

import Bot.Base.Log (HasLog, LogLevel (Debug), logMessage)
import qualified Bot.Base.Types as BaseT
import Bot.Database.Types (Database)
import Control.Monad.Catch (Exception, MonadCatch)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), foldM)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Manager, Request, Response (responseBody), httpLbs, parseUrlThrow)

type WithAPI env api m =
  ( MonadReader (env api) m,
    MonadIO m,
    MonadCatch m,
    HasAPI env,
    HasManager api,
    HasLog (env api),
    API api
  )

class HasManager api => API api where
  newUrlWithMethod :: WithAPI env api m => String -> [(String, String)] -> m String
  sendRequest :: WithAPI env api m => Request -> m ByteString
  sendRequest req = do
    env <- ask
    let api = getAPI env
    let manager = getManager api
    res <- liftIO (httpLbs req manager)
    return $ responseBody res

  checkErrors ::
    WithAPI env api m =>
    m ByteString ->
    m ByteString

  sendAPIMethod ::
    WithAPI env api m =>
    String ->
    [(String, String)] ->
    m ByteString
  sendAPIMethod method params = do
    url <- newUrlWithMethod method params
    request <- parseUrlThrow url
    checkErrors (sendRequest request)

  replyMessage ::
    WithAPI env api m =>
    BaseT.Update ->
    String ->
    m ByteString

class HasManager e where
  getManager :: e -> Manager

class HasAPI e where
  getAPI :: (API a) => e a -> a

class LongPoll lp where
  initLongPoll :: (WithAPI env api m) => m lp

  awaitLongPoll :: WithAPI env api m => lp -> m ([BaseT.Update], lp)

  handleLongPoll ::
    WithAPI env api m =>
    lp ->
    Database ->
    (Database -> BaseT.Update -> m Database) ->
    m ()
  handleLongPoll lp db handler = do
    (ups, newLP) <- awaitLongPoll lp
    newDB <- foldM handler db ups
    handleLongPoll newLP newDB handler
