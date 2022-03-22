module LongPoll where

import API (API, HasAPI (getAPI), HasManager (getManager))
import Control.Monad.Catch (Exception, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (ask))
import Data.ByteString.Lazy (ByteString)
import GenericUpdate (GenericUpdate)
import Log (HasLog)
import Network.HTTP.Client (Manager, Request)

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
