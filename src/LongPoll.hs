module LongPoll where

import API (API, HasManager (getManager))
import Control.Monad.Catch (Exception, MonadThrow)
import Control.Monad.Reader (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Manager, Request)

type LongPollHandler api m = (api -> ByteString -> m ())

class LongPoll lp where
  initLongPoll :: (MonadIO m, MonadThrow m, API api) => api -> m lp
  awaitLongPoll :: (MonadIO m, MonadThrow m, API api, HasManager api) => lp -> api -> m (ByteString, lp)

  handleLongPoll :: (MonadIO m, MonadThrow m, API api, HasManager api) => lp -> api -> LongPollHandler api m -> m ()
  handleLongPoll lp api handler = do
    let manager = getManager api
    (ups, newLP) <- awaitLongPoll lp api
    handler api ups
    handleLongPoll newLP api handler
