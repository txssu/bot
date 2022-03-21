module Telegram.LongPoll where

import API (APIException (APIException), newRequestWithMethod, sendRequest)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Maybe (isNothing)
import LongPoll (LongPoll (..))
import Telegram.Parse (parseUpdates)
import qualified Telegram.Types as TGTypes

newtype TelegramLongPoll = TelegramLongPoll {lpLastUpdateID :: Integer} deriving Show

instance LongPoll TelegramLongPoll where
  initLongPoll api = do
    req <- newRequestWithMethod api "getUpdates" []
    a <- sendRequest api req
    let udts = parseUpdates a
    when (isNothing udts) (throwM APIException)
    let Just updates = udts

    let lst = TGTypes.usUpdates updates
    let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst

    return $ TelegramLongPoll {lpLastUpdateID = newUpdateID}

  awaitLongPoll lp@TelegramLongPoll {lpLastUpdateID = updateID} api = do
    req <- newRequestWithMethod api "getUpdates" [("timeout", "25"), ("offset", show $ updateID + 1)]
    a <- sendRequest api req
    let udts = parseUpdates a
    when (isNothing udts) (throwM APIException)
    let Just updates = udts

    let lst = TGTypes.usUpdates updates
    let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst

    return (a, lp {lpLastUpdateID = newUpdateID})
