module Telegram.LongPoll where

import API (APIException (APIException), HasAPI (getAPI), newRequestWithMethod, sendRequest)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (ask)
import Data.Maybe (isNothing)
import LongPoll (LongPoll (..))
import Telegram.Parse (parseUpdates, toGenericUpdate)
import qualified Telegram.Types as TGTypes

newtype TelegramLongPoll = TelegramLongPoll {lpLastUpdateID :: Integer} deriving (Show)

instance LongPoll TelegramLongPoll where
  initLongPoll = do
    req <- newRequestWithMethod "getUpdates" []
    a <- sendRequest req
    let udts = parseUpdates a
    when (isNothing udts) (throwM APIException)
    let Just updates = udts

    let lst = TGTypes.usUpdates updates
    let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst

    return $ TelegramLongPoll {lpLastUpdateID = newUpdateID}

  awaitLongPoll lp@TelegramLongPoll {lpLastUpdateID = updateID} = do
    req <- newRequestWithMethod "getUpdates" [("timeout", "25"), ("offset", show $ updateID + 1)]
    a <- sendRequest req
    let udts = parseUpdates a
    when (isNothing udts) (throwM APIException)
    let Just updates = udts

    let lst = TGTypes.usUpdates updates
    let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst

    return (map toGenericUpdate lst, lp {lpLastUpdateID = newUpdateID})
