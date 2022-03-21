module VK.LongPoll where

import API (APIException (APIException), HasAPI (getAPI), newRequestWithMethod, sendRequest)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Data.Maybe (isNothing)
import LongPoll (LongPoll (..))
import qualified Network.HTTP.Client as HClient
import VK.Parse (parseResponse, parseUpdates)
import VK.Types (Updates)
import qualified VK.Types as VKTypes

newtype VKLongPoll = VKLongPoll {vkLP :: VKTypes.LongPollServer} deriving (Show)

instance LongPoll VKLongPoll where
  initLongPoll = do
    req <- newRequestWithMethod "groups.getById" []
    a <- sendRequest req
    let response = parseResponse a
    when (isNothing response) (throwM APIException)
    let Just groups = response
    let myID = VKTypes.gID . head $ groups

    req <- newRequestWithMethod "groups.getLongPollServer" [("group_id", show myID)]
    a <- sendRequest req
    let response = parseResponse a
    when (isNothing response) (throwM APIException)
    let Just lp = response

    return $ VKLongPoll lp

  awaitLongPoll VKLongPoll {vkLP = lp} = do
    req <- HClient.parseRequest (VKTypes.lpURL lp)
    a <- sendRequest req
    let response = parseUpdates a
    when (isNothing response) (throwM APIException)
    let Just updates = response

    let newTS = VKTypes.usTS updates
    let newLP = lp {VKTypes.lpTS = newTS}

    return (a, VKLongPoll newLP)
