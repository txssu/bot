module Bot.VK.LongPoll where

import Bot.Base.API (APIException (APIException), HasAPI (getAPI), LongPoll (..), newRequestWithMethod, sendRequest)
import Bot.Base.Log (LogLevel (Debug, Error), logMessage)
import Bot.VK.Parse (parseResponse, parseUpdates, toBaseUpdate)
import qualified Bot.VK.Types as T
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.Aeson as A
import Data.Maybe (isNothing)
import Network.HTTP.Client (parseRequest)

newtype VKLongPoll = VKLongPoll {vkLP :: T.LongPollServer} deriving (Show)

instance LongPoll VKLongPoll where
  initLongPoll = do
    logMessage Debug "Init VK long poll"

    req <- newRequestWithMethod "groups.getById" []
    a <- sendRequest req
    let response = parseResponse a
    when (isNothing response) (throwM APIException)
    let Just groups = response
    let myID = T.gID . head $ groups

    req <- newRequestWithMethod "groups.getLongPollServer" [("group_id", show myID)]
    a <- sendRequest req
    let response = parseResponse a
    when (isNothing response) (throwM APIException)
    let Just lp = response

    return $ VKLongPoll lp

  awaitLongPoll VKLongPoll {vkLP = lp} = do
    req <- parseRequest (T.lpURL lp)
    a <- sendRequest req
    let response = parseUpdates a
    when (isNothing response) (throwM APIException)
    let Just updates = response

    let newTS = T.usTS updates
    let newLP = lp {T.lpTS = newTS}

    let us = mapM toBaseUpdate $ T.usUpdates updates
    case us of
      A.Error err -> do
        logMessage Error err
        throwM APIException
      A.Success a ->
        return (a, VKLongPoll newLP)
