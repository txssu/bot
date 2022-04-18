module Bot.VK.LongPoll where

import Bot.Base.API (API (sendAPIMethod), LongPoll (awaitLongPoll, initLongPoll), sendRequest)
import Bot.Base.Error (APIException (ParseError))
import Bot.Base.Log (LogLevel (Debug, Error), logMessage)
import Bot.VK.API (getMyID)
import Bot.VK.Parse (parseResponse, parseUpdates, toBaseUpdate)
import qualified Bot.VK.Types as T
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.Aeson as A
import Data.Maybe (isNothing)
import Network.HTTP.Client (parseUrlThrow)

newtype VKLongPoll = VKLongPoll {vkLP :: T.LongPollServer} deriving (Show)

instance LongPoll VKLongPoll where
  initLongPoll = do
    logMessage Debug "Init VK long poll"

    myID <- getMyID

    response <- sendAPIMethod "groups.getLongPollServer" [("group_id", show myID)]
    let responseLP = parseResponse response
    when (isNothing responseLP) (throwM $ ParseError "")
    let Just lp = responseLP

    return $ VKLongPoll lp

  awaitLongPoll VKLongPoll {vkLP = lp} = do
    req <- parseUrlThrow (T.lpURL lp)
    a <- sendRequest req
    let response = parseUpdates a
    when (isNothing response) (throwM $ ParseError "")
    let Just updates = response

    let newTS = T.usTS updates
    let newLP = lp {T.lpTS = newTS}

    let us = mapM toBaseUpdate $ T.usUpdates updates
    case us of
      A.Error err -> do
        logMessage Error err
        throwM $ ParseError ""
      A.Success ok ->
        return (ok, VKLongPoll newLP)
