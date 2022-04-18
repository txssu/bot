module Bot.Telegram.LongPoll where

import Bot.Base.API (API (sendAPIMethod), LongPoll (awaitLongPoll, initLongPoll))
import Bot.Base.Error (APIException (ParseError))
import Bot.Base.Log (LogLevel (Debug, Error), logMessage)
import Bot.Telegram.Parse (parseUpdates, toBaseUpdate)
import qualified Bot.Telegram.Types as T
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Either (isLeft)

newtype TelegramLongPoll = TelegramLongPoll {lpLastUpdateID :: Integer} deriving (Show)

instance LongPoll TelegramLongPoll where
  initLongPoll = do
    logMessage Debug "Init Telegram long poll"
    response <- sendAPIMethod "getUpdates" []
    let udts = parseUpdates response
    when
      (isLeft udts)
      ( do
          let Left err = udts
          logMessage Error $ "Error when trying init telegram long poll: " ++ err
          throwM $ ParseError err
      )
    let Right updates = udts

    let lst = T.usUpdates updates
    let newUpdateID = if null lst then 0 else T.uID . last $ lst

    return $ TelegramLongPoll {lpLastUpdateID = newUpdateID}

  awaitLongPoll lp@TelegramLongPoll {lpLastUpdateID = updateID} = do
    response <- sendAPIMethod "getUpdates" [("timeout", "25"), ("offset", show $ updateID + 1)]
    let udts = parseUpdates response
    when
      (isLeft udts)
      ( do
          let Left err = udts
          logMessage Error $ "Error when await telegram long poll: " ++ err
          throwM $ ParseError err
      )
    let Right updates = udts

    let lst = T.usUpdates updates
    let newUpdateID = if null lst then 0 else T.uID . last $ lst

    return (map toBaseUpdate lst, lp {lpLastUpdateID = newUpdateID})
