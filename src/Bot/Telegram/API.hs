{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.API (api) where

import Bot.Base.API (API (checkErrors, newUrlWithMethod, replyMessage, sendAPIMethod), HasAPI (getAPI), HasManager (getManager))
import qualified Bot.Base.Error as BaseE
import Bot.Base.Log (LogLevel (Info), logMessage)
import qualified Bot.Base.Types as BaseT
import Bot.Telegram.Parse (parseError)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.Reader (ask)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException), Manager, Response (responseStatus))
import Network.HTTP.Types (Status (statusCode))
import Text.Printf (printf)

api :: String -> Manager -> TelegramAPI
api = TelegramAPI

data TelegramAPI = TelegramAPI
  { apiToken :: String,
    apiManager :: Manager
  }

instance HasManager TelegramAPI where
  getManager = apiManager

instance API TelegramAPI where
  newUrlWithMethod method params = do
    env <- ask
    let TelegramAPI {apiToken = token} = getAPI env
    return $ printf "https://api.telegram.org/bot%s/%s?%s" token method (urlEncodeVars params)

  checkErrors m = do
    catch m errorHandler

  replyMessage update msg = do
    let peerID = BaseT.uSender update
    logMessage Info $ "Telegram: Reply for " ++ show peerID
    sendAPIMethod "sendMessage" [("chat_id", show peerID), ("text", msg)]

errorHandler :: MonadThrow m => HttpException -> m a
errorHandler e@(HttpExceptionRequest _ (StatusCodeException resp body)) = do
  let st = statusCode $ responseStatus resp
  if st >= 400 && st < 500
    then do
      tgError <- parseError body
      throwM $ BaseE.ClientError (show tgError)
    else throwM e
errorHandler e = throwM e
