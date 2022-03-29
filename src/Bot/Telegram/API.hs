module Bot.Telegram.API (api) where

import Bot.Base.API (API (replyMessage, sendAPIMethod), HasAPI (getAPI), HasManager (getManager), newRequestWithMethod)
import Bot.Base.Log (LogLevel (Info), logMessage)
import qualified Bot.Base.Types as BaseT
import Control.Monad.Reader (ask)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (Manager, parseRequest)
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
  newRequestWithMethod method params = do
    env <- ask
    let TelegramAPI {apiToken = token} = getAPI env
    let url = printf "https://api.telegram.org/bot%s/%s?%s" token method (urlEncodeVars params)
    parseRequest url

  replyMessage update msg = do
    let peerID = BaseT.uSender update
    logMessage Info $ "Telegram: Reply for " ++ show peerID
    sendAPIMethod "sendMessage" [("chat_id", show peerID), ("text", msg)]
