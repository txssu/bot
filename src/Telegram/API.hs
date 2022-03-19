module Telegram.API (api) where

import API (API, APIException, HasManager (getManager), newRequestWithMethod)
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.Aeson as A
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (Manager, parseRequest)
import qualified Telegram.Types as T
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
  newRequestWithMethod TelegramAPI {apiToken = token} method params = do
    let url = printf "https://api.telegram.org/bot%s/%s?%s" token method (urlEncodeVars params)
    parseRequest url

newtype TelegramLongPoll = TelegramLongPoll {lpLastUpdateID :: Integer}
