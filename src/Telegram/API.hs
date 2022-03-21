module Telegram.API (api) where

import API (API, APIException, HasAPI (getAPI), HasManager (getManager), newRequestWithMethod)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (ask)
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
  newRequestWithMethod method params = do
    env <- ask
    let TelegramAPI {apiToken = token} = getAPI env
    let url = printf "https://api.telegram.org/bot%s/%s?%s" token method (urlEncodeVars params)
    parseRequest url
