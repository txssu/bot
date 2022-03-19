module Telegram.API where

import API (API, APIException, newRequestWithMethod)
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.Aeson as A
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (parseRequest)
import Requests (sendRequest)
import qualified Telegram.Types as T
import Text.Printf (printf)

newtype TelegramAPI = TelegramAPI {apiToken :: String}

instance API TelegramAPI where
  newRequestWithMethod TelegramAPI {apiToken = token} method params = do
    let url = printf "https://api.telegram.org/bot%s/%s?%s" token method (urlEncodeVars params)
    parseRequest url

newtype TelegramLongPoll = TelegramLongPoll {lpLastUpdateID :: Integer}

