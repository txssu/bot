module VK.API where

import API (API, HasAPI (getAPI), HasManager (getManager), newRequestWithMethod)
import Control.Monad.Reader (ask)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (Manager, parseRequest)
import Text.Printf (printf)

api :: String -> String -> Manager -> VKAPI
api token version = VKAPI token version

data VKAPI = VKAPI
  { apiToken :: String,
    apiVersion :: String,
    apiManager :: Manager
  }

instance HasManager VKAPI where
  getManager = apiManager

instance API VKAPI where
  newRequestWithMethod method params = do
    env <- ask
    let VKAPI {apiToken = token, apiVersion = version} = getAPI env
    let addParams = params ++ [("access_token", token), ("v", version)]
        url = printf "https://api.vk.com/method/%s?%s" method (urlEncodeVars addParams)
    parseRequest url
