module VK.API where

import API (API, HasManager (getManager), newRequestWithMethod)
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
  newRequestWithMethod VKAPI {apiToken = token, apiVersion = version} method params = do
    let addParams = params ++ [("access_token", token), ("v", version)]
        url = printf "https://api.vk.com/method/%s?%s" method (urlEncodeVars addParams)
    parseRequest url
