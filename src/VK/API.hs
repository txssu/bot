module VK.API where

import API (API, newRequestWithMethod)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP.Client (parseRequest)
import Text.Printf (printf)

data VKAPI = VKAPI
  { vkToken :: String,
    vkVersion :: String
  }

instance API VKAPI where
  newRequestWithMethod VKAPI {vkToken = token, vkVersion = version} method params = do
    let addParams = params ++ [("access_token", token), ("v", version)]
        url = printf "https://api.vk.com/method/%s?%s" method (urlEncodeVars addParams)
    parseRequest url
