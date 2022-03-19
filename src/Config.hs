{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Yaml (FromJSON, decodeFileThrow, (.:))
import qualified Data.Yaml as Y
import GHC.Generics (Generic)

data Config = Config
  { configTelegram :: TelegramBot,
    configVK :: VKBot
  }

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config
      <$> v .: "telegram"
      <*> v .: "vk"
  parseJSON _ = fail "Expected Object for Config value"

data TelegramBot = TelegramBot
  { tgToken :: String,
    tgEnable :: Bool
  }

instance FromJSON TelegramBot where
  parseJSON (Y.Object v) =
    TelegramBot <$> v .: "token" <*> v .: "enable"
  parseJSON _ = fail "Expected Object for Config value"

data VKBot = VKBot
  { vkToken :: String,
    vkVersion :: String,
    vkEnable :: Bool
  }

instance FromJSON VKBot where
  parseJSON (Y.Object v) =
    VKBot <$> v .: "token" <*> v .: "version" <*> v .: "enable"
  parseJSON _ = fail "Expected Object for Config value"

loadConfig :: IO Config
loadConfig = decodeFileThrow "botconfig.yaml"
