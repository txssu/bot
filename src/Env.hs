{-# LANGUAGE FlexibleInstances #-}

module Env
  ( env,
    Env,
    HasLog (..),
    HasManager (..),
    HasBot (..),
    Bot (..),
    LongPoll (..),
  )
where

import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified VK.Types

env :: Bot -> IO Env
env bot = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  return $ Env bot putStrLn manager

data Env = Env
  { envBot :: !Bot,
    envLog :: !(String -> IO ()),
    envHTTPManager :: HClient.Manager
  }

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog (String -> IO ()) where
  getLog = id

instance HasLog Env where
  getLog = envLog

class HasManager a where
  getManager :: a -> HClient.Manager

instance HasManager HClient.Manager where
  getManager = id

instance HasManager Env where
  getManager = envHTTPManager

class HasBot a where
  getBot :: a -> Bot

instance HasBot Bot where
  getBot = id

instance HasBot Env where
  getBot = envBot

data Bot
  = VKBot
      { botToken :: String,
        botVersion :: String
      }
  | TelegramBot
      { botToken :: String
      }

data LongPoll
  = VKLongpoll
      { lpServer :: VK.Types.LongPollServer
      }
  | TelegramLongpoll
      { lpLastUpdateID :: Integer
      }
  deriving (Show)