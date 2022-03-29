{-# LANGUAGE ScopedTypeVariables #-}

module Bot
  ( start,
  )
where

import qualified Bot.Base.API as API
import Bot.Base.Config (Config (..), TelegramBot (..), VKBot (..), loadConfig)
import Bot.Base.Env (env, manager)
import Bot.Database.Types (emptyDB)
import Bot.Handler (handler)
import qualified Bot.Telegram.API as TG
import Bot.Telegram.LongPoll (TelegramLongPoll)
import qualified Bot.VK.API as VK
import Bot.VK.LongPoll (VKLongPoll)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Reader (ReaderT (runReaderT))

start :: IO ()
start = do
  config <- loadConfig

  let v = vk $ configVK config
  let t = telegram $ configTelegram config

  _ <- concurrently v t
  return ()

telegram :: TelegramBot -> IO ()
telegram TelegramBot {tgEnable = False} = return ()
telegram TelegramBot {tgEnable = True, tgToken = token} = do
  m <- manager
  let api = TG.api token m
  let e = env api
  lp :: TelegramLongPoll <- runReaderT API.initLongPoll e
  runReaderT (API.handleLongPoll lp emptyDB handler) e

vk :: VKBot -> IO ()
vk VKBot {vkEnable = False} = return ()
vk VKBot {vkEnable = True, vkToken = token, vkVersion = v} = do
  m <- manager
  let api = VK.api token v m
  let e = env api
  lp :: VKLongPoll <- runReaderT API.initLongPoll e
  runReaderT (API.handleLongPoll lp emptyDB handler) e
