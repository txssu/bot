{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( start,
  )
where

import API (API (sendAPIMethod))
import Config (Config (..), TelegramBot (..), VKBot (..), loadConfig)
import Control.Concurrent.Async
import Control.Monad.Reader (ReaderT (runReaderT))
import Env (env, manager)
import Log (LogLevel (Info), logMessage)
import LongPoll (LongPoll (handleLongPoll))
import qualified LongPoll as LP
import qualified Telegram.API as TG
import Telegram.LongPoll (TelegramLongPoll (TelegramLongPoll))
import qualified VK.API as VK
import VK.LongPoll (VKLongPoll (VKLongPoll))

start = do
  config <- loadConfig
  let v = vk $ configVK config
  let t = telegram $ configTelegram config

  concurrently v t
  return ()

telegram TelegramBot {tgEnable = False} = return ()
telegram TelegramBot {tgEnable = True, tgToken = token} = do
  m <- manager
  let api = TG.api token m
  let e = env api
  lp :: TelegramLongPoll <- runReaderT LP.initLongPoll e
  runReaderT (LP.handleLongPoll lp (\bs -> logMessage Info $ "New tg message: " ++ show bs)) e

vk VKBot {vkEnable = False} = return ()
vk VKBot {vkEnable = True, vkToken = token, vkVersion = v} = do
  m <- manager
  let api = VK.api token v m
  let e = env api
  lp :: VKLongPoll <- runReaderT LP.initLongPoll e
  runReaderT (LP.handleLongPoll lp (\bs -> logMessage Info $ "New vk message: " ++ show bs)) e
