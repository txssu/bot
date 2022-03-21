module Lib
  ( start,
  )
where

import API (API (sendAPIMethod))
import Config (Config (..), TelegramBot (..), VKBot (..), loadConfig)
import Control.Concurrent.Async
import Env (env, manager)
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
  lp <- LP.initLongPoll api :: IO TelegramLongPoll
  LP.handleLongPoll lp api (\_ bs -> print $ "New TG message: " ++ show bs)

vk VKBot {vkEnable = False} = return ()
vk VKBot {vkEnable = True, vkToken = token, vkVersion = v} = do
  m <- manager
  let api = VK.api token v m
  lp <- LP.initLongPoll api :: IO VKLongPoll
  LP.handleLongPoll lp api (\_ bs -> print $ "New VK message: " ++ show bs)
