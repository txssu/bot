{-# LANGUAGE ScopedTypeVariables #-}

module Bot
  ( start,
  )
where

import Bot.Base.API (API (replyMessage, sendAPIMethod), LongPoll (handleLongPoll))
import Bot.Base.Config (Config (..), TelegramBot (..), VKBot (..), loadConfig)
import Bot.Base.Env (env, manager)
import Bot.Base.Log (LogLevel (Info), logMessage)
import Bot.Base.Types (Update (NewMessage, UndefinedUpdate, uText))
import Control.Concurrent.Async ( concurrently )
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Bot.Telegram.API as TG
import Bot.Telegram.LongPoll (TelegramLongPoll (TelegramLongPoll))
import qualified Bot.VK.API as VK
import Bot.VK.LongPoll (VKLongPoll (VKLongPoll))
import qualified Bot.Base.API as LP

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
  runReaderT (LP.handleLongPoll lp handler) e

vk VKBot {vkEnable = False} = return ()
vk VKBot {vkEnable = True, vkToken = token, vkVersion = v} = do
  m <- manager
  let api = VK.api token v m
  let e = env api
  lp :: VKLongPoll <- runReaderT LP.initLongPoll e
  runReaderT (LP.handleLongPoll lp handler) e

handler u@NewMessage {uText = text} = do
  replyMessage u text
  return ()
handler UndefinedUpdate = return ()
