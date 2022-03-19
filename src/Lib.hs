module Lib
  ( start,
  )
where

import API (API (sendAPIMethod))
import Config (Config (..), TelegramBot (..), VKBot (..), loadConfig)
import Control.Monad.Reader (runReaderT)
import Control.Parallel (pseq)
import Env (env, manager)
import qualified Telegram.API as TG
import qualified VK.API as VK

start = do
  config <- loadConfig
  pseq (vk $ configVK config) (telegram $ configTelegram config)

telegram TelegramBot {tgEnable = False} = return ()
telegram TelegramBot {tgEnable = True, tgToken = token} = do
  m <- manager
  let api = TG.api token m
  r <- sendAPIMethod api "getMe" []
  print r

vk VKBot {vkEnable = False} = return ()
vk VKBot {vkEnable = True, vkToken = token, vkVersion = v} = do
  m <- manager
  let api = VK.api token v m
  r <- sendAPIMethod api "groups.getById" []
  print r
