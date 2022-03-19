{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
  ( startTG,
    startVK,
  )
where

import Control.Monad.Reader (runReaderT)
import Env (env)
import Requests (envSendAPIMethdod)
import Telegram.API (TelegramAPI (..))
import VK.API (VKAPI (..))

startTG = do
  e <- env (TelegramAPI "token")
  r <- runReaderT (envSendAPIMethdod "getMe" []) e
  print r

startVK = do
  e <- env (VKAPI "token" "5.131")
  r <- runReaderT (envSendAPIMethdod "groups.getById" []) e
  print r
