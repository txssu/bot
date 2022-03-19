{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
  ( startTG,
    startVK,
  )
where

import API (API (sendAPIMethod))
import Control.Monad.Reader (runReaderT)
import Env (env, manager)
import qualified Telegram.API as TG
import qualified VK.API as VK

startTG = do
  m <- manager
  let api = TG.api "token" m
  r <- sendAPIMethod api "getMe" []
  print r

startVK = do
  m <- manager
  let api = VK.api "token" "5.131" m
  r <- sendAPIMethod api "groups.getById" []
  print r
