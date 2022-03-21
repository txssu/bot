{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Env
  ( env,
    manager,
    Env (..),
    HasLog (..),
  )
where

import API (HasAPI (getAPI))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT)
import Log (HasLog (getLog), HasLogLevel (getLogLevel), LogLevel (Debug))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

manager :: IO Manager
manager = newManager tlsManagerSettings

env :: api -> Env api
env api = Env api putStrLn Debug

data Env t = Env
  { envAPI :: !t,
    envLog :: !(String -> IO ()),
    envLogLevel :: !LogLevel
  }

instance HasAPI Env where
  getAPI = envAPI

instance HasLog (Env a) where
  getLog = envLog

instance HasLogLevel (Env a) where
  getLogLevel = envLogLevel
