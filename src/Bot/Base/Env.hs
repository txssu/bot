{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Bot.Base.Env
  ( env,
    manager,
    Env (..),
    HasLog (..),
  )
where

import Bot.Base.API (HasAPI (getAPI))
import Bot.Base.Log (HasLog (getLog), HasLogLevel (getLogLevel), LogLevel (Debug), logger)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

manager :: IO Manager
manager = newManager tlsManagerSettings

env :: api -> Env api
env api = Env api (logger Debug putStrLn) Debug

data Env t = Env
  { envAPI :: !t,
    envLog :: !(LogLevel -> String -> IO ()),
    envLogLevel :: !LogLevel
  }

instance HasAPI Env where
  getAPI = envAPI

instance HasLog (Env a) where
  getLog = envLog

instance HasLogLevel (Env a) where
  getLogLevel = envLogLevel
