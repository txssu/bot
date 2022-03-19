{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Env
  ( env,
    manager,
    Env,
    HasEnv,
    HasLog (..),
    HasAPI (..),
  )
where

import API (API, newRequestWithMethod)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT)
import Network.HTTP.Client ( newManager, Manager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

manager :: IO Manager
manager = newManager tlsManagerSettings

env :: (API api) => api -> Env api
env api = Env api putStrLn

data Env t = Env
  { envAPI :: t,
    envLog :: !(String -> IO ())
  }

type HasEnv e a = (HasLog (e a), HasAPI e)

class HasLog e where
  getLog :: e -> (String -> IO ())

instance HasLog (Env a) where
  getLog = envLog

class HasAPI e where
  getAPI :: (API a) => e a -> a

instance HasAPI Env where
  getAPI = envAPI
