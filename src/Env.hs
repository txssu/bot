{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Env
  ( env,
    Env,
    HasEnv,
    HasLog (..),
    HasManager (..),
    HasAPI (..),
  )
where

import API (API, newRequestWithMethod)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import System.Environment (getEnv)

env :: t -> IO (Env t)
env api = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  return $ Env api putStrLn manager

data Env t = Env
  { envAPI :: t,
    envLog :: !(String -> IO ()),
    envHTTPManager :: HClient.Manager
  }

type HasEnv e a = (HasLog (e a), HasManager (e a), HasAPI e)

class HasLog e where
  getLog :: e -> (String -> IO ())

instance HasLog (String -> IO ()) where
  getLog = id

instance HasLog (Env a) where
  getLog = envLog

class HasManager e where
  getManager :: e -> HClient.Manager

instance HasManager HClient.Manager where
  getManager = id

instance HasManager (Env a) where
  getManager = envHTTPManager

class HasAPI e where
  getAPI :: (API a) => e a -> a

instance HasAPI Env where
  getAPI = envAPI
