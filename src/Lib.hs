{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startTG,
    startVK,
    Env (..),
    Bot (..),
    sendAPIMethod,
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.Catch as MCatch
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT (runReaderT))
import Data.ByteString.Lazy (ByteString)
import Data.Typeable (Typeable)
import qualified Network.HTTP.Base as HBase
import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as HStatus
import Text.Printf (printf)

startTG = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  let env = Env (TelegramBot "token") putStrLn manager
  res <- runReaderT (sendAPIMethod "getMe" []) env
  print res

startVK = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  let env = Env (VKBot "token" "5.131") putStrLn manager
  res <- runReaderT (sendAPIMethod "groups.getById" []) env
  print res

data Env = Env
  { envBot :: !Bot,
    envLog :: !(String -> IO ()),
    envHTTPManager :: HClient.Manager
  }

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog (String -> IO ()) where
  getLog = id

instance HasLog Env where
  getLog = envLog

data LogLevel = Error | Info | Debug

logMessage ::
  (MonadReader env m, HasLog env, MonadIO m) =>
  LogLevel ->
  String ->
  m ()
logMessage logLevel msg = do
  env <- ask
  liftIO $
    getLog env $ case logLevel of
      Error -> "Error: " ++ msg
      Info -> "Info:  " ++ msg
      Debug -> "Debug: " ++ msg

class HasManager a where
  getManager :: a -> HClient.Manager

instance HasManager HClient.Manager where
  getManager = id

instance HasManager Env where
  getManager = envHTTPManager

class HasBot a where
  getBot :: a -> Bot

instance HasBot Bot where
  getBot = id

instance HasBot Env where
  getBot = envBot

data Bot
  = VKBot
      { botToken :: String,
        botVersion :: String
      }
  | TelegramBot
      { botToken :: String
      }

data APIException = APIException
  deriving (Show, Typeable)

instance E.Exception APIException

createRequest ::
  (MonadReader a m, MCatch.MonadThrow m, HasBot a) =>
  String ->
  [(String, String)] ->
  m HClient.Request
createRequest method params = do
  env <- ask
  let bot = getBot env
  let url = case bot of
        TelegramBot {botToken = token} -> printf "https://api.telegram.org/bot%s/%s?%s" token method (HBase.urlEncodeVars params)
        VKBot {botToken = token, botVersion = version} ->
          let addParams = params ++ [("access_token", token), ("v", version)]
           in printf "https://api.vk.com/method/%s?%s" method (HBase.urlEncodeVars addParams)
  HClient.parseRequest url

sendRequest ::
  (MonadReader a m, MonadIO m, HasManager a) =>
  HClient.Request ->
  m (HClient.Response ByteString)
sendRequest req = do
  env <- ask
  let manager = getManager env
  liftIO (HClient.httpLbs req manager)

sendAPIMethod ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    HasBot env,
    HasLog env,
    HasManager env
  ) =>
  String ->
  [(String, String)] ->
  m ByteString
sendAPIMethod method params = do
  req <- createRequest method params
  let url = show $ HClient.host req <> HClient.path req <> HClient.queryString req
  logMessage Debug $ "send request to " ++ url
  res <- sendRequest req
  let status = HClient.responseStatus res
  case HStatus.statusCode status of
    200 -> do
      logMessage Debug "Status 200"
      return $ HClient.responseBody res
    code -> do
      logMessage Error $ printf "Status %d %s" code (show $ HStatus.statusMessage status)
      E.throw APIException