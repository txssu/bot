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
import Control.Monad (when)
import qualified Control.Monad.Catch as MCatch
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT (runReaderT), guard)
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Network.HTTP.Base as HBase
import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as HStatus
import qualified TelegramTypes as TGTypes
import Text.Printf (printf)
import qualified VKTypes

startTG = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  let env = Env (TelegramBot "token") putStrLn manager
  runReaderT (handleLongpoll testHandler) env

startVK = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  let env = Env (VKBot "token" "5.131") putStrLn manager
  runReaderT (handleLongpoll testHandler) env

testHandler (VKUpdates us) = do
  mapM_ (const $ logMessage Info "VK MESSAGE") us
testHandler (TelegramUpdates us) = do
  mapM_ (logMessage Info . TGTypes.mText . TGTypes.uMessage) us

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

data LongPoll
  = VKLongpoll
      { lpServer :: VKTypes.LongPollServer
      }
  | TelegramLongpoll
      { lpLastUpdateID :: Integer
      }
  deriving (Show)

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
  (MonadReader env m, MonadIO m, HasLog env, HasManager env) =>
  HClient.Request ->
  m ByteString
sendRequest req = do
  env <- ask
  let manager = getManager env
  let url = show $ HClient.host req <> HClient.path req <> HClient.queryString req
  logMessage Debug $ "Send request to " ++ url
  res <- liftIO (HClient.httpLbs req manager)
  let status = HClient.responseStatus res
  case HStatus.statusCode status of
    200 -> do
      logMessage Debug "Status 200"
      return $ HClient.responseBody res
    code -> do
      logMessage Error $ printf "Status %d %s" code (show $ HStatus.statusMessage status)
      E.throw APIException

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
  logMessage Info $ printf "Send method %s with params %s" method (show params)
  req <- createRequest method params
  res <- sendRequest req
  logMessage Info $ printf "Got %s" (show res)
  return res

initLongpoll ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    HasBot env,
    HasLog env,
    HasManager env
  ) =>
  m LongPoll
initLongpoll = do
  logMessage Info "Trying to init longpoll"
  env <- ask
  case getBot env of
    bot@TelegramBot {} -> do
      resp <- sendAPIMethod "getUpdates" []
      let updates = A.decode resp :: Maybe TGTypes.Updates
      case updates of
        Just udts ->
          if TGTypes.usOk udts
            then do
              let lst = TGTypes.usUpdates udts
              let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst
              return TelegramLongpoll {lpLastUpdateID = newUpdateID}
            else undefined
        Nothing -> undefined
    bot@VKBot {} -> do
      -- Get VK group id
      resp <- sendAPIMethod "groups.getById" []
      let response = A.decode resp :: Maybe (VKTypes.Response [VKTypes.Group])
      when (isNothing response) (E.throw APIException)
      let Just groups = response
      let myID = VKTypes.gID . head . VKTypes.rResponse $ groups
      -- Get Longpoll data
      resp <- sendAPIMethod "groups.getLongPollServer" [("group_id", show myID)]
      let response = A.decode resp :: Maybe (VKTypes.Response VKTypes.LongPollServer)
      when (isNothing response) (E.throw APIException)
      let Just rServer = response
      let server = VKTypes.rResponse rServer

      return $ VKLongpoll server

data Updates = TelegramUpdates [TGTypes.Update] | VKUpdates [VKTypes.Update]

awaitLongpoll ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    HasBot env,
    HasLog env,
    HasManager env
  ) =>
  LongPoll ->
  m (Updates, LongPoll)
awaitLongpoll longpoll@TelegramLongpoll {lpLastUpdateID = updateID} = do
  logMessage Info "Await updates"
  resp <- sendAPIMethod "getUpdates" [("timeout", "25"), ("offset", show $ updateID + 1)]
  logMessage Debug $ show resp
  let updates = A.decode resp :: Maybe TGTypes.Updates
  case updates of
    Just udts ->
      if TGTypes.usOk udts
        then do
          let lst = TGTypes.usUpdates udts
          let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst
          return (TelegramUpdates lst, longpoll {lpLastUpdateID = newUpdateID})
        else undefined
    Nothing -> undefined
awaitLongpoll VKLongpoll {lpServer = longpoll} = do
  logMessage Info "Await updates"
  req <- HClient.parseRequest (VKTypes.lpURL longpoll)
  resp <- sendRequest req
  logMessage Debug $ show resp
  let mUpdates = A.decode resp :: Maybe VKTypes.Updates -- TODO: Check errors
  when (isNothing mUpdates) (E.throw APIException)
  let Just updates = mUpdates
  let newTS = VKTypes.usTS updates
  let lst = VKTypes.usUpdates updates
  let newLP = longpoll {VKTypes.lpTS = newTS}

  return (VKUpdates lst, VKLongpoll {lpServer = newLP})

handleLongpoll ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    HasBot env,
    HasLog env,
    HasManager env
  ) =>
  (Updates -> m ()) ->
  m ()
handleLongpoll handler = do
  lp <- initLongpoll
  handleLongpoll' handler lp

handleLongpoll' ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    HasBot env,
    HasLog env,
    HasManager env
  ) =>
  (Updates -> m ()) ->
  LongPoll ->
  m ()
handleLongpoll' handler longpoll = do
  (updates, lp) <- awaitLongpoll longpoll
  handler updates
  handleLongpoll' handler lp
