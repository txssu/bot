{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startTG,
    startVK,
    sendAPIMethod,
  )
where

import qualified Control.Exception as Ex
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
import qualified Env as E
import qualified Network.HTTP.Base as HBase
import qualified Network.HTTP.Client as HClient
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as HStatus
import qualified Telegram.Types as TGTypes
import Text.Printf (printf)
import qualified VK.Types

startTG = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  env <- E.env (E.TelegramBot "token")
  runReaderT (handleLongpoll testHandler) env

startVK = do
  manager <- HClient.newManager TLS.tlsManagerSettings
  env <- E.env (E.VKBot "token" "5.131")
  runReaderT (handleLongpoll testHandler) env

testHandler (VKUpdates us) = do
  mapM_ (const $ logMessage Info "VK MESSAGE") us
testHandler (TelegramUpdates us) = do
  mapM_ (logMessage Info . TGTypes.mText . TGTypes.uMessage) us

data LogLevel = Error | Info | Debug

logMessage ::
  (MonadReader env m, E.HasLog env, MonadIO m) =>
  LogLevel ->
  String ->
  m ()
logMessage logLevel msg = do
  env <- ask
  liftIO $
    E.getLog env $ case logLevel of
      Error -> "Error: " ++ msg
      Info -> "Info:  " ++ msg
      Debug -> "Debug: " ++ msg

data APIException = APIException
  deriving (Show, Typeable)

instance Ex.Exception APIException

createRequest ::
  (MonadReader a m, MCatch.MonadThrow m, E.HasBot a) =>
  String ->
  [(String, String)] ->
  m HClient.Request
createRequest method params = do
  env <- ask
  let bot = E.getBot env
  let url = case bot of
        E.TelegramBot {E.botToken = token} -> printf "https://api.telegram.org/bot%s/%s?%s" token method (HBase.urlEncodeVars params)
        E.VKBot {E.botToken = token, E.botVersion = version} ->
          let addParams = params ++ [("access_token", token), ("v", version)]
           in printf "https://api.vk.com/method/%s?%s" method (HBase.urlEncodeVars addParams)
  HClient.parseRequest url

sendRequest ::
  (MonadReader env m, MonadIO m, E.HasLog env, E.HasManager env) =>
  HClient.Request ->
  m ByteString
sendRequest req = do
  env <- ask
  let manager = E.getManager env
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
      Ex.throw APIException

sendAPIMethod ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    E.HasBot env,
    E.HasLog env,
    E.HasManager env
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
    E.HasBot env,
    E.HasLog env,
    E.HasManager env
  ) =>
  m E.LongPoll
initLongpoll = do
  logMessage Info "Trying to init longpoll"
  env <- ask
  case E.getBot env of
    bot@E.TelegramBot {} -> do
      resp <- sendAPIMethod "getUpdates" []
      let updates = A.decode resp :: Maybe TGTypes.Updates
      case updates of
        Just udts ->
          if TGTypes.usOk udts
            then do
              let lst = TGTypes.usUpdates udts
              let newUpdateID = if null lst then 0 else TGTypes.uID . last $ lst
              return E.TelegramLongpoll {E.lpLastUpdateID = newUpdateID}
            else undefined
        Nothing -> undefined
    bot@E.VKBot {} -> do
      -- Get VK group id
      resp <- sendAPIMethod "groups.getById" []
      let response = A.decode resp :: Maybe (VK.Types.Response [VK.Types.Group])
      when (isNothing response) (Ex.throw APIException)
      let Just groups = response
      let myID = VK.Types.gID . head . VK.Types.rResponse $ groups
      -- Get Longpoll data
      resp <- sendAPIMethod "groups.getLongPollServer" [("group_id", show myID)]
      let response = A.decode resp :: Maybe (VK.Types.Response VK.Types.LongPollServer)
      when (isNothing response) (Ex.throw APIException)
      let Just rServer = response
      let server = VK.Types.rResponse rServer

      return $ E.VKLongpoll server

data Updates = TelegramUpdates [TGTypes.Update] | VKUpdates [VK.Types.Update]

awaitLongpoll ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    E.HasBot env,
    E.HasLog env,
    E.HasManager env
  ) =>
  E.LongPoll ->
  m (Updates, E.LongPoll)
awaitLongpoll longpoll@E.TelegramLongpoll {E.lpLastUpdateID = updateID} = do
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
          return (TelegramUpdates lst, longpoll {E.lpLastUpdateID = newUpdateID})
        else undefined
    Nothing -> undefined
awaitLongpoll E.VKLongpoll {E.lpServer = longpoll} = do
  logMessage Info "Await updates"
  req <- HClient.parseRequest (VK.Types.lpURL longpoll)
  resp <- sendRequest req
  logMessage Debug $ show resp
  let mUpdates = A.decode resp :: Maybe VK.Types.Updates -- TODO: Check errors
  when (isNothing mUpdates) (Ex.throw APIException)
  let Just updates = mUpdates
  let newTS = VK.Types.usTS updates
  let lst = VK.Types.usUpdates updates
  let newLP = longpoll {VK.Types.lpTS = newTS}

  return (VKUpdates lst, E.VKLongpoll {E.lpServer = newLP})

handleLongpoll ::
  ( MonadReader env m,
    MonadIO m,
    MCatch.MonadThrow m,
    E.HasBot env,
    E.HasLog env,
    E.HasManager env
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
    E.HasBot env,
    E.HasLog env,
    E.HasManager env
  ) =>
  (Updates -> m ()) ->
  E.LongPoll ->
  m ()
handleLongpoll' handler longpoll = do
  (updates, lp) <- awaitLongpoll longpoll
  handler updates
  handleLongpoll' handler lp
