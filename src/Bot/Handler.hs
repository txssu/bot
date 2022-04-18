{-# LANGUAGE ConstraintKinds #-}

module Bot.Handler where

import qualified Bot.Base.API as API
import qualified Bot.Base.Log as Log
import qualified Bot.Base.Types as T
import qualified Bot.Database.Actions as DB
import Bot.Database.Schema (UserStatus (Global, Settings))
import Bot.Database.Types (Database)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Reader (MonadIO, MonadReader)
import Data.List (intercalate)
import Text.Read (readMaybe)

type IsHandler env api m =
  ( MonadReader (env api) m,
    API.API api,
    MonadIO m,
    Log.HasLog (env api),
    API.HasAPI env,
    MonadCatch m
  )

type Handler m = Database -> T.Update -> m Database

headDefault :: a -> [a] -> a
headDefault d [] = d
headDefault _ (x : _) = x

handler :: IsHandler env api m => Handler m
handler db u@T.NewMessage {T.uText = text, T.uSender = sender} = do
  let status = DB.getStatus db sender
  case status of
    Global -> do
      let command = headDefault "" $ words text
      let com = lookup command handlersList
      maybe (handleRepeat db u) (\x -> x db u) com
    Settings -> handleSettings db u
handler db _ = return db

handlersList :: IsHandler env api m => [(String, Handler m)]
handlersList =
  [ ("/repeat", handleToSettings),
    ("/help", handleHelp)
  ]

handleHelp :: IsHandler env api m => Handler m
handleHelp db u = do
  _ <- API.replyMessage u "The bot just repeats your messages.\n/help - see this message \n/repeat - set the number of repetitions"
  return db

handleSettings :: IsHandler env api m => Handler m
handleSettings db u@T.NewMessage {T.uText = text, T.uSender = sender} = do
  let n = readMaybe text :: Maybe Int
  case n of
    Just repeatCount -> do
      _ <- API.replyMessage u $ "Repeat count now is " ++ show repeatCount
      let updatedDB = DB.setStatus db sender Global
      return $ DB.setRepeatCount updatedDB sender repeatCount
    Nothing -> do
      _ <- API.replyMessage u "Must be a number"
      return db
handleSettings db _ = return db

handleToSettings :: IsHandler env api m => Handler m
handleToSettings db u@T.NewMessage {T.uSender = sender} = do
  let newDB = DB.setStatus db sender Settings
  _ <- API.replyMessage u "Please send repeat count (1-5)"
  return newDB
handleToSettings db _ = return db

handleRepeat :: IsHandler env api m => Handler m
handleRepeat db u@T.NewMessage {T.uText = text, T.uSender = sender} = do
  let count = DB.getRepeatCount db sender
  let msg = intercalate "\n" $ replicate count text
  _ <- API.replyMessage u msg
  return db
handleRepeat db _ = return db
