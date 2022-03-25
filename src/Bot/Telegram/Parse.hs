module Bot.Telegram.Parse where

import qualified Bot.Base.Types as BaseT
import qualified Bot.Telegram.Types as T
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)

parseUpdates :: ByteString -> Either String T.Updates
parseUpdates = A.eitherDecode

toBaseUpdate :: T.Update -> BaseT.Update
toBaseUpdate (T.Update _ Nothing) = BaseT.UndefinedUpdate
toBaseUpdate (T.Update _ (Just m)) = newMessageToBaseUpdate m

newMessageToBaseUpdate :: T.Message -> BaseT.Update
newMessageToBaseUpdate (T.Message user Nothing) = BaseT.NewMessage "Debug text" (T.userID user) -- TODO: delete "Debug text"
newMessageToBaseUpdate (T.Message user (Just text)) = BaseT.NewMessage text (T.userID user)
