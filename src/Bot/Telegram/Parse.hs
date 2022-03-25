module Bot.Telegram.Parse where

import Bot.Base.Types (GenericUpdate (UndefinedUpdate))
import qualified Bot.Base.Types as GU
import qualified Bot.Telegram.Types as T
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)

parseUpdates :: ByteString -> Either String T.Updates
parseUpdates = A.eitherDecode

toGenericUpdate :: T.Update -> GU.GenericUpdate
toGenericUpdate (T.Update _ Nothing) = UndefinedUpdate
toGenericUpdate (T.Update _ (Just m)) = newMessageToGeneric m

newMessageToGeneric :: T.Message -> GU.GenericUpdate
newMessageToGeneric (T.Message user Nothing) = GU.NewMessage "Debug text" (T.userID user) -- TODO: delete "Debug text"
newMessageToGeneric (T.Message user (Just text)) = GU.NewMessage text (T.userID user)
