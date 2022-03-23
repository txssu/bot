module Telegram.Parse where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import GenericUpdate (GenericUpdate (UndefinedUpdate))
import qualified GenericUpdate as GU
import qualified Telegram.Types as T

parseUpdates :: ByteString -> Either String T.Updates
parseUpdates = A.eitherDecode

toGenericUpdate :: T.Update -> GU.GenericUpdate
toGenericUpdate (T.Update _ Nothing) = UndefinedUpdate
toGenericUpdate (T.Update _ (Just m)) = newMessageToGeneric m

newMessageToGeneric :: T.Message -> GU.GenericUpdate
newMessageToGeneric (T.Message user Nothing) = GU.NewMessage "Debug text" (T.userID user) -- TODO: delete "Debug text"
newMessageToGeneric (T.Message user (Just text)) = GU.NewMessage text (T.userID user)
