module Telegram.Parse where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Telegram.Types as T
import qualified GenericUpdate as GU

parseUpdates :: ByteString -> Maybe T.Updates
parseUpdates = A.decode

toGenericUpdate :: T.Update -> GU.GenericUpdate
toGenericUpdate (T.Update _ m) = do
  newMessageToGeneric m

newMessageToGeneric :: T.Message -> GU.GenericUpdate
newMessageToGeneric (T.Message user text) = GU.NewMessage text (T.userID user)
