module VK.Parse where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified GenericUpdate as GU
import qualified VK.Types as T

parseResponse :: A.FromJSON a => ByteString -> Maybe a
parseResponse bs = do
  result <- A.decode bs
  return $ T.rResponse result

parseUpdates :: ByteString -> Maybe T.Updates
parseUpdates = A.decode

toGenericUpdate :: T.Update -> A.Result GU.GenericUpdate
toGenericUpdate (T.Update tp obj) = do
  case tp of
    "message_new" -> do
      r <- A.fromJSON obj
      return $ newMessageToGeneric $ T.nmoMessage r
    _ -> return GU.UndefinedUpdate

newMessageToGeneric :: T.NewMessage -> GU.GenericUpdate
newMessageToGeneric (T.NewMessage pID text) = GU.NewMessage text pID
