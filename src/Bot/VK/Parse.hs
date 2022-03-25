module Bot.VK.Parse where

import qualified Bot.Base.Types as BaseT
import qualified Bot.VK.Types as T
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)

parseResponse :: A.FromJSON a => ByteString -> Maybe a
parseResponse bs = do
  result <- A.decode bs
  return $ T.rResponse result

parseUpdates :: ByteString -> Maybe T.Updates
parseUpdates = A.decode

toBaseUpdate :: T.Update -> A.Result BaseT.Update
toBaseUpdate (T.Update tp obj) = do
  case tp of
    "message_new" -> do
      r <- A.fromJSON obj
      return $ newMessageToBase $ T.nmoMessage r
    _ -> return BaseT.UndefinedUpdate

newMessageToBase :: T.NewMessage -> BaseT.Update
newMessageToBase (T.NewMessage pID text) = BaseT.NewMessage text pID
