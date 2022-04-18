module Bot.Telegram.Parse where

import Bot.Base.Error (APIException (ParseError))
import qualified Bot.Base.Types as BaseT
import qualified Bot.Telegram.Types as T
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.Aeson as A
import qualified Data.ByteString as B'
import qualified Data.ByteString.Lazy as B

parseUpdates :: B.ByteString -> Either String T.Updates
parseUpdates = A.eitherDecode

toBaseUpdate :: T.Update -> BaseT.Update
toBaseUpdate (T.Update _ Nothing) = BaseT.UndefinedUpdate
toBaseUpdate (T.Update _ (Just m)) = newMessageToBaseUpdate m

newMessageToBaseUpdate :: T.Message -> BaseT.Update
newMessageToBaseUpdate (T.Message user Nothing) = BaseT.NewMessage "" (T.userID user) -- TODO: delete "Debug text"
newMessageToBaseUpdate (T.Message user (Just text)) = BaseT.NewMessage text (T.userID user)

parseError :: (MonadThrow m) => B'.ByteString -> m T.Error
parseError bs = do
  case A.eitherDecodeStrict bs of
    Left errorDescription ->
      throwM $ ParseError errorDescription
    Right e ->
      return e
