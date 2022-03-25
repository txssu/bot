module Bot.Handler where

import qualified Bot.Base.API as API
import qualified Bot.Base.Types as T

handler u@T.NewMessage {T.uText = text} = do
  API.replyMessage u text
  return ()
handler T.UndefinedUpdate = return ()
