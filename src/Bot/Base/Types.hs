module Bot.Base.Types where

data Update
  = NewMessage {uText :: String, uSender :: Integer}
  | UndefinedUpdate
  deriving (Show)
