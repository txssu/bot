module GenericUpdate where

data APIType = Telegram | VK deriving (Show)

data GenericUpdate
  = NewMessage {uText :: String, uSender :: Integer}
  | UndefinedUpdate
  deriving (Show)
