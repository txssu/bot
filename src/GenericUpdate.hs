module GenericUpdate where

data GenericUpdate
  = NewMessage {uText :: String, uSender :: Integer}
  | UndefinedUpdate
  deriving (Show)
