{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types (Updates (..), Update (..), Message (..), User (..), Error(..)) where

import Control.Applicative (Alternative (empty))
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:), (.:?))
import Text.Printf (printf)

data Updates = Updates
  { usOk :: Bool,
    usUpdates :: [Update]
  }
  deriving (Show)

instance FromJSON Updates where
  parseJSON (Object updates) = Updates <$> updates .: "ok" <*> updates .: "result"
  parseJSON _ = empty

data Update = Update
  { uID :: Integer,
    uMessage :: Maybe Message
  }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object update) = Update <$> update .: "update_id" <*> update .:? "message"
  parseJSON _ = empty

data Message = Message
  { mFrom :: User,
    mText :: Maybe String
  }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object message) = Message <$> message .: "from" <*> message .:? "text"
  parseJSON _ = empty

data User = User
  { userID :: Integer
  }
  deriving (Show)

instance FromJSON User where
  parseJSON (Object user) = User <$> user .: "id"
  parseJSON _ = empty

data Error = Error
  { errorCode :: Integer,
    errorDescription :: String
  }

instance Show Error where
  show (Error code description) = printf "Response code %d: %s" code description

instance FromJSON Error where
  parseJSON (Object e) = Error <$> e .: "error_code" <*> e .: "description"
  parseJSON _ = empty
