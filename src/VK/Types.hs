{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module VK.Types (Response (..), Group (..), LongPollServer (..), lpURL, Updates (..), Update (..)) where

import Control.Applicative (Alternative (empty))
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (Object)
import Text.Printf (printf)

data Response a = Response
  { rResponse :: a
  }
  deriving (Show)

instance (FromJSON v) => FromJSON (Response v) where
  parseJSON (Object resp) = Response <$> resp .: "response"
  parseJSON _ = empty

data Group = Group
  { gID :: Integer
  }
  deriving (Show)

instance FromJSON Group where
  parseJSON (Object group) = Group <$> group .: "id"
  parseJSON _ = empty

data LongPollServer = LongPollServer
  { lpKey :: String,
    lpServer :: String,
    lpTS :: String
  }
  deriving (Show)

instance FromJSON LongPollServer where
  parseJSON (Object lps) = LongPollServer <$> lps .: "key" <*> lps .: "server" <*> lps .: "ts"
  parseJSON _ = empty

lpURL :: LongPollServer -> String
lpURL LongPollServer {lpKey = key, lpServer = srv, lpTS = ts} = printf "%s?act=a_check&key=%s&ts=%s&wait=25" srv key ts

data Updates = Updates
  { usTS :: String,
    usUpdates :: [Update]
  }

instance FromJSON Updates where
  parseJSON (Object us) = Updates <$> us .: "ts" <*> us .: "updates"
  parseJSON _ = empty

data Update = Update
  { uType :: String
  }

instance FromJSON Update where
  parseJSON (Object u) = Update <$> u .: "type"
  parseJSON _ = empty