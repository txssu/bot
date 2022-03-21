module VK.Parse where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import VK.Types (Group, LongPollServer, Response (rResponse), Updates)

parseResponse :: A.FromJSON a => ByteString -> Maybe a
parseResponse bs = do
  result <- A.decode bs
  return $ rResponse result

parseUpdates :: ByteString -> Maybe Updates
parseUpdates = A.decode
