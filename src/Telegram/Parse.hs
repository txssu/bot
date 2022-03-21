module Telegram.Parse where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Telegram.Types as Types

parseUpdates :: ByteString -> Maybe Types.Updates
parseUpdates = A.decode
