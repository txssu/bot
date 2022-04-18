module Bot.Base.Error where

import Control.Exception (Exception)
import Data.Data (Typeable)

data APIException
  = ClientError String
  | ParseError String
  deriving (Show, Typeable)

instance Exception APIException
