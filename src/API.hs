module API
  ( API (..),
    APIException (..),
  )
where

import Control.Monad.Catch (Exception, MonadThrow)
import Data.Data (Typeable)
import Network.HTTP.Client (Manager, Request)

class API a where
  newRequestWithMethod :: (MonadThrow m) => a -> String -> [(String, String)] -> m Request

data APIException = APIException
  deriving (Show, Typeable)

instance Exception APIException
