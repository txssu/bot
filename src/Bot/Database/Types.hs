module Bot.Database.Types where

import qualified Bot.Database.Schema as Schema
import qualified Data.HashMap.Strict as Map

type Database = Map.HashMap Integer Schema.User

emptyDB :: Database
emptyDB = Map.empty
