{-# LANGUAGE BangPatterns #-}

module Bot.Database.Actions where

import Bot.Database.Schema (User, UserStatus, defaultUserData)
import qualified Bot.Database.Schema as Schema
import Bot.Database.Types (Database)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)

getUser :: Database -> Integer -> Maybe User
getUser db userID = Map.lookup userID db

getUser' :: Database -> Integer -> User
getUser' db userID = fromMaybe defaultUserData $ getUser db userID

getRepeatCount :: Database -> Integer -> Int
getRepeatCount db userID = Schema.userRepeatCount $ getUser' db userID

setRepeatCount :: Database -> Integer -> Int -> Database
setRepeatCount !db !userID !count =
  let user = fromMaybe defaultUserData $ getUser db userID
      newUser = user {Schema.userRepeatCount = count}
   in Map.insert userID newUser db

getStatus :: Database -> Integer -> UserStatus
getStatus db userID = Schema.userStatus $ getUser' db userID

setStatus :: Database -> Integer -> UserStatus -> Database
setStatus db userID status =
  let user = fromMaybe defaultUserData $ getUser db userID
      newUser = user {Schema.userStatus = status}
   in Map.insert userID newUser db
