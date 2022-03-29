module Bot.Database.Schema where

defaultUserData :: User
defaultUserData = User Global 1

data User = User
  { userStatus :: !UserStatus,
    userRepeatCount :: !Int
  }
  deriving (Show)

data UserStatus = Global | Settings deriving (Show)
