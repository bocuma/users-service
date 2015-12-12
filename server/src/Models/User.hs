module Models.User (User(..)) where

data User = User {
  email :: String,
  password :: String
}

instance ToJSON User
