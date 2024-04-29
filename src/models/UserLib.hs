-- This directive enables the OverloadedStrings language extension, allowing string literals to be overloaded to work with any type that has an instance of the IsString type class
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE InstanceSigs #-}

module Models.UserLib (User (..), UserType (..)) where

import Data.Aeson
import Database.PostgreSQL.Simple.FromRow

class UserType user where
  getEmail :: user -> String
  getPassword :: user -> String

data User = UserConstructor
  { userId :: Int,
    userName :: String,
    userEmail :: String,
    userPassword :: String,
    userAge :: Int
  }
  deriving (Show)
  
instance FromJSON User where
  parseJSON (Object v) =
    UserConstructor
      <$> v .:? "id" .!= 0
      <*> v .: "name"
      <*> v .: "email"
      <*> v .: "password"
      <*> v .: "age"
  parseJSON _ = error "Failed to parse User"
  
instance UserType User where
  getEmail :: User -> String
  getEmail = userEmail
  getPassword = userPassword

instance FromRow User where
  fromRow = UserConstructor <$> field <*> field <*> field <*> field <*> field

instance ToJSON User where
  toJSON (UserConstructor jsonId jsonName jsonEmail _ jsonAge) =
    object
      [ "id" .= jsonId,
        "name" .= jsonName,
        "email" .= jsonEmail,
        "age" .= jsonAge
      ]
