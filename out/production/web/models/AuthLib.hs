-- This directive enables the OverloadedStrings language extension, allowing string literals to be overloaded to work with any type that has an instance of the IsString type class
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Models.AuthLib (LoginRequest (..), Token (..)) where

import Data.Aeson
import Models.UserLib (UserType (..))

data LoginRequest = LoginRequestConstructor
  { requestEmail :: String,
    requestPassword :: String
  }
  deriving (Show)

instance UserType LoginRequest where
  getEmail = requestEmail
  getPassword = requestPassword

instance FromJSON LoginRequest where
  parseJSON (Object o) = LoginRequestConstructor <$> o .: "email" <*> o .: "password"
  parseJSON _ = fail "Expected an object for login request"

newtype Token = TokenConstructor {token :: String}
  deriving (Show)

instance ToJSON Token where
  toJSON (TokenConstructor jsonToken) =
    object ["token" .= jsonToken]

instance FromJSON Token where
  parseJSON (Object o) = TokenConstructor <$> o .: "token"
  parseJSON _ = fail "Expected a token object"

-- todo understand parse after typeclass
--instance FromJSON LoginRequest where
--  parseJSON = withObject "LoginRequest" $ \v -> LoginRequest
--    <$> v .: "email"
--    <*> v .: "password"
