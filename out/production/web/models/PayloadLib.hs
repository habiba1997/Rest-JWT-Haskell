{-# LANGUAGE OverloadedStrings #-}

module Models.PayloadLib
  ( Payload (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))

data Payload = Payload String String
  deriving (Show)

instance ToJSON Payload where
  toJSON (Payload userId userEmail) =
    object
      [ "Id" .= userId,
        "email" .= userEmail
      ]
