{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.Signup (signUp) where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Database.PostgreSQL.Simple (Connection, query)
import Helpers.HashPassword (hashPassword)
import Models.UserLib
import Network.HTTP.Types.Status (status201, status400, status404)
import Web.Scotty (ActionM, jsonData, status)
import qualified Web.Scotty as S

signUp :: Connection -> ActionM ()
signUp conn = do
  (UserConstructor _ _name _email _password _age) <- jsonData
  -- hash password
  maybePass <- liftIO $ hashPassword _password
  case maybePass of
    Nothing -> do
      status status404
      S.json $ object ["error" .= ("Failed to hash password" :: String)]
    Just pass -> do
      -- insert user into database
      let queryExecuted = query conn "INSERT INTO users (name, email, password, age) VALUES (?, ?, ?, ?) RETURNING id, name, email, password, age" (_name, _email, pass, _age::Int) :: IO [User]
      -- save user to database
      result <- liftIO $ try queryExecuted :: ActionM (Either SomeException [User])
      -- return result
      case result of
        Left err -> do
          status status404
          S.json $ object ["error" .= (displayException err :: String)]
        Right users -> do
          case users of
            [] -> do
              -- if empty list [-] []
              status status400
              S.json $ object ["error" .= ("User not found" :: String)]
            (userObj : _) -> do
              -- else
              status status201
              S.json userObj
