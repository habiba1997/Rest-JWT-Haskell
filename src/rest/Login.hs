{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.Login (login) where

import Auth.Jwt (createJwtToken)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Helpers.HashPassword (checkPassword)
import Models.AuthLib (LoginRequest (..))
import Models.PayloadLib
import Models.UserLib
import Network.HTTP.Types.Status (status200, status404)
import Web.Scotty (ActionM, jsonData, status)
import qualified Web.Scotty as S

login :: Connection -> ActionM ()
login conn = do
  (LoginRequestConstructor _email _password) <- jsonData
  let queryExecuted = query conn "SELECT * FROM users WHERE email = ?" (Only _email) :: IO [User]
  userObj <- liftIO queryExecuted :: ActionM [User]
  case userObj of
    [] -> do
      -- if empty list [-] []
      status status404
      S.json $ object ["error" .= ("User email not found" :: String)]
    (userItem : _) -> do
      if checkPassword (userPassword userItem) _password
        then do
          token <- liftIO $ createJwtToken (Payload (show $ userId userItem) (userEmail userItem))
          status status200
          S.json $
            object
              [ "token" .= token,
                "user" .= userItem
              ]
        else do
          status status404
          S.json $ object ["error" .= ("Password is incorrect" :: String)]
