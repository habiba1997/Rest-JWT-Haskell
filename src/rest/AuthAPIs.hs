{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.AuthAPIs (fetchToken, authenticateToken) where

import Auth.Jwt
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Models.AuthLib
import Models.PayloadLib (Payload (..))
import Web.Scotty (ActionM, jsonData)
import qualified Web.Scotty as S

fetchToken :: ActionM ()
fetchToken = do
  let payload = Payload "1" "habiba@gmail.com"
  (TokenConstructor jwttoken) <- liftIO $ createJwtToken payload
  S.json $ object ["token" .= jwttoken]

authenticateToken :: ActionM ()
authenticateToken = do
  (TokenConstructor request) <- jsonData :: ActionM Token
  -- todo understand why use $ and not use composition or normal appply
  reaultOrPayload <- liftIO $ authenticateJwt (TokenConstructor request)
  case reaultOrPayload of
    Right payload -> S.json $ object ["payload" .= payload]
    Left msg ->
      S.json $
        object
          [ "Error" .= ("Failed to authenticate token" :: String),
            "Description" .= msg,
            "token" .= request
          ]
