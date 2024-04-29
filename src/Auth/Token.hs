module Auth.Token (validateHeader) where

import Auth.Jwt
import Control.Monad.IO.Class (liftIO)
import Models.AuthLib
import Models.PayloadLib (Payload (..))
import Web.Scotty (ActionM, headers)

validateHeader :: ActionM (Either String Payload)
validateHeader = do
  headersPair <- headers
  case extractBearerToken headersPair of
    Just value -> do
      liftIO $ authenticateJwt (TokenConstructor value)
    Nothing -> pure $ Left "No token found in header!"
