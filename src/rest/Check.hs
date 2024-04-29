{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.Check (check) where

import Web.Scotty (captureParam, ActionM)
import qualified Web.Scotty as S
import Data.Aeson



check:: ActionM ()
check = do
    number <- captureParam "value" :: ActionM Int 
    case number of 
        0 -> S.json $ object ["error" .= ("Zero values are not permitted":: String)]
        _ -> S.json $ object ["value" .= ((number* 10) ::Int)]

