{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module RoutesLib (routes) where

import Database.PostgreSQL.Simple
import Rest.AuthAPIs (authenticateToken, fetchToken)
import Rest.Check (check)
import Rest.Login (login)
import Rest.ProductAPI
  ( changePrice,
    createProduct,
    deleteProduct,
    getProduct,
    getProducts,
    updateProduct,
  )
import Rest.Signup (signUp)
import Rest.Upload (upload)
import Web.Scotty (delete, get, post, put, scotty)

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/api/product/" $ getProducts conn
  get "/api/product/:id" $ getProduct conn
  post "/api/product/" $ createProduct conn
  put "/api/product/:id" $ updateProduct conn
  delete "/api/product/:id" $ deleteProduct conn
  put "/api/product/change-price/:id" $ changePrice conn
  post "/api/upload/" upload
  get "/api/check/:value" check
  get "/api/token/" fetchToken
  post "/api/authenticate-token/" authenticateToken
  post "/api/signup/" $ signUp conn
  post "/api/login/" $ login conn


