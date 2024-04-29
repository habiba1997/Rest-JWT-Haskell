{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Main (main) where

import qualified Data.HashMap.Strict as HM
import Database.PostgreSQL.Simple
import Helpers.Config (getConfigMap)
import RoutesLib

valueOrDefault :: String -> Maybe String -> String
valueOrDefault _ (Just a) = a
valueOrDefault a Nothing = a

lookUpInConfigMap :: HM.HashMap String String -> String -> String -> String
lookUpInConfigMap configMap key dfValue = valueOrDefault dfValue hashValue
  where
    hashValue = HM.lookup key configMap

main :: IO ()
main = do
  configHashMap <- getConfigMap
  let dbHost = lookUpInConfigMap configHashMap "DB_HOST" "localhost"
  let dbPort = lookUpInConfigMap configHashMap "DB_PORT" "5436"
  let dbName = lookUpInConfigMap configHashMap "DB_NAME" "products"
  let dbUser = lookUpInConfigMap configHashMap "DB_USER" "postgres"
  let dbPass = lookUpInConfigMap configHashMap "DB_PASS" "123456"
  let localPG =
        ConnectInfo
          { connectHost = dbHost,
            connectPort = read dbPort,
            connectDatabase = dbName,
            connectUser = dbUser,
            connectPassword = dbPass
          }
  conn <- connect localPG
  routes conn
  -- Insert a new product into the database
  -- _ <- execute conn "INSERT INTO products (name, price, description) VALUES (?, ?, ?)" ("Laptop" :: String, 100 :: Int, "A powerful laptop" :: String)

  -- Retrieve all products from the database and print them
  --    products <- query_ conn "SELECT * FROM products" :: IO [Product]
  --     --    In this example, putStrLn "All Products:" is an IO action that is lifted into the monadic context m using liftIO.
  --    liftIO $ putStrLn "All Products:"
  --    liftIO $ mapM_ print products

  -- Convert a Product to JSON
  --    let productJson = encode (Product 1 "Phone" 500 "A smartphone")
  --    putStrLn "Product JSON:"
  --    BS.putStrLn productJson
  --
  --    -- Parse JSON into a Product
  --    let parsedProduct = decode "{\"id\":2,\"name\":\"Tablet\",\"price\":300,\"description\":\"A portable tablet\"}" :: Maybe Product
  --    putStrLn "Parsed Product:"
  --    print parsedProduct

  -- Close the database connection
  close conn
