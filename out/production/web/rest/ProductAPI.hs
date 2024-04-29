{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.ProductAPI (changePrice, getProduct, getProducts, createProduct, updateProduct, deleteProduct) where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.PostgreSQL.Simple (Connection, Only (Only), execute, query, query_)
import Network.HTTP.Types.Status (status200, status201, status204, status400)
import Models.ProductLib
import Web.Scotty (ActionM, captureParam, jsonData, status)
import qualified Web.Scotty as S
import Auth.Token(validateHeader)

-- import Debug.Trace
changePrice :: Connection -> ActionM ()
changePrice conn = do
  _idProduct <- captureParam "id" :: ActionM Int
  (ProductPrice _pPrice) <- jsonData
  productExist <- liftIO $ haveProduct conn _idProduct
  if productExist && (_pPrice /= 0)
    then do
      liftIO $ putStrLn ("before" ++ show _pPrice)
      let update = query conn "UPDATE products SET price = ? WHERE id = ? RETURNING id, name, price, description" (_pPrice, _idProduct) :: IO [Product]
      result <- liftIO $ try update
      case result of
        Left err -> do
          liftIO $ putStrLn $ "Error: " ++ show (err :: SomeException)
        Right productList -> do
          -- liftIO $ putStrLn $ "after" ++ show (length productList)
          case productList of
            [] -> do
              -- if empty list [-] []
              status status400
              S.json $ object ["error" .= ("Product not found" :: String)]
            _ -> do
              -- else
              status status200
              S.json productList
    else do
      --    liftAndCatchIO :: IO a -> ActionM a
      --    print x         =  putStrLn (show x)
      S.liftAndCatchIO (print _pPrice)
      S.json $ object ["error" .= ("Product was not updated" :: String)]

-- The function getProducts is a function that response a list of products.
-- the function truly return a ActionM (), this is similar to IO monad and it is way to produce side effects.
-- This function receives a connection to the database and returns an empty ActionM,
-- this is very similar to a IO monad, but in simple words, it provides a response to the client.
getProducts :: Connection -> ActionM ()
getProducts conn = do
  validation <- validateHeader
  case validation of
    Left errResult -> do
      status status400
      S.json $ object ["error" .= (errResult :: String)]
    Right _ -> do
      products <- (liftIO $ query_ conn "SELECT * FROM products") :: ActionM [Product]
      --  _ <- debug products
      S.json $ object ["products" .= products]

-- The query_ function is very similar to query the difference is that this one does n-o-t  receive values to replace in the query.
-- Also, the result of this query is interpreting as an IO [Product] type and liftIO is a function that take a IO a and transform to m a value,
-- where m is another Monad. That is, it transforms IO [Product] result of query_ into ActionM [Product], that is why the :: operator is using here,
-- to give information to Haskell about final type.

getProduct :: Connection -> ActionM ()
getProduct conn = do
  _idProduct <- captureParam "id" :: ActionM Int
  let result = query conn "SELECT * FROM products WHERE id = ?" (Only _idProduct)
  productItem <- liftIO result :: ActionM [Product]
  case productItem of
    [] -> do
      -- if empty list [-] []
      status status400
      S.json $ object ["error" .= ("Product NOT found" :: String)]
    _ -> do
      -- else
      status status200
      S.json (head productItem)

-- | - => l-e-t
createProduct :: Connection -> ActionM ()
createProduct conn = do
  --  we use the jsonData function to get the JSON data from the request, for this work we created the instances of the FromJSON type-class.
  (Product _ _name _price _description) <- jsonData
  let result =
        execute
          conn
          "INSERT INTO products (name, price, description) VALUES (?, ?, ?)"
          (_name, _price, _description)
  n <- liftIO result
  if n > 0
    then do
      status status201
      S.json $ object ["message" .= ("Product created" :: String)]
    else do
      status status400
      S.json $ object ["error" .= ("Product NOT created" :: String)]

updateProduct :: Connection -> ActionM ()
updateProduct conn = do
  _idProduct <- captureParam "id" :: ActionM Int

  (Product _ _name _price _description) <- jsonData :: ActionM Product

  productExist <- liftIO $ haveProduct conn _idProduct
  -- if n-o-t
  if not productExist
    then createProduct conn
    else do
      let result =
            execute
              conn
              "UPDATE products SET name = ?, price = ?, description = ? WHERE id = ?"
              (_name, _price, _description, _idProduct)
      n <- liftIO result
      status $ if n > 0 then status200 else status204
      S.json $ object ["message" .= ("Product updated" :: String)]

deleteProduct :: Connection -> ActionM ()
deleteProduct conn = do
  _idProduct <- captureParam "id" :: ActionM Int

  productExist <- liftIO $ haveProduct conn _idProduct

  if not productExist
    then do
      status status400
      S.json $ object ["error" .= ("Product NOT found" :: String)]
    else do
      status status200
      _ <- liftIO $ execute conn "DELETE FROM products WHERE id = ?" (Only _idProduct)
      S.json $ object ["message" .= ("Product deleted" :: String)]

haveProduct :: Connection -> Int -> IO Bool
haveProduct conn _idProduct = do
  --  ONLY create a tuple, Only is used to construct a parameter tuple for the SQL query.
  [Only n] <- query conn "SELECT COUNT(*) FROM products WHERE id = ?" (Only _idProduct) :: IO [Only Int]
  return $ n > 0
