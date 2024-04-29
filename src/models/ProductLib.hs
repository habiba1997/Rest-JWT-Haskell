--This directive is used to disable the -Wname-shadowing warning, which occurs when a variable in an inner scope shadows a variable with the same name in an outer scope.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- This directive enables the OverloadedStrings language extension, allowing string literals to be overloaded to work with any type that has an instance of the IsString type class
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.ProductLib(Product(..), ProductPrice(..)) where

import Data.Aeson
    ( (.!=),
      (.:),
      (.:?),
      object,
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromRow ( field, FromRow(..) )

data Product = Product
   { idProduct :: Int,
     name :: String,
     price :: Int,
     description :: String
   } deriving (Show, Generic, ToRow)
   


newtype ProductPrice = ProductPrice Int
     deriving (Show)
     
instance FromJSON ProductPrice where
   parseJSON (Object o) = ProductPrice <$> o .:? "price" .!= 0 
   parseJSON _ = fail "Expected an int object for Price"

instance FromRow Product where
   fromRow = Product <$> field <*> field <*> field <*> field


instance ToJSON Product where
     toJSON (Product idProduct name price description) =
         object
             [ "id" .= idProduct
             , "name" .= name
             , "price" .= price
             , "description" .= description
             ]
             
instance FromJSON Product where
   -- o .:? "id" .!= 0: This parses the field "id" from the JSON object o.
   -- The .:? operator attempts to parse the field as an optional value. If the field exists, it returns its value; otherwise,
   -- it defaults to 0 using the .!= operator.
   parseJSON (Object o) = --  It pattern matches on the input JSON data. If the JSON data is an Object, it proceeds to parse it. If the JSON data is anything else, it fails with an error message.
     Product <$> o .:? "id" .!= 0 -- .:? operator maybe get field,
       <*> o .: "name" -- .:  get field
       <*> o .: "price"
       <*> o .: "description"
 --- If the input JSON data is not an object, this case is triggered, and it fails with an error message stating that an object was expected for parsing into a Product
   parseJSON _ = fail "Expected an object for Product"


   -- The <$> and <*> operators are used to apply the Product constructor to the parsed values, creating a Product value from the parsed JSON data.

 -- This mean that now we have defined the function parseJSON, this receives a JSON and when this JSON is an object,
 --  it will look for the fields id, name, price and description. For that, we use the .: and .:? operators,
 --  which means get field and maybe get field, correspondingly. And the operator .!= is used to set the default
 --  value for the field when it is not found. For example, when you receive a JSON object
 --  (called simplyo) {"name": "foo"} and you get the field "id" using o .:? "id" in
 --   this case you get a Nothing, but if next to it, you use .!= 0 then the result will be 0 for the id.