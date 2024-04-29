module Helpers.Helper (
    printType,
    debug,
    toText,
    toString,
    charToInt,
    mapConvert,
    getStringFromValue
) where 

import Data.Typeable (typeOf, Typeable)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.Map as Map
import Data.Aeson.Types (Value (String))
import Prelude as P 
printType :: (Typeable a, MonadIO m) => a -> m ()
printType a = liftIO $ print (typeOf a)

-- todo debug function
debug :: a -> a
debug x = x
  
-- Convert String to T.Text
toText :: String -> T.Text
toText = T.pack

toString :: T.Text -> String
toString = T.unpack


-- Convert a Char to Word16
charToInt :: [Char] -> Int
charToInt = read

mapConvert:: Map.Map String String -> Map.Map T.Text Value
mapConvert p = Map.fromList $ P.map (\(k,v) -> (toText k, String $ toText v)) $ Map.toList p



getStringFromValue :: Value -> String
getStringFromValue (String s) = toString s
getStringFromValue _          = ""