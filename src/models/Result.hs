module Models.Result(Result(..)) where
  
data Result = Error String | Success String | Warning String
  deriving (Show)
  
