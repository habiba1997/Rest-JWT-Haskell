module Helpers.HashPassword (hashPassword, checkPassword) where

import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy, validatePassword)
import Data.String (fromString)
import Data.ByteString.Char8

-- Hash a password
hashPassword :: String -> IO (Maybe String)
hashPassword password = do
    hashed <- hashPasswordUsingPolicy fastBcryptHashingPolicy (fromString password)
    case hashed of
        Just h  -> return (Just (unpack h))
        Nothing -> return Nothing

-- Compare a hashed password with a non-hashed password
checkPassword :: String -> String -> Bool
checkPassword hashedPassword passwordToCheck = validatePassword  (fromString hashedPassword) (fromString passwordToCheck)

--main :: IO ()
--main = do
--    -- Hash a password
--    hashed <- hashPassword "mySecurePassword"
--    case hashed of
--        Just h -> do
--            putStrLn $ "Hashed password: " ++ h
--
--            -- Check the password
--            isValid <- checkPassword h "mySecurePassword"
--            if isValid
--                then putStrLn "Password is valid!"
--                else putStrLn "Password is invalid!"
--        Nothing -> putStrLn "Failed to hash password."
