{-# LANGUAGE OverloadedStrings #-}

module Helpers.Config (getConfig, getConfigValueOrDefault, getConfigMap) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Helpers.Helper (toText, toString)
import System.Directory (getCurrentDirectory)
import System.FilePath (dropFileName, hasDrive, normalise, splitFileName, (</>))

convertHashMap :: HM.HashMap T.Text T.Text -> HM.HashMap String String
convertHashMap = 
    HM.foldrWithKey (\key value acc -> HM.insert (toString key) (toString value) acc) HM.empty


computeCurrentDirectory :: FilePath -> FilePath
computeCurrentDirectory currentDir =
  if hasDrive currentDir && ((snd . splitFileName $ currentDir) == "src")
    then dropFileName currentDir
    else currentDir

-- | Load and parse .env file
loadEnv :: IO (HM.HashMap T.Text T.Text)
loadEnv = do
  -- Read the contents of the .env file
  -- todo if this return error -> add it in a try catch block
  envPath <- getEnvPath
  contents <- TIO.readFile envPath
  -- Split the contents into lines and filter out empty lines
  let lines' = filter (not . T.null) $ T.lines contents
  -- Convert the list of key-value pairs into a HashMap
  return $ HM.fromList $ map parseLine lines'
  where
    parseLine :: T.Text -> (T.Text, T.Text)
    parseLine line = case T.splitOn "=" line of
      -- If the line contains a key and a value separated by "=", return them
      (key : value : _) -> (key, value)
      -- Otherwise, raise an error indicating an invalid .env line
      _ -> error $ "Invalid .env line: " ++ T.unpack line

-- todo validate/ understand why with do always the arrow and return, why cann't we return string normally (why use arrow)
getEnvPath :: IO String
getEnvPath = do
  -- Get the current directory
  currentDir <- getCurrentDirectory :: IO FilePath
  let cleanedPath = computeCurrentDirectory currentDir
  -- Construct the final path
  let finalPath = cleanedPath </> ".env"
  -- Convert FilePath to String and return
  return (normalise finalPath)

-- | Get a config value by key
getConfig :: String -> IO (Maybe T.Text)
getConfig key = HM.lookup (toText key) <$> loadEnv

getConfigMap :: IO (HM.HashMap String String)
getConfigMap = convertHashMap <$> loadEnv

getConfigValueOrDefault :: String -> String -> IO String
getConfigValueOrDefault key defaultValue = do
  maybeValue <- getConfig key
  case maybeValue of
    Just value -> return (toString value)
    Nothing -> return defaultValue
