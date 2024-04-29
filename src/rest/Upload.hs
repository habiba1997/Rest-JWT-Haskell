{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Rest.Upload (upload) where

-- import Data.Text.Lazy (Text)
-- import Data.Text.Lazy.Encoding (decodeUtf8)

-- import Network.HTTP.Types.Status (status200, status201, status204, status400)

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types
import Data.ByteString as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal as Internal
import Network.Wai.Parse (fileContent, fileName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Web.Scotty
import qualified Web.Scotty as S

upload :: ActionM ()
upload = do
  --  file1 <- files
  -- Get the file from the request
  --  >>= is the bind operator, which allows sequencing of actions within a monadic context.
  -- >>= applies lookup "file" to the result of files. This effectively searches for the file named "file" among the uploaded files.
  fileUploaded <- files
  let fileL = lookup "file" fileUploaded
  case fileL of
    Nothing -> do
      S.json $ object ["error" .= ("No file provided" :: String)]
    Just fileInfo -> do
      --       network wai parse bytstring
      liftIO $ print fileInfo
      let filename = fileName fileInfo :: S.ByteString
      let filecontent = (fileContent fileInfo :: Internal.ByteString)
      let destination = "uploads"

      -- Create the uploads directory if it doesn't exist
      liftIO $ createDirectoryIfMissing True destination

      -- Save the file to the server
      --  </> combine
      let filepath = (destination </> BS.unpack filename) :: FilePath
      --      printType filepath
      --      liftIO $ print filepath
      
      --      `SomeException` is a type that represents any exception.
      --      By using @SomeException, we're telling Haskell to interpret try as "try to execute this IO action and catch any exception of type SomeException that may be thrown".
      let writeContent2File = BL.writeFile filepath filecontent :: IO ()
      result <- liftIO $ try @SomeException writeContent2File
      case result of
        Left e -> do
          let errorMessage = "Error saving file: " ++ show e
          liftIO $ putStrLn errorMessage
          S.json $ object ["error" .= (errorMessage :: String)]
        Right _ -> do
          -- Print the result on the console
          liftIO $ putStrLn $ "File saved as: " ++ filepath
          -- Return the result as text in the response body
          --  decodeUtf8:   data text lazy decode
          S.json $ object ["file" .= filepath]

--  todo (return - catch - left - liftIO - do)
