{-# LANGUAGE OverloadedStrings #-}

module Subunit where

import qualified Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Data.ByteString.Lazy as File (writeFile)
import qualified System.Directory as File (doesFileExist)
import Control.Monad (when)

import qualified Data.OpenStack.Subunit as OS

binaryExt :: String
binaryExt = ".raw"

-- | Processes a python subunit json file.
processSubunit :: (OS.OSTest -> Bool) -> String -> String -> IO ()
processSubunit pred fileIn fileOut = do
  let fileBinary = fileIn ++ binaryExt

  -- Parse OSTests from file and save it (if needed)
  alreadyParsed <- File.doesFileExist fileBinary
  when (not alreadyParsed) (do
    putStrLn $ "Parsing file " ++ fileIn ++ " ..."
    eitherOSTests <- OS.parse fileIn
    case eitherOSTests of
      Left  err -> putStrLn $ "Error while parsing " ++ fileIn ++ ": " ++ err
      Right osTests -> do
        -- Saved file
        OS.save osTests fileBinary
        putStrLn $ "Saved into " ++ fileBinary)

  -- Load OSTests from file
  putStrLn $ "Loading file " ++ fileBinary ++ " ..."
  eitherOSTests <- OS.load fileBinary
  case eitherOSTests of
    Left err -> putStrLn $ "Error while loading " ++ fileBinary ++ ": " ++ (show err)
    Right osTests -> do
      -- Display result on standard output
      -- putStrLn $ show $ JSON.encodePretty (filter pred osTests)
      File.writeFile fileOut $ JSON.encodePretty (filter pred osTests)
      putStrLn $ "Filtered saved into " ++ fileOut
