module Main where

import Data.List (nubBy)
import Data.Store as Store (PeekException)

import qualified Data.Aeson as JSON (ToJSON())
import qualified Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr(..))
import qualified Data.ByteString.Lazy as File (writeFile)
import qualified System.Directory as File (doesFileExist)
import Control.Monad (when)

import qualified Data.OpenStack.Subunit as OSS
import qualified Data.OpenStack.OSPTrace as OSP
import qualified Data.OpenStack.Sql as OS (isCorrelated)


-- | Processes a test `a`.
process :: JSON.ToJSON b
        => (String -> IO (Either String a))         -- ^ Parse test `a`
        -> (a -> String -> IO ())                   -- ^ Save `a` on the disk
        -> (String -> IO (Either PeekException a))  -- ^ Load `a` from the disk
        -> (a -> b) -> String -> String -> IO ()    -- ^ fold, fileIn, fileOut
process parse save load m fileIn fileOut = do
  let fileBinary = fileIn ++ binaryExt

  -- Parse test from file and save it (if needed)
  alreadyParsed <- File.doesFileExist fileBinary
  when (not alreadyParsed) (do
    putStrLn $ "Parsing file " ++ fileIn ++ " ..."
    eitherTest <- parse fileIn
    case eitherTest of
      Left  err -> putStrLn $ "Error while parsing " ++ fileIn ++ ": " ++ err
      Right test -> do
        -- Save test to file
        save test fileBinary
        putStrLn $ "Saved into " ++ fileBinary)

  -- Load test from file
  putStrLn $ "Loading file " ++ fileBinary ++ " ..."
  eitherTest <- load fileBinary
  case eitherTest of
    Left err -> putStrLn $ "Error while loading " ++ fileBinary ++ ": " ++ (show err)
    Right test -> do
      -- Apply `m` on test and save it
      putStrLn "Folding..."
      let test' = m test
      File.writeFile fileOut $ JSON.encodePretty test'
      putStrLn $ "Folded saved into " ++ fileOut

  where
  binaryExt :: String
  binaryExt = ".raw"

-- | Processes `OSTests`.
processOSS :: JSON.ToJSON b => (OSS.OSTests -> b) -> String -> String -> IO ()
processOSS = process OSS.parse OSS.save OSS.load

-- | Processes `OSPTrace`.
processOSP :: JSON.ToJSON b => (OSP.OSPTrace -> b) -> String -> String -> IO ()
processOSP = process OSP.parse OSP.save OSP.load

corrNotSeen :: ([ SQL.QueryExpr ], [ OSS.OSTest ]) -> OSS.OSTest -> ([ SQL.QueryExpr ], [ OSS.OSTest ])
corrNotSeen (seen, ts) t =

correlatedNotSeen :: [ OSS.OSTest ] -> [ OSS.OSTest ]
correlatedNotSeen ts = snd $ foldl corrNotSeen ([], []) ts





-- Utils

-- | Draws a section line with a title
___s :: String  -- ^ Section title
     -> IO ()
___s s = putStrLn $ (replicate (80 - (length s + 1)) '=') ++ " " ++ s

-- | Draws a line
____ :: IO ()
____ = putStrLn (replicate 80 '-')



-- Main

main :: IO ()
main = do
  ___s "Subunit Tests"
  correlatedOSS "./rsc/pike-keystone-tempest.json"
                    "./rsc/pike-keystone-tempest-correlated.json"

  ____
  correlatedOSS "./rsc/pike-nova-tempest.json"
                    "./rsc/pike-nova-tempest-correlated.json"

  ____
  correlatedOSS "./rsc/pike-nova-functional.json"
                    "./rsc/pike-nova-functional-correlated.json"

  ____
  correlatedOSS "./rsc/pike-nova-unit.json"
                    "./rsc/pike-nova-unit-correlated.json"

  ___s "OSProfiler Tests"
  correlatedOSP "./rsc/pike-nova-osptrace-boot-and-delete.json"
                     "./rsc/pike-nova-osptrace-boot-and-delete-correlated.json"

  where
    -- | Only keeps correlated query from an OSTest.
    correlatedOSTest :: OSS.OSTest -> OSS.OSTest
    correlatedOSTest t = t { OSS.sql = filter OS.isCorrelated (OSS.sql t) }

    equalOSTestQuery :: OSS.OSTest -> OSS.OSTest -> Bool
    equalOSTestQuery t t' = OSS.sql t == OSS.sql t'

    -- correlatedOSS = processOSS (filter (not . null . OSS.sql) . map correlatedOSTest)
    correlatedOSS = processOSS (nubBy undefined . filter (not . null . OSS.sql) . map correlatedOSTest)

    correlatedOSP = processOSP id
