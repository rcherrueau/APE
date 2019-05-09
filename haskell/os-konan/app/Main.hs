module Main where

import Data.List
import Data.Maybe
import Data.Either
import Data.Store as Store (PeekException)

import Data.Tree
import qualified Data.Aeson as JSON (ToJSON())
import qualified Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr(..))
import qualified Data.ByteString.Lazy as File (ByteString, writeFile)
import qualified Data.ByteString.Lazy.Char8 as File (pack)
import qualified System.Directory as File (doesFileExist)
import Control.Monad (unless)

import qualified Data.OpenStack.Subunit as OSS (save, parse, load, sql)
import qualified Data.OpenStack.OSPTrace as OSP (save, parse, load, sql)
import Data.OpenStack.Subunit (OSTest(..))
import Data.OpenStack.OSPTrace (OSPTrace, TraceType(..), TraceInfo(..))
import qualified Data.OpenStack.Sql as OS (isCorrelated)

-- | Processes a test `a`.
process :: (String -> IO (Either String a))         -- ^ Parse test `a`
        -> (a -> String -> IO ())                   -- ^ Save `a` on the disk
        -> (String -> IO (Either PeekException a))  -- ^ Load `a` from the disk
        -> (a -> File.ByteString) -> String -> String -> IO ()  -- ^ transform, fileIn, fileOut
process parse save load t fileIn fileOut = do
  let fileBinary = fileIn ++ ".raw"

  -- Parse test from file and save it (if needed)
  alreadyParsed <- File.doesFileExist fileBinary
  unless alreadyParsed (do
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
    Left err -> putStrLn $ "Error while loading " ++ fileBinary ++ ": " ++ show err
    Right test -> do
      -- Apply `m` on test and save it
      putStrLn "Folding..."
      let test' = t test
      File.writeFile fileOut test'
      putStrLn $ "Folded saved into " ++ fileOut

-- | Processes `OSTests`.
processOSS :: ([OSTest] -> File.ByteString) -> String -> String -> IO ()
processOSS = process OSS.parse OSS.save OSS.load

-- | Processes `OSPTrace`.
processOSP :: (OSPTrace -> File.ByteString) -> String -> String -> IO ()
processOSP = process OSP.parse OSP.save OSP.load


-- Transformation functions

-- | Returns SQL correlated subqueries of an OSPTrace.
--
-- This function removes duplication.
correlatedUniqueOSP :: OSPTrace -> [SQL.QueryExpr]
correlatedUniqueOSP = nub . filter OS.isCorrelated . foldMap filterDBReq
  where
    -- `foldMap f` walks across the list, applies `f` to each element
    -- and collects the results by combining them with `mappend`.
    filterDBReq :: TraceType -> [SQL.QueryExpr]
    filterDBReq (DB TraceInfo {  req = dbreq }) =
      -- I may better access `dbreq` with lenses
      let mSql = OSP.sql dbreq
      in  maybeToList mSql
    filterDBReq _ = []

-- | Filters OSPTrace to only keep REST call
restView :: OSPTrace -> OSPTrace
restView = fromRight (Node Root []) . foldTree keepWsgi
  where
    -- Puts WSGI nodes on Right and WSGI children of other nodes on
    -- Left. The second argument of `keepWsgi` is the result of
    -- applying `keepWsgi` on the children of current node. Children
    -- may be Right or Left. Hence, the function `getWsgiChildren`
    -- takes that list and transforms it back to a normal list of
    -- children.
    keepWsgi :: TraceType -> [Either [OSPTrace] OSPTrace] -> Either [OSPTrace] OSPTrace
    keepWsgi t@(Wsgi _) ts = Right (Node t $ getWsgiChildren ts)
    keepWsgi Root       ts = Right (Node Root $ getWsgiChildren ts)
    keepWsgi _          ts = Left (getWsgiChildren ts)

    getWsgiChildren :: [Either [OSPTrace] OSPTrace] -> [OSPTrace]
    getWsgiChildren = concatMap (either id (:[]))

-- | Filters OSPTrace to only keep REST call and intermediate node
restView' :: OSPTrace -> OSPTrace
restView' = fromMaybe (Node Root []) . foldTree keepWsgi
  where
    keepWsgi :: TraceType -> [Maybe OSPTrace] -> Maybe OSPTrace
    keepWsgi t@(Wsgi _) ts = Just (Node t    (catMaybes ts))
    keepWsgi Root       ts = Just (Node Root (catMaybes ts))
    keepWsgi t          ts = case catMaybes ts of
                               [] -> Nothing
                               xs -> Just (Node t xs)

-- Utils

-- | Draws a section line with a title
___s :: String  -- ^ Section title
     -> IO ()
___s s = putStrLn $ replicate (80 - (length s + 1)) '=' ++ " " ++ s

-- | Draws a line
____ :: IO ()
____ = putStrLn (replicate 80 '-')


-- Main

main :: IO ()
main = do
  ___s "Subunit Tests"
  correlatedOSS "rsc/pike-keystone-tempest.json"
                "rsc/pike-keystone-tempest-correlated.json"

  ____
  correlatedOSS "rsc/pike-nova-tempest.json"
                "rsc/pike-nova-tempest-correlated.json"

  ____
  correlatedOSS "rsc/pike-nova-functional.json"
                "rsc/pike-nova-functional-correlated.json"

  ____
  correlatedOSS "rsc/pike-nova-unit.json"
                "rsc/pike-nova-unit-correlated.json"

  ___s "OSProfiler Tests"
  correlatedOSP "rsc/queens-osptrace-server-create.json"
                "rsc/queens-osptrace-server-create-correlated.json"

  ____
  restOSP "rsc/queens-osptrace-server-create.json"
          "rsc/queens-osptrace-server-create-rest.txt"

  ____
  restOSP "/home/rfish/prog/TER-17/real/server-create-real.json"
          "rsc/ter-17-server-create-rest.txt"

  where
    -- | Only keeps correlated query from an OSTest.
    correlatedOSS = processOSS (JSON.encodePretty . nub . filter OS.isCorrelated . concatMap sql)

    -- | Only keeps correlated query from an OSPTrace
    correlatedOSP = processOSP (JSON.encodePretty . correlatedUniqueOSP)

    -- | Only keeps WSGI from OSPTrace and print the tree
    restOSP = processOSP (File.pack . drawTree . fmap show . restView)
