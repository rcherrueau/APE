module Main where

import Data.List
import Data.Maybe
import Data.Store as Store (PeekException)

import qualified Data.Aeson as JSON (ToJSON())
import qualified Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr(..))
import qualified Data.ByteString.Lazy as File (writeFile)
import qualified System.Directory as File (doesFileExist)
import Control.Monad (unless)

import qualified Data.OpenStack.Subunit as OSS (save, parse, load, sql)
import qualified Data.OpenStack.OSPTrace as OSP (save, parse, load, sql)
import Data.OpenStack.Subunit (OSTest(..))
import Data.OpenStack.OSPTrace (OSPTrace, TraceType(..), TraceInfo(..))
import qualified Data.OpenStack.Sql as OS (isCorrelated)


-- | Processes a test `a`.
process :: JSON.ToJSON b
        => (String -> IO (Either String a))         -- ^ Parse test `a`
        -> (a -> String -> IO ())                   -- ^ Save `a` on the disk
        -> (String -> IO (Either PeekException a))  -- ^ Load `a` from the disk
        -> (a -> b) -> String -> String -> IO ()    -- ^ fold, fileIn, fileOut
process parse save load m fileIn fileOut = do
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
      let test' = m test
      File.writeFile fileOut $ JSON.encodePretty test'
      putStrLn $ "Folded saved into " ++ fileOut

-- | Processes `OSTests`.
processOSS :: JSON.ToJSON b => ([OSTest] -> b) -> String -> String -> IO ()
processOSS = process OSS.parse OSS.save OSS.load

-- | Processes `OSPTrace`.
processOSP :: JSON.ToJSON b => (OSPTrace -> b) -> String -> String -> IO ()
processOSP = process OSP.parse OSP.save OSP.load


-- Transformation functions

sqlUnique :: [ OSTest ] -> [ OSTest ]
sqlUnique ts = snd $ foldl notSeenOSTestLevel ([], []) ts
  where
    notSeenOSTestLevel :: ([ SQL.QueryExpr ], [ OSTest ]) -> OSTest -> ([ SQL.QueryExpr ], [ OSTest ])
    notSeenOSTestLevel (seen, ts) t =
      let sqls           = OSS.sql t
          (seen', sqls') = notSeenSqlLevel seen sqls
      in  (seen', t { OSS.sql = sqls' } : ts)

    notSeenSqlLevel :: [ SQL.QueryExpr ] -> [ SQL.QueryExpr ] -> ([ SQL.QueryExpr ], [ SQL.QueryExpr ])
    notSeenSqlLevel seen []     = (seen, [])
    notSeenSqlLevel seen (t:ts)
      -- sql has been already seen, do not keep it in test
      | t `elem` seen = notSeenSqlLevel seen ts
      -- sql did not have been already seen, put it in seen, and keep it
      | otherwise     = let (seen', ts') = notSeenSqlLevel (t : seen) ts
                        in  (seen', t : ts')

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

-- | Only keeps edges that ends by a REST call
restEdges :: OSPTrace -> OSPTrace
restEdges t = _hole


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

  where
    -- | Only keeps correlated query from an OSTest.
    correlatedOSS = processOSS (nub . filter OS.isCorrelated . concatMap sql)

    correlatedOSP = processOSP correlatedUniqueOSP
