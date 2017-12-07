{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- module Subunit
module Main
  -- Parses the result produced by the python subunit program.
  -- Subunit program produces the following JSON object:
  --
  -- > [
  -- >   {
  -- >    "name" : "a string that is the full name of the test",
  -- >    "sql"  : [ "a string that is a sql expression done in this test",
  -- >               ["or, a couple with first a string which is a
  -- >                 parametric sql expression (?)", ["together",
  -- >                 "with", "its", "list", "of", "arguments"]
  -- >               ], ...
  -- >             ]
  -- >   }, ...
  -- > ]
  where

import GHC.Generics (Generic)
import Data.Either (rights)

import Control.Monad
import qualified Data.Text as Text (unpack, pack)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)

import Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import qualified Data.Aeson.Encode.Pretty as JSON (encodePretty)

import qualified Language.SQL.SimpleSQL.Pretty as SQL
import qualified Language.SQL.SimpleSQL.Parser as SQL
import Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr, Dialect(MySQL))

import Sql (isCorrelated)

-- | A unit/functional OpenStack test with metrics.
data Test = Test
  { name       :: String       -- ^ Full test name
  , sql        :: [QueryExpr]  -- ^ List of SQL queries performed in
                               --   the test
  , correlated :: [QueryExpr]  -- ^ List of SQL queries that
                               --   contained, at lest, one correlated
                               --   sub-query
  } deriving (Generic, Show, Eq)

-- | A list of unit/functional OpenStack tests.
type Tests = [Test]

-- | Decodes data into a `Test`.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > >>> decode "{\"name\": \"nova.tests.foo\", \"sql\": []}" :: Maybe Test
-- > Just (Test {name = "nova.tests.foo", sql = [], correlated = []})
instance JSON.FromJSON Test where
  parseJSON (JSON.Object o) = do
    name  <- o .: "name"
    sql  <- (parseSQLs <=< (.: "sql")) o
    let correlated = filter isCorrelated sql
    pure (Test name sql correlated)

    where
      parseSQLs :: [ JSON.Value ] -> JSON.Parser [ QueryExpr ]
      parseSQLs =
        pure . rights . map (SQL.parseQueryExpr MySQL "" Nothing . extractSQL)

      extractSQL :: JSON.Value -> String
      extractSQL (JSON.String s) = Text.unpack s         -- The sql query
      extractSQL (JSON.Array v)  = extractSQL (V.head v) -- A parametric sql query
      extractSQL v          = error "Getting SQL statement" v
  parseJSON v          = JSON.typeMismatch "Test" v

-- | Encodes a `Test` into a JSON object.
--
-- > >>> encode (Test {name = "nova.tests.foo", sql = [], correlated = []})
-- > "{\"name\": \"nova.tests.foo\", \"sql\": []}"
instance JSON.ToJSON Test where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | Encodes `QueryExpr` into a JSON object.
instance JSON.ToJSON QueryExpr where
  toJSON = JSON.String . Text.pack . SQL.prettyQueryExpr MySQL

-- | Processes a python subunit json file.
processTestFile :: String         -- ^ Input file path
                -> String         -- ^ Output file path
                -> (Test -> Bool) -- ^ Filter predicate
                -> IO ()
processTestFile fpi fpo p =  do
  putStrLn $ "Processing file " ++ fpi ++ " ..."
  json <- BS.readFile fpi
  case (JSON.eitherDecode json :: Either String Tests) of
    Left  err -> putStrLn err
    Right tests -> BS.writeFile fpo $ JSON.encodePretty (filter p tests)

main :: IO ()
main = do
  processTestFile "./pike-nova-tests-unit.sql.json"
                  "./pike-nova-tests-unit+.sql.json"
                  (not . null . correlated)
  processTestFile "./pike-nova-tests-functional.sql.json"
                  "./pike-nova-tests-functional+.sql.json"
                  (not . null . correlated)

  -- json <- BS.readFile "./pike-nova-tests-unit.sql.json"
  -- case (JSON.eitherDecode json :: Either String Tests) of
  --   Left error -> putStrLn error
  --   Right tests ->
  --     let tests' = filter  (not . null . correlated) tests
  --         json' = JSON.encodePretty tests'
  --     in  BS.writeFile "./pike-nova-tests-unit+.sql.json" json'

--  fmap eitherDecode (BS.readFile "./pike-nova-tests-unit.sql.json")
