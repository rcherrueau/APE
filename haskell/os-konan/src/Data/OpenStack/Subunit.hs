{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.OpenStack.Subunit (
  -- Parses the result produced by the python subunit program
  -- `rsc/subunit_trace.py`. Subunit program produces the following
  -- JSON object:
  --
  -- > [
  -- >   {
  -- >    "name" : "a string that is the full name of the test",
  -- >    "duration": a number that is the duration time of the
  -- >                test in second,
  -- >    "sql"  : [ "a string that is a sql expression done in this test",
  -- >               ["or, a couple with first a string which is a
  -- >                 parametric sql expression (?)", ["together",
  -- >                 "with", "its", "list", "of", "arguments"]
  -- >               ], ...
  -- >             ]
  -- >   }, ...
  -- > ]
  OSTest(..)
, OSTests
, parse
, save
, load
  ) where

import GHC.Generics (Generic)
import Control.Monad ((<=<))
import Data.Either (rights)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as V (head)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL (readFile)

import Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import qualified Language.SQL.SimpleSQL.Parser as SQL (parseQueryExpr)
import qualified Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr, Dialect(MySQL))
import Data.Store as Store (Store, encode, decode, PeekException)

import Data.OpenStack.Sql()

-- | A unit/functional/integration OpenStack test with metrics.
data OSTest = OSTest
  { name       :: String           -- ^ Full test name
  , duration   :: Maybe Double     -- ^ The duration time in second
                                   --   of the test
  , sql        :: [SQL.QueryExpr]  -- ^ List of SQL queries performed in
                                   --   the test
  } deriving (Generic, Show, Eq)

-- | A list of unit/functional/integration OpenStack tests.
type OSTests = [OSTest]


-- | Decodes data into a `OSTest`.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > >>> decode "{ \"name\":\"nova.tests.foo\",\"duration\":1.0e-3,\"sql\":[] }" :: Maybe OSTest
-- > Just (OSTest {name = "nova.tests.foo", duration = Just 1.0e-3, sql = []})
instance JSON.FromJSON OSTest where
  parseJSON (JSON.Object o) = do
    n  <- o .:  "name"
    d  <- o .:? "duration"
    s  <- (parseSQLs <=< (.: "sql")) o
    pure (OSTest n d s)

    where
      parseSQLs :: [ JSON.Value ] -> JSON.Parser [ SQL.QueryExpr ]
      parseSQLs =
        pure . rights . map (SQL.parseQueryExpr SQL.MySQL "" Nothing . extractSQL)

      extractSQL :: JSON.Value -> String
      extractSQL (JSON.String s) = Text.unpack s         -- The sql query
      extractSQL (JSON.Array v)  = extractSQL (V.head v) -- A parametric sql query
      extractSQL v               = error "Getting SQL statement" v
  parseJSON v          = JSON.typeMismatch "OSTest" v

-- | Encodes a `OSTest` into a JSON object.
--
-- > >>> encode (OSTest {name = "nova.tests.foo", duration = Just 0.001, sql = []})
-- > "{\"name\": \"nova.tests.foo\", \"sql\": []}"
instance JSON.ToJSON OSTest where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | Serialize a `OSTest` in a binary object
instance Store OSTest


-- API

-- | Parse a python subunit JSON file
parse :: String
      -> IO (Either String OSTests)
parse = pure . JSON.eitherDecode <=< BSL.readFile

-- | Saves an OSTests one the disk for later use.
save :: OSTests  -- ^ OSTests to save
     -> String   -- ^ Binary output file path
     -> IO ()
save ts fb =  BS.writeFile fb $ Store.encode ts

-- | Loads a previously saved OSTests file.
load :: String        -- ^ Binary Input file
     -> IO (Either PeekException OSTests)
load = (pure . Store.decode) <=< BS.readFile
