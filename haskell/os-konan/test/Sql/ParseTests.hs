module Sql.ParseTests (testsAll) where

import Data.Either (isRight)

import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax

import Test.HUnit

assertParsable :: String -> Assertion
assertParsable file = do
  sql <- readFile file
  let res  = parseQueryExpr MySQL file (Just (0,0)) sql
  let msg  = either show (const $ file ++ " -- OK") res
  let isOk = isRight res
  assertBool msg isOk

testParse :: Test
testParse = TestLabel "SQL Parsing" $ TestList $
  map (TestCase . assertParsable)
  ["rsc/sql/correlated-000.sql"
  , "rsc/sql/correlated-001.sql"
  , "rsc/sql/correlated-004.sql"
  , "rsc/sql/correlated-005.sql"
  , "rsc/sql/correlated-007.sql"
  , "rsc/sql/correlated-009.sql"
  , "rsc/sql/correlated-010.sql"
  , "rsc/sql/correlated-011.sql"
  , "rsc/sql/correlated-012.sql"
  , "rsc/sql/correlated-013.sql"
  , "rsc/sql/correlated-014.sql"
  , "rsc/sql/correlated-015.sql"
  , "rsc/sql/correlated-016.sql"
  , "rsc/sql/correlated-018.sql"
  , "rsc/sql/correlated-021.sql"
  , "rsc/sql/correlated-022.sql"
  , "rsc/sql/correlated-023.sql"
  , "rsc/sql/correlated-028.sql"
  , "rsc/sql/parameter-01.sql"
  -- , "rsc/sql/parameter-02.sql" -- INSERT INTO not supported
  , "rsc/sql/non-regression-01.sql"
  ]

testsAll :: Test
testsAll = testParse
