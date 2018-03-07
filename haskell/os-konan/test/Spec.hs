import Test.HUnit

import System.Exit (exitSuccess, exitFailure)

import qualified Sql.ParseTests
import qualified OSPTrace.ParseTests


main :: IO ()
main = do
  c <- runTestTT tests
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure

  where
    tests :: Test
    tests = TestList [
        Sql.ParseTests.testsAll
      , OSPTrace.ParseTests.testsAll
      ]
