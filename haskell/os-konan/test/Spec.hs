import Test.HUnit

import System.Exit (exitSuccess, exitFailure)

import qualified Sql.ParseTests


main :: IO ()
main = do
  c <- runTestTT tests
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure

  where
    tests :: Test
    tests = Sql.ParseTests.testsAll
