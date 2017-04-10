module Main (main) where

import Prelude
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

import qualified ParseTests

main :: IO ()
main = do
  c <- runTestTT ParseTests.testsAll
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure
