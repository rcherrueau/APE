module Main where

import qualified Data.OpenStack.Subunit as OS
import qualified Data.OpenStack.Sql as OS (isCorrelated)
import Subunit

main :: IO ()
main = do
  putStrLn (replicate 80 '-')
  correlatedSubunit "./rsc/pike-keystone-tempest.json"
                    "./rsc/pike-keystone-tempest-correlated.json"

  putStrLn (replicate 80 '-')
  correlatedSubunit "./rsc/pike-nova-tempest.json"
                    "./rsc/pike-nova-tempest-correlated.json"

  putStrLn (replicate 80 '-')
  correlatedSubunit "./rsc/pike-nova-functional.json"
                    "./rsc/pike-nova-functional-correlated.json"

  putStrLn (replicate 80 '-')
  correlatedSubunit "./rsc/pike-nova-unit.json"
                    "./rsc/pike-nova-unit-correlated.json"

  where
    correlatedSubunit = processSubunit (not . null . (filter OS.isCorrelated) . OS.sql)
