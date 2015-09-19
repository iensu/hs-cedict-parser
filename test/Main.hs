module Main (main) where

import           Cedict.Parser
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

test1 :: Assertion
test1 = assertEqual "for (myFunc 1)," 1 (myFunc 0)

test2 :: Assertion
test2 = assertEqual "for (myFunc 22)," 22 (myFunc 22)

tests :: Test
tests = testGroup "unit" [
          testCase "test1" test1
        , testCase "test2" test2
        ]

main :: IO ()
main = do
  defaultMain [tests]
