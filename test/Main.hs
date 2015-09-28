module Main where

import           Test.Tasty          (defaultMain, testGroup)

import qualified Cedict.Parser.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ Cedict.Parser.Tests.tests ]
