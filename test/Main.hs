module Main (main) where

import qualified CedictParserTests as CPT
import           Test.Framework    (defaultMain)

main :: IO ()
main = do
  defaultMain [CPT.tests]
