{-# LANGUAGE FlexibleContexts #-}

module CedictParserTests (tests) where

import qualified Cedict.Parser                  as CP
import           Control.Applicative
import           Data.Functor.Identity          (Identity)
import           Text.Parsec                    (ParseError, Parsec, Stream)
import qualified Text.Parsec                    as P

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

parse :: Parsec String () a -> String -> a
parse rule input =
    case P.parse rule "(Example)" input of
      Left {} -> error "Parse error!"
      Right result -> result

parseLine :: String -> String
parseLine = parse CP.line

parseLines :: String -> [String]
parseLines = parse CP.lines

parseComment :: String -> ()
parseComment = parse CP.comment

tests :: Test
tests = testGroup "Cedict.Parser"
        [
         testGroup "line"
         [
           testCase "" $ "" @=? (parseLine "")
         , testCase "x" $ "x" @=? (parseLine "x")
         , testCase "hello there\nbye bye!"
              $ "hello there" @=? (parseLine "hello there\nbye bye!")
         ],
         testGroup "lines"
         [
           testCase "" $ [""] @=? (parseLines "")
         , testCase "hej" $ ["hej"] @=? (parseLines "hej")
         , testCase "hello there\nbye bye!"
              $ ["hello there", "bye bye!"] @=? (parseLines "hello there\nbye bye!")
         ],
         testGroup "comment"
         [
          testCase "# comment" $ () @=? (parseComment "# comment")
         ]
        ]
