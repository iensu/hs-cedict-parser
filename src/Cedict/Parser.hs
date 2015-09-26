{-# LANGUAGE FlexibleContexts #-}

module Cedict.Parser (line, lines, comment) where

import           Control.Applicative
import           Prelude             hiding (lines)
import           Text.Parsec         ((<?>))
import qualified Text.Parsec         as P


comment :: P.Parsec String () ()
comment = do
  P.char '#'
  P.skipMany $ P.noneOf "\r\n"
  <?> "comment"

line :: P.Parsec String () String
line = P.many $ P.noneOf "\r\n"

lines :: P.Parsec String () [String]
lines = do
  l <- line
  ls <- P.many $ do
          P.newline
          line
  P.eof
  return (l:ls)
