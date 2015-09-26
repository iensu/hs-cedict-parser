{-# LANGUAGE FlexibleContexts #-}

module Cedict.Parser (parse, Entry) where

import           Control.Applicative           ((*>), (<$>), (<*>))
import           Prelude                       hiding (lines)
import           Text.Parsec                   ((<?>))
import           Text.ParserCombinators.Parsec (ParseError, Parser, anyChar,
                                                between, char, digit, eof,
                                                letter, many, many1, manyTill,
                                                noneOf, oneOf, skipMany, space,
                                                (<|>))
import qualified Text.ParserCombinators.Parsec as P

data Entry = Entry { traditional :: String
                   , simplified  :: String
                   , pinyin      :: String
                   , translation :: String} deriving (Show)

parse :: String -> Either ParseError [Entry]
parse = P.parse lines ""

lines :: Parser [Entry]
lines = (:) <$> entryLine <*> (many entryLine) <* skippable <* eof <?> "lines"
        where entryLine = id <$> skippable *> line
              skippable = skipMany (comment <|> eol)

line :: Parser Entry
line = Entry <$> entry <*> (delim *> entry) <*> (delim *> pron) <*> (delim *> trans) <?> "line"
       where delim = skipMany space

entry :: Parser String
entry = many1 (letter <|> digit) <?> "entry"

trans :: Parser String
trans = (:) <$> char '/' <*> (manyTill anyChar (eol <|> comment)) <?> "translation"

pron :: Parser String
pron = between (char '[') (char ']') (many1 (letter <|> digit <|> space)) <?> "pronunciation"

comment :: Parser ()
comment = id <$> (char '#' *> (skipMany $ noneOf "\r\n")) <?> "comment"

eol :: Parser ()
eol = do oneOf "\n\r"
         return ()
      <?> "end of line"
