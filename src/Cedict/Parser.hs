{-# LANGUAGE FlexibleContexts #-}

module Cedict.Parser (entries) where

import           Cedict.Entry                  (Entry (..))
import           Prelude                       hiding (lines)
import           Text.Parsec                   ((<?>))
import           Text.ParserCombinators.Parsec (Parser, anyChar, between, char,
                                                digit, eof, letter, many, many1,
                                                manyTill, noneOf, oneOf,
                                                skipMany, space, (<|>))

entries :: Parser [Entry]
entries =
    (:) <$> entryLine <*> many entryLine <* eof <?> "lines"
    where entryLine = skippable *> line
          skippable = skipMany (comment <|> eol)

line :: Parser Entry
line =
    Entry <$> entry <*> (delim *> entry) <*> (delim *> pron) <*> (delim *> trans) <?> "line"
    where delim = skipMany space

entry :: Parser String
entry = many1 (noneOf " \n\r\t") <?> "entry"

trans :: Parser String
trans = (:) <$> char '/' <*> manyTill anyChar (eol <|> eof) <?> "translation"

pron :: Parser String
pron = between (char '[') (char ']') (many1 allowedChars) <?> "pronunciation"
       where allowedChars = letter <|> digit <|> space <|> oneOf ",:·"

comment :: Parser ()
comment = char '#' *> skipMany (noneOf "\r\n") <?> "comment"

eol :: Parser ()
eol = do _ <- oneOf "\n\r"
         return ()
      <?> "end of line"
