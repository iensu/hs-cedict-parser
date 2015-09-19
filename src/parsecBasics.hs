{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import qualified Control.Monad.State    as S
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as P

parse rule = P.parse rule "(source)"

myParser :: P.Parsec String () (String,String)
myParser = do
  letters <- P.many1 P.letter
  P.spaces
  digits <- P.many1 P.digit
  return (letters,digits)

-- myParser1 and myParser2 are equivalent
myParser1 :: P.ParsecT String () Identity (String,String)
myParser1 = myParser

myParser2 :: P.Parsec String () (String,String)
myParser2 = myParser

mySeparator :: P.Parsec String () ()
mySeparator = P.spaces >> P.char ',' >> P.spaces
{- equivalent to:
 mySeparator = do
  P.spaces
  P.char ','
  P.spaces
-}

myPairs :: P.Parsec String () [(String,String)]
myPairs = P.many (myParser >>= \pair -> mySeparator >> return pair)
{- equivalent to:
   myPairs = P.many $ do
            pair <- myParser
            mySeparator
            return pair
-}

myPairs2a :: P.Parsec String () [(String,String)]
myPairs2a = P.endBy myParser mySeparator

myPairs2b :: P.Parsec String () [(String,String)]
myPairs2b = P.sepBy myParser mySeparator

myPairs2 :: P.Parsec String () [(String,String)]
myPairs2 = P.many $ do
             pair <- myParser
             P.eof <|> mySeparator
             return pair

helloOrHowdy :: P.Parsec String () String
helloOrHowdy = do
  first <- P.char 'h'
  rest <- P.string "ello" <|> P.string "owdy"
  return (first:rest)

helloOrHowdy2 :: P.Parsec String () String
helloOrHowdy2 = P.try (P.string "hello") <|> P.string "howdy"

-- applicative style
myParserApp :: P.Parsec String () (String,String)
myParserApp = (,) <$> P.many1 P.letter <*> (P.spaces *> P.many1 P.digit)

-- equivalent to
myParserApp2 :: P.Parsec String () (String,String)
myParserApp2 = liftA2 (,) (P.many1 P.letter) (P.spaces *> P.many1 P.digit)

-- and also to
myParserApp3 :: P.Parsec String () (String,String)
myParserApp3 = liftA2 (,) (P.many1 P.letter) (P.spaces >> P.many1 P.digit)

-- mathes char 'h', incrementing int state by 1
-- each time one is seen.
hCountParser :: P.Parsec String Int ()
hCountParser = do
  P.char 'h'
  c <- P.getState
  let c' = c+1
  P.putState c'
  return ()

hCountParser' :: P.Parsec String Int ()
hCountParser' = do
  P.char 'h'
  P.modifyState (+1)
  return ()

-- parse as many h's as we can, then return the state
-- to see how many there were
-- P.runParser (P.many hCountParser >> P.getState) 0 "" "hhhhhelloh"

hCountParser'' :: P.ParsecT String () (S.State Int) ()
hCountParser'' = do
  P.char 'h'
  S.lift $ S.modify (+1)

-- after running our åarser transformer, we get back our unevaluated inner state,
-- which contains our parser result and state ('h' count). We conly want the state
-- so we use execState rather than runState or evalState to execute and unwrap the
-- state monad, providing an initial state of 0
-- S.execState (P.runParserT (P.many hCountParser'') () "" "hhhhhellloh") 0
