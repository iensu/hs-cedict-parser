{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Cedict.Entry                  (Entry)
import           Cedict.Parser                 (entries)
import           Codec.Compression.GZip        (decompress)
import           Data.ByteString.Char8         (unpack)
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.List                     (intercalate)
import           System.Environment            (getArgs)
import           Text.Parsec                   (parse)
import           Text.ParserCombinators.Parsec (ParseError)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:_) -> do
              content <- fmap decompress (LBS.readFile file)
              case parse entries file (bsToString content) of
                Left err -> report err
                Right es -> printResult es
    _ -> putStrLn "ERROR: Please specify a file path!"

    where bsToString = unpack . LBS.toStrict
          report err = putStrLn $ "Error: " ++ show err
          printResult r = putStrLn $ intercalate "\n" $ map show r
