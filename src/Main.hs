module Main (main) where

import           Cedict.Entry                  (Entry)
import           Cedict.Parser                 (entries)
import           Data.List                     (intercalate)
import           System.Environment            (getArgs)
import           Text.Parsec.String            (parseFromFile)
import           Text.ParserCombinators.Parsec (ParseError)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:_) -> parseFromFile entries file >>= either report printResult
    _ -> putStrLn "ERROR: Please specify a file path!"

report :: ParseError -> IO ()
report err = putStrLn $ "Error: " ++ show err

printResult :: [Entry] -> IO ()
printResult res =
    putStrLn $ intercalate "\n" $ map show res
