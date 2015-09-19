{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Cedict.Parser

main :: IO ()
main = do putStrLn $ show $ myFunc 3
