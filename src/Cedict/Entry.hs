module Cedict.Entry ( Entry(..) ) where

data Entry = Entry { traditional :: String
                   , simplified  :: String
                   , pinyin      :: String
                   , translation :: String} deriving (Show, Eq)
