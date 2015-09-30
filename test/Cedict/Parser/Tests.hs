module Cedict.Parser.Tests (tests) where

import           Test.HUnit                    (Assertion, (@=?))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase)
import           Text.ParserCombinators.Parsec (runParser)

import           Cedict.Entry                  (Entry (..))
import           Cedict.Parser                 (entries)

parse :: String -> [Entry]
parse s = case runParser entries () "(TEST)" s of
            Left _ -> []
            Right es -> es

singleEntry :: [Entry]
singleEntry = [Entry "a" "b" "c d" "/e f/"]

multipleEntries :: [Entry]
multipleEntries = [ Entry "早飯" "早饭" "zao3 fan4" "/breakfast/"
                  , Entry "寫" "写" "xie3" "/write/"
                  , Entry "好" "好" "hao3" "/ok/good/"]

testSimpleLine :: Assertion
testSimpleLine =
    singleEntry @=? parse "a b [c d] /e f/"

testMultiLine :: Assertion
testMultiLine =
    singleEntry @=? parse "\n\na b [c d] /e f/\n"

testMultiEntry :: Assertion
testMultiEntry =
    multipleEntries @=? parse "早飯 早饭 [zao3 fan4] /breakfast/\n\
                              \寫 写 [xie3] /write/\n\
                              \好 好 [hao3] /ok/good/\n"

testMultiEntryWithComments :: Assertion
testMultiEntryWithComments =
    multipleEntries @=? parse "# This is a comment\n\
                              \早飯 早饭 [zao3 fan4] /breakfast/\n\
                              \# Here's another comment\n\
                              \# and another...\n\
                              \寫 写 [xie3] /write/\n\
                              \好 好 [hao3] /ok/good/\n"

testSingleEntryWithPercent :: Assertion
testSingleEntryWithPercent =
    [Entry "%" "%" "percent" "/percent/"] @=? parse "% % [percent] /percent/\n"

testWeirdPronunciation :: Assertion
testWeirdPronunciation =
    [Entry "䶊" "衄" "nu:4" "/old variant of 衄[nu:4]/"] @=? parse "䶊 衄 [nu:4] /old variant of 衄[nu:4]/\n"

testYiBuZuoErBuXiu :: Assertion
testYiBuZuoErBuXiu =
    [Entry "一不做，二不休" "一不做，二不休" "yi1 bu4 zuo4 , er4 bu4 xiu1" "/don't do it, or don't rest (idiom); either give up, or go through to the end/Since we started, we must carry it through whatever happens./in for a penny, in for a pound/"] @=?
    parse "一不做，二不休 一不做，二不休 [yi1 bu4 zuo4 , er4 bu4 xiu1] /don't do it, or don't rest (idiom); either give up, or go through to the end/Since we started, we must carry it through whatever happens./in for a penny, in for a pound/\n"

testAlexanderDubcek :: Assertion
testAlexanderDubcek = [Entry "亞歷山大·杜布切克" "亚历山大·杜布切克" "Ya4 li4 shan1 da4 · Du4 bu4 qie1 ke4" "/Alexander Dubček (1921-1992), leader of Czechoslovakia (1968-1969)/"] @=?
       parse "亞歷山大·杜布切克 亚历山大·杜布切克 [Ya4 li4 shan1 da4 · Du4 bu4 qie1 ke4] /Alexander Dubček (1921-1992), leader of Czechoslovakia (1968-1969)/\n"

tests :: TestTree
tests = testGroup "Cedict.Parser"
        [ testCase "Single line" testSimpleLine
        , testCase "Multiple lines" testMultiLine
        , testCase "Multiple entries" testMultiEntry
        , testCase "Multiple entries and comments" testMultiEntryWithComments
        , testCase "Single entry with percent" testSingleEntryWithPercent
        , testCase "Weird pronunciation" testWeirdPronunciation
        , testCase "一不做，二不休" testYiBuZuoErBuXiu
        , testCase "亞歷山大·杜布切克" testAlexanderDubcek]
