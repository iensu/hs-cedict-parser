import           Cedict.Parser
import           Test.Framework
import           Test.HUnit

test1 = TestCase (assertEqual "for (myFunc 1)," 1 (myFunc 0))
test2 = TestCase (assertEqual "for (myFunc 22)," 23 (myFunc 22))

tests = TestList [
          TestLabel "myFunc 1" test1
        , TestLabel "myFunc 2" test2
        ]
