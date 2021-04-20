import           Abbot.Reference

import           Test.Tasty
import           Test.Tasty.HUnit


test1, test2 :: TestTree
test1 = testCase "one plus one is two?" (1 + 1 @?= 2)
test2 = testCase "two times two is five?" (2 * 2 @?= 4)

tests :: TestTree
tests = testGroup "arithmetic" [test1, test2]

main :: IO ()
main = defaultMain tests
