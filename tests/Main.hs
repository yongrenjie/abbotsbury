import qualified Reference

import           Test.Tasty

allTests :: TestTree
allTests = testGroup "Reference" [Reference.tests]

h :: Int
h = (1)

main :: IO ()
main = defaultMain allTests
