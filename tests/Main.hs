import qualified Format.Author

import           Test.Tasty

allTests :: TestTree
allTests = testGroup "Format" [Format.Author.tests]

main :: IO ()
main = defaultMain allTests
