import qualified Cite.Helpers.Author

import           Test.Tasty

allTests :: TestTree
allTests = testGroup "Cite" [Cite.Helpers.Author.tests]

main :: IO ()
main = defaultMain allTests
