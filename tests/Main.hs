import qualified Cite.Author

import           Test.Tasty

allTests :: TestTree
allTests = testGroup "Cite" [Cite.Author.tests]

main :: IO ()
main = defaultMain allTests
