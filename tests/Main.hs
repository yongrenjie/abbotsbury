module Main where

import qualified Cite.Helpers.Author

import           Data.Text                      ( Text )
import           Test.Tasty


import           Abbot.Crossref


allTests :: TestTree
allTests = testGroup "Cite" [Cite.Helpers.Author.tests]


main :: IO ()
main = defaultMain allTests


main' :: IO ()
main' = do
  getCrossrefJSON "yongrenjie@gmail.com" "10.1021/acs.orglett.9b0097" >>= print
