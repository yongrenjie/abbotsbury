{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Cite.Helpers.Author
import qualified Crossref

import           Data.Text                      ( Text )
import           Test.Tasty


allTests :: TestTree
allTests = testGroup "Abbotsbury" [ citeTests
                                  , Crossref.tests
                                  ]
  where
    citeTests = testGroup "Cite" [helperTests]
      where
        helperTests = testGroup "Helpers" [Cite.Helpers.Author.tests]


main :: IO ()
main = defaultMain allTests
