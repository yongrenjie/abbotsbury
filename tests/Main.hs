{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Cite.Helpers.Author
import qualified Crossref

import           Data.Text                      ( Text )
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Directory
import           System.FilePath


import           Abbot.Crossref                 ( fetchWork )


checkWorkingDirectory :: TestTree
checkWorkingDirectory = testCase "check working directory" $ do
  pwd <- getCurrentDirectory
  testDataDirExists <- doesDirectoryExist (pwd </> "tests" </> "test-data")
  assertBool "tests must be run from the project root directory" testDataDirExists


allTests :: TestTree
allTests = testGroup "Abbotsbury" [ checkWorkingDirectory
                                  , aft citeTests
                                  , aft Crossref.tests
                                  ]
  where
    -- Only run the other tests if the working directory is set correctly.
    aft :: TestTree -> TestTree
    aft = after AllSucceed "check working directory"
    -- The rest are tests organised by their imports.
    citeTests = testGroup "Cite" [helperTests]
      where
        helperTests = testGroup "Helpers" [Cite.Helpers.Author.tests]


main :: IO ()
main = defaultMain allTests


main' :: IO ()
main' = do
  fetchWork "yongrenjie@gmail.com" "10.1021/acs.orglett.9b0097" >>= print
