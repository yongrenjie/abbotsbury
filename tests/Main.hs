{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Abbotsbury.Crossref            ( fetchWork )
import qualified Cite
import qualified Cite.Helpers.Author
import qualified Crossref
import           Data.Text                      ( Text )
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit

checkWorkingDirectory :: TestTree
checkWorkingDirectory = testCase "check working directory" $ do
  pwd               <- getCurrentDirectory
  testDataDirExists <- doesDirectoryExist (pwd </> "tests" </> "test-data")
  assertBool "tests must be run from the project root directory"
             testDataDirExists

allTests :: TestTree
allTests = testGroup
  "Abbotsbury"
  [checkWorkingDirectory, aft citeTests, aft Crossref.tests]
 where
    -- Only run the other tests if the working directory is set correctly.
  aft :: TestTree -> TestTree
  aft       = after AllSucceed "check working directory"
  -- The rest are tests organised by their imports.
  citeTests = testGroup "Cite" [Cite.tests, helperTests]
    where helperTests = testGroup "Helpers" [Cite.Helpers.Author.tests]

main :: IO ()
main = defaultMain allTests
