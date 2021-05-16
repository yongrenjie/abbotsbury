module Crossref where

import           Abbotsbury.Crossref
import           Abbotsbury.Crossref.Internal
import           Abbotsbury.Work
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified TestWorks                     as TW

-- Testing parsing of various Crossref returned JSON.
-- Note that these are cached, i.e. the JSON is obtained from a file rather than dynamically via the
-- Internet. This is to make tests quicker and more reliable.
tests :: TestTree
tests = testGroup
  "Crossref"
  [testOL, testNRMP, testNoFirstName, testNoJournalShort]

testOL :: TestTree
testOL = testCase "parseCrossrefMessage - 2019 OL" checkOLJSON
 where
  checkOLJSON :: Assertion --- equivalent to IO ()
  checkOLJSON = do
    work <- fetchWorkFile "tests/test-data/orglett.json"
    work @?= Right TW.orgLett

testNRMP :: TestTree
testNRMP = testCase "parseCrossrefMessage - 2021 NRMP" checkNRMPJSON
 where
  checkNRMPJSON :: Assertion --- equivalent to IO ()
  checkNRMPJSON = do
    work <- fetchWorkFile "tests/test-data/nrmp.json" 
    work
      @?= Right TW.nrmpCorrected

testNoFirstName :: TestTree
testNoFirstName = testCase "parseCrossrefMessage - author without first name"
                           checkNFNJSON
 where
  checkNFNJSON :: Assertion
  checkNFNJSON = do
    work <- fetchWorkFile "tests/test-data/nofirstname.json"
    work @?= Right TW.noFirstNameN2020

testNoJournalShort :: TestTree
testNoJournalShort = testCase "parseCrossrefMessage - no short journal name"
                              checkNoJournalShortJSON
 where
  checkNoJournalShortJSON :: Assertion
  checkNoJournalShortJSON = do
    work <- fetchWorkFile "tests/test-data/science_oct.json"
    work @?= Right TW.glaserS1998
