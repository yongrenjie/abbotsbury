module Crossref where

import qualified TestWorks                     as TW

import           Data.Maybe                     ( fromJust )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Lens.Micro
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Aeson                    as Aeson
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import           Abbotsbury.Crossref
import           Abbotsbury.Crossref.Internal
import           Abbotsbury.Work


-- Testing parsing of various Crossref returned JSON.
-- Note that these are cached, i.e. the JSON is obtained from a file rather than dynamically via the
-- Internet. This is to make tests quicker and more reliable.
tests :: TestTree
tests = testGroup "Crossref" [ testOL
                             , testNRMP
                             , testNoFirstName
                             , testNoJournalShort
                             , testfixJShortNRMP
                             ]


auth :: (Text, Text) -> Author
auth (gvn, fmy) = Author (Just gvn) fmy


parseCrossrefJsonFromFile :: FilePath -> IO (Either CrossrefException Work)
parseCrossrefJsonFromFile fp = do
  jsonValue <- fromJust . Aeson.decode <$> BL.readFile fp
  pure $ getJsonMessage jsonValue >>= parseCrossrefMessage


testOL :: TestTree
testOL = testCase "parseCrossrefMessage - 2019 OL" checkOLJSON
 where
  checkOLJSON :: Assertion  --- equivalent to IO ()
  checkOLJSON = do
    work <- parseCrossrefJsonFromFile "tests/test-data/orglett.json"
    work @?= Right TW.orgLett


testNRMP :: TestTree
testNRMP = testCase "parseCrossrefMessage - 2021 NRMP" checkNRMPJSON
 where
  checkNRMPJSON :: Assertion  --- equivalent to IO ()
  checkNRMPJSON = do
    work <- parseCrossrefJsonFromFile "tests/test-data/nrmp.json"
    work @?= Right (TW.nrmpCorrected & journalShort .~ "Nat Rev Methods Primers")


testNoFirstName :: TestTree
testNoFirstName = testCase "parseCrossrefMessage - author without first name" checkNFNJSON
 where
  checkNFNJSON :: Assertion
  checkNFNJSON = do
    work <- parseCrossrefJsonFromFile "tests/test-data/nofirstname.json"
    work @?= Right TW.noFirstNameN2020


testNoJournalShort :: TestTree
testNoJournalShort =
  testCase "parseCrossrefMessage - no short journal name" checkNoJournalShortJSON
  where
    checkNoJournalShortJSON :: Assertion
    checkNoJournalShortJSON = do
      work <- parseCrossrefJsonFromFile "tests/test-data/science_oct.json"
      work @?= Right TW.glaserS1998


testfixJShortNRMP :: TestTree
testfixJShortNRMP = testCase "fixJournalShortInWork - 2021 NRMP"
                             checkFixedNRMPJSON
 where
  checkFixedNRMPJSON :: Assertion
  checkFixedNRMPJSON = do
    work <- parseCrossrefJsonFromFile "tests/test-data/nrmp.json"
    let fixedWork = fixJournalShortInWork defaultJournalFix <$> work
    fixedWork @?= Right TW.nrmpCorrected
