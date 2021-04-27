module Crossref where


import           Data.Maybe                     ( fromJust )
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Aeson                    as Aeson
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text )
import           Abbot.Crossref
import           Abbot.Work


-- Testing parsing of various Crossref returned JSON.
-- Note that these are cached, i.e. the JSON is obtained from a file rather than dynamically via the
-- Internet. This is to make tests quicker and more reliable.
tests :: TestTree
tests = testGroup "Crossref" [testOL, testNRMP, testfixJShortNRMP]


auth :: (Text, Text) -> Author
auth (gvn, fmy) = Author (Just gvn) fmy


testOL :: TestTree
testOL = testCase "parseCrossrefValue_2019OL" checkOLJSON
 where
  checkOLJSON :: Assertion  --- equivalent to IO ()
  checkOLJSON = do
    olJSON <- fromJust . Aeson.decode <$> BL.readFile
      "tests/test-data/orglett.json"
    let Right (Article t a jL jS y v i p d) = parseCrossrefValue olJSON
    t
      @?= "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides"
    a @?= NE.fromList [mansfield, smith, yong, garry, anderson]
    jL @?= "Organic Letters"
    jS @?= "Org. Lett."
    y @?= 2019
    v @?= "21"
    i @?= "8"
    p @?= "2918-2922"
    d @?= "10.1021/acs.orglett.9b00971"
  mansfield, smith, yong, garry, anderson :: Author
  mansfield = auth ("Steven J.", "Mansfield")
  smith     = auth ("Russell C.", "Smith")
  yong      = auth ("Jonathan R. J.", "Yong")
  garry     = auth ("Olivia L.", "Garry")
  anderson  = auth ("Edward A.", "Anderson")


testNRMP :: TestTree
testNRMP = testCase "parseCrossrefValue_2021NRMP" checkNRMPJSON
 where
  checkNRMPJSON :: Assertion  --- equivalent to IO ()
  checkNRMPJSON = do
    nrmpJSON <- fromJust . Aeson.decode <$> BL.readFile
      "tests/test-data/nrmp.json"
    let Right (Article t a jL jS y v i p d) = parseCrossrefValue nrmpJSON
    t @?= "Parallel nuclear magnetic resonance spectroscopy"
    a @?= NE.fromList [kupce, frydman, webb, yong, claridge]
    jL @?= "Nature Reviews Methods Primers"
    jS @?= "Nat Rev Methods Primers"
    y @?= 2021
    v @?= "1"
    i @?= "1"
    p @?= ""  -- should be 27 but Crossref doesn't have it
    d @?= "10.1038/s43586-021-00024-3"
  kupce, frydman, webb, yong, claridge :: Author
  kupce    = auth ("Ēriks", "Kupče")
  frydman  = auth ("Lucio", "Frydman")
  webb     = auth ("Andrew G.", "Webb")
  yong     = auth ("Jonathan R. J.", "Yong")
  claridge = auth ("Tim D. W.", "Claridge")


testfixJShortNRMP :: TestTree
testfixJShortNRMP = testCase "fixJournalShortInWork_2021NRMP"
                             checkFixedNRMPJSON
 where
  checkFixedNRMPJSON :: Assertion  --- equivalent to IO ()
  checkFixedNRMPJSON = do
    nrmpJSON <- fromJust . Aeson.decode <$> BL.readFile
      "tests/test-data/nrmp.json"
    let Right (Article t a jL jS y v i p d) =
          fixJournalShortInWork defaultJournalShortMap
            <$> parseCrossrefValue nrmpJSON
    t @?= "Parallel nuclear magnetic resonance spectroscopy"
    a @?= NE.fromList [kupce, frydman, webb, yong, claridge]
    jL @?= "Nature Reviews Methods Primers"
    jS @?= "Nat. Rev. Methods Primers"
    y @?= 2021
    v @?= "1"
    i @?= "1"
    p @?= ""  -- should be 27 but Crossref doesn't have it
    d @?= "10.1038/s43586-021-00024-3"
  kupce, frydman, webb, yong, claridge :: Author
  kupce    = auth ("Ēriks", "Kupče")
  frydman  = auth ("Lucio", "Frydman")
  webb     = auth ("Andrew G.", "Webb")
  yong     = auth ("Jonathan R. J.", "Yong")
  claridge = auth ("Tim D. W.", "Claridge")
