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
    Right (Work wt (Metadata t a jL jS y v i p d)) <- parseCrossrefJsonFromFile "tests/test-data/orglett.json"
    wt @?= JournalArticle
    t @?= "A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides"
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
testNRMP = testCase "parseCrossrefMessage - 2021 NRMP" checkNRMPJSON
 where
  checkNRMPJSON :: Assertion  --- equivalent to IO ()
  checkNRMPJSON = do
    Right (Work wt (Metadata t a jL jS y v i p d)) <- parseCrossrefJsonFromFile "tests/test-data/nrmp.json"
    wt @?= JournalArticle
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


testNoFirstName :: TestTree
testNoFirstName = testCase "parseCrossrefMessage - author without first name" checkNFNJSON
 where
  checkNFNJSON :: Assertion  --- equivalent to IO ()
  checkNFNJSON = do
    Right (Work wt (Metadata t a jL jS y v i p d)) <- parseCrossrefJsonFromFile "tests/test-data/nofirstname.json"
    wt @?= JournalArticle
    t @?= "Attention science: some people have only one name"
    a @?= NE.fromList [sheherazade, ardiantiono]
    jL @?= "Nature"
    jS @?= "Nature"
    y @?= 2020
    v @?= ""
    i @?= ""
    p @?= ""
    d @?= "10.1038/d41586-020-02761-z"
  sheherazade, ardiantiono :: Author
  sheherazade = Author Nothing "Sheherazade"
  ardiantiono = Author Nothing "Ardiantiono"


testNoJournalShort :: TestTree
testNoJournalShort =
  testCase "parseCrossrefMessage - no short journal name" checkNoJournalShortJSON
  where
    checkNoJournalShortJSON :: Assertion
    checkNoJournalShortJSON = do
      Right (Work wt (Metadata t a jL jS y v i p d)) <- parseCrossrefJsonFromFile "tests/test-data/science_oct.json"
      wt @?= JournalArticle
      t @?= "Unitary Control in Quantum Ensembles: Maximizing Signal Intensity in Coherent Spectroscopy"
      NE.length a @?= 1  -- Crossref gives entirely wrong data for this. It has 7 authors.
      NE.head a @?= auth ("S. J.", "Glaser")
      jL @?= "Science"
      jS @?= "Science"   -- not inside the Crossref JSON, so defaults to long name
      y @?= 1998
      v @?= "280"
      i @?= "5362"
      p @?= "421-424"
      d @?= "10.1126/science.280.5362.421"


testfixJShortNRMP :: TestTree
testfixJShortNRMP = testCase "fixJournalShortInWork - 2021 NRMP"
                             checkFixedNRMPJSON
 where
  checkFixedNRMPJSON :: Assertion
  checkFixedNRMPJSON = do
    work <- parseCrossrefJsonFromFile "tests/test-data/nrmp.json"
    let Right (Work wt (Metadata t a jL jS y v i p d)) = fixJournalShortInWork defaultJournalShortMap <$> work
    wt @?= JournalArticle
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
