module Cite
  ( tests
  ) where

import           Abbotsbury
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text                     as T
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified TestWorks                     as TW

-- | This is the only test for which we will exhaustively check all formats. For
-- other tests we can just check Markdown alone.
testCite1 :: TestTree
testCite1 = testGroup "cite - work 1 (all styles)"
                      [acsText, acsMarkdown, acsRestructured, acsHtml, bib, acsShortMarkdown]
 where
  orgLett :: Work
  orgLett = TW.testWorks IM.! 1
  acsText :: TestTree
  acsText    = testCase "ACS/Text" $ textActual @?= textExpected
  textActual = cite acsStyle textFormat (TW.testWorks IM.! 1)
  textExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. Org. Lett. 2019, 21 (8), 2918–2922. DOI: 10.1021/acs.orglett.9b00971."
  acsMarkdown :: TestTree
  acsMarkdown    = testCase "ACS/Markdown" $ markdownActual @?= markdownExpected
  markdownActual = cite acsStyle markdownFormat orgLett
  markdownExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918–2922. DOI: [10.1021/acs.orglett.9b00971](https://doi.org/10.1021/acs.orglett.9b00971)."
  acsRestructured :: TestTree
  acsRestructured =
    testCase "ACS/reStructuredText"
      $   restructuredActual
      @?= restructuredExpected
  restructuredActual = cite acsStyle restructuredFormat orgLett
  restructuredExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. *Org. Lett.* **2019,** *21* (8), 2918–2922. DOI: `10.1021/acs.orglett.9b00971 <https://doi.org/10.1021/acs.orglett.9b00971>`_."
  acsHtml :: TestTree
  acsHtml    = testCase "ACS/HTML" $ htmlActual @?= htmlExpected
  htmlActual = cite acsStyle htmlFormat orgLett
  htmlExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides. <i>Org. Lett.</i> <b>2019,</b> <i>21</i> (8), 2918–2922. DOI: <a href=\"https://doi.org/10.1021/acs.orglett.9b00971\">10.1021/acs.orglett.9b00971</a>."
  bib :: TestTree
  bib         = testCase "BibLaTeX" $ bibActual @?= bibExpected
  bibActual   = cite bibStyle textFormat orgLett
  bibExpected = T.intercalate
    "\n"
    [ "@article{Mansfield2019OL,"
    , "    doi = {10.1021/acs.orglett.9b00971},"
    , "    author = {Mansfield, Steven J. and Smith, Russell C. and Yong, Jonathan R.\\ J. and Garry, Olivia L. and Anderson, Edward A.},"
    , "    title = {A General Copper-Catalyzed Synthesis of Ynamides from 1,2-Dichloroenamides},"
    , "    journaltitle = {Org.\\ Lett.},"
    , "    year = {2019},"
    , "    volume = {21},"
    , "    number = {8},"
    , "    pages = {2918--2922},"
    , "}"
    ]
  acsShortMarkdown :: TestTree
  acsShortMarkdown = testCase "ACS-Short/Markdown" $ asMdActual @?= asMdExpected
  asMdActual = cite acsShortStyle markdownFormat orgLett
  asMdExpected
    = "Mansfield, S. J.; Smith, R. C.; Yong, J. R. J.; Garry, O. L.; Anderson, E. A. *Org. Lett.* **2019,** *21* (8), 2918–2922."

testCite2 :: TestTree
testCite2 = testCase "cite - work2" $ actual @?= expected
 where
  actual = cite acsStyle markdownFormat (TW.testWorks IM.! 2)
  expected
    = "Kupče, Ē.; Frydman, L.; Webb, A. G.; Yong, J. R. J.; Claridge, T. D. W. Parallel nuclear magnetic resonance spectroscopy. *Nat. Rev. Methods Primers* **2021,** *1* (1), No. 27. DOI: [10.1038/s43586-021-00024-3](https://doi.org/10.1038/s43586-021-00024-3)."

testCite5 :: TestTree
testCite5 = testCase "cite - work5" $ actual @?= expected
 where
  actual = cite acsStyle markdownFormat (TW.testWorks IM.! 5)
  expected
    = "Smith, A. B., III. Twenty Years of Organic Letters, A Look Back...and Forward. *Org. Lett.* **2018,** *20* (1), 1–3. DOI: [10.1021/acs.orglett.7b03845](https://doi.org/10.1021/acs.orglett.7b03845)."

testCite6 :: TestTree
testCite6 = testCase "cite - work6 (corrected metadata)" $ actual @?= expected
 where
  -- using set over .~ because that avoids nesting parentheses
  correctedWork = TW.testWorks IM.! 6 & _book %~ ( set number "98"
                                                 . set publisherLoc "Cham, Switzerland"
                                                 )
  actual = cite acsStyle markdownFormat correctedWork
  expected
    = "Gatti, F.; Lasorne, B.; Meyer, H.-D.; Nauts, A. *Applications of Quantum Dynamics in Chemistry;* Lecture Notes in Chemistry 98; Springer International Publishing: Cham, Switzerland, 2017."

tests :: TestTree
tests = testGroup "Cite" [testCite1, testCite2, testCite5, testCite6]
